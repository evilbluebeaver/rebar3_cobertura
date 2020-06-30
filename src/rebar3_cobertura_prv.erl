-module(rebar3_cobertura_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, cobertura).
-define(DEPS, [lock, app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    ProviderOptions = [{name, ?PROVIDER},            % The 'user friendly' name of the task
                       {module, ?MODULE},            % The module implementation of the task
                       {bare, true},                 % The task can be run by the user, always true
                       {deps, ?DEPS},                % The list of dependencies
                       {example, "rebar3 cobertura"},  % How to use the plugin
                       {opts, cobertura_opts(State)},  % list of options understood by the plugin
                       {short_desc, "generate cobertura coverage report"},
                       {profiles, [test]},
                       {desc,
                        "Process the cover log file and produce a cobertura-compatible xml"
                        }],
    Provider = providers:create(ProviderOptions),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    CoverDir = filename:join([rebar_dir:base_dir(State), "cover"]),
    RootDir = filename:dirname(rebar_dir:root_dir(State)) ++ "/",
    InputFiles = input_files(CoverDir),
    OutputFile = output_file(CoverDir),
    Apps = rebar_state:project_apps(State),
    ExclApps = excl_apps(State),
    ExclMods = excl_mods(State),
    Verbose = verbose(State),
    case generate(RootDir, InputFiles, OutputFile, Apps, ExclApps, ExclMods, Verbose) of
      ok ->
          {ok, State};
      Error ->
          {error, {?MODULE, Error}}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

output_file(CoverDir) ->
    filename:join([CoverDir, "cov.xml"]).

input_files(CoverDir) ->
    CoverDataFiles = ["eunit.coverdata", "ct.coverdata"],
    FullPaths = [filename:join([CoverDir, File]) || File <- CoverDataFiles],
    filter_existing_inputs(FullPaths).

filter_existing_inputs([]) ->
    [];
filter_existing_inputs([H | T]) ->
    case file_exists(H) of
      true ->
          [H | filter_existing_inputs(T)];
      false ->
          %rebar_api:info("Skipping non-existing file ~s", [H]),
          filter_existing_inputs(T)
    end.

file_exists(Filename) ->
    case file:read_file_info(Filename) of
      {ok, _} ->
          true;
      {error, enoent} ->
          false;
      Reason ->
          exit(Reason)
    end.

generate(_, [], _, _, _, _, _) ->
    rebar_api:warn("No coverdata found", []),
    ok;
generate(RootDir, InputFiles, OutputFile, Apps, ExclApps, ExclMods, Verbose) ->
    rebar_api:info("Performing cover analysis...", []),
    case rebar3_coverage:init(InputFiles) of
      ok ->
          Coverage = generate_apps(RootDir, Apps, ExclApps, ExclMods),
          OutputCoverage = output_coverage(Coverage),
          ok = file:write_file(OutputFile, OutputCoverage),
          print_analysis(Verbose, Coverage),
          print_summary(Verbose, InputFiles, OutputFile);
      Otherwise ->
          Otherwise
    end.

-define(DEFAULT_COVERAGE_INFO, #{covered => 0, total => 0, rate => 0}).

generate_apps(RootDir, Apps, ExclApps, ExclMods) ->
    Fun = fun (App, AppsAcc) ->
                  AppName = binary_to_atom(rebar_app_info:name(App), utf8),
                  case lists:member(AppName, ExclApps) of
                    false ->
                        SourceDir = filename:join(rebar_app_info:dir(App), "src/"),
                        SourceFiles = filelib:wildcard(SourceDir ++ "/**/*.erl"),
                        Modules = modules(RootDir, SourceFiles, ExclMods),
                        AppsAcc1 = maps:put(AppName,
                                            ?DEFAULT_COVERAGE_INFO#{modules => Modules},
                                            AppsAcc),
                        AppsAcc1;
                    true ->
                        AppsAcc
                  end
          end,
    AppsInfo = lists:foldl(Fun, #{}, Apps),
    Coverage = generate_coverage(?DEFAULT_COVERAGE_INFO#{apps => AppsInfo}),
    Coverage#{src_dirs => ["."]}.

generate_coverage(AppsInfo) ->
    rebar3_coverage:analyze(AppsInfo).

print_summary(Verbose, InputFiles, OutputFile) ->
    VerboseSummary = case Verbose of
                       true ->
                           [io_lib:format("  coverage calculated from:~n", []),
                            lists:map(fun (File) ->
                                              io_lib:format("    ~ts~n", [File])
                                      end,
                                      InputFiles)];
                       false ->
                           []
                     end,
    InfoSummary = io_lib:format("  coverage info written to: ~s~n", [OutputFile]),
    io:format([VerboseSummary, InfoSummary]).

print_analysis(true, Coverage) ->
    Output = format_table(Coverage),
    io:format(Output);
print_analysis(_, _) ->
    ok.

format_table(#{rate := Rate, apps := Apps}) ->
    MaxLength = 30,
    Separator = separator(MaxLength),
    TotalLabel = format("total", MaxLength),
    TotalStr = format(format_percent(Rate), 8),
    ModuleFun = fun (Module, #{rate := ModuleRate}, Acc) ->
                        Name = format(atom_to_list(Module), MaxLength),
                        Cov = format(float_to_list(ModuleRate * 100, [{decimals, 2}]) ++ "%", 8),
                        [io_lib:format("  |  ~ts  |  ~ts  |~n", [Name, Cov]) | Acc]
                end,
    PackageFun = fun (App, #{rate := AppRate, modules := Modules}, Acc) ->
                         Name = format(atom_to_list(App), MaxLength),
                         Cov = format(format_percent(AppRate), 8),
                         ModulesStr = maps:fold(ModuleFun, [], Modules),
                         [io_lib:format("~n~n", []),
                          io_lib:format("~ts~n", [Separator]),
                          ModulesStr,
                          io_lib:format("~ts~n", [Separator]),
                          io_lib:format("  |  ~ts  |  ~ts  |~n", [Name, Cov]),
                          io_lib:format("~ts~n", [Separator])
                          | Acc]
                 end,
    ResultStr = maps:fold(PackageFun, [], Apps),

    [lists:reverse(ResultStr),
     io_lib:format("~ts~n", [Separator]),
     io_lib:format("  |  ~ts  |  ~ts  |~n", [TotalLabel, TotalStr]),
     io_lib:format("~ts~n", [Separator])].

separator(Width) ->
    ["  |--", io_lib:format("~*c", [Width, $-]), "--|------------|"].

format(String, Width) ->
    io_lib:format("~*.ts", [Width, String]).

format_rate(Rate) ->
    float_to_list(Rate, [{decimals, 3}, compact]).

format_percent(Rate) ->
    float_to_list(Rate * 100, [{decimals, 2}]) ++ "%".

output_coverage(#{src_dirs := SrcDirs,
                  covered := Covered,
                  total := Total,
                  rate := Rate,
                  apps := Apps}) ->
    Prolog = ["<?xml version=\"1.0\" encoding=\"utf-8\"?>\n",
              "<!DOCTYPE coverage SYSTEM \"http://cobertura.sourceforge.net/xml/cov"
              "erage-04.dtd\">\n"],
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    Timestamp = MegaSecs * 1000000000 + Secs * 1000 + MicroSecs div 1000, % in milliseconds

    Version = "1.9.4.1", % emulate Cobertura 1.9.4.1
    Sources = [{source, [SrcDir]} || SrcDir <- SrcDirs],

    Root = {coverage,
            [{timestamp, Timestamp},
             {'line-rate', format_rate(Rate)},
             {'lines-covered', Covered},
             {'lines-valid', Total},
             {'branch-rate', "0.0"},
             {'branches-covered', "0"},
             {'branches-valid', "0"},
             {complexity, "0"},
             {version, Version}],
            [{sources, Sources}, {packages, get_packages(Apps)}]},
    xmerl:export_simple([Root], xmerl_xml, [{prolog, Prolog}]).

get_packages(Packages) ->
    PackageFun = fun (App, #{rate := PackageRate, modules := Modules}, Acc) ->
                         Package = {package,
                                    [{name, App},
                                     {'line-rate', format_rate(PackageRate)},
                                     {'branch-rate', "0.0"},
                                     {complexity, "0"}],
                                    [{classes, get_classes(Modules)}]},
                         [Package | Acc]
                 end,
    maps:fold(PackageFun, [], Packages).

get_classes(Modules) ->
    ClassFun = fun (Module, #{file := File, rate := ModuleRate, lines := Lines}, Acc) ->
                       Class = {class,
                                [{name, Module},
                                 {filename, File},
                                 {'line-rate', format_rate(ModuleRate)},
                                 {'branch-rate', "0.0"},
                                 {complexity, "0"}],
                                [{methods, []}, {lines, get_lines(Lines)}]},
                       [Class | Acc]
               end,
    maps:fold(ClassFun, [], Modules).

get_lines(Lines) ->
    LinesFun = fun ({Line, Calls}) ->
                       {line, [{number, Line}, {hits, Calls}, {branch, "False"}], []}
               end,
    lists:map(LinesFun, Lines).

modules(SourceDir, Filenames, Excludes) ->
    Fun = fun (Filename, Acc) ->
                  Module = list_to_atom(filename:basename(Filename, ".erl")),
                  case lists:member(Module, Excludes) of
                    false ->
                        RelativeFilename = Filename -- SourceDir,
                        maps:put(Module,
                                 ?DEFAULT_COVERAGE_INFO#{lines => [], file => RelativeFilename},
                                 Acc);
                    true ->
                        Acc
                  end
          end,
    lists:foldl(Fun, #{}, Filenames).

cobertura_opts(_State) ->
    [{verbose, $v, "verbose", boolean, help(verbose)}].

help(verbose) ->
    "Print summary".

command_line_opts(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    Opts.

config_opts(State) ->
    rebar_state:get(State, cobertura_opts, []).

excl_mods(State) ->
    proplists:get_value(excl_mods, config_opts(State), []).

excl_apps(State) ->
    proplists:get_value(excl_apps, config_opts(State), []).

verbose(State) ->
    Command = proplists:get_value(verbose, command_line_opts(State), undefined),
    Config = proplists:get_value(verbose, config_opts(State), undefined),
    case {Command, Config} of
      {undefined, undefined} ->
          false;
      {undefined, Verbose} ->
          Verbose;
      {Verbose, _} ->
          Verbose
    end.

