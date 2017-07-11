-module(rebar3_coverme_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, coverme).
-define(DEPS, [lock, app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 coverme"},  % How to use the plugin
            {opts, coverme_opts(State)},  % list of options understood by the plugin
            {short_desc, "generate coverme report"},
            {profiles, [test]},
            {desc, "Process the .coverdata file and produce a compact representation of covered ann uncovered lines"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    CoverDir = filename:join([rebar_dir:base_dir(State), "cover"]),
    InputFiles = input_files(CoverDir),
    OutputFile = output_file(CoverDir),
    Apps = rebar_state:project_apps(State),
    ExclApps = excl_apps(State),
    ExclMods = excl_mods(State),
    Verbose = verbose(State),
    case generate(InputFiles, OutputFile, Apps, ExclApps, ExclMods, Verbose) of
        ok ->
            {ok, State};
        Error ->
            {error, {?MODULE, Error}}
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

output_file(CoverDir) ->
    filename:join([CoverDir, "coverme"]).

input_files(CoverDir) ->
    CoverDataFiles = ["eunit.coverdata", "ct.coverdata"],
    FullPaths = [filename:join([CoverDir, File]) || File <- CoverDataFiles],
    filter_existing_inputs(FullPaths).

filter_existing_inputs([]) ->
    [];
filter_existing_inputs([H|T]) ->
    case file_exists(H) of
        true ->
            [H|filter_existing_inputs(T)];
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

generate([], _, _, _, _, _) ->
    rebar_api:warn("No coverdata found", []),
    ok;

generate(InputFiles, OutputFile, Apps, ExclApps, ExclMods, Verbose) ->
    rebar_api:info("Performing cover analysis...", []),
    case rebar3_coverage:init(InputFiles, "_build/test/covertool.log") of
        {ok, F} ->
            Coverage = generate_apps(Apps, ExclApps, ExclMods),
            OutputCoverage = output_coverage(Coverage),
            ok = file:write_file(OutputFile, OutputCoverage),
            print_analysis(Verbose, Coverage),
            print_summary(Verbose, InputFiles, OutputFile),
            rebar3_coverage:finish(F);
        Otherwise ->
            Otherwise
    end.

generate_apps(Apps, ExclApps, ExclMods) ->
    {ExclApps, ExclMods, Coverage} = lists:foldl(fun generate_app/2, {ExclApps, ExclMods, #{}}, Apps),
    Coverage.

generate_app(App, {ExclApps, ExclMods, Result}) ->
    AppName = rebar_app_info:name(App),
    case lists:member(AppName, ExclApps) of
        false ->
            %AppName = binary_to_atom(rebar_app_info:name(App), latin1),
            SourceDir = filename:join(rebar_app_info:dir(App), "src/"),
            SourceFiles = filelib:wildcard(SourceDir ++ "/**/*.erl"),
            Modules = modules(SourceFiles),
            CoverModules = maps:keys(Modules) -- ExclMods,
            %rebar_api:info("Analyzing ~p", [AppName]),
            Coverage = rebar3_coverage:analyze(CoverModules),
            Fun = fun(Module, {CoveredLines, UncoveredLines}, Acc) ->
                          File = maps:get(Module, Modules),
                          maps:put(Module, {File, CoveredLines, UncoveredLines}, Acc)
                  end,
            {ExclApps, ExclMods, maps:fold(Fun, Result, Coverage)};
        true ->
            {ExclApps, ExclMods, Result}
    end.

print_summary(Verbose, InputFiles, OutputFile) ->
    VerboseSummary = case Verbose of
                         true ->
                             [io_lib:format("  coverage calculated from:~n", []),
                              lists:map(fun(File) ->
                                                io_lib:format("    ~ts~n", [File])
                                        end, InputFiles)];
                         false ->
                             []
                     end,
    InfoSummary = io_lib:format("  coverage info written to: ~s~n",
                                [OutputFile]),
    io:format([VerboseSummary, InfoSummary]).

print_analysis(true, Coverage) when map_size(Coverage) > 0 ->
    Fun = fun(Module, {_File, CoveredLines, UncoveredLines},
                      {Acc, TotalCovered, TotalUncovered}) ->
                  CoveredCount = length(CoveredLines),
                  UncoveredCount = length(UncoveredLines),
                  ModulePercentage = cover_percentage(CoveredCount, UncoveredCount),
                  Acc1 = [{Module, ModulePercentage} | Acc],
                  TotalCovered1 = TotalCovered + CoveredCount,
                  TotalUncovered1 = TotalUncovered + UncoveredCount,
                  {Acc1, TotalCovered1, TotalUncovered1}
          end,
    {ByModule, TotalCovered, TotalUncovered} = maps:fold(Fun, {[], 0, 0}, Coverage),
    TotalPercentage = cover_percentage(TotalCovered, TotalUncovered),
    io:format(format_table(lists:sort(ByModule), TotalPercentage));

print_analysis(_, _) ->
    ok.

cover_percentage(0, 0) ->
    0.0;
cover_percentage(CoveredCount, UncoveredCount) ->
    100 * CoveredCount / (CoveredCount + UncoveredCount).

format_table(ByModule, Total) ->
    MaxLength = max(lists:foldl(fun max_length/2, 0, ByModule), 20),
    Header = header(MaxLength),
    Separator = separator(MaxLength),
    TotalLabel = format("total", MaxLength),
    TotalStr = format(float_to_list(Total, [{decimals, 2}]) ++ "%", 8),
    ModuleFun = fun({Mod, Coverage}) ->
                        Name = format(atom_to_list(Mod), MaxLength),
                        Cov = format(float_to_list(Coverage, [{decimals, 2}]) ++ "%", 8),
                        io_lib:format("  |  ~ts  |  ~ts  |~n", [Name, Cov])
                end,
    ByModuleStr = lists:map(ModuleFun, ByModule),
    [io_lib:format("~ts~n~ts~n~ts~n", [Separator, Header, Separator]),
        ByModuleStr,
        io_lib:format("~ts~n", [Separator]),
        io_lib:format("  |  ~ts  |  ~ts  |~n", [TotalLabel, TotalStr]),
        io_lib:format("~ts~n", [Separator])].

max_length({Module, _}, Min) ->
    Length = length(atom_to_list(Module)),
    case Length > Min of
        true  -> Length;
        false -> Min
    end.

header(Width) ->
    ["  |  ", format("module", Width), "  |  ", format("coverage", 8), "  |"].

separator(Width) ->
    ["  |--", io_lib:format("~*c", [Width, $-]), "--|------------|"].

format(String, Width) ->
    io_lib:format("~*.ts", [Width, String]).

format_lines(Lines) ->
    string:join(lists:map(fun integer_to_list/1, Lines), ",").

output_coverage(Coverage) ->
    Fun = fun(_Module, {File, CoveredLines, UncoveredLines}, Acc) ->
                  CoveredLinesString = format_lines(CoveredLines),
                  UncoveredLinesString = format_lines(UncoveredLines),
                  Arguments = [File, CoveredLinesString, UncoveredLinesString],
                  FileString = string:join(Arguments, ";") ++ ";\n",
                  [FileString | Acc]
          end,
    maps:fold(Fun, [], Coverage).

modules(Filenames) ->
    maps:from_list(lists:map(fun module/1, Filenames)).
module(Filename) ->
    {list_to_atom(filename:basename(Filename, ".erl")), Filename}.

coverme_opts(_State) ->
    [{verbose, $v, "verbose", boolean, help(verbose)}].

help(verbose) -> "Print summary".

command_line_opts(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    Opts.

config_opts(State) ->
    rebar_state:get(State, coverme_opts, []).

excl_mods(State) ->
    proplists:get_value(excl_mods, config_opts(State), []).

excl_apps(State) ->
    proplists:get_value(excl_apps, config_opts(State), []).

verbose(State) ->
    Command = proplists:get_value(verbose, command_line_opts(State), undefined),
    Config = proplists:get_value(verbose, config_opts(State), undefined),
    case {Command, Config} of
        {undefined, undefined} -> false;
        {undefined, Verbose}   -> Verbose;
        {Verbose, _}           -> Verbose
    end.
