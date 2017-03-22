-module(rebar3_coverage).

-export([init/2,
         finish/1,
         analyze/1]).

init(Files, LogFilePath) ->
    case logfile(LogFilePath) of
        {ok, LogFile} ->
            case import(Files) of
                ok ->
                    {ok, LogFile};
                Otherwise ->
                    file:close(LogFile),
                    Otherwise
            end;
        Otherwise ->
            Otherwise
    end.

finish(LogFile) ->
    file:close(LogFile).

import([]) ->
    ok;
import([File | T]) ->
    case cover:import(File) of
        ok ->
            import(T);
        Otherwise ->
            Otherwise
    end.

logfile(CoverLog) ->
    {ok, CoverPid} = case cover:start() of
                         {ok, _P} = OkStart ->
                             OkStart;
                         {error,{already_started, P}} ->
                             {ok, P};
                         {error, _Reason} = ErrorStart ->
                             ErrorStart
                     end,
    ok = filelib:ensure_dir(CoverLog),
    {ok, F} = file:open(CoverLog, [write, append]),
    group_leader(F, CoverPid),
    {ok, F}.

analyze(Modules) ->
    Default = lists:foldl(fun(Module, Acc) ->
                                  maps:put(Module, {[], []}, Acc)
                          end, #{}, Modules),
    {result, Coverage, _Errors} = cover:analyze(Modules, coverage, line),
    ModuleFun = fun({{Module, Line}, CoverResult}, CoverageAcc) ->
                        UpdateFun = fun({Covered, Uncovered}) ->
                                            case CoverResult of
                                                {1, 0} ->
                                                    {[Line | Covered], Uncovered};
                                                {0, 1} ->
                                                    {Covered, [Line | Uncovered]}
                                            end
                                    end,
                        maps:update_with(Module, UpdateFun, {[], []}, CoverageAcc)
                end,
    SortFun = fun(_, {Covered, Uncovered}) ->
                      {lists:sort(Covered), lists:sort(Uncovered)}
              end,
    maps:map(SortFun, lists:foldl(ModuleFun, Default, Coverage)).

