-module(rebar3_coverage).

-export([init/1, analyze/1]).

init(Files) ->
    cover:start(),
    case import(Files) of
      ok ->
          ok;
      Otherwise ->
          Otherwise
    end.

import([]) ->
    ok;
import([File | T]) ->
    case cover:import(File) of
      ok ->
          import(T);
      Otherwise ->
          Otherwise
    end.

rate(#{covered := Covered, total := Total}) ->
    case Total of
      0 ->
          0.0;
      Total ->
          round(Covered / Total * 1000) / 1000
    end.

analyze_app(AppName,
            AppResult = #{modules := Modules},
            ResultAcc = #{covered := CoveredAcc, total := TotalAcc, apps := Apps}) ->
    {result, Calls, _Errors} = cover:analyze(maps:keys(Modules), calls, line),
    ModuleFun = fun ({{Module, Line}, LineCalls},
                     #{covered := AppCovered, total := AppTotal, modules := AppModules}) ->
                        CoveredInc = case LineCalls of
                                       0 ->
                                           0;
                                       _ ->
                                           1
                                     end,
                        UpdateFun = fun (ModuleInfo = #{lines := Lines,
                                                        covered := Covered,
                                                        total := Total}) ->
                                            Lines1 = [{Line, LineCalls} | Lines],
                                            ModuleInfo#{lines => Lines1,
                                                        covered => Covered + CoveredInc,
                                                        total => Total + 1}
                                    end,
                        AppModules1 = maps:update_with(Module, UpdateFun, AppModules),
                        #{covered => AppCovered + CoveredInc,
                          total => AppTotal + 1,
                          modules => AppModules1}
                end,
    FinishFun = fun (_, M = #{lines := Lines}) ->
                        M#{lines => lists:sort(Lines), rate => rate(M)}
                end,
    AppResult1 = #{covered := AppCovered, total := AppTotal, modules := ModulesInfo} =
                     lists:foldl(ModuleFun, AppResult, Calls),
    AppResult2 = AppResult1#{rate => rate(AppResult1),
                             modules => maps:map(FinishFun, ModulesInfo)},
    Apps1 = maps:put(AppName, AppResult2, Apps),
    ResultAcc1 = ResultAcc#{covered => CoveredAcc + AppCovered,
                            total => TotalAcc + AppTotal,
                            apps => Apps1},
    ResultAcc1#{rate => rate(ResultAcc1)}.

analyze(CoverageInfo = #{apps := Apps}) ->
    maps:fold(fun analyze_app/3, CoverageInfo, Apps).

