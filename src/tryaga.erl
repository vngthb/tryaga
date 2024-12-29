-module(tryaga).

-export([
    retry/4
]).


retry(Function, Predicate, Attempts, Duration0) ->
    GrowthRatio = 0.66,
    Duration1 = Duration0 * (1 - GrowthRatio) / (1 - math:pow(GrowthRatio, Attempts)),
    retry(Function, Predicate, Attempts, Duration1, GrowthRatio).

retry(Function, Predicate, 0, _, _) ->
    Result = Function(),
    case Predicate(Result) of
        true -> {ok, Result};
        false -> {error, Result}
    end;
retry(Function, Predicate, Attempts, Duration, GrowthRatio) ->
    Result = Function(),
    case Predicate(Result) of
        true -> {ok, Result};
        false ->
            Sleep0 = Duration * math:pow(GrowthRatio, Attempts - 1),
            Sleep1 = round(Sleep0),
            timer:sleep(Sleep1),
            retry(Function, Predicate, Attempts - 1, Duration, GrowthRatio)
    end.