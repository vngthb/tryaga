-module(tryaga).

-define(DEFAULT_FACTOR, 0.66).

-export([
    retry/4,
    retry/5
]).

retry(Function, Predicate, Attempts, Duration) ->
    retry(Function, Predicate, Attempts, Duration, ?DEFAULT_FACTOR).

retry(Function, Predicate, Attempts, Duration, Factor) ->
    Base = Duration * (1 - Factor) / (1 - math:pow(Factor, Attempts)),
    retry(internal, Function, Predicate, Attempts, Base, Factor).

retry(internal, Function, Predicate, 0, _, _) ->
    Result = Function(),
    case Predicate(Result) of
        true -> {ok, Result};
        false -> {error, Result}
    end;
retry(internal, Function, Predicate, Attempts, Duration, Factor) ->
    Result = Function(),
    case Predicate(Result) of
        true -> {ok, Result};
        false ->
            Timeout0 = Duration * math:pow(Factor, Attempts - 1),
            Timeout1 = round(Timeout0),
            timer:sleep(Timeout1),
            retry(internal, Function, Predicate, Attempts - 1, Duration, Factor)
    end.