-module(tryaga).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_FACTOR, 0.66).

-export([
    retry/4,
    retry/5
]).

retry(Function, Predicate, Retries, Duration) ->
    retry(Function, Predicate, Retries, Duration, ?DEFAULT_FACTOR).

retry(Function, Predicate, 0, _, _) ->
    retry(internal, Function, Predicate, 0, undefined, undefined);
retry(Function, Predicate, Retries, Duration, Factor) ->
    Base = Duration * (1 - Factor) / (1 - math:pow(Factor, Retries)),
    retry(internal, Function, Predicate, Retries, Base, Factor).

retry(internal, Function, Predicate, Retries, Duration, Factor) ->
    Result = Function(),
    Expected = Predicate(Result),
    if
        Expected == true -> {ok, Result};
        Retries == 0 -> {error, Result};
        true -> 
            Timeout0 = Duration * math:pow(Factor, Retries - 1),
            Timeout1 = round(Timeout0),
            timer:sleep(Timeout1),
            retry(internal, Function, Predicate, Retries - 1, Duration, Factor)
    end.