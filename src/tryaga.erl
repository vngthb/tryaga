-module(tryaga).

-include_lib("kernel/include/logger.hrl").

-export([
    apply/3,
    apply/4,
    describe/1
]).

-type configuration() :: #{
    retries => pos_integer(),
    duration => milliseconds(),
    base => milliseconds(),
    factor => float(),
    jitter => float(),
    logger => function()
}.

-type milliseconds() :: pos_integer().

-spec apply(Function, Predicate, Configuration) -> Result
    when Function :: function(),
         Predicate :: function(),
         Configuration :: configuration(),
         Result :: {ok, Term} | {error, Term}.
apply(Function, Predicate, Configuration0 = #{retries := 0}) ->
    Configuration1 = tryaga_configuration:ensure_validity(Configuration0),
    Configuration2 = Configuration1#{base => undefined},
    apply0(Function, Predicate, Configuration2);
apply(Function, Predicate, Configuration0) ->
    Configuration1 = tryaga_configuration:ensure_validity(Configuration0),
    Base = assess_base(Configuration1),
    Configuration2 = Configuration1#{base => Base},
    apply0(Function, Predicate, Configuration2).

-spec apply(Function, Predicate, Retries, Duration) -> Result
    when Function :: function(),
         Predicate :: function(),
         Retries :: pos_integer(),
         Duration :: pos_integer(),
         Result :: {ok, Term} | {error, Term}.
apply(Function, Predicate, Retries, Duration) ->
    Configuration = #{
        retries => Retries,
        duration => Duration
    },
    tryaga:apply(Function, Predicate, Configuration).

-spec describe(Configuration) -> Result
    when Configuration :: configuration(),
         Result :: [pos_integer()].
describe(Configuration0) ->
    Configuration1 = tryaga_configuration:ensure_validity(Configuration0),
    Configuration2 = Configuration1#{base => assess_base(Configuration1)},
    #{retries := Retries} = Configuration2,
    FoldrFun = 
        fun(Retry, Acc) ->
            Timeout = compute_timeout(Configuration2#{retries => Retry}),
            Acc ++ [Timeout]
        end,
    lists:foldr(FoldrFun, [], lists:seq(1, Retries)).

apply0(Function, Predicate, #{retries := Retries, logger := Logger} = Configuration) ->
    Result = Function(),
    Expected = Predicate(Result),
    if
        Expected == true -> {ok, Result};
        Retries == 0 -> {error, Result};
        true -> 
            Timeout = compute_timeout(Configuration),
            Logger(Result, Timeout, Configuration),
            timer:sleep(Timeout),
            apply0(Function, Predicate, Configuration#{retries => Retries - 1})
    end.

assess_base(#{retries := Retries, duration := Duration, factor := Factor}) ->
    Duration * (1 - Factor) / (1 - math:pow(Factor, Retries)).

compute_timeout(#{base := Base, factor := Factor, retries := Retries, jitter := Jitter}) ->
    Timeout0 = Base * math:pow(Factor, Retries - 1),
    Timeout1 = round(Timeout0),
    Diff = round(Timeout1 * Jitter),
    rand(Diff) + Timeout1 - Diff.

rand(0) ->
    0;
rand(Diff) ->
    rand:uniform(Diff).