-module(tryaga).
-compile({no_auto_import, [apply/3]}).
-moduledoc """
Synchronous retries with geometric backoff and optional jitter.
""".

-export([
    apply/3,
    apply/4,
    describe/1
]).

-export_type([configuration/0, milliseconds/0, logger/0]).

-type configuration() :: #{
    duration := pos_integer(),
    retries => non_neg_integer(),
    factor => number(),
    jitter => number(),
    logger => logger()
}.

-type milliseconds() :: non_neg_integer().
-type logger() :: fun((term(), milliseconds(), map()) -> term()).

-doc """
Call `Function` immediately, retrying until `Predicate` returns `true`.

Returns `{ok, Result}` when accepted or `{exhausted, LastResult}` when retries run out.
Configuration errors are thrown as maps. Callback exceptions propagate.
The caller sleeps between attempts; `duration` is a sleep budget, not a deadline.
""".
-spec apply(Function, Predicate, Configuration) -> Result
    when Function :: fun(() -> Term),
         Predicate :: fun((Term) -> boolean()),
         Configuration :: configuration(),
         Result :: {ok, Term} | {exhausted, Term},
         Term :: term().
apply(Function, Predicate, Configuration0) ->
    retry(Function, Predicate, prepare(Configuration0)).

-doc """
Retry using the default factor (0.66), no jitter, and no logging.

`Retries` counts additional attempts after the first call; zero calls once.
""".
-spec apply(Function, Predicate, Retries, Duration) -> Result
    when Function :: fun(() -> Term),
         Predicate :: fun((Term) -> boolean()),
         Retries :: non_neg_integer(),
         Duration :: pos_integer(),
         Result :: {ok, Term} | {exhausted, Term},
         Term :: term().
apply(Function, Predicate, Retries, Duration) ->
    Configuration = #{
        retries => Retries,
        duration => Duration
    },
    apply(Function, Predicate, Configuration).

-doc """
Return delays in execution order without sleeping or calling the logger.

Zero retries returns an empty list. With jitter, each call samples new delays
and consumes random state in the calling process. Without jitter, the delays
sum to `duration` when at least one retry is configured.
""".
-spec describe(Configuration) -> Result
    when Configuration :: configuration(),
         Result :: [milliseconds()].
describe(Configuration0) ->
    Configuration = prepare(Configuration0),
    #{retries := Retries} = Configuration,
    [compute_delay(Configuration#{retries => Retry})
     || Retry <- lists:seq(Retries, 1, -1)].

prepare(Configuration0) ->
    Configuration = tryaga_configuration:ensure_validity(Configuration0),
    Configuration#{base => assess_base(Configuration)}.

retry(Function, Predicate, #{retries := Retries, logger := Logger} = Configuration) ->
    Result = Function(),
    case Predicate(Result) of
        true -> {ok, Result};
        _ when Retries =:= 0 -> {exhausted, Result};
        _ ->
            Delay = compute_delay(Configuration),
            Logger(Result, Delay, Configuration),
            timer:sleep(Delay),
            retry(Function, Predicate, Configuration#{retries => Retries - 1})
    end.

assess_base(#{retries := 0}) ->
    undefined;
assess_base(#{retries := Retries, duration := Duration, factor := Factor}) ->
    Duration * (1 - Factor) / (1 - math:pow(Factor, Retries)).

compute_delay(#{base := Base, factor := Factor, retries := Retries, jitter := Jitter}) ->
    %% Differences of rounded cumulative budgets avoid per-delay rounding drift.
    Delay = remaining_budget(Retries, Base, Factor) -
            remaining_budget(Retries - 1, Base, Factor),
    jitter(Delay, round(Delay * Jitter)).

remaining_budget(Retries, Base, Factor) ->
    round(Base * (1 - math:pow(Factor, Retries)) / (1 - Factor)).

jitter(Delay, 0) ->
    Delay;
jitter(Delay, Spread) ->
    Delay - Spread + rand:uniform(Spread).
