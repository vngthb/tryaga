-module(tryaga_tests).

-include_lib("eunit/include/eunit.hrl").

immediate_success_test() ->
    Logger = fun(_, _, _) -> error(unexpected_log) end,
    ?assertEqual({ok, ready}, tryaga:apply(
        script([ready]), fun is_ready/1,
        #{duration => 10, retries => 3, logger => Logger})).

exhaustion_returns_last_result_test() ->
    ?assertEqual({exhausted, last}, tryaga:apply(
        script([first, second, last]), fun is_ready/1, 2, 1)).

exhaustion_preserves_callback_error_test() ->
    ?assertEqual({exhausted, {error, unavailable}}, tryaga:apply(
        script([{error, unavailable}]), fun is_ready/1, 0, 1)).

stops_after_successful_retry_test() ->
    ?assertEqual({ok, ready}, tryaga:apply(
        script([pending, ready]), fun is_ready/1, 5, 1)).

explicit_zero_retries_test() ->
    ?assertEqual({exhausted, pending}, tryaga:apply(
        script([pending]), fun is_ready/1, 0, 1)).

default_zero_retries_test() ->
    ?assertEqual({exhausted, pending}, tryaga:apply(
        script([pending]), fun is_ready/1, #{duration => 1})).

zero_retries_do_not_log_test() ->
    Logger = fun(_, _, _) -> error(unexpected_log) end,
    ?assertEqual({exhausted, pending}, tryaga:apply(
        script([pending]), fun is_ready/1,
        #{duration => 1, retries => 0, logger => Logger})).

only_true_accepts_a_result_test() ->
    ?assertEqual({exhausted, ready}, tryaga:apply(
        script([ready]), fun(_) -> truthy end, 0, 1)).

logger_receives_each_rejected_result_and_delay_test() ->
    Ref = make_ref(),
    Logger = fun(Result, Delay, Configuration) ->
        self() ! {Ref, Result, Delay, Configuration}
    end,
    ?assertEqual({ok, ready}, tryaga:apply(
        script([first, second, ready]), fun is_ready/1,
        #{duration => 3, retries => 2, factor => 0.5, logger => Logger})),
    ?assertMatch({first, 1, #{retries := 2, factor := 0.5, jitter := 0}}, log(Ref)),
    ?assertMatch({second, 2, #{retries := 1}}, log(Ref)),
    receive
        {Ref, _, _, _} -> error(unexpected_log)
    after 0 -> ok
    end.

callback_errors_propagate_test() ->
    ?assertError(callback_failed, tryaga:apply(
        fun() -> error(callback_failed) end, fun is_ready/1, 2, 1)).

predicate_exceptions_propagate_test() ->
    ?assertThrow(predicate_failed, tryaga:apply(
        script([ready]), fun(_) -> throw(predicate_failed) end, 2, 1)).

logger_errors_propagate_test() ->
    ?assertError(logger_failed, tryaga:apply(
        script([pending]), fun is_ready/1,
        #{duration => 1, retries => 1,
          logger => fun(_, _, _) -> error(logger_failed) end})).

invalid_configuration_prevents_callback_test() ->
    ?assertThrow(#{duration := invalid}, tryaga:apply(
        fun() -> error(callback_ran) end, fun is_ready/1, 1, invalid)).

describe_default_retries_test() ->
    ?assertEqual([], tryaga:describe(#{duration => 1})).

describe_zero_retries_test() ->
    ?assertEqual([], tryaga:describe(#{duration => 1, retries => 0})).

describe_default_factor_test() ->
    ?assertEqual([369, 558, 847, 1283, 1943],
        tryaga:describe(#{duration => 5000, retries => 5})).

describe_doubling_delays_test() ->
    ?assertEqual([161, 323, 645, 1290, 2581],
        tryaga:describe(#{duration => 5000, retries => 5, factor => 0.5})).

describe_single_retry_test() ->
    ?assertEqual([12], tryaga:describe(#{duration => 12, retries => 1})).

describe_zero_factor_test() ->
    ?assertEqual([0, 0, 10],
        tryaga:describe(#{duration => 10, retries => 3, factor => 0})).

zero_delays_are_valid_test() ->
    ?assertEqual([0, 0, 1],
        tryaga:describe(#{duration => 1, retries => 3, factor => 0.5})).

rounded_delays_preserve_budget_test_() ->
    [?_assertEqual(Duration, lists:sum(tryaga:describe(
        #{duration => Duration, retries => Retries, factor => Factor})))
     || {Duration, Retries, Factor} <- [
        {3, 5, 0.99},
        {100, 150, 0.9995},
        {1, 5, 0.66},
        {5000, 5, 0.33},
        {5000, 5, 0.66},
        {100, 150, 0},
        {100, 150, 0.9999999999999999}
     ]].

jitter_reduces_delays_test() ->
    rand:seed(exsss, {10, 20, 30}),
    Configuration = #{duration => 100, retries => 3, factor => 0.5},
    Nominal = tryaga:describe(Configuration),
    Actual = tryaga:describe(Configuration#{jitter => 1}),
    ?assert(lists:sum(Actual) < lists:sum(Nominal)).

apply_uses_the_described_schedule_test() ->
    Ref = make_ref(),
    Logger = fun(Result, Delay, Configuration) ->
        self() ! {Ref, Result, Delay, Configuration}
    end,
    Configuration = #{duration => 8, retries => 5, factor => 0.99,
                      jitter => 1, logger => Logger},
    rand:seed(exsss, {10, 20, 30}),
    Expected = tryaga:describe(Configuration),
    rand:seed(exsss, {10, 20, 30}),
    ?assertEqual({exhausted, pending}, tryaga:apply(
        fun() -> pending end, fun is_ready/1, Configuration)),
    Actual = [Delay || {_, Delay, _} <- [log(Ref) || _ <- Expected]],
    ?assertEqual(Expected, Actual),
    ?assert(lists:sum(Actual) < 8).

jitter_stays_within_documented_bounds_test() ->
    rand:seed(exsss, {10, 20, 30}),
    Configuration = #{duration => 100, retries => 3, factor => 0.5},
    Nominal = tryaga:describe(Configuration),
    lists:foreach(fun(Jitter) ->
        lists:foreach(fun(_) ->
            Actual = tryaga:describe(Configuration#{jitter => Jitter}),
            assert_jitter_bounds(Actual, Nominal, Jitter)
        end, lists:seq(1, 20))
    end, [0, 0.1, 1]).

describe_never_calls_logger_test() ->
    ?assertEqual([1], tryaga:describe(#{duration => 1, retries => 1,
        logger => fun(_, _, _) -> error(logger_called) end})).

missing_duration_test() ->
    ?assertThrow(#{duration := missing}, tryaga:describe(#{})).

invalid_configuration_test_() ->
    [?_assertThrow(#{configuration := invalid}, tryaga:describe(Value))
     || Value <- [[], undefined, 10]].

invalid_duration_test_() ->
    [?_assertThrow(#{duration := invalid}, tryaga:describe(#{duration => Value}))
     || Value <- [0, -1, 1.5, invalid, <<"10">>, [], #{}]].

invalid_retries_test_() ->
    [?_assertThrow(#{retries := invalid},
        tryaga:describe(#{duration => 1, retries => Value}))
     || Value <- [-1, 1.0, invalid, <<"2">>, [], #{}]].

invalid_factor_test_() ->
    [?_assertThrow(#{factor := invalid},
        tryaga:describe(#{duration => 1, factor => Value}))
     || Value <- [-0.1, 1, 1.1, invalid, <<"0.5">>, [], #{}]].

invalid_jitter_test_() ->
    [?_assertThrow(#{jitter := invalid},
        tryaga:describe(#{duration => 1, jitter => Value}))
     || Value <- [-0.1, 1.1, invalid, <<"0.1">>, [], #{}]].

invalid_logger_test_() ->
    [?_assertThrow(#{logger := invalid},
        tryaga:describe(#{duration => 1, logger => Value}))
     || Value <- [undefined, fun() -> ok end, fun(_) -> ok end]].

assert_jitter_bounds(Actual, Nominal, Jitter) ->
    lists:foreach(fun({Delay, Base}) ->
        Diff = round(Base * Jitter),
        Minimum = case Diff of 0 -> Base; _ -> Base - Diff + 1 end,
        ?assert(Delay >= Minimum),
        ?assert(Delay =< Base)
    end, lists:zip(Actual, Nominal)).

is_ready(ready) -> true;
is_ready(_) -> false.

%% Each test process owns its scripted results; unexpected extra attempts fail.
script(Results) ->
    Ref = make_ref(),
    lists:foreach(fun(Result) -> self() ! {Ref, Result} end, Results),
    fun() ->
        receive
            {Ref, Result} -> Result
        after 0 -> error(unexpected_attempt)
        end
    end.

log(Ref) ->
    receive
        {Ref, Result, Delay, Configuration} -> {Result, Delay, Configuration}
    after 0 -> error(missing_log)
    end.
