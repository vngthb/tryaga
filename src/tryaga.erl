-module(tryaga).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_FACTOR, 0.66).

-export([
    apply/3,
    apply/4,
    describe/1
]).

-type configuration() :: #{
    retries => pos_integer(),
    duration => pos_integer(),
    base => pos_integer(),
    factor => pos_integer(),
    log_fun => function(),
    jitter => pos_integer()
}.

-spec apply(Function, Predicate, Configuration) ->Result
    when Function :: function(),
         Predicate :: function(),
         Configuration :: configuration(),
         Result :: {ok, Term} | {error, Term}.
apply(Function, Predicate, Configuration0 = #{retries := 0}) ->
    Configuration1 = validate(Configuration0),
    Configuration2 = Configuration1#{base => undefined},
    apply0(Function, Predicate, Configuration2);
apply(Function, Predicate, Configuration0) ->
    Configuration1 = validate(Configuration0),
    Base = base(Configuration1),
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

describe(Configuration0) ->
    Configuration1 = validate(Configuration0),
    Configuration2 = Configuration1#{base => base(Configuration1)},
    #{retries := Retries} = Configuration2,
    FoldrFun = 
        fun(R, A) ->
            T = timeout(Configuration2#{retries => R}),
            A ++ [T]
        end,
    lists:foldr(FoldrFun, [], lists:seq(1, Retries)).

apply0(Function, Predicate, #{retries := Retries, log_fun := LogFun} = Configuration) ->
    Result = Function(),
    Expected = Predicate(Result),
    if
        Expected == true -> {ok, Result};
        Retries == 0 -> {error, Result};
        true -> 
            Timeout = timeout(Configuration),
            LogFun(Result, Timeout, Configuration),
            timer:sleep(Timeout),
            apply0(Function, Predicate, Configuration#{retries => Retries - 1})
    end.

base(#{retries := Retries, duration := Duration, factor := Factor}) ->
    Duration * (1 - Factor) / (1 - math:pow(Factor, Retries)).

timeout(#{base := Base, factor := Factor, retries := Retries, jitter := Jitter}) ->
    Timeout0 = Base * math:pow(Factor, Retries - 1),
    Timeout1 = round(Timeout0),
    Diff = round(Timeout1 * Jitter),
    rand(Diff) + Timeout1 - Diff.

rand(0) ->
    0;
rand(Diff) ->
    rand:uniform(Diff).

validate(Configuration) ->
    ValidationFuns = [
        fun ensure_jitter/1,
        fun ensure_factor/1,
        fun ensure_duration/1,
        fun ensure_retries/1,
        fun ensure_log/1
    ],
    FoldlFun = fun(Validation, Arg) -> Validation(Arg) end,
    lists:foldl(FoldlFun, Configuration, ValidationFuns).

ensure_jitter(Configuration = #{jitter := Jitter})
  when Jitter > 0,
       Jitter =< 1 ->
    Configuration;
ensure_jitter(#{jitter := Jitter})
  when Jitter =< 0;
       Jitter > 1 ->
    Ex = #{
        jitter => invalid,
        comment => <<"Jitter should be greater than 0 and less than or equal to 1.">>
    },
    throw(Ex);
ensure_jitter(Configuration) ->
    Configuration#{jitter => 0}.

ensure_factor(Configuration = #{factor := Factor})
  when Factor >= 0,
       Factor < 1 ->
    Configuration;
ensure_factor(#{factor := Factor})
  when Factor < 0; Factor >= 1 ->
    Ex = #{
        factor => invalid,
        comment => <<"Factor should be greater than or equal to 0 and less than 1.">>
    },
    throw(Ex);
ensure_factor(Configuration) ->
    Configuration#{factor => ?DEFAULT_FACTOR}.

ensure_duration(Configuration = #{duration := Duration})
  when Duration > 0 ->
    Configuration;
ensure_duration(#{duration := Duration})
  when Duration =< 0 ->
    Ex = #{
        duration => invalid,
        comment => <<"Duration should not be less than or equal to 0.">>
    },
    throw(Ex);
ensure_duration(_Configuration) ->
    Ex = #{
        duration => missing,
        comment => <<"Please provide a total duration for the retries.">>
    },
    throw(Ex).

ensure_retries(Configuration = #{retries := Retries})
  when Retries >= 0 ->
    Configuration;
ensure_retries(#{retries := Retries})
  when Retries < 0 ->
    Ex = #{
        retries => invalid,
        comment => <<"The number of retries should not be less than 0.">>
    },
    throw(Ex);
ensure_retries(Configuration) ->
    Configuration#{retries => 0}.

ensure_log(Configuration = #{log_fun := LogFun})
  when is_function(LogFun, 3) ->
    Configuration;
ensure_log(#{log_fun := LogFun})
  when not is_function(LogFun, 3) ->
    Ex = #{
        log_fun => invalid,
        comment => <<"The log function needs to be a function with an arity of 3.">>
    },
    throw(Ex);
ensure_log(Configuration) ->
    LogFun = fun(_, _, _) -> ok end,
    Configuration#{log_fun => LogFun}.