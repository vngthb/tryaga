-module(tryaga).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_FACTOR, 0.66).

-export([
    apply/3
]).

% -type configuration() :: #{
%     retries => pos_integer(),
%     duration => pos_integer(),
%     base => pos_integer(),
%     factor => pos_integer(),
%     log => function(),
%     jitter => false | slight | half | full
% }.

apply(Function, Predicate, Configuration0 = #{retries := 0}) ->
    Configuration1 = validate(Configuration0),
    Configuration2 = Configuration1#{base => undefined},
    apply(internal, Function, Predicate, Configuration2);
apply(Function, Predicate, Configuration0) ->
    Configuration1 = validate(Configuration0),
    Base = base(Configuration1),
    Configuration2 = Configuration1#{base => Base},
    apply(internal, Function, Predicate, Configuration2).

apply(internal, Function, Predicate, Configuration) ->
    #{
        retries := Retries,
        base := Base,
        factor := Factor,
        jitter := Jitter,
        log_fun := LogFun
    } = Configuration,
    Result = Function(),
    Expected = Predicate(Result),
    if
        Expected == true -> {ok, Result};
        Retries == 0 -> {error, Result};
        true -> 
            Timeout0 = Base * math:pow(Factor, Retries - 1),
            Timeout1 = round(Timeout0),
            Timeout2 = jitter(Timeout1, Jitter),
            LogFun(Result, Timeout2, Configuration),
            timer:sleep(Timeout2),
            apply(internal, Function, Predicate, Configuration#{retries => Retries - 1})
    end.

base(#{retries := Retries, duration := Duration, factor := Factor}) ->
    Duration * (1 - Factor) / (1 - math:pow(Factor, Retries)).

jitter(Timeout0, false) ->
    Timeout0;
jitter(Timeout0, slight) ->
    Diff = Timeout0 div 10,
    rand:uniform(Diff) + Timeout0 - Diff;
jitter(Timeout0, half) ->
    rand:uniform(Timeout0 div 2) + Timeout0 div 2;
jitter(Timeout0, full) ->
    rand:uniform(Timeout0).

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
  when Jitter == false;
       Jitter == slight;
       Jitter == half;
       Jitter == full ->
    Configuration;
ensure_jitter(#{jitter := _Jitter}) ->
    Ex = #{
        jitter => invalid,
        comment => <<"Jitter should be set to either false, slight, half and full.">>
    },
    throw(Ex);
ensure_jitter(Configuration) ->
    Configuration#{jitter => false}.

ensure_factor(Configuration = #{factor := Factor})
  when Factor >= 0,
       Factor < 1 ->
    Configuration;
ensure_factor(#{factor := Factor})
  when Factor < 0; Factor >= 1 ->
    Ex = #{
        factor => invalid,
        comment => <<"Factor should be greater or equal to 0 and less than 1.">>
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
        comment => <<"Duration should not be less or equal to 0.">>
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
        comment => <<"The number of retries should not be less than 0">>
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
        comment => <<"The log function needs to be a function with an arity of 3">>
    },
    throw(Ex);
ensure_log(Configuration) ->
    LogFun = fun(_, _, _) -> ok end,
    Configuration#{log_fun => LogFun}.