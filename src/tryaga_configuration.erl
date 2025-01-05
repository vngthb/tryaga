-module(tryaga_configuration).

-define(DEFAULT_FACTOR, 0.66).

-export([
    ensure_validity/1
]).

-spec ensure_validity(Configuration0) -> Configuration1
    when Configuration0 :: tryaga:configuration(),
         Configuration1 :: tryaga:configuration().
ensure_validity(Configuration) ->
    ValidationFuns = [
        fun ensure_jitter/1,
        fun ensure_factor/1,
        fun ensure_duration/1,
        fun ensure_retries/1,
        fun ensure_logger/1
    ],
    FoldlFun = fun(Validation, Arg) -> Validation(Arg) end,
    lists:foldl(FoldlFun, Configuration, ValidationFuns).

ensure_jitter(Configuration = #{jitter := Jitter})
  when Jitter >= 0,
       Jitter =< 1 ->
    Configuration;
ensure_jitter(#{jitter := Jitter})
  when Jitter < 0;
       Jitter > 1 ->
    Ex = #{
        jitter => invalid,
        comment => <<"Jitter should be greater than or equal to 0 and less than or equal to 1.">>
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

ensure_logger(Configuration = #{logger := Logger})
  when is_function(Logger, 3) ->
    Configuration;
ensure_logger(#{logger := Logger})
  when not is_function(Logger, 3) ->
    Ex = #{
        logger => invalid,
        comment => <<"The log function needs to be a function with an arity of 3.">>
    },
    throw(Ex);
ensure_logger(Configuration) ->
    Logger = fun(_, _, _) -> ok end,
    Configuration#{logger => Logger}.