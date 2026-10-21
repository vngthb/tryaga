-module(tryaga_configuration).
-moduledoc false.

-define(DEFAULT_FACTOR, 0.66).

-export([
    ensure_validity/1
]).

-spec ensure_validity(Configuration0) -> Configuration1
    when Configuration0 :: tryaga:configuration(),
         Configuration1 :: tryaga:configuration().
ensure_validity(Input) when is_map(Input) ->
    Defaults = #{retries => 0, factor => ?DEFAULT_FACTOR, jitter => 0,
                 logger => fun(_, _, _) -> ok end},
    Configuration = maps:merge(Defaults, Input),
    #{jitter := Jitter, factor := Factor, retries := Retries, logger := Logger} = Configuration,
    ensure_jitter(Jitter),
    ensure_factor(Factor),
    ensure_duration(Configuration),
    ensure_retries(Retries),
    ensure_logger(Logger),
    Configuration;
ensure_validity(_) ->
    throw(#{configuration => invalid, comment => <<"Configuration must be a map.">>}).

ensure_jitter(Jitter)
  when is_number(Jitter),
       Jitter >= 0,
       Jitter =< 1 ->
    ok;
ensure_jitter(_) ->
    invalid(jitter, <<"Jitter must be a number greater than or equal to 0 "
                      "and less than or equal to 1.">>).

ensure_factor(Factor)
  when is_number(Factor),
       Factor >= 0,
       Factor < 1 ->
    ok;
ensure_factor(_) ->
    invalid(factor, <<"Factor must be a number greater than or equal to 0 and less than 1.">>).

ensure_duration(#{duration := Duration})
  when is_integer(Duration), Duration > 0 ->
    ok;
ensure_duration(#{duration := _}) ->
    invalid(duration, <<"Duration must be a positive integer in milliseconds.">>);
ensure_duration(_Configuration) ->
    Ex = #{
        duration => missing,
        comment => <<"Please provide a total duration for the retries.">>
    },
    throw(Ex).

ensure_retries(Retries)
  when is_integer(Retries), Retries >= 0 ->
    ok;
ensure_retries(_) ->
    invalid(retries, <<"The number of retries must be a non-negative integer.">>).

ensure_logger(Logger)
  when is_function(Logger, 3) ->
    ok;
ensure_logger(_) ->
    invalid(logger, <<"The log function needs to be a function with an arity of 3.">>).

invalid(Key, Comment) ->
    throw(#{Key => invalid, comment => Comment}).
