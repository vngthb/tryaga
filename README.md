# tryaga

**Try again.** A small Erlang library for synchronous retries with geometric
backoff and optional jitter.

Three public functions, no runtime dependencies beyond OTP, and no processes to
start. Supply a function and a predicate; tryaga handles the waits between calls.

## Installation

Add tryaga to your application's `rebar.config`:

```erlang
{deps, [
    {tryaga, {git, "https://github.com/vngthb/tryaga.git", {branch, "main"}}}
]}.
```

Use a commit reference instead of a branch when you need a fixed dependency.

## Quick start

Retry reading a file until it becomes available:

```erlang
Read = fun() -> file:read_file("ready.txt") end,
IsReady = fun({ok, _Contents}) -> true; (_) -> false end,

tryaga:apply(Read, IsReady, 3, 1000).
%% {ok, {ok, Contents}} on success
%% {exhausted, {error, Reason}} after all attempts fail
```

The first call happens immediately. `3` retries allows **up to 4 calls** in
total. The outer `{ok, Result}` or `{exhausted, Result}` reports whether the
predicate accepted the result; the callback's result is preserved unchanged.

For more control, pass a configuration map:

```erlang
Configuration = #{
    duration => 5000,
    retries => 5,
    factor => 0.5,
    jitter => 0.1,
    logger => fun(Result, Delay, _Configuration) ->
        io:format("Rejected ~p; retrying in ~p ms~n", [Result, Delay])
    end
},
tryaga:apply(Read, IsReady, Configuration).
```

## API

| Function | Purpose |
| --- | --- |
| `tryaga:apply(Function, Predicate, Configuration)` | Retry with a configuration map. |
| `tryaga:apply(Function, Predicate, Retries, Duration)` | Retry with the default factor, no jitter, and no logging. |
| `tryaga:describe(Configuration)` | Return the retry delays in execution order, without sleeping or running callbacks. |

`Function` takes no arguments. `Predicate` receives each result; only the atom
`true` accepts it. Exhaustion returns `{exhausted, LastResult}`, preserving the
last rejected result even when it is an error tuple.

Exceptions from the function, predicate, or logger propagate to the caller and
stop retrying. To retry an exception, handle it inside your function and return
a value the predicate can inspect.

### Compatibility

Exhaustion now returns `{exhausted, LastResult}` instead of `{error, LastResult}`.
Update callers that match the outer error tag. Individual retry delays can also
differ from earlier versions because cumulative rounding now preserves the
configured sleep budget.

## Configuration

| Key | Meaning | Default | Accepted values |
| --- | --- | --- | --- |
| `duration` | Total sleep budget before jitter, in milliseconds | Required | Positive integer |
| `retries` | Additional attempts after the initial call | `0` | Nonnegative integer |
| `factor` | Ratio of each delay to the next | `0.66` | Number, `0 <= factor < 1` |
| `jitter` | Fraction of each delay available for random reduction | `0` | Number, `0 <= jitter <= 1` |
| `logger` | Callback before each sleep | No-op | Function of arity 3 |

Configuration is validated before the first attempt. Invalid values throw a
map identifying the key, such as `#{retries => invalid, comment => <<"...">>}`.
A missing duration throws `#{duration => missing, comment => <<"...">>}`;
a non-map configuration throws `#{configuration => invalid, comment => <<"...">>}`.
The `comment` is a human-readable explanation.

With zero retries, `apply` calls the function once and `describe` returns `[]`.
`duration` is still required. Additional configuration keys are preserved.

### Backoff

```erlang
tryaga:describe(#{duration => 5000, retries => 5, factor => 0.5}).
%% [161,323,645,1290,2581]
```

Smaller factors produce shorter initial waits and steeper growth:

| Factor | Delays for 5 retries and 5000 ms, without jitter |
| --- | --- |
| `0.66` (default) | `[369,558,847,1283,1943]` |
| `0.5` | `[161,323,645,1290,2581]` |
| `0.33` | `[40,121,366,1110,3363]` |

The delays follow a geometric backoff curve. Cumulative budgets are rounded to
milliseconds before taking their differences, so the full schedule sums to
`duration` before jitter when at least one retry is configured. Small delays can
round to zero. At `factor => 0`, every delay except the last is zero.

### Jitter

Jitter randomly shortens delays; it never increases them. For a rounded delay
`T`, let `D = round(T * Jitter)`. When `D > 0`, the sampled delay is an integer
between `T - D + 1` and `T`, inclusive. When `D = 0`, the delay stays `T`.

`describe/1` samples jitter too, so it previews a possible schedule. A later
`apply` call draws its own values. Both use the calling process's random state.

### Timing and logging

Retries run in the calling process, which sleeps between attempts. `duration`
allocates the waits; it excludes time spent in callbacks and scheduling delays,
so it does **not** enforce a wall-clock deadline. Early success and jitter can
reduce the total sleep time.

The logger receives `(RejectedResult, NextDelay, EffectiveConfiguration)`
before each sleep, including zero-length sleeps. The effective configuration
includes defaults, the number of retries remaining, and an internal `base`
value. There is no log call on success or after the final rejected attempt.
Its return value is ignored.

## Development

Install Erlang/OTP 27 or newer and rebar3, then run:

```sh
rebar3 compile
rebar3 eunit
rebar3 lint
rebar3 dialyzer
```

API documentation uses native `-moduledoc` and `-doc` attributes and is embedded
in the compiled BEAM files. View it with `rebar3 shell`:

```erlang
h(tryaga).
h(tryaga, apply, 3).
```

The GitHub Actions workflow compiles, tests, lints with Elvis, and runs Dialyzer
on OTP 27, 28, and 29.
Tests cover retry outcomes, callback behavior, configuration validation,
backoff boundaries, and jitter bounds without external services.

## License

[The Unlicense](LICENSE).
