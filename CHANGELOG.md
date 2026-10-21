# Changelog

## 2026-10-21

### Changed

- Adopted Apache License 2.0 for new releases. Previously published versions
  under the Unlicense retain their original permissions.
- Exhaustion now returns `{exhausted, LastResult}` instead of `{error, LastResult}`.
  Update callers that match the outer error tag.
- Retry delays now use cumulative rounding to preserve the configured sleep
  budget before jitter. Individual delays can differ from earlier versions.
