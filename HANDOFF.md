# hcurl refactor handoff

Implementation baseline before this documentation-only commit: `b9b16d6`.
This file records the decisions behind the refactor, not just the resulting
module layout. Preserve these constraints unless a replacement design has
equivalent ownership, wake-up, compatibility, and lifecycle tests.

## Key decisions and rationale

### Keep the libcurl hot path in C

- Body, upload, header, socket, and timer callbacks are C functions. Do not add
  `foreign import ... "wrapper"`, `foreign export`, or dynamic Haskell callbacks
  to a transfer hot path: a synchronous C-to-Haskell callback is substantially
  more expensive than the native callback itself.
- The permitted C-to-RTS operation is a one-shot `hs_try_putmvar` wake-up when a
  Haskell reader/writer is blocked or a transfer completes. It does not execute
  an arbitrary Haskell callback on the reactor thread.
- A ready `readBody` or `feedBody` performs one unsafe Haskell-to-C call.
  Blocking adds one waiter-registration call, an `MVar` sleep, and a retry.
  Completion/header/metrics data with the same lifetime is returned in batched
  snapshots rather than several FFI calls.
- There are no normal per-transfer Haskell worker threads. Haskell threads own
  API calls; each agent has one Haskell thread blocked in the C/libuv reactor.

### Share one bounded stream implementation

- Download and upload payloads use `hcurl_stream_t` from `cbits/stream.c`.
  Direction-specific libcurl callbacks sit over the same bounded ring,
  waiter-registration, terminal-state, pause/resume, and close machinery.
- Capacity is expressed in chunks. Payload bytes are copied into C-owned chunk
  allocations, so pointers borrowed from libcurl or a Haskell `ByteString` are
  never retained past their callback/API call.
- Backpressure pauses libcurl. Resume is queued to the agent instead of calling
  the easy handle from a Haskell producer/consumer thread.

### Give transfer ownership to the reactor

- Before a successful execute enqueue, the easy handle belongs to Haskell and
  setup failure cleans it there. Once enqueue succeeds, only the agent thread
  may remove or clean that easy handle.
- The agent thread owns a chained hash registry keyed by monotonic `TransferId`.
  A `transfer_t` has a stable address stored in `CURLOPT_PRIVATE`; completion
  therefore gets the transfer directly while cancel/resume perform average
  O(1) ID lookup.
- Cancel and resume messages contain IDs, never `CURL *`. IDs are not reused;
  controls that arrive after completion are harmless no-ops. Duplicate IDs fail
  only the newer submission and cannot detach the existing transfer.
- Completion, cancellation, setup failure after enqueue, and agent shutdown all
  converge on the same reactor-owned cleanup protocol: remove, snapshot result
  and metrics, complete streams, clean the easy handle, then wake Haskell.

### Treat StablePtr wake-up as an ownership protocol

- `hs_try_putmvar` consumes a fired `newStablePtrPrimMVar` pointer. Never free or
  reuse it after C has detached and fired a waiter. Async cancellation may free
  it only when the matching C unregister function explicitly returns ownership.
- Completion uses its own atomic `waker_fired` flag. The generic `hs_waker_t`
  `waked` field is deliberately plain and must not be read from another thread.
- Result publication stores all fields, then release-publishes `completed`.
  Readers acquire that exact flag before reading the relaxed result fields and
  preceding metrics; acquiring a different atomic is not a valid happens-before
  edge on weakly ordered targets.
- Haskell keeps every `ForeignPtr` borrowed by the C transfer alive until the
  completion MVar fires. C performs no pointer dereference after that wake-up.

### Stream POST through libcurl's upload state

- Streaming POST is configured with `CURLOPT_UPLOAD`, a custom `POST` wire
  method, unknown `CURLOPT_INFILESIZE_LARGE`, and redirect following disabled.
  `CURLOPT_POST` was rejected because curl 7.19 sent `Content-Length: 0` before
  consuming the read callback.
- A streaming producer cannot be replayed safely across 307/308 redirects.
  Effective `OptionFollowLocation True` is therefore rejected before submit.
- Suppress libcurl's implicit `Expect: 100-continue` unless the caller supplied
  an Expect header. curl 7.19.0 can otherwise stall while both peers wait.
- `HeaderList` receives an owned internal entry. `OverrideHeaders` receives an
  owned one-node overlay over its borrowed slist; never copy, mutate, or release
  the caller's reusable tail early.

### Keep the native baseline real

- The package baseline is libcurl 7.18 and libuv 1.0. Streaming upload alone
  requires libcurl 7.19 because 7.18's `CURL_READFUNC_PAUSE` upload path is
  unsafe. Keep the linked-runtime guard and `streamingUploadSupported` probe.
- Newer optional curl settings are compile-time guarded and fail at the point of
  use on old headers instead of raising the package-wide dependency floor.
- Metrics select modern `_T` or legacy `curl_easy_getinfo` queries using the
  linked runtime version as well as compile-time availability.
- `CurlCode` is a header-independent numeric ABI table with
  `UnknownCurlCode Int`. Do not regenerate it from installed headers: doing so
  makes the exposed constructors build-dependent and newer runtime codes
  partial at `toEnum`.

### Keep managed-pool state under one lock

- Growth is admission-driven: a request creates a worker when the least-loaded
  selectable worker has at least `mpGrowLoad` active leases, the pool is below
  `mpMaxAgents`, and the spawn cooldown elapsed. The new request is routed to
  the newly created worker immediately. The controller performs EWMA-based
  shrinking and replacement, not delayed growth.
- `ManagedAgent.maState :: MVar ManagedState` is the synchronization boundary
  for worker load, utilization, and quiescing state, even though admission,
  completion, and controller work originate on different Haskell threads.
- `mwActive :: IORefU Int` and `mwUtilization :: IORefU Double` use unboxed
  storage to avoid boxed primitive updates; `IORefU` is not being used as a
  synchronization primitive. Boolean flags, the callback hook, and the atomic
  `Word64` transfer-ID source remain ordinary `IORef`s. `unboxed-ref-0.4.0.0`
  has no `Prim Bool` instance or atomic Word64 operation.
- Do not independently replace those fields with `TVar`s while retaining the
  outer MVar: that would split one invariant across two synchronization models.
  Moving the entire managed state and decisions to STM can be evaluated as a
  separate redesign if composable transactions become necessary.
- A draining worker remains in the pool registry until its reactor has stopped.
  Stop first and remove second so close or an async exception cannot orphan it.

### Make lifecycle cleanup idempotent

- Agent and controller threads use `asyncWithUnmask` variants because their
  parents are masked during setup. Plain `async` would inherit masking and make
  child-local restore ineffective, potentially making hooks unkillable.
- Shared `Once` state memoizes completion and close outcomes. An interrupted
  cleanup is retried by a replacement thread; successful normal cleanup creates
  no helper thread.
- Prefer `withAgent`, `withThreadedAgent`, `withManagedAgent`, and scoped
  streaming APIs. Abandoning a ResourceT scope deterministically closes the
  transfer and releases its managed-worker lease.

### Accept a smaller, breaking public API

- Public façades are `HCurl.Agent`, `HCurl.Headers`, `HCurl.Metrics`,
  `HCurl.Options`, `HCurl.Request`, `HCurl.Response`, `HCurl.Simple`,
  `HCurl.Streaming`, and `HCurl.Upload`.
- `HCurl.Extras` and `HCurl.PyFCustom` were removed. Request, option, agent,
  response, and streaming interfaces were deliberately redesigned.
- Every `HCurl.Internal.*` module remains exposed. This is intentional, even
  though those modules are less stable than the top-level interface.

## Compatibility matrix

| Path | Minimum | Verified during refactor |
|---|---:|---:|
| Buffered requests and response streaming | libcurl 7.18, libuv 1.0 | 7.18.0 / 1.0.0 |
| Streaming POST upload | libcurl 7.19, libuv 1.0 | 7.19.0 / 1.0.0 |
| Current development stack | current packaged versions | 8.21.0 / 1.52.1 |

The low versions are feature floors, not build-only claims. Exact-prefix test
runs were used. No C code changed after those compatibility and sanitizer runs;
the later `IORefU` change affected managed Haskell storage only.

## Deliberate limitations

- Streaming upload supports POST with an empty pre-set request body; the
  response is currently buffered.
- Streaming upload does not follow redirects because the producer is one-shot.
- Stream bounds count chunks, not total bytes; an individual chunk can be large.
- The refactor establishes fewer language/runtime crossings structurally, but
  makes no throughput claim without a separately controlled benchmark run.

## Build gotcha

`c2hs` does not reliably notice C header layout changes, and Cabal's inplace
package registration is shared by configurations in one build directory.
After changing a C struct/header, compiler optimization, or native dependency,
use a fresh build directory before interpreting crashes or type mismatches.
Pass that same directory and configuration to `cabal list-bin`; an unqualified
`list-bin` can select a stale executable from another configuration.

```console
direnv exec . cabal test all -j1 \
  --builddir=dist-verify-o2 \
  --enable-optimization=2
direnv exec . cabal list-bin test:hcurl-test \
  --builddir=dist-verify-o2 \
  --enable-optimization=2
```

## Required release checks

- clean `-O2` build and both test suites with default RTS capabilities and
  `+RTS -N1 -RTS`;
- C compilation with `-Wall -Wextra -Werror`;
- Valgrind or equivalent lifetime checking for shutdown/cancel tests;
- `cabal check`, Haddock, sdist build, and Nix build;
- mechanically compare every `src/HCurl/Internal/**/*.hs`/`.chs` module with
  Cabal's `exposed-modules` list;
- compatibility compile against the declared libcurl 7.18 and libuv 1.0
  headers after changes to native code, plus the upload suite against libcurl
  7.19.0 so the feature-specific floor remains real rather than inferred.
