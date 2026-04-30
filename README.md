# biff.fx (alpha)

NOTE: this whole thing so far has been entirely ai-generated (except for this paragraph). I need to make a pass over it before it's ready to be officially released.

An effect system for Clojure web applications. Application logic is expressed as
pure state machines; effects (HTTP requests, database queries, etc.) are
declared as data and executed by the framework between state transitions.

## Installation

Add to your `deps.edn`:

```clojure
{:deps {io.github.jacobobryant/biff.fx {:git/tag "..." :git/sha "..."}}}
```

## Core concepts

### Machines

A **machine** is a state machine where each state is a pure function. State
functions receive a context map and return a map describing what to do next:

```clojure
(require '[com.biffweb.fx :as fx])

(fx/defmachine my-handler
  :start
  (fn [ctx]
    {:db-result [:biff.fx/db {:query "SELECT ..."}]
     :biff.fx/next :use-data})

  :use-data
  (fn [{:keys [db-result]}]
    {:status 200
     :body (str "Got: " db-result)}))
```

Call it like a function:

```clojure
(my-handler {})
```

### Effects

Effects are identified by **value**, not by key. If a map entry's value is a
vector whose first element matches a key in `:biff.fx/handlers`, it's treated
as an effect:

```clojure
;; This entry is an effect — the value is a vector starting with a handle key
{:my-result [:biff.fx/http {:method :get :url "https://example.com"}]}

;; This entry is NOT an effect — the value is a plain keyword
{:biff.fx/next :some-state}

;; This is NOT an effect — :not-an-effect isn't in the handlers map
{:data [:not-an-effect 1 2 3]}
```

Libraries add effect implementations by merging handler fns into
`:biff.fx/handlers`, often from a public `fx-handlers` var in the library that
owns those handlers:

```clojure
(def fx-handlers
  {:my.app.fx/add
   (fn [ctx & nums]
     (apply + nums))})
```

The effect's return value is stored under the map entry's key (e.g.
`:my-result`) in the context, available to subsequent states.

If the handler set needs to be computed dynamically, pass a function under
`:biff.fx/get-handlers`. It takes precedence over `:biff.fx/handlers`:

```clojure
(my-handler
  {:biff.fx/handlers
   {:my.app.fx/add (fn [_ctx & nums] (apply + nums))}
   :biff.fx/get-handlers
   (fn []
     {:my.app.fx/add (fn [_ctx & nums] 999)})})
```

### Naming convention

When you define effect keywords for library functions, start with the namespace
alias you would normally use for the function, append `.fx`, and then use the
function name. Examples:

```clojure
com.biffweb.graph/query      => :biff.graph.fx/query
com.biffweb.sqlite/execute   => :biff.sqlite.fx/execute
com.biffweb.sqlite/authorized-write
                             => :biff.sqlite.fx/authorized-write
```

### Deterministic randomness

For testability, `uuid` and `random-bytes` accept a seed and return a
`[value next-seed]` tuple:

```clojure
(let [[id seed'] (fx/uuid 42)
      [id2 seed''] (fx/uuid seed')])
```

## API Reference

### `uuid [seed]`
Generate a UUID from a seed. Returns `[uuid next-seed]`.

### `random-bytes [seed n]`
Generate `n` random bytes from a seed. Returns `[bytes next-seed]`.

### `machine [machine-name & state-kvs]`
Creates a state machine handler function. `machine-name` is a keyword for error
reporting. Remaining args are keyword/function pairs defining states. Must
include a `:start` state.

The returned function has two arities:
- `(handler ctx)` — runs from `:start` until no `:biff.fx/next`
- `(handler ctx state)` — runs a single state (useful for testing)

### `defmachine [sym & state-kvs]`
Macro. Defines a machine as a var. Machine name is derived from the namespace
and symbol.

## Context keys

| Key | Description |
|-----|-------------|
| `:biff.fx/handlers` | Map of effect keyword → function |
| `:biff.fx/get-handlers` | Function that returns effect handlers for this machine run |
| `:biff.fx/next` | Next state keyword (in state return map) |
| `:biff.fx/now` | Injected `java.time.Instant` before each state |
| `:biff.fx/seed` | Injected random long seed |
| `:biff.fx/results` | Previous state's effect output maps |
| `:biff.fx/trace` | Accumulated trace of all state transitions |

## License

MIT
