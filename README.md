# biff.fx

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
(my-handler {:biff.fx/handlers {:biff.fx/db (fn [ctx query] ...)}})
```

### Effects

Effects are identified by **value**, not by key. If a map entry's value is a
vector whose first element matches a key in `:biff.fx/handlers`, it's treated
as an effect:

```clojure
;; This entry is an effect — the value is a vector starting with a handler key
{:my-result [:biff.fx/http {:method :get :url "https://example.com"}]}

;; This entry is NOT an effect — the value is a plain keyword
{:biff.fx/next :some-state}

;; This is NOT an effect — :not-a-handler isn't in the handlers map
{:data [:not-a-handler 1 2 3]}
```

The effect handler receives `ctx` as the first argument, followed by the
remaining elements of the vector:

```clojure
;; For {:my-result [:biff.fx/http {:method :get}]}
;; the handler is called as: (http-handler ctx {:method :get})

;; For {:sum [:biff.fx/add 1 2 3]}
;; the handler is called as: (add-handler ctx 1 2 3)
```

The effect's return value is stored under the map entry's key (e.g.
`:my-result`) in the context, available to subsequent states.

### Routes

`defroute` creates HTTP route handlers backed by machines:

```clojure
(fx/defroute my-page "/my-page"
  :get (fn [ctx] [:div "Hello world"]))
```

Hiccup vectors (vectors starting with a keyword) are automatically wrapped in
`{:body ...}`.

You can optionally provide an initial effect vector that runs before the HTTP
method handler:

```clojure
(fx/defroute my-page "/my-page"
  [:biff.fx/pathom [{:session/user [:user/email]}]]
  :get (fn [ctx {:keys [session/user]}]
    [:div "Hello, " (:user/email user)]))
```

If the URI is omitted, one is auto-generated from the namespace and symbol
name.

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

### `defroute [sym & args]`
Macro. Defines an HTTP route backed by a machine. Accepts an optional URI
string, an optional initial effect vector, and keyword/function pairs for HTTP
methods.

## Context keys

| Key | Description |
|-----|-------------|
| `:biff.fx/handlers` | Map of effect handler keyword → function |
| `:biff.fx/next` | Next state keyword (in state return map) |
| `:biff/now` | Injected `java.time.Instant` before each state |
| `:biff.fx/seed` | Injected random long seed |
| `:biff.fx/results` | Previous state's effect output maps |
| `:biff.fx/trace` | Accumulated trace of all state transitions |

## License

MIT
