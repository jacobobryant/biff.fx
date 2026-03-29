(ns com.biffweb.fx
  "Effect system for Biff web applications.

   The core abstraction is `machine`: a state machine where each state
   is a pure function and effects happen while transitioning between
   states. This keeps application logic pure and testable.

   A machine is created with named state transition functions. Each
   function receives the context map and returns a map where:
     - Values that are vectors whose first element is a key in the fx
       handlers map → that effect is executed, and the result is stored
       under the map entry's key in the context
     - :biff.fx/next → the next state to transition to
     - Other keys → merged into context for subsequent states

   Effect handlers are provided in the context under :biff.fx/handlers.
   :biff/now is injected as a java.time.Instant before each state runs.
   :biff.fx/seed is injected as a random long seed.
   :biff.fx/results is set from the last trace entry."
  (:require [com.biffweb.fx.impl :as impl])
  (:import [java.util Random UUID]))

;; === Seed-based deterministic randomness ===

(defn uuid
  "Generate a UUID from a seed. Returns [uuid next-seed]."
  [seed]
  (let [rng (Random. seed)
        msb (.nextLong rng)
        lsb (.nextLong rng)]
    [(UUID. msb lsb) (.nextLong rng)]))

(defn random-bytes
  "Generate n random bytes from a seed. Returns [bytes next-seed]."
  [seed n]
  (let [rng (Random. seed)
        bs (byte-array n)]
    (.nextBytes rng bs)
    [bs (.nextLong rng)]))

;; === Core state machine ===

(defn machine
  "Creates a state machine handler. `machine-name` is used for error
   reporting. Remaining args are keyword/fn pairs defining the states.

   Usage:
     (machine ::my-handler
       :start (fn [ctx] ...)
       :next-state (fn [ctx] ...))

   The returned function can be called with:
     (handler ctx)        — runs from :start until no :biff.fx/next
     (handler ctx :state) — runs a single state (useful for testing)"
  [machine-name & {:as state->transition-fn}]
  (assert (contains? state->transition-fn :start)
          "machine must have a :start state")
  (fn run
    ([ctx state]
     ((or (get state->transition-fn state)
          (throw (ex-info (str "Invalid state: " state
                               " in machine " machine-name) {})))
      ctx))
    ([ctx]
     (loop [ctx ctx, state :start, trace []]
       (let [{:keys [next-state ctx trace state-output]}
             (try
               (impl/step {:state->transition-fn state->transition-fn
                           :ctx ctx :state state :trace trace})
               (catch Exception e
                 (throw
                  (ex-info "Exception while running biff.fx machine"
                           {:machine machine-name
                            :state state
                            :trace (impl/truncate trace)}
                           e))))]
         (if next-state
           (recur ctx next-state trace)
           state-output))))))

(defmacro defmachine
  "Defines a machine as a var. Machine name keyword is derived from
   the current namespace and the var symbol.

   Usage:
     (defmachine my-handler
       :start (fn [ctx] ...)
       :next-state (fn [ctx] ...))"
  [sym & args]
  (let [machine-name (keyword (str *ns*) (str sym))]
    `(def ~sym (machine ~machine-name ~@args))))

;; === Routing ===

(defmacro defroute
  "Defines a route var with auto-generated or explicit URI.

   Usage:
     (defroute my-route \"/path\"
       :get (fn [ctx] ...)
       :post (fn [ctx] ...))

     (defroute my-route  ; URI auto-generated from namespace and symbol
       :get (fn [ctx] ...))

     (defroute my-route \"/path\"
       [:biff.fx/pathom [{:session/user [:user/email]}]]
       :get (fn [ctx {:keys [session/user]}] ...))"
  [sym & args]
  (let [[uri & rest-args] (if (string? (first args))
                            args
                            (into [nil] args))
        uri (or uri (impl/autogen-endpoint *ns* sym))
        [initial-fx & kvs] (if (vector? (first rest-args))
                             rest-args
                             (into [nil] rest-args))
        route-name (keyword (str *ns*) (str sym))
        initial-fx-sym (gensym "initial-fx")
        params-sym (gensym "params")]
    `(def ~sym
       (let [~initial-fx-sym ~initial-fx
             [& {:as ~params-sym}] [~@kvs]]
         (impl/route* ~uri
                      ~route-name
                      machine
                      (-> ~params-sym
                          (update-vals impl/wrap-hiccup)
                          ~@(when initial-fx
                              [`(impl/wrap-methods impl/wrap-result)])
                          (merge {:start (fn [{:keys [~'request-method]}]
                                          ~(if initial-fx
                                             `{:biff.fx/result ~initial-fx-sym
                                               :biff.fx/next ~'request-method}
                                             `{:biff.fx/next ~'request-method}))})))))))
