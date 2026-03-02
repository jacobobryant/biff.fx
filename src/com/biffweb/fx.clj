(ns com.biffweb.fx
  "Effect system for Biff web applications.

   The core abstraction is `machine`: a state machine where each state
   is a pure function and effects happen while transitioning between
   states. This keeps application logic pure and testable.

   A machine is created with named state transition functions. Each
   function receives the context map and returns a map where:
     - Keys matching an effect handler key → that effect is executed
     - :biff.fx/next → the next state to transition to
     - Other keys → merged into context for subsequent states

   Effect handlers are provided in the context under :biff.fx/handlers.
   :biff/now is injected as a java.time.Instant before each state runs.
   :biff.fx/seed is injected as a random long seed.
   :biff.fx/results is set from the last trace entry."
  (:require [clojure.walk :as walk])
  (:import [java.time Instant]
           [java.util Random UUID]))

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

;; === Internal helpers ===

(defn- truncate-str [s n]
  (if (<= (count s) n) s (str (subs s 0 (dec n)) "…")))

(defn- truncate [data]
  (walk/postwalk
   (fn [data] (if (string? data) (truncate-str data 500) data))
   data))

;; === Core state machine ===

(defn- step [{:keys [state->transition-fn ctx state trace]}]
  (let [{:keys [biff.fx/handlers]} ctx
        last-results (->> (some-> trace peek :biff.fx/results)
                          (mapv :biff.fx/fx-output)
                          (filterv not-empty))
        ctx (assoc ctx
                   :biff/now (Instant/now)
                   :biff.fx/seed (.nextLong (Random.))
                   :biff.fx/results last-results)
        t-fn (or (get state->transition-fn state)
                 (throw (ex-info "Invalid state" {:state state})))
        result (t-fn ctx)
        results (if (map? result) [result] result)
        results (mapv
                 (fn [m]
                   (let [state-output (apply dissoc m (keys handlers))
                         fx-input (select-keys m (keys handlers))
                         ctx (merge ctx state-output)
                         fx-output
                         (into {}
                               (map (fn [[k v]]
                                      [k (try
                                           ((get handlers k) ctx v)
                                           (catch Exception e
                                             (throw
                                              (ex-info
                                               "Exception while running biff.fx effect"
                                               (truncate {:effect k :input v})
                                               e))))]))
                               fx-input)]
                     {:biff.fx/state-output state-output
                      :biff.fx/fx-input fx-input
                      :biff.fx/fx-output fx-output}))
                 results)
        trace (conj trace {:biff.fx/state state
                           :biff.fx/results results})
        {:biff.fx/keys [state-output fx-output fx-input]}
        (apply merge-with merge results)
        next-state (:biff.fx/next state-output)]
    {:next-state next-state
     :ctx (merge ctx fx-output state-output
                 {:biff.fx/trace trace :biff.fx/fx-input fx-input})
     :trace trace
     :state-output state-output}))

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
               (step {:state->transition-fn state->transition-fn
                      :ctx ctx :state state :trace trace})
               (catch Exception e
                 (throw
                  (ex-info "Exception while running biff.fx machine"
                           {:machine machine-name
                            :state state
                            :trace (truncate trace)}
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

(defn safe-for-url?
  "Returns true if the string contains only URL-safe characters."
  [s]
  (boolean (re-matches #"[a-zA-Z0-9-_.+!*]+" s)))

(defn- autogen-endpoint [ns* sym]
  (let [href (str "/_biff/api/" ns* "/" sym)]
    (doseq [segment [ns* sym]]
      (assert (safe-for-url? (str segment))
              (str "URL segment would contain invalid characters: " segment)))
    href))

(let [all-methods [:get :post :put :delete :head :options :trace :patch :connect]]
  (defn route*
    "Creates a route vector [uri handler-map] from a URI, route name, and
     state->transition-fn map. Methods present in the state map are included
     as handlers."
    [uri route-name & {:as state->transition-fn}]
    (let [machine* (machine route-name state->transition-fn)]
      [uri
       (into {:name route-name}
             (comp (filter state->transition-fn)
                   (map (fn [method]
                          [method machine*])))
             all-methods)])))

(defn wrap-pathom
  "Wraps a function so that it receives both ctx and the :biff.fx/pathom
   value from ctx."
  [f]
  (fn [ctx]
    (f ctx (:biff.fx/pathom ctx))))

(defn wrap-hiccup
  "Wraps a function so that if it returns a hiccup vector (a vector starting
   with a keyword), the result is wrapped in {:body result}."
  [f]
  (fn [& args]
    (let [result (apply f args)]
      (if (and (vector? result) (keyword? (first result)))
        {:body result}
        result))))

(defmacro defroute
  "Defines a route var with auto-generated or explicit URI.

   Usage:
     (defroute my-route \"/path\"
       :get (fn [ctx] ...)
       :post (fn [ctx] ...))

     (defroute my-route  ; URI auto-generated from namespace and symbol
       :get (fn [ctx] ...))"
  [sym & args]
  (let [[uri & kvs] (if (string? (first args))
                      args
                      (into [nil] args))
        uri (or uri (autogen-endpoint *ns* sym))
        route-name (keyword (str *ns*) (str sym))]
    `(def ~sym
       (let [[& {:as params#}] [~@kvs]]
         (route* ~uri
                 ~route-name
                 (-> params#
                     (update-vals wrap-hiccup)
                     (merge {:start (fn [{:keys [~'request-method]}]
                                      {:biff.fx/next ~'request-method})})))))))

(defmacro defroute-pathom
  "Like `defroute`, but also runs a Pathom query before dispatching to the
   method handler. Handler functions receive both ctx and the Pathom result.

   Usage:
     (defroute-pathom my-route \"/path\" [:some/query]
       :get (fn [ctx pathom-result] ...))"
  [sym & args]
  (let [[uri query & kvs] (if (string? (first args))
                            args
                            (into [nil] args))
        uri (or uri (autogen-endpoint *ns* sym))
        route-name (keyword (str *ns*) (str sym))]
    `(def ~sym
       (let [query# ~query
             [& {:as params#}] [~@kvs]]
         (route* ~uri
                 ~route-name
                 (-> params#
                     (update-vals (comp wrap-hiccup wrap-pathom))
                     (merge {:start (fn [{:keys [~'request-method]}]
                                      {:biff.fx/pathom query#
                                       :biff.fx/next ~'request-method})})))))))
