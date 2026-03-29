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
                   (let [effect-entry? (fn [[_ v]]
                                         (and (vector? v)
                                              (seq v)
                                              (contains? handlers (first v))))
                         effect-keys (set (map key (filter effect-entry? m)))
                         state-output (apply dissoc m effect-keys)
                         fx-input (select-keys m effect-keys)
                         ctx (merge ctx state-output)
                         fx-output
                         (into {}
                               (map (fn [[k v]]
                                      (let [handler-key (first v)
                                            handler-args (rest v)]
                                        [k (try
                                             (apply (get handlers handler-key) ctx handler-args)
                                             (catch Exception e
                                               (throw
                                                (ex-info
                                                 "Exception while running biff.fx effect"
                                                 (truncate {:effect handler-key
                                                            :key k
                                                            :input (vec handler-args)})
                                                 e))))])))
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

(defn- safe-for-url? [s]
  (boolean (re-matches #"[a-zA-Z0-9-_.+!*]+" s)))

(defn- autogen-endpoint [ns* sym]
  (let [href (str "/_biff/api/" ns* "/" sym)]
    (doseq [segment [ns* sym]]
      (assert (safe-for-url? (str segment))
              (str "URL segment would contain invalid characters: " segment)))
    href))

(def ^:private all-methods
  [:get :post :put :delete :head :options :trace :patch :connect])

(defn- route*
  [uri route-name & {:as state->transition-fn}]
  (let [machine* (machine route-name state->transition-fn)]
    [uri
     (into {:name route-name}
           (comp (filter state->transition-fn)
                 (map (fn [method]
                        [method machine*])))
           all-methods)]))

(defn- wrap-result
  [f]
  (fn [ctx]
    (f ctx (:biff.fx/result ctx))))

(defn- wrap-hiccup
  [f]
  (fn [& args]
    (let [result (apply f args)]
      (if (and (vector? result) (keyword? (first result)))
        {:body result}
        result))))

(defn- wrap-methods
  [params wrapper-fn]
  (reduce (fn [m method]
            (if (contains? m method)
              (update m method wrapper-fn)
              m))
          params
          all-methods))

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
        uri (or uri (autogen-endpoint *ns* sym))
        [initial-fx & kvs] (if (vector? (first rest-args))
                             rest-args
                             (into [nil] rest-args))
        route-name (keyword (str *ns*) (str sym))
        initial-fx-sym (gensym "initial-fx")
        params-sym (gensym "params")]
    `(def ~sym
       (let [~initial-fx-sym ~initial-fx
             [& {:as ~params-sym}] [~@kvs]]
         (@#'route* ~uri
                    ~route-name
                    (-> ~params-sym
                        (update-vals @#'wrap-hiccup)
                        ~@(when initial-fx
                            [`(@#'wrap-methods @#'wrap-result)])
                        (merge {:start (fn [{:keys [~'request-method]}]
                                         ~(if initial-fx
                                            `{:biff.fx/result ~initial-fx-sym
                                              :biff.fx/next ~'request-method}
                                            `{:biff.fx/next ~'request-method}))})))))))
