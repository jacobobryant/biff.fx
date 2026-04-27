(ns com.biffweb.fx
  "Effect system for Biff web applications.

   The core abstraction is `machine`: a state machine where each state
   is a pure function and effects happen while transitioning between
   states. This keeps application logic pure and testable.

   A machine is created with named state transition functions. Each
   function receives the context map and returns a map where:
     - Values that are vectors whose first element is an implemented
       `handle` dispatch key (or a key present in `:biff.fx/overrides`)
       → that effect is executed, and the result is stored under the map
       entry's key in the context
     - :biff.fx/next → the next state to transition to
     - Other keys → merged into context for subsequent states

   Effect handlers are defined with the `handle` multimethod. Tests may
   override handlers via `:biff.fx/overrides` in the context.
   :biff/now is injected as a java.time.Instant before each state runs.
   :biff.fx/seed is injected as a random long seed.
   :biff.fx/results is set from the last trace entry."
  (:require [com.biffweb.fx.impl :as impl])
  (:import [java.security SecureRandom]
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

;; === Effect dispatch ===

(defmulti handle
  "Executes an effect for `fx-key`.

   Libraries can provide effect implementations with:

     (defmethod handle :some.lib.fx/do-thing
       [_fx-key ctx & args]
       ...)"
  (fn [fx-key _ctx & _args] fx-key))

(defmethod handle :biff.fx/http
  [_fx-key _ctx request-or-requests]
  (let [hato-request (requiring-resolve 'hato.client/request)
        http* (fn [request]
                (try
                  (-> (hato-request request)
                      (assoc :url (:url request))
                      (dissoc :http-client))
                  (catch Exception e
                    (if (get request :throw-exceptions true)
                      (throw e)
                      {:url (:url request)
                       :exception e}))))]
    (if (map? request-or-requests)
      (http* request-or-requests)
      (mapv http* request-or-requests))))

(defmethod handle :biff.fx/slurp
  [_fx-key _ctx & args]
  (apply slurp args))

(defmethod handle :biff.fx/spit
  [_fx-key _ctx & args]
  (apply spit args))

(defmethod handle :biff.fx/sleep
  [_fx-key _ctx sleep-ms]
  (Thread/sleep (long sleep-ms)))

(defmethod handle :biff.fx/temp-dir
  [_fx-key _ctx & {:keys [prefix]}]
  (let [dir (java.nio.file.Files/createTempDirectory
             (or prefix "biff")
             (into-array java.nio.file.attribute.FileAttribute []))]
    (.toFile dir)))

(defmethod handle :biff.fx/secure-random-int
  [_fx-key _ctx n]
  (.nextInt (SecureRandom.) n))

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
       (let [{:keys [next-state ctx trace state-output fx-output]}
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
           (let [result (merge state-output
                               (into {} (remove (fn [[k _]] (.startsWith (name k) "_"))) fx-output))]
             (if (contains? result :biff.fx/return)
               (:biff.fx/return result)
               result))))))))

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
