(ns com.biffweb.fx.impl
  "Implementation details for com.biffweb.fx. Do not use directly."
  (:require [clojure.walk :as walk])
  (:import [java.time Instant]
           [java.util Random]))

(defn truncate-str [s n]
  (if (<= (count s) n) s (str (subs s 0 (dec n)) "…")))

(defn truncate [data]
  (walk/postwalk
   (fn [data] (if (string? data) (truncate-str data 500) data))
   data))

(defn step [{:keys [state->transition-fn ctx state trace]}]
  (let [handle-var (requiring-resolve 'com.biffweb.fx/handle)
        handle-fn @handle-var
        overrides (:biff.fx/overrides ctx)
        handled-fx-keys (set (keys (methods handle-fn)))
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
                                               (keyword? (first v))
                                               (or (contains? overrides (first v))
                                                   (contains? handled-fx-keys (first v)))))
                          effect-keys (set (map key (filter effect-entry? m)))
                          state-output (apply dissoc m effect-keys)
                          fx-input (select-keys m effect-keys)
                         ctx (merge ctx state-output)
                         fx-output
                         (into {}
                                (map (fn [[k v]]
                                       (let [handler-key (first v)
                                             handler-args (rest v)
                                             override (get overrides handler-key)]
                                         [k (try
                                              (if override
                                                (apply override ctx handler-args)
                                                (apply handle-fn handler-key ctx handler-args))
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
     :state-output state-output
     :fx-output fx-output}))

(def all-methods
  [:get :post :put :delete :head :options :trace :patch :connect])

(defn safe-for-url? [s]
  (boolean (re-matches #"[a-zA-Z0-9-_.+!*]+" s)))

(defn autogen-endpoint [ns* sym]
  (let [href (str "/_biff/api/" ns* "/" sym)]
    (doseq [segment [ns* sym]]
      (assert (safe-for-url? (str segment))
              (str "URL segment would contain invalid characters: " segment)))
    href))

(defn route*
  [uri route-name machine-fn & {:as state->transition-fn}]
  (let [machine* (machine-fn route-name state->transition-fn)]
    [uri
     (into {:name route-name}
           (comp (filter state->transition-fn)
                 (map (fn [method]
                        [method machine*])))
           all-methods)]))

(defn wrap-result
  [f]
  (fn [ctx]
    (f ctx (:biff.fx/result ctx))))

(defn wrap-hiccup
  [f]
  (fn [& args]
    (let [result (apply f args)]
      (if (and (vector? result) (keyword? (first result)))
        {:body result}
        result))))

(defn wrap-methods
  [params wrapper-fn]
  (reduce (fn [m method]
            (if (contains? m method)
              (update m method wrapper-fn)
              m))
          params
          all-methods))
