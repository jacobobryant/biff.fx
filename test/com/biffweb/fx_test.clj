(ns com.biffweb.fx-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.biffweb.fx :as fx]))

;; === Machine basics ===

(deftest machine-runs-start-and-returns
  (let [m (fx/machine ::basic
            :start (fn [_] {:status 200 :body "ok"}))]
    (is (= {:status 200 :body "ok"} (m {:biff.fx/handlers {}})))))

(deftest machine-chains-states-with-effects
  (let [m (fx/machine ::chain
            :start (fn [_] {:biff.fx/db :load :biff.fx/next :second})
            :second (fn [{:keys [biff.fx/db]}] {:result db}))]
    (is (= {:result :loaded}
           (m {:biff.fx/handlers
               {:biff.fx/db (fn [_ _] :loaded)}})))))

(deftest machine-runs-effects-and-stores-results
  (let [m (fx/machine ::effects
            :start (fn [_] {:biff.fx/double 5 :biff.fx/next :use-result})
            :use-result (fn [ctx] {:result (:biff.fx/double ctx)}))]
    (is (= {:result 10}
           (m {:biff.fx/handlers
               {:biff.fx/double (fn [_ n] (* 2 n))}})))))

(deftest machine-two-arity-runs-single-state
  (let [m (fx/machine ::two-arity
            :start (fn [_] {:biff.fx/db :q :biff.fx/next :second})
            :second (fn [{:keys [x]}] {:result (* x 2)}))]
    (is (= {:result 10} (m {:x 5} :second)))))

(deftest machine-injects-biff-now
  (let [m (fx/machine ::now-test
            :start (fn [ctx] {:now-class (class (:biff/now ctx))}))]
    (is (= java.time.Instant
           (:now-class (m {:biff.fx/handlers {}}))))))

(deftest machine-injects-seed
  (let [m (fx/machine ::seed-test
            :start (fn [ctx] {:has-seed (some? (:biff.fx/seed ctx))}))]
    (is (true? (:has-seed (m {:biff.fx/handlers {}}))))))

(deftest machine-injects-results
  (let [m (fx/machine ::results-test
            :start (fn [_] {:biff.fx/db :q :biff.fx/next :check})
            :check (fn [ctx] {:has-results (some? (:biff.fx/results ctx))}))]
    (is (true? (:has-results
                (m {:biff.fx/handlers
                    {:biff.fx/db (fn [_ _] :ok)}}))))))

(deftest machine-allows-effectless-transition
  (let [m (fx/machine ::pure
            :start (fn [_] {:x 1 :biff.fx/next :second})
            :second (fn [{:keys [x]}] {:result (inc x)}))]
    (is (= {:result 2}
           (m {:biff.fx/handlers {}})))))

(deftest machine-extra-context-ignored-in-output
  (let [m (fx/machine ::extra
            :start (fn [_] {:hello "there"}))]
    (is (= {:hello "there"}
           (m {:extra "stuff" :biff.fx/handlers {}})))))

(deftest machine-invalid-state-throws
  (let [m (fx/machine ::invalid
            :start (fn [_] {:biff.fx/next :nonexistent}))]
    (is (thrown-with-msg? Exception #"Exception while running biff.fx machine"
          (m {:biff.fx/handlers {}})))))

(deftest machine-effect-exception-wraps-context
  (let [m (fx/machine ::fx-err
            :start (fn [_] {:biff.fx/boom "input"}))]
    (is (thrown-with-msg? Exception #"Exception while running biff.fx"
          (m {:biff.fx/handlers
              {:biff.fx/boom (fn [_ _] (throw (Exception. "boom")))}})))))

(deftest machine-multiple-effects-in-one-state
  (let [m (fx/machine ::multi-fx
            :start (fn [_] {:biff.fx/a 1 :biff.fx/b 2 :biff.fx/next :check})
            :check (fn [ctx] {:a (:biff.fx/a ctx) :b (:biff.fx/b ctx)}))]
    (is (= {:a 10 :b 20}
           (m {:biff.fx/handlers
               {:biff.fx/a (fn [_ n] (* 10 n))
                :biff.fx/b (fn [_ n] (* 10 n))}})))))

(deftest machine-results-contain-previous-fx-output
  (let [m (fx/machine ::results-check
            :start (fn [_] {:biff.fx/db :query :biff.fx/next :check})
            :check (fn [ctx]
                     {:results (:biff.fx/results ctx)}))]
    (is (= {:results [{:biff.fx/db :result-val}]}
           (m {:biff.fx/handlers
               {:biff.fx/db (fn [_ _] :result-val)}})))))

;; === Deterministic randomness ===

(deftest uuid-deterministic
  (let [[a _] (fx/uuid 42)
        [b _] (fx/uuid 42)]
    (is (= a b))))

(deftest uuid-returns-next-seed
  (let [[_ s1] (fx/uuid 42)
        [_ s2] (fx/uuid s1)]
    (is (not= s1 s2))))

(deftest uuid-returns-uuid-type
  (let [[u _] (fx/uuid 42)]
    (is (instance? java.util.UUID u))))

(deftest random-bytes-deterministic
  (let [[a _] (fx/random-bytes 42 16)
        [b _] (fx/random-bytes 42 16)]
    (is (java.util.Arrays/equals ^bytes a ^bytes b))))

(deftest random-bytes-correct-length
  (let [[bs _] (fx/random-bytes 42 32)]
    (is (= 32 (alength ^bytes bs)))))

(deftest random-bytes-returns-next-seed
  (let [[_ s1] (fx/random-bytes 42 16)
        [_ s2] (fx/random-bytes s1 16)]
    (is (not= s1 s2))))

;; === defmachine macro ===

(fx/defmachine test-machine
  :start (fn [{:keys [x]}] {:result (* x 3)}))

(deftest defmachine-creates-var
  (is (fn? test-machine)))

(deftest defmachine-runs-correctly
  (is (= {:result 15} (test-machine {:x 5 :biff.fx/handlers {}}))))

;; === Routing ===

(deftest safe-for-url-accepts-valid
  (is (fx/safe-for-url? "hello-world_123"))
  (is (fx/safe-for-url? "a.b.c"))
  (is (fx/safe-for-url? "foo+bar")))

(deftest safe-for-url-rejects-invalid
  (is (not (fx/safe-for-url? "hello world")))
  (is (not (fx/safe-for-url? "foo/bar")))
  (is (not (fx/safe-for-url? ""))))

(deftest route-star-creates-route-vector
  (let [[uri handler-map] (fx/route* "/test" ::test-route
                            :start (fn [{:keys [request-method]}]
                                     {:biff.fx/next request-method})
                            :get (fn [_] {:status 200}))]
    (is (= "/test" uri))
    (is (= ::test-route (:name handler-map)))
    (is (fn? (:get handler-map)))
    (is (nil? (:post handler-map)))))

(deftest wrap-hiccup-wraps-vectors
  (let [f (fx/wrap-hiccup (fn [] [:div "hello"]))]
    (is (= {:body [:div "hello"]} (f)))))

(deftest wrap-hiccup-passes-maps
  (let [f (fx/wrap-hiccup (fn [] {:status 200}))]
    (is (= {:status 200} (f)))))

(deftest wrap-pathom-passes-pathom-from-ctx
  (let [f (fx/wrap-pathom (fn [ctx pathom] {:ctx-keys (keys ctx) :pathom pathom}))]
    (is (= {:ctx-keys [:biff.fx/pathom] :pathom {:user "data"}}
           (f {:biff.fx/pathom {:user "data"}})))))
