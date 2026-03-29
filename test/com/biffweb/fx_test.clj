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
            :start (fn [_] {:db-result [:biff.fx/db :load] :biff.fx/next :second})
            :second (fn [{:keys [db-result]}] {:result db-result}))]
    (is (= {:result :loaded}
           (m {:biff.fx/handlers
               {:biff.fx/db (fn [_ _] :loaded)}})))))

(deftest machine-runs-effects-and-stores-results
  (let [m (fx/machine ::effects
            :start (fn [_] {:doubled [:biff.fx/double 5] :biff.fx/next :use-result})
            :use-result (fn [ctx] {:result (:doubled ctx)}))]
    (is (= {:result 10}
           (m {:biff.fx/handlers
               {:biff.fx/double (fn [_ n] (* 2 n))}})))))

(deftest machine-two-arity-runs-single-state
  (let [m (fx/machine ::two-arity
            :start (fn [_] {:db-result [:biff.fx/db :q] :biff.fx/next :second})
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
            :start (fn [_] {:db-result [:biff.fx/db :q] :biff.fx/next :check})
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
            :start (fn [_] {:boom-result [:biff.fx/boom "input"]}))]
    (is (thrown-with-msg? Exception #"Exception while running biff.fx"
          (m {:biff.fx/handlers
              {:biff.fx/boom (fn [_ _] (throw (Exception. "boom")))}})))))

(deftest machine-multiple-effects-in-one-state
  (let [m (fx/machine ::multi-fx
            :start (fn [_] {:a-result [:biff.fx/a 1] :b-result [:biff.fx/b 2] :biff.fx/next :check})
            :check (fn [ctx] {:a (:a-result ctx) :b (:b-result ctx)}))]
    (is (= {:a 10 :b 20}
           (m {:biff.fx/handlers
               {:biff.fx/a (fn [_ n] (* 10 n))
                :biff.fx/b (fn [_ n] (* 10 n))}})))))

(deftest machine-results-contain-previous-fx-output
  (let [m (fx/machine ::results-check
            :start (fn [_] {:db-result [:biff.fx/db :query] :biff.fx/next :check})
            :check (fn [ctx]
                     {:results (:biff.fx/results ctx)}))]
    (is (= {:results [{:db-result :result-val}]}
           (m {:biff.fx/handlers
               {:biff.fx/db (fn [_ _] :result-val)}})))))

(deftest machine-effect-with-multiple-args
  (let [m (fx/machine ::multi-args
            :start (fn [_] {:sum-result [:biff.fx/add 1 2 3] :biff.fx/next :check})
            :check (fn [ctx] {:result (:sum-result ctx)}))]
    (is (= {:result 6}
           (m {:biff.fx/handlers
               {:biff.fx/add (fn [_ctx & nums] (apply + nums))}})))))

(deftest machine-non-effect-vector-values-preserved
  (let [m (fx/machine ::vec-val
            :start (fn [_] {:data [:not-an-effect 1 2 3]}))]
    (is (= {:data [:not-an-effect 1 2 3]}
           (m {:biff.fx/handlers {}})))))

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
  (is (#'fx/safe-for-url? "hello-world_123"))
  (is (#'fx/safe-for-url? "a.b.c"))
  (is (#'fx/safe-for-url? "foo+bar")))

(deftest safe-for-url-rejects-invalid
  (is (not (#'fx/safe-for-url? "hello world")))
  (is (not (#'fx/safe-for-url? "foo/bar")))
  (is (not (#'fx/safe-for-url? ""))))

(deftest route-star-creates-route-vector
  (let [[uri handler-map] (#'fx/route* "/test" ::test-route
                            :start (fn [{:keys [request-method]}]
                                     {:biff.fx/next request-method})
                            :get (fn [_] {:status 200}))]
    (is (= "/test" uri))
    (is (= ::test-route (:name handler-map)))
    (is (fn? (:get handler-map)))
    (is (nil? (:post handler-map)))))

(deftest wrap-hiccup-wraps-vectors
  (let [f (#'fx/wrap-hiccup (fn [] [:div "hello"]))]
    (is (= {:body [:div "hello"]} (f)))))

(deftest wrap-hiccup-passes-maps
  (let [f (#'fx/wrap-hiccup (fn [] {:status 200}))]
    (is (= {:status 200} (f)))))

(deftest wrap-result-passes-result-from-ctx
  (let [f (#'fx/wrap-result (fn [ctx result] {:ctx-keys (keys ctx) :result result}))]
    (is (= {:ctx-keys [:biff.fx/result] :result {:user "data"}}
           (f {:biff.fx/result {:user "data"}})))))

;; === defroute macro ===

(fx/defroute test-route "/test-route"
  :get (fn [_ctx] {:status 200 :body "hello"}))

(deftest defroute-creates-route-vector
  (let [[uri handler-map] test-route]
    (is (= "/test-route" uri))
    (is (fn? (:get handler-map)))
    (is (nil? (:post handler-map)))))

(deftest defroute-handler-runs-machine
  (let [[_ handler-map] test-route
        handler (:get handler-map)]
    (is (= {:status 200 :body "hello"}
           (handler {:request-method :get :biff.fx/handlers {}})))))

(deftest defroute-wraps-hiccup
  (fx/defroute hiccup-route "/hiccup"
    :get (fn [_ctx] [:div "hello"]))
  (let [[_ handler-map] hiccup-route
        handler (:get handler-map)]
    (is (= {:body [:div "hello"]}
           (handler {:request-method :get :biff.fx/handlers {}})))))

(deftest defroute-with-initial-fx
  (fx/defroute fx-route "/fx-test"
    [:biff.fx/query {:q "data"}]
    :get (fn [_ctx result] {:body (str "got: " result)}))
  (let [[_ handler-map] fx-route
        handler (:get handler-map)]
    (is (= {:body "got: query-result"}
           (handler {:request-method :get
                     :biff.fx/handlers
                     {:biff.fx/query (fn [_ opts] (str "query-result"))}})))))

(deftest defroute-without-fx-no-wrap-result
  (fx/defroute no-fx-route "/no-fx"
    :get (fn [ctx] {:status 200}))
  (let [[_ handler-map] no-fx-route
        handler (:get handler-map)]
    (is (= {:status 200}
           (handler {:request-method :get :biff.fx/handlers {}})))))
