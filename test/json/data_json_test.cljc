(ns json.data-json-test
  (:require [clojure.test :refer [deftest is testing]]
            [json.core :as core]
            [json.data-json :as json]))

(deftest read-str-round-trip
  (is (= {:a 1} (json/read-str "{\"a\":1}" {:key-fn keyword})))
  (is (= {"a" 1} (json/read-str "{\"a\":1}"))))

(deftest read-str-custom-key-fn
  (is (= {:x402-support true}
         (json/read-str "{\"x402Support\":true}"
                        {:key-fn (fn [k] (if (= k "x402Support") :x402-support (keyword k)))}))))

(deftest write-str-custom-key-fn
  (is (= "{\"foo\":1}"
         (json/write-str {:foo 1} {:key-fn (fn [k] (if (keyword? k) (name k) (str k)))}))))

(deftest read-from-reader-jvm
  #?(:clj
     (let [r (java.io.StringReader. "{\"ok\":true}")]
       (is (= {"ok" true} (json/read r))))))

(deftest matches-core-for-plain-maps
  (let [m {:name "ok" :xs [1 2]}]
    (is (= (core/encode m) (json/write-str m)))
    (is (= m (json/read-str (json/write-str m) {:key-fn keyword})))))
