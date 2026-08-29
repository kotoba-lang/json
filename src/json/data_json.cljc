(ns json.data-json
  "clojure.data.json-compatible surface on `json.core`.

  Measured 2026-08-29: kotoba-lang still had 39 repos on
  `org.clojure/data.json` for `read-str` / `write-str` / `read` with
  `:key-fn`. This namespace exists so retiring that JVM dependency is a
  require-line change rather than hundreds of call-site edits.

  Migration surface, not the preferred API — new code should call
  `json.core/encode` and `json.core/decode` directly."
  (:refer-clojure :exclude [read])
  (:require [json.core :as core]
            #?(:clj [clojure.java.io :as io])))

(defn- transform-keys
  "Recursively apply `key-fn` to every map key."
  [key-fn x]
  (cond
    (map? x) (persistent!
              (reduce-kv (fn [m k v]
                           (assoc! m (key-fn k) (transform-keys key-fn v)))
                         (transient {}) x))
    (vector? x) (mapv #(transform-keys key-fn %) x)
    (seq? x) (map #(transform-keys key-fn %) x)
    :else x))

(defn read-str
  "Parse JSON from a string. With `:key-fn`, map keys are transformed
  recursively (same contract as clojure.data.json)."
  ([s] (read-str s {}))
  ([s {:keys [key-fn]}]
   (let [v (core/decode s)]
     (if key-fn (transform-keys key-fn v) v))))

(defn write-str
  "Serialize data to compact JSON. With `:key-fn`, map keys are transformed
  before encoding (same contract as clojure.data.json)."
  ([x] (write-str x {}))
  ([x {:keys [key-fn]}]
   (core/encode (if key-fn (transform-keys key-fn x) x))))

(defn read
  "Read JSON from a character stream. JVM only."
  ([r] (read r {}))
  ([r opts]
   #?(:clj (read-str (slurp r) opts)
      :cljs (throw (ex-info "json.data-json/read requires a JVM Reader" {})))))
