(ns json.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [json.core :as json]
            [kotoba.json :as kotoba-json]
            [kotoba.lang.json :as lang-json]))

(deftest pretty-escaping
  (is (= "\"he said \\\"hi\\\"\"" (json/json "he said \"hi\"")))
  (is (= "\"a\\\\b\"" (json/json "a\\b")))
  (is (= "\"x\\r\\n\\ty\"" (json/json "x\r\n\ty")))
  (is (not (str/includes? (json/json "a\rb") "\r"))))

(deftest control-characters-outside-the-seven-named-escapes-are-escaped
  ;; RFC 8259 requires EVERY control character U+0000-U+001F to be escaped,
  ;; not just \" \\ \b \f \n \r \t -- the fallback case used to pass the
  ;; rest through raw, which real JSON parsers (python's json, jq) reject
  ;; as an invalid unescaped control character.
  (is (= "\"\\u0001\"" (json/json (str (char 1)))) "0x01, not one of the 7 named escapes")
  (is (= "\"\\u001f\"" (json/json (str (char 0x1f)))) "0x1F, the last C0 control code")
  (is (= (str (char 1) "hi" (char 0x1f))
         (get (json/decode (json/encode {"x" (str (char 1) "hi" (char 0x1f))})) "x"))
      "round-trips through encode+decode"))

(deftest pretty-scalars-and-empties
  (is (= "{}" (json/json {})))
  (is (= "[]" (json/json [])))
  (is (= "null" (json/json nil)))
  (is (= "true" (json/json true)))
  (is (= "\"k\"" (json/json :k))))

(deftest encode-is-compact-and-sorted
  (is (= "{\"a\":1,\"b\":[2,\"x\"]}" (json/encode {:b [2 :x] :a 1}))))

(deftest decode-parses-json
  (is (= {"a" 1 "b" [2 "x"] "c" nil}
         (json/decode "{\"a\":1,\"b\":[2,\"x\"],\"c\":null}")))
  (is (= {"s" "tab\there, quote\" and back\\slash, CR\r LF\n"}
         (json/decode (json/encode {"s" "tab\there, quote\" and back\\slash, CR\r LF\n"})))))

(deftest decode-rejects-malformed-numbers-instead-of-truncating
  ;; The number scanner grabs any run of digit/e/E/+/-/. chars -- it does
  ;; NOT itself validate JSON number syntax before handing the token to the
  ;; host parser. JVM's Double/parseDouble and Long/parseLong both throw on
  ;; a malformed token (fail closed) -- but cljs's js/parseFloat/js/parseInt
  ;; are lenient and silently parse just the valid PREFIX instead of
  ;; throwing (parseFloat("1.2.3") => 1.2, parseInt("12--3", 10) => 12), so
  ;; the exact same malformed input that correctly errors here on :clj
  ;; would have silently decoded to a truncated, wrong number on :cljs.
  ;; This must reject on BOTH platforms -- verified here on :clj (where the
  ;; explicit check added is redundant with what parseDouble/parseLong
  ;; already do, but proves the new validation itself is correct and not a
  ;; false-positive on any of these), and relied on for :cljs where it's
  ;; the ONLY thing standing between a malformed number and a silent
  ;; wrong-value bug.
  (doseq [bad ["1.2.3" "1e+5e3" "12--3" "1.5.6.7" "01" "1." ".5" "-" "1e"]]
    (is (thrown? Exception (json/decode bad))
        (str "must reject malformed number: " bad))))

(deftest compatibility-namespaces
  (is (= (json/json {:a [1 :b]}) (kotoba-json/json {:a [1 :b]})))
  (is (= (json/encode {:b 2 :a 1}) (lang-json/encode {:b 2 :a 1})))
  (is (= {"a" 1} (lang-json/decode "{\"a\":1}"))))
