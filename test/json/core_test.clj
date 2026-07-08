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

(deftest compatibility-namespaces
  (is (= (json/json {:a [1 :b]}) (kotoba-json/json {:a [1 :b]})))
  (is (= (json/encode {:b 2 :a 1}) (lang-json/encode {:b 2 :a 1})))
  (is (= {"a" 1} (lang-json/decode "{\"a\":1}"))))
