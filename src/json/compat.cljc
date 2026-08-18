(ns json.compat
  "The two `cheshire.core` functions this workspace actually used, on top of
  `json.core`.

  Measured 2026-08-18 across the 34 repos that declared `cheshire/cheshire`:
  the entire surface in use was `parse-string` (114 call sites) and
  `generate-string` (111). Nothing else. Cheshire is a JVM-only library, and
  19 of those 34 required it from a `.cljc` file, so a portable namespace was
  pinned to the JVM by two function names.

  This namespace exists so retiring that dependency is a require-line change
  rather than 222 call-site edits. It is a MIGRATION surface, not the
  preferred API: new code should call `json.core/encode` and
  `json.core/decode` directly, which say what they do and have no arity that
  silently changes the shape of the result.

  Three behaviours are matched deliberately, because each one breaks SILENTLY
  if it is not:

  1. `(parse-string s true)` keywordizes keys, recursively. `json.core/decode`
     returns string keys. Renaming without this compiles, runs, and returns
     nil from every keyword lookup -- 37 of the 114 call sites pass `true`.
  2. `(parse-string nil)` is nil, not a throw. Cheshire returns nil for nil
     input and the common shape here is `(parse-string (:body resp) true)`
     where the body may be absent.
  3. `(generate-string x {:pretty true})` emits the 2-space form. 6 call sites
     pass opts and all 6 pass exactly this.

  NOT matched, and deliberately so: cheshire throws Jackson's
  `JsonParseException` on malformed input; `json.core/decode` throws an
  `ex-info`. Two repos catch the Jackson type by name (kotoba-captcha,
  cloud-itonami) and must be edited by hand -- a shim cannot honestly
  reproduce a class it does not depend on."
  (:require [json.core :as core]))

(defn keywordize
  "Recursively convert map keys to keywords, as cheshire's `true` flag does.
  Vectors are walked; other values are returned unchanged."
  [x]
  (cond
    (map? x) (persistent!
              (reduce-kv (fn [m k v]
                           (assoc! m (if (string? k) (keyword k) k) (keywordize v)))
                         (transient {}) x))
    (vector? x) (mapv keywordize x)
    (seq? x) (map keywordize x)
    :else x))

(defn parse-string
  "cheshire.core/parse-string. `nil` in, `nil` out. With a truthy second
  argument, map keys become keywords."
  ([s] (parse-string s false))
  ([s key-fn]
   (when (some? s)
     (let [v (core/decode s)]
       (if key-fn (keywordize v) v)))))

(defn generate-string
  "cheshire.core/generate-string. `{:pretty true}` selects the 2-space form."
  ([x] (core/encode x))
  ([x opts] (if (:pretty opts) (core/json x) (core/encode x))))
