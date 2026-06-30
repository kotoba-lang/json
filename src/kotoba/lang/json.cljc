(ns kotoba.lang.json
  "Pure-Clojure JSON encode/decode for the kotoba foundational stdlib. Layer 1
  (data): no host capability, no third-party deps, no `clojure.data.json`. Runs
  on JVM / SCI / ClojureScript / GraalVM / kotoba-WASM.

  Why this exists: `langchain` and other vertical libs currently punt JSON
  parsing to the host (\"the host feeds us parsed maps\"). A capability-confined
  kotoba cell on WASM cannot assume a host JSON parser, so the language needs an
  in-language one. This is it.

  Encoding is deterministic: object keys are sorted (shorter-first, then
  bytewise) and set elements are sorted, so the same data always yields the same
  bytes — content-addressable, consistent with `dag-cbor` and `multiformats`.

  Decoding returns Clojure data with string keys (JSON has no keywords). `null`
  decodes to `nil`. Numbers without a fraction or exponent decode to integers
  (Long on the JVM); numbers with `.` or `e`/`E` decode to doubles."
  (:refer-clojure :exclude [read]))

;; ---------- encode ----------

(defn- int-like? [x]
  (and (number? x)
       (not (true? x))
       (not (false? x))
       (zero? (mod (double x) 1.0))))

(defn- escape-char [c]
  (case c
    \" "\\\""
    \\ "\\\\"
    \newline "\\n"
    \return "\\r"
    \tab "\\t"
    \backspace "\\b"
    \formfeed "\\f"
    ;; printable ASCII (0x20–0x7E) pass through; everything else → \uXXXX
    (let [cp #?(:clj  (long c)
                :cljs (.charCodeAt c 0))]
      (if (<= 0x20 cp 0x7E)
        (str c)
        ;; \u00XX — JSON requires 4 hex digits
        (str "\\u"
             (let [h #?(:clj  (Integer/toHexString cp)
                        :cljs (.toString cp 16))]
               (str (apply str (repeat (- 4 (count h)) \0)) h)))))))

(defn- encode-string [s]
  (str \" (reduce str "" (map escape-char s)) \"))

(declare encode)

(defn- key->string [k]
  (cond
    (string? k)  k
    (keyword? k) (name k)
    (number? k)  (str k)
    (symbol? k)  (name k)
    :else        (str k)))

(defn- sort-keyword-like [ks]
  ;; stable sort: shorter string form first, then bytewise — matches dag-cbor.
  (let [pairs (map (fn [k] [(key->string k) k]) ks)]
    (->> pairs
         (sort-by first
                  (fn [a b]
                    (let [la (count a) lb (count b)]
                      (cond
                        (< la lb) true
                        (> la lb) false
                        :else (neg? (compare a b))))))
         (mapv second))))

(defn- encode-map [m]
  (str "{"
       (->> (sort-keyword-like (keys m))
            (map (fn [k] (str (encode-string (key->string k))
                              ":"
                              (encode (get m k)))))
            (interpose ",")
            (reduce str ""))
       "}"))

(defn- encode-array [xs]
  (str "["
       (->> (seq xs)
            (map encode)
            (interpose ",")
            (reduce str ""))
       "]"))

(defn- encode-number [x]
  (if (int-like? x)
    (str #?(:clj  (long x)
            :cljs (Math/floor x)))
    (str (double x))))

(defn encode
  "Encode Clojure data `x` as a JSON string. Object keys are sorted for
  deterministic output. Keywords (as keys or values) become strings via `name`."
  [x]
  (cond
    (nil? x)            "null"
    (true? x)           "true"
    (false? x)          "false"
    (string? x)         (encode-string x)
    (keyword? x)        (encode-string (name x))
    (symbol? x)         (encode-string (name x))
    (number? x)         (encode-number x)
    (map? x)            (encode-map x)
    (vector? x)         (encode-array x)
    (set? x)            (encode-array (sort-by str (seq x)))
    (seq? x)            (encode-array x)
    :else               (encode-string (str x))))

;; ---------- decode ----------

(defn- parse-error [msg i]
  (ex-info msg {::reason :parse-error ::at i}))

(defn- skip-ws [s i]
  (let [n (count s)]
    (loop [i i]
      (if (and (< i n) (contains? #{\space \tab \newline \return} (nth s i)))
        (recur (inc i))
        i))))

(defn- parse-hex4 [s i]
  ;; reads 4 hex digits starting at i, returns [codepoint i+4]
  (let [n (count s)]
    (if (< (+ i 4) (inc n))
      (let [hex (subs s i (+ i 4))
            cp #?(:clj  (Integer/parseInt hex 16)
                  :cljs (js/parseInt hex 16))]
        (if (nil? cp)
          (throw (parse-error (str "bad \\u escape: " hex) i))
          [cp (+ i 4)]))
      (throw (parse-error "truncated \\u escape" i)))))

(defn- parse-escape
  "Parse a `\\` escape starting at index `i` (which points at the backslash).
  Returns `[char idx-after-escape]`. `n` is the string length. Throws on a
  truncated or unknown escape."
  [s i n]
  (let [ni (inc i)]
    (if (>= ni n)
      (throw (parse-error "truncated escape" ni))
      (let [e (nth s ni)]
        (case e
          \"  [\" (inc ni)]
          \\  [\\ (inc ni)]
          \/  [\/ (inc ni)]
          \b  [\backspace (inc ni)]
          \f  [\formfeed (inc ni)]
          \n  [\newline (inc ni)]
          \r  [\return (inc ni)]
          \t  [\tab (inc ni)]
          \u  (let [[cp i2] (parse-hex4 s (inc ni))]
                [#?(:clj  (char cp)
                    :cljs (.fromCharCode js/String cp)) i2])
          (throw (parse-error (str "bad escape: \\" e) ni)))))))

(defn- parse-string [s i]
  ;; i points at the opening quote; returns [string i-after-closing-quote]
  (let [n (count s)]
    (loop [i (inc i)                    ; skip opening quote
           out (transient [])]
      (if (>= i n)
        (throw (parse-error "unterminated string" i))
        (let [c (nth s i)]
          (cond
            (= c \")  [(apply str (persistent! out)) (inc i)]
            (= c \\)  (let [[ch ni] (parse-escape s i n)]
                        (recur ni (conj! out ch)))
            ;; normal char (control chars < 0x20 are invalid in JSON strings,
            ;; but we accept them leniently here for M1)
            :else      (recur (inc i) (conj! out c))))))))

(defn- parse-number [s i]
  ;; i points at the first number char; returns [number i-after-number]
  (let [n (count s)
        stop? (fn [c] (or (nil? c)
                          (contains? #{\, \] \} \space \tab \newline \return} c)))
        buf (loop [i i out (transient [])]
              (if (and (< i n) (not (stop? (nth s i))))
                (recur (inc i) (conj! out (nth s i)))
                [i out]))
        [i out] buf
        text (apply str (persistent! out))
        float? (or (some #(= % \.) text) (some #(#{\e \E} %) text))]
    (if (empty? text)
      (throw (parse-error "expected number" i))
      (let [v (if float?
                #?(:clj  (Double/parseDouble text)
                   :cljs (js/parseFloat text))
                #?(:clj  (Long/parseLong text)
                   :cljs (let [p (js/parseInt text 10)] (if (js/isNaN p) (throw (parse-error (str "bad number: " text) i)) p))))]
        (when (nil? v)
          (throw (parse-error (str "bad number: " text) i)))
        [v i]))))

(declare parse-value)

(defn- parse-array [s i]
  ;; i points at '['; returns [vector i-after-']']
  (let [n (count s)]
    (loop [i (skip-ws s (inc i)) out (transient [])]
      (cond
        (>= i n)            (throw (parse-error "unterminated array" i))
        (= (nth s i) \])    [(persistent! out) (inc i)]
        (= (nth s i) \,)    (recur (skip-ws s (inc i)) out)
        :else               (let [[v i2] (parse-value s i)]
                              (recur (skip-ws s i2) (conj! out v)))))))

(defn- parse-object [s i]
  ;; i points at '{'; returns [map i-after-'}']
  (let [n (count s)]
    (loop [i (skip-ws s (inc i)) out (transient {})]
      (cond
        (>= i n)            (throw (parse-error "unterminated object" i))
        (= (nth s i) \})    [(persistent! out) (inc i)]
        (= (nth s i) \,)    (recur (skip-ws s (inc i)) out)
        (= (nth s i) \")    (let [[k i2] (parse-string s i)
                                  i3 (skip-ws s i2)]
                              (if (or (>= i3 n) (not= (nth s i3) \:))
                                (throw (parse-error "expected ':' after key" i3))
                                (let [[v i4] (parse-value s (skip-ws s (inc i3)))]
                                  (recur (skip-ws s i4) (assoc! out k v)))))
        :else               (throw (parse-error (str "expected string key in object, got " (nth s i)) i))))))

(defn- parse-value [s i]
  (let [i  (skip-ws s i)
        n  (count s)]
    (if (>= i n)
      (throw (parse-error "unexpected end of input" i))
      (let [c (nth s i)]
        (case c
          \{  (parse-object s i)
          \[  (parse-array s i)
          \"  (parse-string s i)
          \t  (if (<= (+ i 4) n)
                (let [kw (subs s i (+ i 4))]
                  (if (= kw "true") [true (+ i 4)]
                    (throw (parse-error (str "bad literal: " kw) i))))
                (throw (parse-error "truncated true" i)))
          \f  (if (<= (+ i 5) n)
                (let [kw (subs s i (+ i 5))]
                  (if (= kw "false") [false (+ i 5)]
                    (throw (parse-error (str "bad literal: " kw) i))))
                (throw (parse-error "truncated false" i)))
          \n  (if (<= (+ i 4) n)
                (let [kw (subs s i (+ i 4))]
                  (if (= kw "null") [nil (+ i 4)]
                    (throw (parse-error (str "bad literal: " kw) i))))
                (throw (parse-error "truncated null" i)))
          ;; number
          (if (or (= c \-) (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} c))
            (parse-number s i)
            (throw (parse-error (str "unexpected char " (prn-str c)) i))))))))

(defn decode
  "Decode a JSON string into Clojure data. Object keys are strings. `null` →
  `nil`. Throws `ex-info` with `::reason :parse-error` and `::at` on malformed
  input."
  [s]
  (let [[v i] (parse-value s 0)
        i2    (skip-ws s i)]
    (if (= i2 (count s))
      v
      (throw (parse-error "trailing characters" i2)))))

(def read decode)
