# kotoba-lang/json

[![CI](https://github.com/kotoba-lang/json/actions/workflows/ci.yml/badge.svg)](https://github.com/kotoba-lang/json/actions/workflows/ci.yml)

**Layer 1 (data) of the kotoba foundational stdlib** — pure-Clojure JSON
encode/decode with **no host dependency**. `langchain` and other vertical libs
currently punt JSON parsing to the host; this lib is the in-language
replacement that runs on kotoba-WASM. Zero third-party runtime deps; every
namespace is `.cljc` (JVM / SCI / ClojureScript / GraalVM / kotoba-WASM). See
[`docs/adr/ADR-kotoba-lang-foundational-stdlib.md`](https://github.com/kotoba-lang/kotoba-lang/blob/main/docs/adr/ADR-kotoba-lang-foundational-stdlib.md).

## Current surface

`kotoba.lang.json`:

- `encode` — Clojure data → JSON string (deterministic key order: map keys
  sorted, so output is content-addressable)
- `decode` — JSON string → Clojure data (object keys are strings; JSON has no
  keywords)
- `nil` ↔ `null`, `true/false`, integers vs doubles, full string escape set
  (`\" \\ \n \t \r \b \f \/ \uXXXX`), nested arrays/objects

## Install

```clojure
io.github.kotoba-lang/json {:git/sha "<sha>"}
```

## Use

```clojure
(require '[kotoba.lang.json :as json])

(json/encode {:name "ada" :age 36})   ;=> "{\"age\":36,\"name\":\"ada\"}"
(json/decode "{\"a\":1,\"b\":[2,3]}")  ;=> {"a" 1 "b" [2 3]}
```

## Verify

```sh
clojure -M:test
```
