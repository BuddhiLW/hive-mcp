# Clojure Patterns & Code Smells Reference

Source: https://bsless.github.io/code-smells/ + https://clojurepatterns.com/

## Critical Code Smells to Watch For

### Mapping
- `(apply concat (map f xs))` -> `(mapcat f xs)`
- `(map #(f %) xs)` -> `(map f xs)`
- `(for [x xs] (f x))` -> `(map f xs)`
- Sequential lookup in vectors -> `group-by` or `clojure.set/index`

### Filtering
- `(filter #(not (p %)) xs)` -> `(remove p xs)`

### Emptiness
- `(when (not (empty? x)) ...)` -> `(when (seq x) ...)`
- `(when (= 0 (count x)) ...)` -> `(when (empty? x) ...)`

### Into Misuse
- `(into [] xs)` -> `(vec xs)`
- `(into #{} xs)` -> `(set xs)`
- `(into coll (map f xs))` -> `(into coll (map f) xs)` (transducer!)

### Map Operations
- Conditional assoc chains -> use `cond->`
- `{:a (:a m1) :b (:b m2)}` -> `select-keys` + `merge`
- Performance: `merge` is slow for large maps -> `reduce-kv` + transients

### Numbers
- `(= 0 x)` -> `(zero? x)`, `(> x 0)` -> `(pos? x)`, `(< x 0)` -> `(neg? x)`
- `(+ 1 x)` -> `(inc x)`, `(- x 1)` -> `(dec x)`

### Style
- `(-> x f)` -> `(f x)` (trivial threading)
- Nested when-let -> `some->`
- Let shadowing `(let [x (f0 x) x (f1 x)])` -> `(-> x f0 f1)`
- 7+ function params -> break into smaller fns or use map arg
- `[x y & {:keys [...]}]` -> `[x y opts-map]` (destructure opts-map inside)
- Positional returns `[(filtered) (removed)]` -> `{:true ... :false ...}`

## Polylith Architecture
- **Components**: reusable building blocks with interfaces (protocols/contracts)
- **Bases**: entry points that expose APIs
- **Projects**: deployed artifacts assembled from components + base
- **Workspace**: single monorepo containing all bricks
- Key insight: same components recomposed into different deployments

## Relevance to hive-mcp
- hive-mcp already follows component-like decomposition (agent, memory, kg, etc.)
- Could benefit from Polylith's interface discipline: explicit protocols per module
- Code smells to audit: check for `into []`, negated filters, manual merge patterns
