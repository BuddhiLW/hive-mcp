(ns hive-mcp.dns.result
  "Lightweight Result monad for railway-oriented error handling.

   ok  results: {:ok value}
   err results: {:error category ...extra-data}")

(defn ok
  "Wrap a value in a success Result."
  [value]
  {:ok value})

(defn err
  "Create an error Result with category keyword and optional data map."
  ([category]
   {:error category})
  ([category data]
   (merge {:error category} data)))

(defn ok?
  "True if result is a success."
  [r]
  (and (map? r) (contains? r :ok)))

(defn err?
  "True if result is an error."
  [r]
  (and (map? r) (contains? r :error)))

(defn bind
  "Monadic bind. If result is ok, applies f to the unwrapped value.
   If result is err, short-circuits and returns the error unchanged.
   f must return a Result."
  [result f]
  (if (ok? result)
    (f (:ok result))
    result))

(defn map-ok
  "Functor map over ok values. Applies f to the unwrapped value and
   re-wraps in ok. Errors pass through unchanged.
   Unlike bind, f returns a plain value (not a Result)."
  [result f]
  (if (ok? result)
    (ok (f (:ok result)))
    result))

(defn map-err
  "Map over error values. Applies f to the error map (without :error key)
   and merges result back. Ok values pass through unchanged.
   f receives the full error result map."
  [result f]
  (if (err? result)
    (f result)
    result))

(defmacro let-ok
  "Monadic let for Results. Binds :ok values; short-circuits on first error.

   (let-ok [x (may-fail)
            y (use x)]
     (ok (+ x y)))"
  [bindings & body]
  (if (empty? bindings)
    `(do ~@body)
    (let [[sym expr & rest-bindings] bindings]
      `(let [r# ~expr]
         (if (ok? r#)
           (let [~sym (:ok r#)]
             (let-ok ~(vec rest-bindings) ~@body))
           r#)))))

(defmacro try-effect
  "Execute body in try/catch, returning ok on success or err on exception.
   Category defaults to :effect/exception.

   (try-effect (do-side-effect!))
   => (ok result) or (err :effect/exception {:message \"...\"})"
  [& body]
  `(try
     (ok (do ~@body))
     (catch Exception e#
       (err :effect/exception {:message (.getMessage e#)
                               :class  (str (class e#))}))))

(defmacro try-effect*
  "Like try-effect but with a custom error category.

   (try-effect* :io/read-failure (slurp path))
   => (ok content) or (err :io/read-failure {:message \"...\"})"
  [category & body]
  `(try
     (ok (do ~@body))
     (catch Exception e#
       (err ~category {:message (.getMessage e#)
                       :class  (str (class e#))}))))
