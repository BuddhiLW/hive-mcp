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

;; --- rescue: best-effort fallback (replaces 780+ try-catch-log patterns) -----

(defmacro rescue
  "Try body. On exception return fallback — error info as metadata (data, not strings).
   Eliminates the ubiquitous (try expr (catch Exception e (log ...) fallback)) nesting.
   Zero dependencies — error context attached via Clojure metadata, not logging.

   (rescue []  (traverse ids))        ;; => [] on failure, error in ^{::error {...}}
   (rescue nil (get-entry id))        ;; => nil on failure (nil can't carry meta)
   (rescue {}  (compute-stats data))  ;; => {} on failure, (::error (meta result)) for details

   Error data shape: {::error {:message \"...\" :form \"(traverse ids)\"}}"
  [fallback & body]
  `(try ~@body
        (catch Exception e#
          (let [fb# ~fallback]
            (if (instance? clojure.lang.IObj fb#)
              (with-meta fb# {::error {:message (.getMessage e#)
                                       :form    ~(str (first body))}})
              fb#)))))

(defn rescue-fn
  "Wrap f: on exception return fallback (default nil). For keep/map pipelines.
   Eliminates (keep (fn [x] (try (f x) (catch Exception _ nil))) coll).

   (keep (rescue-fn #(parse %)) items)           ;; nil on error → filtered by keep
   (map  (rescue-fn #(parse %) :not-found) items) ;; :not-found on error"
  ([f] (rescue-fn f nil))
  ([f fallback]
   (fn [& args]
     (try (apply f args)
          (catch Exception _ fallback)))))
