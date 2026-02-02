(ns hive-mcp.schema.elisp
  "Elisp struct generation from Malli schemas.

   This namespace provides functions to generate Elisp cl-defstruct code
   from Malli :map schemas, enabling type-safe data exchange between
   Clojure and Emacs Lisp.

   Usage:
   ```clojure
   (require '[hive-mcp.schema.elisp :as se])

   (def MemoryEntry
     [:map
      [:id :string]
      [:type [:enum :note :snippet :decision]]
      [:content :string]
      [:tags [:vector :string]]])

   (se/emit-struct MemoryEntry {:name \"hive-memory-entry\"})
   ;; => \"(cl-defstruct (hive-memory-entry (:constructor hive-memory-entry-create))\\n  id type content tags)\"
   ```"
  (:require [malli.core :as m]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Schema Introspection
;; =============================================================================

(defn- map-schema?
  "Check if schema is a :map schema."
  [schema]
  (= :map (m/type schema)))

(defn- extract-field-names
  "Extract field names from a :map schema.

   Returns a sequence of field name keywords.

   Example:
     (extract-field-names [:map [:id :string] [:name :string]])
     ;; => (:id :name)"
  [schema]
  (when (map-schema? schema)
    (->> (m/children schema)
         (map first))))

;; =============================================================================
;; Elisp Code Generation
;; =============================================================================

(defn- field-name->elisp
  "Convert a Clojure keyword field name to elisp symbol.

   Converts kebab-case to kebab-case (preserved) and removes namespace."
  [field-kw]
  (name field-kw))

(defn- elisp-value
  "Convert Clojure value to elisp representation."
  [v]
  (cond
    (nil? v) "nil"
    (true? v) "t"
    (false? v) "nil"
    (string? v) (pr-str v)
    (number? v) (str v)
    (keyword? v) (str "'" (name v))
    :else (pr-str v)))

(defn- format-struct-options
  "Format cl-defstruct options as elisp string.

   Currently supports:
   - :constructor - generates (:constructor <name>-create)"
  [struct-name opts]
  (let [constructor-name (or (:constructor opts)
                             (str struct-name "-create"))]
    (format "(:constructor %s)" constructor-name)))

(defn emit-struct
  "Generate elisp cl-defstruct from Malli :map schema.

   Takes a Malli schema and options map, returns elisp code as string.

   Options:
   - :name (required) - The struct name (e.g., \"hive-memory-entry\")
   - :constructor - Custom constructor name (default: <name>-create)
   - :include - Include another struct type (for inheritance)

   Example:
     (emit-struct
       [:map [:id :string] [:content :string]]
       {:name \"hive-note\"})
     ;; => \"(cl-defstruct (hive-note (:constructor hive-note-create))\\n  id content)\"

   Throws: ex-info if schema is not a :map or :name is missing"
  [schema opts]
  (let [struct-name (:name opts)]
    ;; Validate inputs
    (when-not struct-name
      (throw (ex-info "emit-struct requires :name option"
                      {:opts opts})))
    (when-not (map-schema? schema)
      (throw (ex-info "emit-struct requires a :map schema"
                      {:schema-type (m/type schema)
                       :schema schema})))

    (let [field-names (extract-field-names schema)
          fields-str (str/join " " (map field-name->elisp field-names))
          struct-opts (format-struct-options struct-name opts)
          include-clause (when-let [include (:include opts)]
                           (format " (:include %s)" include))]
      (format "(cl-defstruct (%s %s%s)\n  %s)"
              struct-name
              struct-opts
              (or include-clause "")
              fields-str))))

(defn emit-struct-with-defaults
  "Generate elisp cl-defstruct with default slot values.

   Like emit-struct but allows specifying default values for slots.

   Options:
   - :name (required) - The struct name
   - :defaults - Map of field-name -> default-value

   Example:
     (emit-struct-with-defaults
       [:map [:id :string] [:enabled :boolean]]
       {:name \"hive-config\"
        :defaults {:enabled true}})
     ;; => \"(cl-defstruct (hive-config (:constructor hive-config-create))\\n  id (enabled t))\""
  [schema opts]
  (let [struct-name (:name opts)
        defaults (or (:defaults opts) {})]
    ;; Validate inputs
    (when-not struct-name
      (throw (ex-info "emit-struct-with-defaults requires :name option"
                      {:opts opts})))
    (when-not (map-schema? schema)
      (throw (ex-info "emit-struct-with-defaults requires a :map schema"
                      {:schema-type (m/type schema)
                       :schema schema})))

    (let [field-names (extract-field-names schema)
          format-field (fn [field-kw]
                         (let [field-name (field-name->elisp field-kw)]
                           (if (contains? defaults field-kw)
                             (format "(%s %s)" field-name (elisp-value (get defaults field-kw)))
                             field-name)))
          fields-str (str/join " " (map format-field field-names))
          struct-opts (format-struct-options struct-name opts)]
      (format "(cl-defstruct (%s %s)\n  %s)"
              struct-name
              struct-opts
              fields-str))))

;; =============================================================================
;; Batch Generation
;; =============================================================================

(defn emit-structs
  "Generate multiple cl-defstruct definitions.

   Takes a sequence of [schema opts] pairs.
   Returns elisp code with all struct definitions.

   Example:
     (emit-structs
       [[[:map [:id :string]] {:name \"hive-item\"}]
        [[:map [:name :string] [:value :int]] {:name \"hive-pair\"}]])"
  [schema-opts-pairs]
  (->> schema-opts-pairs
       (map (fn [[schema opts]] (emit-struct schema opts)))
       (str/join "\n\n")))

;; =============================================================================
;; JSON-RPC Deserialization (plist → struct)
;; =============================================================================

(defn- extract-field-entries
  "Extract field entries from a :map schema.

   Returns a sequence of [field-key field-schema optional?] tuples.

   Example:
     (extract-field-entries [:map [:id :string] [:tags {:optional true} [:vector :string]]])
     ;; => ([:id :string false] [:tags [:vector :string] true])"
  [schema]
  (when (map-schema? schema)
    (for [child (m/children schema)]
      (let [[k props-or-schema schema-or-nil] (if (= 3 (count child))
                                                child
                                                [(first child) {} (second child)])
            field-schema (or schema-or-nil props-or-schema)
            optional? (and (map? props-or-schema)
                           (get props-or-schema :optional false))]
        [k field-schema optional?]))))

(defn- schema-type
  "Get the type tag from a Malli schema.
   Returns keywords like :map, :string, :enum, :vector, etc."
  [schema]
  (if (keyword? schema)
    schema
    (m/type schema)))

(defn- elisp-keyword
  "Convert Clojure keyword to Elisp keyword syntax."
  [kw]
  (str ":" (name kw)))

(defn- plist-get-expr
  "Generate plist-get expression for a field."
  [field-key]
  (format "(plist-get plist %s)" (elisp-keyword field-key)))

(defn- wrap-for-field-type
  "Wrap plist-get expression based on the field's type."
  [field-schema plist-expr]
  (let [typ (schema-type field-schema)]
    (case typ
      ;; Enums need string→symbol conversion
      :enum (format "(intern %s)" plist-expr)
      ;; Everything else passes through as-is
      plist-expr)))

(defn- wrap-optional-field
  "Wrap expression with default value handling for optional fields."
  [expr optional?]
  (if optional?
    (format "(or %s nil)" expr)
    expr))

(defn- field->plist-arg
  "Convert a schema field entry to elisp keyword argument for struct-create.
   Returns string like ':id (plist-get plist :id)'."
  [[field-key field-schema optional?]]
  (let [plist-expr (plist-get-expr field-key)
        typed-expr (wrap-for-field-type field-schema plist-expr)
        final-expr (wrap-optional-field typed-expr optional?)]
    (str (elisp-keyword field-key) " " final-expr)))

(defn emit-from-plist
  "Generate elisp -from-plist function from Malli schema.

   For converting JSON-decoded plists to structs.

   Args:
   - schema: A Malli [:map ...] schema
   - opts: Options map with:
     - :name (required) - Base name for the struct (e.g., \"hive-memory-entry\")

   Returns elisp code as string.

   Example:
   ```clojure
   (def MemoryEntry
     [:map
      [:id :string]
      [:type [:enum :note :snippet :decision]]
      [:content :string]
      [:tags [:vector :string]]])

   (emit-from-plist MemoryEntry {:name \"hive-memory-entry\"})
   ;; =>
   ;; \"(defun hive-memory-entry-from-plist (plist)
   ;;    \\\"Create hive-memory-entry from PLIST.\\\"
   ;;    (hive-memory-entry-create
   ;;     :id (plist-get plist :id)
   ;;     :type (intern (plist-get plist :type))
   ;;     :content (plist-get plist :content)
   ;;     :tags (plist-get plist :tags)))\"
   ```"
  [schema opts]
  (let [struct-name (:name opts)]
    ;; Validate inputs
    (when-not struct-name
      (throw (ex-info "emit-from-plist requires :name option" {:opts opts})))
    (when-not (map-schema? schema)
      (throw (ex-info "emit-from-plist requires a :map schema"
                      {:schema-type (m/type schema)
                       :schema schema})))

    (let [fn-name (str struct-name "-from-plist")
          create-fn (str struct-name "-create")
          entries (extract-field-entries schema)
          args-str (->> entries
                        (map field->plist-arg)
                        (str/join "\n   "))]
      (format "(defun %s (plist)
  \"Create %s from PLIST.\"
  (%s
   %s))"
              fn-name struct-name create-fn args-str))))

(defn emit-from-plist-all
  "Generate -from-plist functions for multiple schemas.

   Args:
   - schemas: Sequence of [schema opts] pairs

   Returns elisp code as string with all functions."
  [schemas]
  (->> schemas
       (map (fn [[schema opts]] (emit-from-plist schema opts)))
       (str/join "\n\n")))

;; =============================================================================
;; Validator Predicate Generation
;; =============================================================================

(defn- type->elisp-predicate
  "Convert a Malli type to an Elisp predicate function name.

   Type mapping:
   - :string → stringp
   - :int/:integer → integerp
   - :boolean → booleanp
   - :keyword → symbolp
   - :map → hash-table-p (or plist check)
   - :any → always true (no check needed)

   Returns nil for types that need special handling (enum, vector, maybe)."
  [schema-type]
  (case schema-type
    :string "stringp"
    :int "integerp"
    :integer "integerp"
    :boolean "booleanp"
    :keyword "symbolp"
    :symbol "symbolp"
    :number "numberp"
    :float "floatp"
    :double "floatp"
    :any nil
    nil))

(defn- enum-values
  "Extract enum values from [:enum ...] schema.
   Returns list of keyword/symbol values."
  [schema]
  (when (= :enum (schema-type schema))
    (m/children schema)))

(defn- maybe-inner-schema
  "Extract inner schema from [:maybe ...] schema.
   Returns inner schema or nil if not a maybe."
  [schema]
  (when (= :maybe (schema-type schema))
    (first (m/children schema))))

(defn- vector-inner-schema
  "Extract inner schema from [:vector ...] schema.
   Returns inner schema or nil if not a vector."
  [schema]
  (when (= :vector (schema-type schema))
    (first (m/children schema))))

(defn- generate-type-check
  "Generate elisp type check expression for a field.

   Args:
   - field-schema: The Malli schema for this field
   - accessor-expr: The elisp accessor expression (e.g., \"(hive-entry-id entry)\")

   Returns elisp expression string."
  [field-schema accessor-expr]
  (let [typ (schema-type field-schema)]
    (cond
      ;; Handle [:maybe X] - (or (null ...) (X-check ...))
      (= typ :maybe)
      (let [inner (maybe-inner-schema field-schema)
            inner-check (generate-type-check inner accessor-expr)]
        (format "(or (null %s) %s)" accessor-expr inner-check))

      ;; Handle [:enum ...] - (memq ... '(a b c))
      (= typ :enum)
      (let [values (enum-values field-schema)
            quoted-list (str "'(" (str/join " " (map name values)) ")")]
        (format "(memq %s %s)" accessor-expr quoted-list))

      ;; Handle [:vector ...] - listp (elisp lists represent vectors)
      (= typ :vector)
      (format "(listp %s)" accessor-expr)

      ;; Handle [:or ...] schemas - generate combined check
      (= typ :or)
      (let [children (m/children field-schema)
            checks (map #(generate-type-check % accessor-expr) children)]
        (format "(or %s)" (str/join " " checks)))

      ;; Handle primitive types
      :else
      (if-let [predicate (type->elisp-predicate typ)]
        (format "(%s %s)" predicate accessor-expr)
        ;; Unknown type - skip validation (truthy by default)
        "t"))))

(defn- generate-field-check
  "Generate elisp check expression for a schema field.

   Args:
   - struct-name: Name of the struct (e.g., \"hive-memory-entry\")
   - entry-var: Variable name in the elisp function (e.g., \"entry\")
   - field-entry: [field-key field-schema optional?] tuple

   Returns elisp expression string."
  [struct-name entry-var [field-key field-schema optional?]]
  (let [field-name (field-name->elisp field-key)
        accessor-expr (format "(%s-%s %s)" struct-name field-name entry-var)
        type-check (generate-type-check field-schema accessor-expr)]
    (if optional?
      ;; Optional fields: null is always OK
      (format "(or (null %s) %s)" accessor-expr type-check)
      type-check)))

(defn emit-validator
  "Generate elisp -valid-p predicate from Malli schema.

   Creates an elisp defun that validates a struct instance against
   the schema's type constraints.

   Args:
   - schema: A Malli [:map ...] schema
   - opts: Options map with:
     - :name (required) - Base name for the struct (e.g., \"hive-memory-entry\")
     - :var (optional) - Variable name in generated function (default: \"entry\")

   Returns elisp code as string.

   Type Mapping:
   - :string → stringp
   - :int/:integer → integerp
   - :boolean → booleanp
   - [:enum ...] → memq with quoted list
   - [:vector ...] → listp
   - [:maybe X] → (or (null ...) (X ...))

   Example:
   ```clojure
   (def MemoryEntry
     [:map
      [:id :string]
      [:type [:enum :note :snippet :decision]]
      [:content :string]])

   (emit-validator MemoryEntry {:name \"hive-memory-entry\"})
   ;; =>
   ;; \"(defun hive-memory-entry-valid-p (entry)
   ;;    \\\"Validate ENTRY against hive-memory-entry schema.\\\"
   ;;    (and (hive-memory-entry-p entry)
   ;;         (stringp (hive-memory-entry-id entry))
   ;;         (memq (hive-memory-entry-type entry) '(note snippet decision))
   ;;         (stringp (hive-memory-entry-content entry))))\"
   ```"
  [schema opts]
  (let [struct-name (:name opts)
        entry-var (or (:var opts) "entry")]
    ;; Validate inputs
    (when-not struct-name
      (throw (ex-info "emit-validator requires :name option" {:opts opts})))
    (when-not (map-schema? schema)
      (throw (ex-info "emit-validator requires a :map schema"
                      {:schema-type (m/type schema)
                       :schema schema})))

    (let [fn-name (str struct-name "-valid-p")
          struct-predicate (str struct-name "-p")
          entries (extract-field-entries schema)
          field-checks (map #(generate-field-check struct-name entry-var %) entries)
          checks-str (str/join "\n       " field-checks)]
      (format "(defun %s (%s)
  \"Validate %s against %s schema.\"
  (and (%s %s)
       %s))"
              fn-name
              (str/upper-case entry-var)
              (str/upper-case entry-var)
              struct-name
              struct-predicate
              entry-var
              checks-str))))

(defn emit-validators
  "Generate -valid-p functions for multiple schemas.

   Args:
   - schemas: Sequence of [schema opts] pairs

   Returns elisp code as string with all validator functions."
  [schemas]
  (->> schemas
       (map (fn [[schema opts]] (emit-validator schema opts)))
       (str/join "\n\n")))

(comment
  ;; Example usage
  (def MemoryEntry
    [:map
     [:id :string]
     [:type [:enum :note :snippet :decision]]
     [:content :string]
     [:tags [:vector :string]]])

  (emit-struct MemoryEntry {:name "hive-memory-entry"})
  ;; => "(cl-defstruct (hive-memory-entry (:constructor hive-memory-entry-create))\n  id type content tags)"

  (emit-struct-with-defaults
   [:map [:id :string] [:enabled :boolean]]
   {:name "hive-config"
    :defaults {:enabled true}})
  ;; => "(cl-defstruct (hive-config (:constructor hive-config-create))\n  id (enabled t))"

  (emit-validator MemoryEntry {:name "hive-memory-entry"})
  ;; => "(defun hive-memory-entry-valid-p (ENTRY)
  ;;       \"Validate ENTRY against hive-memory-entry schema.\"
  ;;       (and (hive-memory-entry-p entry)
  ;;            (stringp (hive-memory-entry-id entry))
  ;;            (memq (hive-memory-entry-type entry) '(note snippet decision))
  ;;            (stringp (hive-memory-entry-content entry))
  ;;            (listp (hive-memory-entry-tags entry))))"
  )
