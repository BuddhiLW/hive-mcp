(ns hive-mcp.tools.cli
  "CLI-style subcommand dispatcher for consolidated tools.

   Supports n-depth nested handler trees via parse-command + resolve-handler.
   Single-word commands remain backward compatible."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

;; =============================================================================
;; Command Normalization
;; =============================================================================

(defn- normalize-command
  "Normalize command to string. Handles keyword, string, or nil."
  [command]
  (cond
    (keyword? command) (name command)
    (string? command) command
    :else nil))

;; =============================================================================
;; Help Formatting (supports nested trees)
;; =============================================================================

(defn- collect-command-paths
  "Collect all command paths from a handler tree.
   Returns a seq of keyword vectors like [[:status] [:status :list]].
   Skips :_handler entries but includes parent path when :_handler exists."
  [handlers prefix]
  (reduce-kv
   (fn [acc k v]
     (if (= k :_handler)
       acc
       (cond
         (fn? v)
         (conj acc (conj prefix k))

         (map? v)
         (let [nested (collect-command-paths v (conj prefix k))
               ;; If subtree has _handler, also list the parent path as valid
               with-default (if (contains? v :_handler)
                              (into [(conj prefix k)] nested)
                              nested)]
           (into acc with-default))

         :else acc)))
   [] handlers))

(defn format-help
  "Format help text listing all available commands.
   Supports nested handler trees - shows full command paths."
  [handlers]
  (let [paths (collect-command-paths handlers [])
        sorted (sort-by #(str/join " " (map name %)) paths)]
    (str "Available commands:\n"
         (str/join "\n" (map #(str "  - " (str/join " " (map name %)))
                             sorted)))))

;; =============================================================================
;; Command Parsing
;; =============================================================================

(defn parse-command
  "Parse command string into keyword path.
   \"status list\" → [:status :list]
   \"spawn\" → [:spawn]"
  [command]
  (when (and command (not (str/blank? command)))
    (->> (str/split (str/trim command) #"\s+")
         (mapv keyword))))

;; =============================================================================
;; Handler Resolution (n-depth tree walking)
;; =============================================================================

(defn resolve-handler
  "Resolve handler from nested tree given command path.
   Returns {:handler fn :path-used [...] :remaining [...]}

   Supports:
   - Leaf handlers (fn)
   - Nested maps with :_handler for defaults
   - Partial matches falling back to :_handler"
  [handlers path]
  (loop [tree handlers
         used []
         remaining path]
    (cond
      ;; No more path - check for _handler or return tree
      (empty? remaining)
      (if-let [h (or (when (fn? tree) tree)
                     (get tree :_handler))]
        {:handler h :path-used used :remaining []}
        {:tree tree :path-used used})

      ;; Try next segment
      :else
      (let [seg (first remaining)
            next-node (get tree seg)]
        (cond
          ;; Leaf handler found
          (fn? next-node)
          {:handler next-node :path-used (conj used seg) :remaining (vec (rest remaining))}

          ;; Subtree found - recurse
          (map? next-node)
          (recur next-node (conj used seg) (rest remaining))

          ;; Not found - check for _handler fallback
          :else
          (if-let [default (get tree :_handler)]
            {:handler default :path-used used :remaining (vec remaining)}
            {:error :not-found :path-used used :remaining (vec remaining)}))))))

;; =============================================================================
;; CLI Handler Factory (n-depth dispatch)
;; =============================================================================

(defn make-cli-handler
  "Create a CLI-style handler that dispatches on :command param.

   handlers: map of keyword -> handler-fn (flat) or nested handler tree.
   Supports n-depth command dispatch: \"status list\" walks {:status {:list fn}}.
   Single-word commands remain backward compatible.

   Returns: fn that dispatches to appropriate handler"
  [handlers]
  (fn [{:keys [command] :as params}]
    (let [cmd-str (normalize-command command)
          path (parse-command cmd-str)]
      (cond
        ;; No command or empty → error
        (nil? path)
        {:isError true :text (str "Unknown command: " command
                                  ". Valid: " (keys handlers))}

        ;; Help at root level
        (= [:help] path)
        {:type "text" :text (format-help handlers)}

        ;; Normal dispatch via n-depth resolve-handler
        :else
        (let [result (resolve-handler handlers path)]
          (if-let [handler (:handler result)]
            (handler params)
            {:isError true :text (str "Unknown command: " command
                                      ". Valid: " (keys handlers))}))))))

;; =============================================================================
;; Batch Handler Factory (generic batch middleware)
;; =============================================================================

(defn make-batch-handler
  "Higher-order function: takes a handlers map (same as make-cli-handler),
   returns a handler that accepts {:operations [{:command ... :param1 ...}, ...], :parallel bool}.
   Maps each operation through existing dispatch, collects all results (no fail-fast).

   Shared params from the outer call (minus :operations/:parallel/:command) merge
   with per-op params. Per-op params win on conflict.

   Sequential by default, :parallel true uses pmap.

   Returns: {:results [...] :summary {:total N :success M :failed F}}"
  [handlers]
  (fn [{:keys [operations parallel] :as params}]
    (if (or (nil? operations) (empty? operations))
      {:isError true :text "operations is required (array of {command, ...} objects)"}
      (let [shared-params (dissoc params :operations :parallel :command)
            exec-fn       (if parallel pmap mapv)
            results       (exec-fn
                           (fn [op]
                             (try
                               (let [cmd-str (normalize-command (:command op))
                                     path    (parse-command cmd-str)]
                                 (if (nil? path)
                                   {:success false :command (:command op)
                                    :error "Missing or blank command in operation"}
                                   (let [resolved (resolve-handler handlers path)]
                                     (if-let [handler (:handler resolved)]
                                       (let [merged (merge shared-params (dissoc op :command))
                                             result (handler (assoc merged :command (:command op)))]
                                         {:success true :command (:command op) :result result})
                                       {:success false :command (:command op)
                                        :error (str "Unknown command: " (:command op))}))))
                               (catch Exception e
                                 {:success false :command (:command op) :error (ex-message e)})))
                           operations)]
        {:type "text"
         :text (json/write-str
                {:results (vec results)
                 :summary {:total   (count operations)
                           :success (count (filter :success results))
                           :failed  (count (remove :success results))}})}))))
