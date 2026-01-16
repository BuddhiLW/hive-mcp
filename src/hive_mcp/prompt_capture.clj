(ns hive-mcp.prompt-capture
  "Prompt Engineering Knowledge Base - Clojure-native prompt capture system.

   Captures well-structured LLM prompts with analysis for RAG-based prompt improvement.

   Features:
   - Schema validation with Malli (delegated to hive-mcp.prompt.validation)
   - Taxonomy categories (coding, debug, planning, meta, research, config)
   - Quality filtering (success/failure/partial)
   - Pluggable storage via PromptStore protocol (CLARITY-L, DIP)
   - Confirmation display after save

   Usage:
     (capture-prompt {:prompt \"...\", :accomplishes \"...\", ...})
     (list-prompts)
     (list-prompts {:category :coding})
     (search-prompts \"keyword\")"
  (:require [clojure.string :as str]
            [hive-mcp.prompt.validation :as validation]
            [hive-mcp.prompt.storage :as storage]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;;; ============================================================
;;; Default Store Instance (DIP - depend on abstraction)
;;; ============================================================

(def ^:private default-store
  "Default OrgPromptStore instance using the default file path."
  (delay (storage/create-org-store)))

(defn- get-store
  "Get store instance, either from file-path or default."
  [file-path]
  (if file-path
    (storage/create-org-store file-path)
    @default-store))

;;; ============================================================
;;; Core API
;;; ============================================================

(defn format-confirmation
  "Format a confirmation message for display."
  [{:keys [id category quality tags prompt accomplishes]}]
  (str "╔══════════════════════════════════════════════════════════════╗\n"
       "║           📝 PROMPT CAPTURED SUCCESSFULLY                    ║\n"
       "╠══════════════════════════════════════════════════════════════╣\n"
       "║ ID: " id (str/join "" (repeat (- 55 (count id)) " ")) "║\n"
       "║ Category: " (name category) (str/join "" (repeat (- 50 (count (name category))) " ")) "║\n"
       "║ Quality: " (name quality) (str/join "" (repeat (- 51 (count (name quality))) " ")) "║\n"
       "║ Tags: " (str/join ", " (take 5 tags))
       (str/join "" (repeat (max 0 (- 54 (count (str/join ", " (take 5 tags))))) " ")) "║\n"
       "╠══════════════════════════════════════════════════════════════╣\n"
       "║ Preview:                                                     ║\n"
       "║ " (if (> (count prompt) 58)
              (str (subs prompt 0 55) "...")
              (str prompt (str/join "" (repeat (- 58 (count prompt)) " ")))) " ║\n"
       "╠══════════════════════════════════════════════════════════════╣\n"
       "║ Accomplishes:                                                ║\n"
       "║ " (if (> (count accomplishes) 58)
              (str (subs accomplishes 0 55) "...")
              (str accomplishes (str/join "" (repeat (max 0 (- 58 (count accomplishes))) " ")))) " ║\n"
       "╚══════════════════════════════════════════════════════════════╝"))

(defn capture-prompt
  "Capture a prompt with analysis. Returns the saved entry with confirmation.

   Required keys:
   - :prompt - The prompt text
   - :accomplishes - What the prompt accomplishes
   - :well-structured - Why it's well-structured

   Optional keys:
   - :improvements - Suggested improvements
   - :category - Category keyword (auto-inferred if not provided)
   - :tags - Vector of tag strings
   - :quality - :success, :partial, :failure, or :untested
   - :source - \"user\", \"observed\", \"generated\"
   - :model - Model used (e.g., \"claude-opus-4-5\")
   - :context - Additional context
   - :file-path - Custom storage file path"
  [{:keys [prompt accomplishes well-structured improvements
           category tags quality source model context file-path]
    :or {quality :untested
         source "user"
         tags []}}]
  (let [store (get-store file-path)
        inferred-category (or category (validation/infer-category prompt))
        auto-tags (into tags (get validation/category-tags inferred-category []))
        entry {:prompt prompt
               :accomplishes accomplishes
               :well-structured well-structured
               :improvements (or improvements
                                 (str/join "\n" (validation/suggest-improvements prompt)))
               :category inferred-category
               :tags (vec (distinct auto-tags))
               :quality quality
               :source source
               :model model
               :context context}
        validation-result (validation/validate-entry entry)]
    (if (:valid? validation-result)
      (let [save-result (storage/save-prompt! store entry)
            saved-entry (:entry save-result)]
        {:success true
         :entry saved-entry
         :quality-assessment (validation/assess-prompt-quality prompt)
         :confirmation (format-confirmation saved-entry)})
      {:success false
       :errors (:errors validation-result)})))

(defn list-prompts
  "List captured prompts with optional filtering.

   Options:
   - :category - Filter by category
   - :quality - Filter by quality rating
   - :tags - Filter by tags (any match)
   - :limit - Max results
   - :file-path - Custom storage file path"
  [& [{:keys [category quality tags limit file-path]
       :or {limit 50}}]]
  (let [store (get-store file-path)]
    (storage/list-prompts store {:category category
                                 :quality quality
                                 :tags tags
                                 :limit limit})))

(defn search-prompts
  "Search prompts by keyword in prompt text or accomplishes."
  [query & [{:keys [file-path limit] :or {limit 20}}]]
  (let [store (get-store file-path)]
    (storage/search-prompts store query {:limit limit})))

(defn get-prompt
  "Get a specific prompt by ID."
  [id & [{:keys [file-path]}]]
  (let [store (get-store file-path)]
    (storage/get-prompt store id)))

(defn update-prompt-quality
  "Update the quality rating of a prompt."
  [id new-quality & [{:keys [file-path]}]]
  (let [store (get-store file-path)]
    (storage/update-prompt! store id {:quality new-quality})
    {:updated true :id id :quality new-quality}))

(defn get-statistics
  "Get statistics about captured prompts."
  [& [{:keys [file-path]}]]
  (let [store (get-store file-path)]
    (storage/get-statistics store)))

;;; ============================================================
;;; MCP Tool Handlers
;;; ============================================================

(defn handle-prompt-capture
  "MCP tool handler for /capture command."
  [{:keys [prompt accomplishes well_structured improvements
           category tags quality source model context]}]
  (try
    (let [result (capture-prompt {:prompt prompt
                                  :accomplishes accomplishes
                                  :well-structured well_structured
                                  :improvements improvements
                                  :category (when category (keyword category))
                                  :tags (or tags [])
                                  :quality (if quality (keyword quality) :untested)
                                  :source (or source "user")
                                  :model model
                                  :context context})]
      {:type "text" :text (pr-str result)})
    (catch Exception e
      {:type "text" :text (str "Error capturing prompt: " (.getMessage e)) :isError true})))

(defn handle-prompt-list
  "MCP tool handler for listing prompts."
  [{:keys [category quality limit] :as _entry}]
  (try
    (let [result (list-prompts {:category (when category (keyword category))
                                :quality (when quality (keyword quality))
                                :limit (or limit 20)})]
      {:type "text" :text (pr-str result)})
    (catch Exception e
      {:type "text" :text (str "Error listing prompts: " (.getMessage e)) :isError true})))

(defn handle-prompt-search
  "MCP tool handler for searching prompts."
  [{:keys [query limit]}]
  (try
    (let [result (search-prompts query {:limit (or limit 20)})]
      {:type "text" :text (pr-str result)})
    (catch Exception e
      {:type "text" :text (str "Error searching prompts: " (.getMessage e)) :isError true})))

(defn handle-prompt-analyze
  "MCP tool handler for analyzing a prompt without saving."
  [{:keys [prompt]}]
  (try
    (let [quality (validation/assess-prompt-quality prompt)
          improvements (validation/suggest-improvements prompt)
          category (validation/infer-category prompt)
          result {:analysis {:category category
                             :quality-score (:score quality)
                             :assessment (:assessment quality)
                             :structure {:has-context (:has-context? quality)
                                         :has-specific-task (:has-specific-task? quality)
                                         :has-constraints (:has-constraints? quality)
                                         :has-output-spec (:has-output-spec? quality)}
                             :suggested-improvements improvements
                             :suggested-tags (get validation/category-tags category [])}}]
      {:type "text" :text (pr-str result)})
    (catch Exception e
      {:type "text" :text (str "Error analyzing prompt: " (.getMessage e)) :isError true})))

(defn handle-prompt-stats
  "MCP tool handler for prompt statistics."
  [_]
  (try
    {:type "text" :text (pr-str (get-statistics))}
    (catch Exception e
      {:type "text" :text (str "Error getting stats: " (.getMessage e)) :isError true})))
