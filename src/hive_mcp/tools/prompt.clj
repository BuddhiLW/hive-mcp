(ns hive-mcp.tools.prompt
  "Prompt capture and analysis tool handlers.

   Extracted from hive-mcp.tools for modularity."
  (:require [hive-mcp.tools.core :refer [mcp-success mcp-error mcp-json]]
            [hive-mcp.prompt-capture :as prompt-capture]))

(defn handle-prompt-capture
  "Capture a well-structured prompt with analysis for RAG."
  [{:keys [prompt accomplishes well_structured improvements
           category tags quality source model context]}]
  (prompt-capture/handle-prompt-capture
   {:prompt prompt
    :accomplishes accomplishes
    :well_structured well_structured
    :improvements improvements
    :category category
    :tags tags
    :quality quality
    :source source
    :model model
    :context context}))

(defn handle-prompt-list
  "List captured prompts with optional filtering."
  [{:keys [category quality limit]}]
  (prompt-capture/handle-prompt-list
   {:category category :quality quality :limit limit}))

(defn handle-prompt-search
  "Search captured prompts by keyword."
  [{:keys [query limit]}]
  (prompt-capture/handle-prompt-search
   {:query query :limit limit}))

(defn handle-prompt-analyze
  "Analyze a prompt's structure without saving."
  [{:keys [prompt]}]
  (prompt-capture/handle-prompt-analyze {:prompt prompt}))

(defn handle-prompt-stats
  "Get statistics about captured prompts."
  [_]
  (prompt-capture/handle-prompt-stats nil))

;; Tool definitions for prompt capture handlers
(def tools
  [{:name "prompt_capture"
    :description "Capture a well-structured LLM prompt with analysis for RAG knowledge base. Stores prompt text, what it accomplishes, why it's well-structured, and improvement suggestions. Auto-categorizes and validates quality."
    :inputSchema {:type "object"
                  :properties {"prompt" {:type "string"
                                         :description "The prompt text to capture (verbatim)"}
                               "accomplishes" {:type "string"
                                               :description "What this prompt accomplishes"}
                               "well_structured" {:type "string"
                                                  :description "Why this prompt is well-structured"}
                               "improvements" {:type "string"
                                               :description "How the prompt could be improved (optional, auto-generated if not provided)"}
                               "category" {:type "string"
                                           :enum ["coding" "debug" "planning" "meta" "research" "config" "workflow" "architecture"]
                                           :description "Category (optional, auto-inferred from content)"}
                               "tags" {:type "array"
                                       :items {:type "string"}
                                       :description "Additional tags for categorization"}
                               "quality" {:type "string"
                                          :enum ["success" "partial" "failure" "untested"]
                                          :description "Quality/outcome rating (default: untested)"}
                               "source" {:type "string"
                                         :description "Source: 'user', 'observed', 'generated' (default: user)"}
                               "model" {:type "string"
                                        :description "Model used (e.g., claude-opus-4-5)"}
                               "context" {:type "string"
                                          :description "Additional context about the prompt"}}
                  :required ["prompt" "accomplishes" "well_structured"]}
    :handler handle-prompt-capture}

   {:name "prompt_list"
    :description "List captured prompts with optional filtering by category, quality, or tags."
    :inputSchema {:type "object"
                  :properties {"category" {:type "string"
                                           :enum ["coding" "debug" "planning" "meta" "research" "config" "workflow" "architecture"]
                                           :description "Filter by category"}
                               "quality" {:type "string"
                                          :enum ["success" "partial" "failure" "untested"]
                                          :description "Filter by quality rating"}
                               "limit" {:type "integer"
                                        :description "Maximum results to return (default: 20)"}}
                  :required []}
    :handler handle-prompt-list}

   {:name "prompt_search"
    :description "Search captured prompts by keyword in prompt text or accomplishes field."
    :inputSchema {:type "object"
                  :properties {"query" {:type "string"
                                        :description "Search keyword"}
                               "limit" {:type "integer"
                                        :description "Maximum results (default: 20)"}}
                  :required ["query"]}
    :handler handle-prompt-search}

   {:name "prompt_analyze"
    :description "Analyze a prompt's structure without saving. Returns quality score, category inference, and improvement suggestions."
    :inputSchema {:type "object"
                  :properties {"prompt" {:type "string"
                                         :description "The prompt text to analyze"}}
                  :required ["prompt"]}
    :handler handle-prompt-analyze}

   {:name "prompt_stats"
    :description "Get statistics about captured prompts including counts by category, quality, and recent entries."
    :inputSchema {:type "object" :properties {}}
    :handler handle-prompt-stats}])
