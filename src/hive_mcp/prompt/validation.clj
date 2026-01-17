(ns hive-mcp.prompt.validation
  "Pure validation functions for prompt capture system.

   CLARITY-L: Layers stay pure - no I/O, only validation logic.

   Provides:
   - Malli schemas for prompt entries
   - Category inference from text
   - Quality assessment
   - Improvement suggestions"
  (:require [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;;; ============================================================
;;; Schema Definitions
;;; ============================================================

(def Category
  "Taxonomy of prompt categories."
  [:enum :coding :debug :planning :meta :research :config :workflow :architecture])

(def QualityRating
  "Quality/outcome rating for captured prompts."
  [:enum :success :partial :failure :untested])

(def PromptEntry
  "Schema for a captured prompt entry."
  [:map
   [:id :string]
   [:prompt :string]
   [:accomplishes :string]
   [:well-structured :string]
   [:improvements {:optional true} [:maybe :string]]
   [:category Category]
   [:tags [:vector :string]]
   [:quality QualityRating]
   [:created :string]
   [:updated {:optional true} [:maybe :string]]
   [:source {:optional true} [:maybe :string]] ; "user", "observed", "generated"
   [:model {:optional true} [:maybe :string]] ; "claude-opus-4-5", etc
   [:context {:optional true} [:maybe :string]]]) ; Additional context

(def PromptDatabase
  "Schema for the prompt database (org document)."
  [:map
   [:title :string]
   [:description {:optional true} :string]
   [:entries [:vector PromptEntry]]])

;;; ============================================================
;;; Schema Validation
;;; ============================================================

(defn validate-entry
  "Validate a prompt entry against schema.
   Returns {:valid? true :entry entry} or {:valid? false :errors ...}"
  [entry]
  (if (m/validate PromptEntry entry)
    {:valid? true :entry entry}
    {:valid? false
     :errors (me/humanize (m/explain PromptEntry entry))}))

(defn validate-prompt-input
  "Validate raw input before creating an entry.
   Returns {:valid? true} or {:valid? false :errors [...]}"
  [{:keys [prompt accomplishes well-structured]}]
  (let [errors (cond-> []
                 (str/blank? prompt) (conj "prompt is required")
                 (str/blank? accomplishes) (conj "accomplishes is required")
                 (str/blank? well-structured) (conj "well-structured is required"))]
    (if (empty? errors)
      {:valid? true}
      {:valid? false :errors errors})))

;;; ============================================================
;;; Taxonomy & Categories
;;; ============================================================

(def category-descriptions
  "Human-readable descriptions for each category."
  {:coding "Code writing, refactoring, implementation tasks"
   :debug "Debugging, error analysis, troubleshooting"
   :planning "Architecture, design decisions, roadmaps"
   :meta "Meta-prompts, prompt engineering, LLM guidance"
   :research "Information gathering, exploration, analysis"
   :config "Configuration, setup, deployment tasks"
   :workflow "Process automation, workflow definition"
   :architecture "System design, ADRs, technical decisions"})

(def category-tags
  "Default tags associated with each category."
  {:coding ["implementation" "code" "development"]
   :debug ["troubleshooting" "errors" "analysis"]
   :planning ["roadmap" "design" "decisions"]
   :meta ["prompt-engineering" "llm" "rag"]
   :research ["exploration" "investigation" "research"]
   :config ["setup" "configuration" "deployment"]
   :workflow ["automation" "process" "workflow"]
   :architecture ["architecture" "adr" "system-design"]})

(defn infer-category
  "Attempt to infer category from prompt text using keyword matching."
  [prompt-text]
  (let [text (str/lower-case prompt-text)]
    (cond
      (or (str/includes? text "debug")
          (str/includes? text "error")
          (str/includes? text "fix")
          (str/includes? text "failing")) :debug

      (or (str/includes? text "implement")
          (str/includes? text "write code")
          (str/includes? text "create function")
          (str/includes? text "refactor")) :coding

      (or (str/includes? text "plan")
          (str/includes? text "roadmap")
          (str/includes? text "milestone")
          (str/includes? text "design")) :planning

      (or (str/includes? text "prompt")
          (str/includes? text "llm")
          (str/includes? text "rag")
          (str/includes? text "meta")) :meta

      (or (str/includes? text "research")
          (str/includes? text "find")
          (str/includes? text "search")
          (str/includes? text "explore")) :research

      (or (str/includes? text "config")
          (str/includes? text "setup")
          (str/includes? text "deploy")
          (str/includes? text "install")) :config

      (or (str/includes? text "workflow")
          (str/includes? text "automate")
          (str/includes? text "process")) :workflow

      (or (str/includes? text "architecture")
          (str/includes? text "adr")
          (str/includes? text "system design")) :architecture

      :else :meta)))

;;; ============================================================
;;; Quality Assessment
;;; ============================================================

(defn assess-prompt-quality
  "Analyze prompt structure and return quality assessment.
   Returns map with :score, boolean flags, and :assessment string."
  [prompt-text]
  (let [has-context? (or (str/includes? prompt-text "context")
                         (str/includes? prompt-text "currently")
                         (str/includes? prompt-text "we have")
                         (> (count (str/split prompt-text #"\n")) 3))
        has-specific-task? (or (str/includes? prompt-text "create")
                               (str/includes? prompt-text "implement")
                               (str/includes? prompt-text "fix")
                               (str/includes? prompt-text "help me")
                               (str/includes? prompt-text "make"))
        has-constraints? (or (str/includes? prompt-text "should")
                             (str/includes? prompt-text "must")
                             (str/includes? prompt-text "don't")
                             (str/includes? prompt-text "ensure"))
        has-output-spec? (or (str/includes? prompt-text "return")
                             (str/includes? prompt-text "output")
                             (str/includes? prompt-text "format")
                             (str/includes? prompt-text "show"))
        score (+ (if has-context? 25 0)
                 (if has-specific-task? 25 0)
                 (if has-constraints? 25 0)
                 (if has-output-spec? 25 0))]
    {:score score
     :has-context? has-context?
     :has-specific-task? has-specific-task?
     :has-constraints? has-constraints?
     :has-output-spec? has-output-spec?
     :assessment (cond
                   (>= score 75) "Excellent - well-structured prompt"
                   (>= score 50) "Good - has most key elements"
                   (>= score 25) "Fair - missing some structure"
                   :else "Basic - could use more context/specificity")}))

(defn suggest-improvements
  "Generate improvement suggestions based on quality assessment."
  [prompt-text]
  (let [{:keys [has-context? has-specific-task? has-constraints? has-output-spec?]}
        (assess-prompt-quality prompt-text)]
    (cond-> []
      (not has-context?) (conj "Add context about current state or background")
      (not has-specific-task?) (conj "Be more specific about what task to accomplish")
      (not has-constraints?) (conj "Add constraints or requirements")
      (not has-output-spec?) (conj "Specify expected output format or structure"))))
