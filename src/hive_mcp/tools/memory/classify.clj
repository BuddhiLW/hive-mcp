(ns hive-mcp.tools.memory.classify
  "Abstraction Level Auto-Classification (P1.5).

   Multi-tier classification pipeline:
     Tier 1: Type-based default from kg-schema (16 types mapped)
     Tier 2: Content-keyword bumping (heuristic signals)
     Tier 3: Enhanced similarity-based (extension, noop fallback)

   SOLID-S: Single responsibility - abstraction level classification only.
   CLARITY-Y: Enhanced classification via extension (noop fallback)."
  (:require [hive-mcp.extensions.registry :as ext]
            [hive-mcp.knowledge-graph.schema :as kg-schema]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Content Pattern Definitions
;; ============================================================

(def ^:private l4-patterns
  "Regex patterns that indicate L4 (Intent) abstraction.
   Matches axiom/decision/principle-level language."
  [#"(?i)\b(must\s+always|must\s+never|inviolable|non-negotiable)\b"
   #"(?i)^#*\s*(axiom|principle|ADR)\s*[:.]\s"
   #"(?i)\b(architectural\s+decision|design\s+rationale|strategic\s+decision)\b"
   #"(?i)\[ax\]"])  ; axiom marker convention

(def ^:private l3-patterns
  "Regex patterns that indicate L3 (Pattern) abstraction.
   Matches convention/idiom/pattern-level language."
  [#"(?i)^#*\s*(pattern|convention|idiom|recipe|guideline|workflow)\s*[:.]\s"
   #"(?i)\b(always\s+do|never\s+do|when\s+.*?,\s+(do|use)|best\s+practice)\b"
   #"(?i)\b(anti-pattern|code\s+smell|recurring\s+(pattern|structure))\b"])

(def ^:private l1-patterns
  "Regex patterns that indicate L1 (Disc/File) abstraction.
   Matches file-level, concrete code references."
  [#"(?i)\b(file|path|directory):\s*\S+"
   #"(?i)\b(git\s+commit|commit\s+hash|SHA-?256)\b"
   #"(?i)\bat\s+line\s+\d+"
   #"(?i)\bline\s+\d+[-\u2013]\d+\b"
   #"(?i)\b(kondo|lint|compilation)\s+(error|warning)\b"])

;; ============================================================
;; Classification Functions
;; ============================================================

(defn content-keyword-level
  "Analyze content text for abstraction level signals.
   Returns an integer level (1-4) if keywords suggest a level,
   or nil if no strong signal detected.

   Priority: L4 > L3 > L1 (L2 is the default, never explicitly detected).
   Only the first matching tier wins."
  [content]
  (when (and content (not (str/blank? content)))
    (let [content-str (str content)]
      (cond
        ;; Check L4 patterns first (highest priority bump)
        (some #(re-find % content-str) l4-patterns) 4

        ;; Check L3 patterns
        (some #(re-find % content-str) l3-patterns) 3

        ;; Check L1 patterns (downward bump)
        (some #(re-find % content-str) l1-patterns) 1

        ;; No strong signal - return nil (use type-based default)
        :else nil))))

(defn tag-level-signal
  "Check tags for abstraction level signals.
   Certain tags strongly correlate with specific levels.
   Returns integer level or nil."
  [tags]
  (let [tag-set (set (or tags []))]
    (cond
      ;; Tags that indicate L4 intent
      (some tag-set ["axiom" "principle" "ADR" "strategic"]) 4

      ;; Tags that indicate L3 patterns
      (some tag-set ["convention" "pattern" "idiom" "best-practice"
                     "anti-pattern" "workflow" "recipe"]) 3

      ;; Tags that indicate L1 file-level
      (some tag-set ["file-state" "kondo" "lint" "git-state"
                     "disc" "compilation"]) 1

      :else nil)))

(defn classify-abstraction-level
  "Auto-classify abstraction level for a memory entry.

   Multi-tier classification (highest specificity wins):
     1. Type-based default from kg-schema/derive-abstraction-level
     2. Content-keyword analysis (can bump up or down)
     3. Tag-based signal (secondary content signal)
     4. Enhanced similarity-based classification (extension)

   Bumping rules:
     - Content keywords can bump UP (e.g., note with 'must always' -> L4)
     - Content keywords can bump DOWN (e.g., note with 'at line 42' -> L1)
     - Tags provide secondary signal, used when content has no signal
     - Enhanced extension can override all heuristics if available

   Arguments:
     entry-type - String type (e.g., 'decision', 'note')
     content    - Entry content text
     tags       - Entry tags vector

   Returns:
     Integer abstraction level 1-4"
  [entry-type content tags]
  (let [;; Tier 1: Type-based default from comprehensive schema map
        type-level (kg-schema/derive-abstraction-level entry-type)

        ;; Tier 2: Content keyword analysis
        content-level (content-keyword-level content)

        ;; Tier 3: Tag-based signal
        tag-level (tag-level-signal tags)

        ;; Combine: content signal > tag signal > type default
        ;; Content bumps are always respected since they're most specific
        heuristic-level (or content-level tag-level type-level)

        ;; Tier 4: Enhanced similarity-based classification (extension)
        enhanced-level (when-let [classify-fn (ext/get-extension :gs/classify)]
                         (try
                           (classify-fn entry-type content tags heuristic-level)
                           (catch Exception _ nil)))]

    ;; Enhanced result overrides heuristics if available, otherwise use heuristic
    (or enhanced-level heuristic-level type-level)))
