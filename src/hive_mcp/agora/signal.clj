(ns hive-mcp.agora.signal
  "Data-driven signal schema for Agora dialogues.

   Single source of truth for all signal types, validation, and construction.
   Replaces NLP-based signal detection with structured data.

   Signal Schema:
   {:type      keyword  ;; :propose :counter :approve :no-change :defer
    :strength  float    ;; 0.0-1.0 confidence/conviction
    :target    string   ;; What this signal targets (proposal ID, turn ref, etc.)
    :evidence  vector   ;; Supporting evidence [{:source :content :confidence}]
    :message   string}  ;; Human-readable argument text"

  (:require [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Signal Type Constants (Single Source of Truth)
;; =============================================================================

(def signal-types
  "All valid signal type keywords."
  #{:propose :counter :no-change :approve :defer})

(def equilibrium-signals
  "Signals that contribute toward Nash equilibrium (+1)."
  #{:no-change :approve :lgtm})

(def disruption-signals
  "Signals that reset/disrupt Nash equilibrium."
  #{:propose :counter})

(def neutral-signals
  "Signals that neither block nor contribute to equilibrium."
  #{:defer})

(def all-valid-signals
  "Union of all recognized signal keywords."
  (into signal-types #{:lgtm}))

;; =============================================================================
;; Signal Schema Defaults
;; =============================================================================

(def default-strength
  "Default signal strength when not specified."
  1.0)

(def default-evidence
  "Default empty evidence vector."
  [])

;; =============================================================================
;; Signal Construction
;; =============================================================================

(defn signal-from-map
  "Construct a validated signal from a structured map.

   Used by drones emitting structured JSON responses.

   Input map keys:
   - :type or :signal  - Signal keyword (required)
   - :strength or :confidence - Float 0.0-1.0 (default 1.0)
   - :target           - What this signal targets (optional)
   - :evidence          - Vector of evidence maps (optional)
   - :message           - Human-readable text (required)

   Returns: validated signal map or {:error ...}

   Example:
   (signal-from-map {:type :propose
                     :strength 0.8
                     :message \"Strategy pattern enables runtime switching\"
                     :evidence [{:source \"GOF\" :content \"Chapter 5\"}]})"
  [{:keys [type signal strength confidence target evidence message] :as input}]
  (let [sig-type (or (when type (keyword type))
                     (when signal (keyword signal)))
        sig-strength (or strength confidence default-strength)]
    (cond
      (nil? sig-type)
      {:error "Missing signal type (provide :type or :signal)"
       :input input}

      (not (all-valid-signals sig-type))
      {:error (str "Invalid signal type: " sig-type
                   ". Valid: " (str/join ", " (map name all-valid-signals)))
       :input input}

      (nil? message)
      {:error "Missing :message in signal"
       :input input}

      :else
      {:type sig-type
       :strength (min 1.0 (max 0.0 (double sig-strength)))
       :target target
       :evidence (vec (or evidence default-evidence))
       :message message})))

(defn signal-from-legacy
  "Parse signal from legacy string format [SIGNAL: X] prefix.

   Emits deprecation warning. Callers should migrate to signal-from-map.

   Returns: [signal-map cleaned-message detection-method]

   Example:
   (signal-from-legacy \"[SIGNAL: propose] Here's my change\")
   => [{:type :propose :strength 1.0 ...} \"Here's my change\" :prefix]"
  [message]
  (log/debug "Legacy signal parsing (consider migrating to structured signals)")
  (if-let [[_ signal-str rest] (re-matches #"(?i)\[SIGNAL:\s*([\w-]+)\]\s*([\s\S]*)" message)]
    (let [sig (keyword (str/lower-case signal-str))]
      (if (all-valid-signals sig)
        [{:type sig
          :strength default-strength
          :target nil
          :evidence default-evidence
          :message (str/trim rest)}
         (str/trim rest)
         :prefix]
        (do
          (log/warn "Unknown legacy signal" signal-str "- defaulting to :propose")
          [{:type :propose
            :strength default-strength
            :target nil
            :evidence default-evidence
            :message message}
           message
           :default])))
    ;; No prefix found - default to :propose
    [{:type :propose
      :strength default-strength
      :target nil
      :evidence default-evidence
      :message message}
     message
     :default]))

;; =============================================================================
;; Signal Validation
;; =============================================================================

(defn valid-signal?
  "Check if a signal map is valid (has required fields and valid type)."
  [signal-map]
  (and (map? signal-map)
       (contains? all-valid-signals (:type signal-map))
       (string? (:message signal-map))
       (number? (:strength signal-map))
       (not (:error signal-map))))

(defn signal-type
  "Extract the signal type keyword from a signal map."
  [signal-map]
  (:type signal-map))

;; =============================================================================
;; Signal Classification
;; =============================================================================

(defn equilibrium-contribution
  "Determine if a signal contributes to equilibrium.

   Returns: :positive, :negative, or :neutral"
  [signal-or-type]
  (let [t (if (map? signal-or-type) (:type signal-or-type) signal-or-type)]
    (cond
      (equilibrium-signals t) :positive
      (disruption-signals t) :negative
      (neutral-signals t) :neutral
      :else :negative)))

;; =============================================================================
;; Signal Formatting
;; =============================================================================

(defn format-signal-prefix
  "Format a signal type into legacy [SIGNAL: X] prefix.

   (format-signal-prefix :approve \"LGTM\")
   => \"[SIGNAL: approve] LGTM\""
  [signal-type message]
  (str "[SIGNAL: " (name signal-type) "] " message))

(defn signal->json-schema
  "Return the expected JSON schema for drone signal output.
   Used to inject into prompts."
  []
  {:type "object"
   :properties {:signal {:type "string"
                         :enum (mapv name signal-types)
                         :description "Nash equilibrium signal"}
                :message {:type "string"
                          :description "Your argument (2-4 sentences)"}
                :confidence {:type "number"
                             :minimum 0.0
                             :maximum 1.0
                             :description "Confidence in your position (0.0-1.0)"}
                :target {:type "string"
                         :description "What this signal targets (optional)"}
                :evidence {:type "array"
                           :items {:type "object"
                                   :properties {:source {:type "string"}
                                                :content {:type "string"}
                                                :confidence {:type "number"}}}
                           :description "Supporting evidence (optional)"}}
   :required ["signal" "message" "confidence"]})

;; =============================================================================
;; Parse Unified (Smart Dispatch)
;; =============================================================================

(defn parse-signal
  "Unified signal parser. Dispatches based on input type:
   - Map with :type/:signal -> signal-from-map (structured, preferred)
   - String -> signal-from-legacy (backward compat, deprecated)
   - Keyword -> wraps in minimal signal map

   Returns: signal map (always a map with :type :strength :message etc.)
            or {:error ...} on failure"
  ([input]
   (parse-signal input nil))
  ([input default-message]
   (cond
     ;; Already a valid signal map
     (and (map? input) (or (:type input) (:signal input)))
     (signal-from-map input)

     ;; String input - legacy parsing
     (string? input)
     (let [[signal-map _cleaned _method] (signal-from-legacy input)]
       signal-map)

     ;; Keyword shorthand
     (keyword? input)
     (if (all-valid-signals input)
       {:type input
        :strength default-strength
        :target nil
        :evidence default-evidence
        :message (or default-message (str "Signal: " (name input)))}
       {:error (str "Invalid signal keyword: " input)})

     :else
     {:error (str "Cannot parse signal from: " (type input))})))
