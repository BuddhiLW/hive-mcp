(ns hive-mcp.agent.provider.policy
  "Provider PROMOTE stratum: pure data to data.

   Everything that decides WHICH provider, in WHAT order, and WITH WHAT model
   lives here, taking the world it needs as arguments (registry, configured
   overlay, present secret keys). No config read, no HTTP, no clock.

   May require `hive-mcp.agent.provider.model` and clojure.string, nothing
   else. `hive-mcp.agent.provider.strata-test` gates that."
  (:require [clojure.string :as str]
            [hive-mcp.agent.provider.model :as model]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; Registry overlay
;;; ---------------------------------------------------------------------------

(defn overlay
  "Overlay a config `:llm-providers` map onto `seed`, returning the effective
   registry.

   Per config value:
     map            merges into the seeded entry, field by field (config wins)
     false or nil   REMOVES the provider, the disable lever
     anything else  is ignored, leaving the seeded entry untouched

   A non-map `config-providers` (absent config) leaves the seed alone."
  [seed config-providers]
  (if (map? config-providers)
    (reduce-kv (fn [acc k v]
                 (cond
                   (not (keyword? k))       acc
                   (map? v)                 (update acc k merge v)
                   (or (nil? v) (false? v)) (dissoc acc k)
                   :else                    acc))
               seed
               config-providers)
    seed))

;;; ---------------------------------------------------------------------------
;;; Discovery order
;;; ---------------------------------------------------------------------------

(defn discovery-order
  "The order auto-discovery walks, over `registry`.

   `configured` (config :llm-provider-priority) REPLACES the order outright when
   it is a sequence: nothing is appended to an explicit list. Otherwise the
   order is `seed-order` first, then every provider known only to config,
   sorted by name so the answer is deterministic.

   Dispatch-routed entries are excluded either way: auto-discovery may only
   hand out OpenAI-compat providers, and `openai-compat-backend` refuses a
   dispatch-routed one. Unknown keys are dropped."
  [registry seed-order configured]
  (let [compat?   (fn [k] (model/openai-compat? (get registry k)))
        explicit? (sequential? configured)
        order     (if explicit? (mapv keyword configured) (vec seed-order))
        seeded    (filterv compat? order)]
    (if explicit?
      seeded
      (into seeded
            (->> (keys registry)
                 (remove (set order))
                 (filter compat?)
                 (sort-by name))))))

(defn available
  "Providers in `order` that can actually be called: an entry whose :secret-key
   is nil needs no auth, otherwise its key must be in `present-secret-keys`."
  [registry order present-secret-keys]
  (let [present? (set present-secret-keys)]
    (filterv (fn [p]
               (let [{:keys [secret-key]} (get registry p)]
                 (or (nil? secret-key) (contains? present? secret-key))))
             order)))

(defn secret-keys-of
  "The secret keys `order` would consult, in order, ready for a collector to
   resolve. nil entries (no auth needed) are dropped."
  [registry order]
  (into [] (comp (map #(:secret-key (get registry %))) (remove nil?)) order))

;;; ---------------------------------------------------------------------------
;;; Validation
;;; ---------------------------------------------------------------------------

(defn validate-provider
  "Validate a provider keyword against `registry`. nil on success, error map on
   failure."
  [registry provider]
  (when-not (contains? registry provider)
    {:error     :unknown-provider
     :requested provider
     :available (vec (keys registry))
     :fix       "Use one of the available providers, or add via: hive config set llm-providers.<name>.api-url <url>"}))

(defn validate-model
  "Validate a model against a provider's :available-models, when it declares
   any. nil on success, error map on failure."
  [registry provider model]
  (let [available (:available-models (get registry provider))]
    (when (and (seq available) (not (some #{model} available)))
      {:error     :unknown-model-for-provider
       :provider  provider
       :model     model
       :available (vec available)
       :fix       (str "Use one of the available models for " (name provider)
                       ", or add via: hive config set llm-providers." (name provider)
                       ".available-models [...]")})))

;;; ---------------------------------------------------------------------------
;;; Model-name policy
;;; ---------------------------------------------------------------------------

(defn parse-model-prefix
  "Parse an optional '<provider>:<model>' prefix, against `registry`.
   Returns [provider-kw-or-nil clean-model]. Only a prefix naming a provider
   the registry knows is recognized."
  [registry model]
  (if-let [idx (and (string? model) (str/index-of model ":"))]
    (let [prefix  (subs model 0 idx)
          tail    (subs model (inc idx))
          prov-kw (keyword prefix)]
      (if (contains? registry prov-kw)
        [prov-kw tail]
        [nil model]))
    [nil model]))

(defn claude-model-name?
  "True if the model string identifies a native Anthropic Claude model.
   Routes directly through Anthropic OAuth/API (bypassing OpenRouter relay).
   Strips `anthropic/` prefix so `anthropic/claude-sonnet-4-6` and
   `claude-sonnet-4-6` both match."
  [model]
  (when (string? model)
    (let [clean (if (str/starts-with? model "anthropic/")
                  (subs model (count "anthropic/"))
                  model)]
      (boolean (re-find #"^claude-" clean)))))

(defn strip-anthropic-prefix
  "Strip the `anthropic/` prefix from a model name so native Anthropic API
   receives `claude-sonnet-4-6` rather than `anthropic/claude-sonnet-4-6`
   (the latter is OpenRouter's naming scheme, not Anthropic's)."
  [model]
  (if (and (string? model) (str/starts-with? model "anthropic/"))
    (subs model (count "anthropic/"))
    model))

;;; ---------------------------------------------------------------------------
;;; Routing
;;; ---------------------------------------------------------------------------

(defn resolve-routing
  "Decide {:provider :model} for a spawn, purely.

   Inputs are the world already collected: `registry`, the caller's request,
   this agent-type's `type-defaults` from config, and `fallback-provider` (the
   best available one, or nil).

   Resolution order (first match wins):
     0. explicit :provider                    caller forced routing
     1. '<provider>:<model>' prefix in :model  e.g. 'venice:qwen3-...'
     2. Claude model-name auto-detection       routes to :anthropic OAuth
     3. type-defaults :provider from config
     4. fallback-provider

   Explicit routing (steps 0 and 1) always wins over Claude auto-detection.
   This is the privacy escape hatch: callers who need Claude models called
   through an anonymity-preserving relay (e.g. Venice, OpenRouter) can pass
   either `:provider :venice` OR prefix the model as `venice:claude-...`, and
   both bypass the OAuth shortcut. Normal use keeps in-plan Claude billing.

   Returns {:provider kw :model str}; validation belongs to the caller."
  [registry {:keys [provider model type-defaults fallback-provider]}]
  (let [explicit-prov             (some-> provider keyword)
        [prefix-prov clean-model] (parse-model-prefix registry model)
        candidate-model           (or clean-model (:model type-defaults))
        explicit-routing?         (or explicit-prov prefix-prov)
        claude?                   (and (not explicit-routing?)
                                       (claude-model-name? candidate-model))
        eff-provider              (or explicit-prov
                                      prefix-prov
                                      (when claude? :anthropic)
                                      (some-> type-defaults :provider keyword)
                                      fallback-provider)]
    {:provider eff-provider
     :model    (if claude?
                 (strip-anthropic-prefix candidate-model)
                 (or candidate-model
                     (:default-model (get registry eff-provider))))}))
