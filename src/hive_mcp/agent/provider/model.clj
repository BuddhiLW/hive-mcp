(ns hive-mcp.agent.provider.model
  "Provider DOMAIN: the value objects of the LLM-provider concept, and the seed
   data every other stratum reads.

   CPPB stratum: none. This namespace is pure values and predicates over them.
   It may require malli and clojure.string and nothing else — no config, no
   HTTP, no sibling stratum. `hive-mcp.agent.provider.strata-test` gates that.

   Vocabulary:
     provider entry  — how to reach one provider (endpoint, secret, default model)
     registry        — provider keyword -> entry
     dispatch-routed — an entry that is NOT an OpenAI-compat endpoint and must
                       be reached through its own client (e.g. Anthropic OAuth)"
  (:require [clojure.string :as str]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; Value objects
;;; ---------------------------------------------------------------------------

(def ChatCompletionsUrl
  "Malli schema for an OpenAI-compatible chat-completions endpoint URL."
  [:and
   [:string {:gen/fmap (fn [s]
                         (let [host (str/replace (str s) #"[^a-zA-Z0-9]" "")]
                           (str "https://api." (if (str/blank? host) "example" host)
                                ".test/v1/chat/completions")))}]
   [:fn {:error/message "must be a URL ending in /chat/completions"}
    (fn [s] (and (string? s) (str/ends-with? s "/chat/completions")))]])

(def ProviderEntry
  "Malli schema for one registry entry, dispatched on `:dispatch`.

   :anthropic-oauth branch — native Anthropic Messages API. Carries NO
     :api-url: it is not an OpenAI-compat chat-completions endpoint, and
     `openai-compat-backend` refuses it.
   default branch (no :dispatch) — OpenAI-compat provider: :api-url is
     REQUIRED and must end in \"/chat/completions\"; :secret-key may be nil
     (nil = no auth needed, e.g. :ollama-compat).

   Both branches are open maps: config overrides may add keys such as
   :available-models."
  [:multi {:dispatch :dispatch}
   [:anthropic-oauth
    [:map
     [:dispatch [:= :anthropic-oauth]]
     [:secret-key :keyword]
     [:default-model :string]
     [:available-models {:optional true} [:sequential :string]]]]
   [::m/default
    [:map
     [:api-url ChatCompletionsUrl]
     [:secret-key [:maybe :keyword]]
     [:default-model :string]
     [:available-models {:optional true} [:sequential :string]]]]])

(def ProviderRegistry
  "Malli schema for the provider registry: provider keyword -> ProviderEntry."
  [:map-of :keyword ProviderEntry])

(defn valid-provider-entry?
  "True when `entry` conforms to `ProviderEntry`."
  [entry]
  (m/validate ProviderEntry entry))

(defn dispatch-routed?
  "True when `entry` names its own client rather than an OpenAI-compat endpoint."
  [entry]
  (boolean (and (map? entry) (:dispatch entry))))

(defn openai-compat?
  "True when `entry` is a reachable OpenAI-compat provider entry."
  [entry]
  (and (map? entry) (not (dispatch-routed? entry))))

;;; ---------------------------------------------------------------------------
;;; Seeds
;;; ---------------------------------------------------------------------------

(def seed-registry
  "Known LLM providers, as a SEED.

   `:anthropic` is special — it uses Anthropic's native Messages API
   (OAuth when available, else API key) via hive-agent.llm.anthropic.
   The `:dispatch :anthropic-oauth` marker tells the spawn plumbing to
   route through the anthropic HTTP client rather than the OpenAI-compat
   path. All others hit OpenAI-compat /v1/chat/completions endpoints.

   This is a SEED, not the definition: config `:llm-providers` extends,
   overrides and REMOVES entries through `hive-mcp.agent.provider/effective-registry`,
   so a new provider is a config entry, never an edit here.

   It is also the ONE literal: `hive-mcp.config.merge/default-config`
   carries this var under `:llm-providers` rather than a second copy."
  {:anthropic     {:dispatch      :anthropic-oauth
                   :secret-key    :anthropic-api-key
                   :default-model "claude-sonnet-4-6"}
   :openrouter    {:api-url       "https://openrouter.ai/api/v1/chat/completions"
                   :secret-key    :openrouter-api-key
                   :default-model "anthropic/claude-opus-4-7"
                   :available-models ["moonshotai/kimi-k2.5"
                                      "qwen/qwen3.6-plus"
                                      "z-ai/glm-5.1"
                                      "xiaomi/mimo-v2-pro"
                                      "anthropic/claude-opus-4-7"
                                      "anthropic/claude-opus-4-6"
                                      "anthropic/claude-sonnet-4-6"]}
   :venice        {:api-url       "https://api.venice.ai/api/v1/chat/completions"
                   :secret-key    :venice-api-key
                   :default-model "venice-uncensored"
                   :available-models ["venice-uncensored"
                                      "qwen-3-6-plus"]}
   :groq          {:api-url       "https://api.groq.com/openai/v1/chat/completions"
                   :secret-key    :groq-api-key
                   :default-model "llama-3.3-70b-versatile"
                   :available-models ["llama-3.3-70b-versatile"]}
   :together      {:api-url       "https://api.together.xyz/v1/chat/completions"
                   :secret-key    :together-api-key
                   :default-model "meta-llama/Llama-3.3-70B-Instruct-Turbo"
                   :available-models ["meta-llama/Llama-3.3-70B-Instruct-Turbo"]}
   :fireworks     {:api-url       "https://api.fireworks.ai/inference/v1/chat/completions"
                   :secret-key    :fireworks-api-key
                   :default-model "accounts/fireworks/models/llama-v3p3-70b-instruct"
                   :available-models ["accounts/fireworks/models/llama-v3p3-70b-instruct"]}
   :openai        {:api-url       "https://api.openai.com/v1/chat/completions"
                   :secret-key    :openai-api-key
                   :default-model "gpt-4o-mini"
                   :available-models ["gpt-4o-mini" "gpt-4o"]}
   :ollama-compat {:api-url       "http://localhost:11434/v1/chat/completions"
                   :secret-key    nil
                   :default-model "devstral-small:24b"
                   :available-models ["devstral-small:24b"]}})

(def seed-priority
  "SEED preference order for auto-discovery — not the order that runs.

   `hive-mcp.agent.provider/effective-priority` is what discovery reads: this
   vector minus whatever config removed, plus config-only providers, minus
   dispatch-routed entries. Config `:llm-provider-priority` replaces it outright.

   Every keyword here MUST resolve to a `seed-registry` entry: availability
   reads that entry's :secret-key, and a missing entry reads as nil = no auth
   needed, silently promoting a phantom provider."
  [:openrouter :venice :groq :together :fireworks :openai :ollama-compat])
