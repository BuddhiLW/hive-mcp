(ns hive-mcp.agent.provider
  "Provider PIPELINE stratum: collect, then promote, and nothing else.

   This is the single seam every caller asks \"which providers exist, which can
   I call, and which one answers this request\". It performs no I/O of its own:
   it calls collectors (`hive-mcp.agent.provider.collect`) and hands what they
   return to pure decisions (`hive-mcp.agent.provider.policy`).

   Adding, removing or reordering a provider is therefore a CONFIG edit:
     hive config set llm-providers.acme.api-url https://acme.test/v1/chat/completions
     hive config set llm-providers.venice false        ;; take one out
     hive config set llm-provider-priority [:acme :ollama-compat]

   The boundary that speaks HTTP lives in `hive-mcp.agent.openrouter`, which
   consumes this namespace and re-exports its surface for back-compat."
  (:require [hive-mcp.agent.provider.collect :as collect]
            [hive-mcp.agent.provider.model :as model]
            [hive-mcp.agent.provider.policy :as policy]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn effective-registry
  "The seed registry overlaid with the host config's :llm-providers.
   Config extends it, overrides entry fields, and removes an entry whose value
   is `false` or `nil`."
  []
  (policy/overlay model/seed-registry (collect/config-providers)))

(defn effective-priority
  "The order auto-discovery actually walks: see `policy/discovery-order`."
  []
  (policy/discovery-order (effective-registry)
                          model/seed-priority
                          (collect/config-priority)))

(defn available-providers
  "Providers that can be called right now: in the effective order, with either
   no secret required or a configured one."
  []
  (let [registry (effective-registry)
        order    (effective-priority)]
    (policy/available registry order
                      (collect/present-secret-keys
                       (policy/secret-keys-of registry order)))))

(defn best-available-provider
  "The highest-priority callable provider, or nil."
  []
  (first (available-providers)))

(defn validate-provider
  "nil when the provider exists in the effective registry, else an error map."
  [provider]
  (policy/validate-provider (effective-registry) provider))

(defn validate-model
  "nil when the model is allowed for the provider, else an error map."
  [provider model]
  (policy/validate-model (effective-registry) provider model))

(defn resolve-provider-model
  "Resolve {:provider :model} for an agent spawn or wave.

   Explicit :provider, then a '<provider>:<model>' prefix, then Claude
   model-name auto-detection, then this agent-type's config default, then the
   best available provider. The fallback is only collected when the earlier
   steps left the provider open, so a routed spawn costs no secret lookups.

   Throws on an unknown provider; an unknown model only warns, since a provider
   that declares no :available-models accepts anything."
  [{:keys [provider model agent-type]}]
  (let [registry (effective-registry)
        request  {:provider      provider
                  :model         model
                  :type-defaults (collect/agent-type-defaults agent-type)}
        resolved (let [r (policy/resolve-routing registry request)]
                   (if (:provider r)
                     r
                     (policy/resolve-routing
                      registry
                      (assoc request :fallback-provider (best-available-provider)))))
        {:keys [provider model]} resolved]
    (when-let [err (policy/validate-provider registry provider)]
      (throw (ex-info (str "Unknown provider: " (some-> provider name)) err)))
    (when-let [err (policy/validate-model registry provider model)]
      (log/warn "Model not in available-models list" err))
    resolved))

(defn provider-diagnostic
  "What auto-discovery checked, for a \"no provider configured\" error."
  []
  (let [registry (effective-registry)]
    (mapv (fn [p] {:provider p :secret-key (:secret-key (get registry p))})
          (effective-priority))))
