(ns hive-mcp.config.merge
  "Pure config transformations — no IO, no atoms, no logging.
   Collect/Promote layer: defaults, deep-merge, key-path parsing."
  (:require [clojure.string :as str]
            [hive-mcp.agent.provider.model :as model]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Defaults
;; =============================================================================

(def default-kg-backend
  "Canonical default KG backend. Referenced by config defaults and connection fallback."
  :datahike)

(def default-secret-slots
  "One nil-valued slot per secret key the seeded providers name.
   Derived, so a provider added to the seed cannot arrive without its slot."
  (into {} (comp (keep :secret-key) (map (fn [k] [k nil])))
        (vals model/seed-registry)))

(def default-config
  "Default configuration. Used as base — user config.edn is deep-merged on top."
  {:project-roots []
   ;; MCP tool-surface shaping. Two complementary knobs:
   ;;   :visible  — allowlist of tool-root names that stay in tools/list.
   ;;               Tools NOT listed here are marked :deprecated (hidden from
   ;;               discovery, still callable via tools/call → back-compat).
   ;;               nil/absent ⇒ no gating (legacy behavior).
   ;;   :absorbed — denylist of addon tool names dropped from standalone
   ;;               roots in get-base-tools (folded as subcommands instead).
   ;; The "neat 9": the 8 substrate generators (the classifier-kernel partition
   ;; f: tool → primary substrate) + `multi`, the universal batch/DSL spine.
   ;;   substrates: fs code memory project swarm git emacs web
   ;; auth + events are cross-cutting, NOT substrate generators — folded out of
   ;; the visible surface (gate-hidden, still callable by name). Likewise the
   ;; folded roots preset/migrate_kanban/transcript are re-exposed as subdomains
   ;; of swarm/project and stay gated-hidden here for back-compat.
   :tool-roots {:visible #{"fs" "code" "memory" "project" "swarm"
                           "git" "emacs" "web" "multi"}
                :absorbed []}
   :defaults {:kg-backend default-kg-backend
              :hot-reload false
              :presets-path nil}
   :project-overrides {}
   :parent-rules []
   :memory {:default-store :chroma
            ;; Kanban-store routing toggle. Decouples kanban from the
            ;; :default slot so it can be served by a dedicated qdrant
            ;; collection (sub-10ms tag-filtered queries). Values:
            ;;   :default    — kanban reads/writes go through :default
            ;;                 (current milvus path; backward compat).
            ;;   :dual-read  — read :kanban first, fall back to :default
            ;;                 on miss. Writes fan out to both for soak.
            ;;   :kanban     — read+write only the :kanban slot.
            ;; Cutover sequence: :default → migrate → :dual-read →
            ;; :kanban. Each flip is config-only; no code change.
            :kanban-store  :default
            :routes {:decision    :chroma
                     :snippet     :chroma
                     :preference  :chroma
                     :pattern     :chroma
                     :system      :chroma
                     :context     :chroma
                     :reflection  :chroma
                     :note        :chroma}
            ;; Routes can also be maps for dual-write scenarios:
            ;; :snippet {:primary :milvus :projection :chroma}
            :stores {:chroma {:addon :hive-chroma
                              :host "localhost"
                              :port 8000}
                     ;; Example: Milvus store (uncomment to enable)
                     ;; :milvus {:addon :hive-milvus
                     ;;          :host "localhost"
                     ;;          :port 19530
                     ;;          :collection "hive_memory"}
                     ;; Example: Proximum store (uncomment to enable)
                     ;; :proximum {:addon :hive-proximum
                     ;;            :host "localhost"
                     ;;            :port 50051}
                     }}
   :embeddings {:ollama {:host "http://localhost:11434"
                         :model "qwen3-embedding:4b"}
                :openrouter {:model "qwen/qwen3-embedding-8b"}}
   :embedder {:default :ollama-qwen3-4b
              ;; Memory types that are structurally addressed (fetched by
              ;; tag/id/project-id, never semantic search) — the write path
              ;; skips embedding them. hive-di-configurable per profile; addons
              ;; may also self-register via embeddings.service/register-no-embed-type!
              :no-embed-types #{}
              ;; Every type embeds into one space. A configured provider is also
              ;; a searched collection, so adding one here fans reads out over it.
              :routes {}
              :providers {:ollama-qwen3-4b {:impl :ollama
                                            :model "qwen3-embedding:4b"
                                            :max-tokens 8192
                                            :dimension 2560
                                            :host "http://localhost:11434"}}}
   :services {:chroma {:mode :local :host "localhost" :port 8000}
              :ollama {:mode :local :host "http://localhost:11434" :model "qwen3-embedding:4b"}
              :datahike {:mode :local :path "data/kg"}
              :nrepl {:mode :local :port 7910}
              :prometheus {:mode :local :url "http://localhost:9090"}
              :loki {:mode :local :url "http://localhost:3100"}
              :websocket {:mode :local :enabled false :port nil :project-dir nil}
              :ws-channel {:mode :local :port 9999}
              :channel {:mode :local :port 9998}
              :olympus {:mode :local :ws-port 7911}
              :overarch {:mode :local :jar nil}
              :presets {:mode :local :dir nil}
              :kg {:mode :local :backend default-kg-backend
                   :writer {:backend :self}}
              :project {:mode :local :id nil :dir nil :src-dirs ["src"]}
              :forge {:mode :local :legacy false :budget-routing false
                      ;; Max ms to wait for a ling to register in DataScript + pass CLI
                      ;; check before dispatch is attempted. Claude CLI can take 10-30s
                      ;; to start, so 60s is the safe default. Configurable via:
                      ;;   {:services {:forge {:readiness-timeout-ms 90000}}}
                      :readiness-timeout-ms 60000}
              :drone {:mode :local :default-model "devstral-small:24b"}
              :nats {:mode :local
                     :enabled false
                     :url "nats://localhost:4222"
                     :connection-timeout 5000
                     :max-reconnects 5
                     :reconnect-wait 1000}
              :scheduler {:mode :local :enabled true :interval-minutes 60
                          :memory-limit 50 :edge-limit 100 :disc-enabled true}
              :memory-store {:backend :chroma}
              :qdrant-carto {:mode :local
                             :host "localhost"
                             :port 6333
                             :collection "carto-snippets"
                             :embedding {:provider :ollama
                                         :model "nomic-embed-code"}}
              :carto-store {:backend :qdrant-carto}}
   :cartography {:sentinel-path (str (System/getProperty "user.home")
                                     "/.config/hive-mcp/data/carto/preferred-backend.edn")
                 :strict-mode?  true}
   ;; Both keys are PROJECTIONS of the provider seed, never a second copy:
   ;; the slot a provider's key occupies and the entry itself come from
   ;; `hive-mcp.agent.provider.model/seed-registry`. A user config.edn is
   ;; deep-merged over this, and `provider/effective-registry` re-reads it.
   :secrets default-secret-slots
   :llm-providers model/seed-registry
   :hivemind {;; Max chars preserved in a shout :message / :task before truncation.
              ;; One bad shout fans out (per-agent ring × backbone × subscribers),
              ;; so aggressive bound protects every downstream context window.
              :shout-message-cap 2048}
   :headless {;; Default concrete backend keyword for the abstract :headless
              ;; spawn-mode. :auto = registry-driven preference per provider.
              ;; Concrete keys (e.g. :hive-agent) come from addons that register
              ;; via META-INF/hive-addons/*.edn + register-headless!.
              ;; hive-mcp source MUST NOT name concrete backends — keywords here
              ;; are inert operator data.
              :default-backend :auto}
   :agent-defaults {:ling       {:provider :openrouter :model "anthropic/claude-opus-4-7"}
                    :drone      {:provider :openrouter :model "qwen/qwen3.6-plus"}
                    :compressor {:provider :venice     :model "venice-uncensored"}}
   :models {:task-models {:coding     "moonshotai/kimi-k2.5"
                          :coding-alt "qwen/qwen3.6-plus"
                          :testing    "moonshotai/kimi-k2.5"
                          :bugfix     "moonshotai/kimi-k2.5"
                          :general    "moonshotai/kimi-k2.5"
                          :arch       "qwen/qwen3.6-plus"
                          :docs       "z-ai/glm-5.1"}
            :routing {:testing        {:primary "moonshotai/kimi-k2.5"
                                       :secondary "qwen/qwen3.6-plus"}
                      :refactoring    {:primary "moonshotai/kimi-k2.5"
                                       :secondary "qwen/qwen3.6-plus"}
                      :implementation {:primary "moonshotai/kimi-k2.5"
                                       :secondary "qwen/qwen3.6-plus"}
                      :bugfix         {:primary "moonshotai/kimi-k2.5"
                                       :secondary "qwen/qwen3.6-plus"}
                      :documentation  {:primary "z-ai/glm-5.1"
                                       :secondary "qwen/qwen3.6-plus"}
                      :general        {:primary "moonshotai/kimi-k2.5"
                                       :secondary "qwen/qwen3.6-plus"}}
            :default-model "moonshotai/kimi-k2.5"}})

;; =============================================================================
;; Pure Transformations
;; =============================================================================

(defn deep-merge
  "Recursively merge maps. User values take priority at every level.
   Missing keys in user-config are filled from defaults at any depth."
  [defaults user-config]
  (reduce-kv
   (fn [acc k default-val]
     (if (contains? acc k)
       (let [user-val (get acc k)]
         (if (and (map? default-val) (map? user-val))
           (assoc acc k (deep-merge default-val user-val))
           acc)) ; user value wins
       (assoc acc k default-val))) ; fill missing from default
   user-config
   defaults))

(defn parse-key-path
  "Parse a dotted key string into a keyword path vector."
  [key-str]
  (when (and key-str (not (str/blank? key-str)))
    (mapv keyword (str/split key-str #"\."))))
