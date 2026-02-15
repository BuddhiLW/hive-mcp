(ns hive-mcp.taxonomy
  "Register hive-mcp domain error categories with hive-dsl taxonomy.
   Called once at startup from server/init."
  (:require [hive-dsl.result.taxonomy :as tax]))

(defn register!
  "Register all hive-mcp domain-specific error categories."
  []
  (tax/register!
   #{;; SDK / MCP protocol
     :sdk/invalid-request :sdk/missing-param :sdk/tool-not-found
     :sdk/handler-error :sdk/protocol-error

     ;; Knowledge graph — core
     :kg/node-not-found :kg/edge-invalid :kg/traversal-error
     :kg/backend-error :kg/scope-mismatch
     ;; Knowledge graph — validation
     :kg/validation-failed :kg/versioning-unavailable
     ;; Knowledge graph — versioning (Yggdrasil)
     :kg/branch-failed :kg/checkout-failed :kg/branches-failed
     :kg/snapshot-failed :kg/history-failed :kg/merge-failed
     :kg/status-failed
     ;; Knowledge graph — migration
     :kg/migrate-failed :kg/export-failed :kg/import-failed
     :kg/validate-migration-failed

     ;; Chroma vector DB
     :chroma/connection-failed :chroma/collection-not-found
     :chroma/embedding-error :chroma/query-error
     :chroma/migrate-failed :chroma/status-failed

     ;; Emacs integration
     :emacs/not-connected :emacs/eval-error :emacs/timeout
     :emacs/buffer-not-found
     ;; Emacs consolidated tool handlers
     :emacs/eval-failed :emacs/buffers-failed :emacs/notify-failed
     :emacs/status-failed :emacs/switch-failed :emacs/find-file-failed
     :emacs/save-failed :emacs/current-buffer-failed

     ;; Drone execution
     :drone/spawn-failed :drone/timeout :drone/budget-exceeded
     :drone/model-error :drone/diff-invalid
     ;; Drone health monitoring
     :drone/kill-failed :drone/claim-release-failed
     :drone/health-status-failed :drone/recover-failed
     :drone/health-control-failed :drone/retry-queue-failed
     :drone/pop-retry-failed

     ;; EDN plan parser
     :edn/parse-failed :edn/validation-failed}))
