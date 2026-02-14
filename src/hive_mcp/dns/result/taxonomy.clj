(ns hive-mcp.dns.result.taxonomy
  "Domain error keywords by subsystem.
   Each category is a namespace-qualified keyword matching :subsystem/cause.")

(def ^:private error-categories
  "Set of all known error category keywords."
  #{;; I/O errors
    :io/read-failure :io/write-failure :io/timeout :io/not-found

    ;; SDK / MCP protocol
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

    ;; Transport / networking
    :transport/connection-refused :transport/timeout
    :transport/protocol-error :transport/serialization-error

    ;; Drone execution
    :drone/spawn-failed :drone/timeout :drone/budget-exceeded
    :drone/model-error :drone/diff-invalid
    ;; Drone health monitoring
    :drone/kill-failed :drone/claim-release-failed
    :drone/health-status-failed :drone/recover-failed
    :drone/health-control-failed :drone/retry-queue-failed
    :drone/pop-retry-failed

    ;; Parsing
    :parse/invalid-json :parse/invalid-edn :parse/malformed-input
    :parse/schema-violation
    ;; EDN plan parser
    :edn/parse-failed :edn/validation-failed

    ;; Effect boundary
    :effect/exception})

(defn known-error?
  "True if category is a recognized domain error keyword."
  [category]
  (contains? error-categories category))
