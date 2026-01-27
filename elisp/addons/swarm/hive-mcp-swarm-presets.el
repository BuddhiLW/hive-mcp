;;; hive-mcp-swarm-presets.el --- Preset management for swarm -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Preset management for hive-mcp-swarm.
;; Handles loading presets from multiple sources with fallback chain.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles preset loading/building
;; - Open/Closed: New preset sources via extension, not modification
;; - Liskov Substitution: All preset sources return same format
;;
;; Preset sources (priority order):
;; 1. Chroma vector DB (semantic search, requires MCP server)
;; 2. Memory-based (project-scoped conventions tagged "swarm-preset")
;; 3. File-based (.md files from preset directories)

;;; Code:

(require 'cl-lib)

;; Soft dependency on memory system
(declare-function hive-mcp-memory-query "hive-mcp-memory")

;;;; Customization:

(defgroup hive-mcp-swarm-presets nil
  "Preset management for swarm."
  :group 'hive-mcp-swarm
  :prefix "hive-mcp-swarm-presets-")

(defcustom hive-mcp-swarm-presets-dir
  (expand-file-name "presets" (file-name-directory
                               (or load-file-name buffer-file-name
                                   default-directory)))
  "Directory containing built-in preset markdown files."
  :type 'directory
  :group 'hive-mcp-swarm-presets)

(defcustom hive-mcp-swarm-presets-custom-dirs nil
  "List of custom directories to scan for preset .md files.
Directories are scanned recursively for .md files only."
  :type '(repeat directory)
  :group 'hive-mcp-swarm-presets)

(defcustom hive-mcp-swarm-presets-use-chroma t
  "Whether to use Chroma vector DB for preset lookup.
When non-nil, presets are first searched in Chroma, with file/memory fallback.
Requires the MCP server to be running with Chroma configured."
  :type 'boolean
  :group 'hive-mcp-swarm-presets)

(defcustom hive-mcp-swarm-presets-chroma-timeout 5
  "Timeout in seconds for Chroma preset queries."
  :type 'integer
  :group 'hive-mcp-swarm-presets)

(defcustom hive-mcp-swarm-default-presets '("ling" "mcp-first" "hivemind")
  "Default presets applied to all spawned swarm slaves.
These presets are merged with any explicit presets passed to spawn.
Explicit presets take priority (listed first in system prompt).
Set to nil to disable default presets."
  :type '(repeat string)
  :group 'hive-mcp-swarm-presets)

;;;; Internal State:

(defvar hive-mcp-swarm-presets--cache nil
  "Cache of loaded presets (name -> file-path).")

(defconst hive-mcp-swarm-presets--auto-shout-footer
  "

---

## MANDATORY: Task Completion Protocol

**CRITICAL**: When you complete your assigned task, you MUST immediately call:

```
hivemind_shout(event_type: \"completed\", task: \"<your task summary>\", message: \"<brief result summary>\")
```

This is NON-NEGOTIABLE. The coordinator cannot see your work until you shout completion.

Rules:
1. NEVER go idle without shouting completion status
2. If blocked or failed, shout with event_type: \"blocked\" or \"error\"
3. Include actionable summary in the message field
4. Shout progress periodically for long tasks: event_type: \"progress\"

Failure to shout completion wastes coordinator context and blocks pipeline progress.

---

## Dogfooding: Report Tool Friction

When you encounter friction, confusion, or pain points with hive-mcp tools:

1. **Log it immediately** via memory (don't wait for task completion):
```
mcp_memory_add(
  type: \"note\",
  content: \"FRICTION: <tool-name>: <what you tried> -> <what went wrong/was confusing>\",
  tags: [\"ling-feedback\", \"dogfood\", \"pain-point\"],
  duration: \"long\"
)
```

2. **Examples of reportable friction**:
   - Tool returned unexpected format
   - Missing parameter wasn't clear from docs
   - Had to work around a limitation
   - Error message was unhelpful
   - Workflow required too many steps
   - Tool name/purpose was confusing

3. **Format**: Keep it actionable: `FRICTION: <tool>: tried X, expected Y, got Z`

This feedback improves the swarm tools for all agents."
  "Footer injected into all presets to ensure lings auto-shout on completion.")

;;;; File-Based Presets:

(defun hive-mcp-swarm-presets--scan-dir (dir)
  "Recursively scan DIR for .md files, return alist of (name . path)."
  (when (and dir (file-directory-p dir))
    (let ((files (directory-files-recursively dir "\\.md$" nil)))
      (mapcar (lambda (f)
                (cons (file-name-sans-extension (file-name-nondirectory f)) f))
              files))))

(defun hive-mcp-swarm-presets--load-all ()
  "Load all presets from built-in and custom directories.
Returns hash-table of name -> file-path."
  (let ((presets (make-hash-table :test 'equal)))
    ;; Built-in presets
    (dolist (entry (hive-mcp-swarm-presets--scan-dir hive-mcp-swarm-presets-dir))
      (puthash (car entry) (cdr entry) presets))
    ;; Custom presets (can override built-in)
    (dolist (dir hive-mcp-swarm-presets-custom-dirs)
      (dolist (entry (hive-mcp-swarm-presets--scan-dir dir))
        (puthash (car entry) (cdr entry) presets)))
    presets))

(defun hive-mcp-swarm-presets-reload ()
  "Reload all presets from disk."
  (interactive)
  (setq hive-mcp-swarm-presets--cache (hive-mcp-swarm-presets--load-all))
  (message "Loaded %d presets" (hash-table-count hive-mcp-swarm-presets--cache)))

(defun hive-mcp-swarm-presets--ensure-loaded ()
  "Ensure presets are loaded."
  (unless hive-mcp-swarm-presets--cache
    (hive-mcp-swarm-presets-reload)))

(defun hive-mcp-swarm-presets--get-file-content (name)
  "Get content of file-based preset NAME."
  (hive-mcp-swarm-presets--ensure-loaded)
  (when-let* ((path (gethash name hive-mcp-swarm-presets--cache)))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

;;;; Chroma-Based Presets:

(declare-function hive-mcp-api-call-tool "hive-mcp-api")

(defvar hive-mcp-swarm-presets--chroma-available nil
  "Cache for Chroma availability check. nil = not checked, t/`error' = result.")

(defun hive-mcp-swarm-presets--chroma-available-p ()
  "Check if Chroma preset lookup is available.
Caches result to avoid repeated MCP calls."
  (when hive-mcp-swarm-presets-use-chroma
    (cond
     ((eq hive-mcp-swarm-presets--chroma-available t) t)
     ((eq hive-mcp-swarm-presets--chroma-available 'error) nil)
     (t
      ;; Check availability via MCP tool
      (condition-case nil
          (when (fboundp 'hive-mcp-api-call-tool)
            (let ((result (hive-mcp-api-call-tool "preset_status" nil)))
              (when (and result (not (plist-get result :error)))
                (setq hive-mcp-swarm-presets--chroma-available
                      (if (plist-get result :chroma-configured?) t 'error))
                (eq hive-mcp-swarm-presets--chroma-available t))))
        (error
         (setq hive-mcp-swarm-presets--chroma-available 'error)
         nil))))))

(defun hive-mcp-swarm-presets--get-chroma-content (name)
  "Get preset NAME from Chroma vector DB via MCP.
Returns content string or nil if not found/unavailable."
  (when (hive-mcp-swarm-presets--chroma-available-p)
    (condition-case err
        (when (fboundp 'hive-mcp-api-call-tool)
          (let ((result (hive-mcp-api-call-tool
                         "preset_get"
                         `(:name ,name))))
            (when (and result (not (plist-get result :error)))
              (let ((preset (plist-get result :preset)))
                (when preset
                  (plist-get preset :content))))))
      (error
       (message "hive-mcp-swarm-presets: Chroma lookup failed: %s" err)
       nil))))

(defun hive-mcp-swarm-presets--list-chroma ()
  "List preset names from Chroma vector DB."
  (when (hive-mcp-swarm-presets--chroma-available-p)
    (condition-case nil
        (when (fboundp 'hive-mcp-api-call-tool)
          (let ((result (hive-mcp-api-call-tool "preset_list" nil)))
            (when (and result (not (plist-get result :error)))
              (mapcar (lambda (p) (plist-get p :name))
                      (plist-get result :presets)))))
      (error nil))))

(defun hive-mcp-swarm-presets-search (query &optional limit)
  "Search presets using semantic similarity via Chroma.
QUERY is a natural language description of desired preset.
LIMIT is max results (default 5).
Returns list of matching preset names or nil if Chroma unavailable."
  (when (hive-mcp-swarm-presets--chroma-available-p)
    (condition-case nil
        (when (fboundp 'hive-mcp-api-call-tool)
          (let ((result (hive-mcp-api-call-tool
                         "preset_search"
                         `(:query ,query :limit ,(or limit 5)))))
            (when (and result (not (plist-get result :error)))
              (mapcar (lambda (r) (plist-get r :name))
                      (plist-get result :results)))))
      (error nil))))

(defun hive-mcp-swarm-presets-reset-chroma-cache ()
  "Reset the Chroma availability cache.
Call this after Chroma configuration changes."
  (interactive)
  (setq hive-mcp-swarm-presets--chroma-available nil)
  (message "Chroma availability cache reset"))

;;;; Memory-Based Presets:

(defun hive-mcp-swarm-presets--list-memory ()
  "List preset names from memory system (conventions tagged swarm-preset)."
  (when (fboundp 'hive-mcp-memory-query)
    (let ((entries (hive-mcp-memory-query 'convention
                                          '("swarm-preset")
                                          nil 100 nil nil)))
      (cl-remove-duplicates
       (cl-remove-if-not
        #'identity
        (mapcar (lambda (e)
                  (cl-find-if (lambda (tag)
                                (and (not (string-prefix-p "scope:" tag))
                                     (not (string= tag "swarm-preset"))))
                              (plist-get e :tags)))
                entries))
       :test #'string=))))

(defun hive-mcp-swarm-presets--get-memory-content (name)
  "Get preset NAME from memory system (conventions tagged swarm-preset).
Memory-based presets allow project-scoped and semantically searchable presets."
  (when (fboundp 'hive-mcp-memory-query)
    (let ((entries (hive-mcp-memory-query 'convention
                                          (list "swarm-preset" name)
                                          nil 1 nil nil)))
      (when entries
        (plist-get (car entries) :content)))))

;;;; Public API:

(defun hive-mcp-swarm-presets-merge-defaults (explicit-presets)
  "Merge EXPLICIT-PRESETS with default presets.
Returns combined list with explicit presets first, then defaults.
Duplicates are removed, preserving first occurrence (explicit wins)."
  (let ((defaults (or hive-mcp-swarm-default-presets '())))
    (cl-remove-duplicates
     (append explicit-presets defaults)
     :test #'string=
     :from-end t)))

(defun hive-mcp-swarm-presets-list ()
  "List all available presets (chroma + file-based + memory-based)."
  (interactive)
  (hive-mcp-swarm-presets--ensure-loaded)
  (let* ((chroma-presets (hive-mcp-swarm-presets--list-chroma))
         (file-presets (hash-table-keys hive-mcp-swarm-presets--cache))
         (memory-presets (hive-mcp-swarm-presets--list-memory))
         (all-names (cl-remove-duplicates
                     (append chroma-presets file-presets memory-presets)
                     :test #'string=)))
    (if (called-interactively-p 'any)
        (message "Available presets: %s (chroma: %d, file: %d, memory: %d)"
                 (string-join (sort all-names #'string<) ", ")
                 (length chroma-presets)
                 (length file-presets)
                 (length memory-presets))
      all-names)))

(defun hive-mcp-swarm-presets-get (name)
  "Get content of preset NAME.
Priority: chroma -> memory-based (project-scoped) -> file-based (.md fallback).
This allows Chroma-indexed presets with semantic search, project-specific
overrides via memory, and file-based fallback."
  (or (hive-mcp-swarm-presets--get-chroma-content name)
      (hive-mcp-swarm-presets--get-memory-content name)
      (hive-mcp-swarm-presets--get-file-content name)))

(defun hive-mcp-swarm-presets-build-system-prompt (presets &optional injected-context)
  "Build combined system prompt from list of PRESETS.
Returns concatenated content with separator and auto-shout footer,
or nil if no presets found.  The footer ensures lings automatically
call hivemind_shout when they complete tasks.

When INJECTED-CONTEXT is non-nil, prepend it as a
\"## Project Context (Auto-Injected)\" section before the preset content.
This implements Architecture > LLM behavior: context injected at spawn
rather than relying on lings to /catchup themselves."
  (let ((contents '()))
    (dolist (preset presets)
      (when-let* ((content (hive-mcp-swarm-presets-get preset)))
        (push content contents)))
    (when (or contents injected-context)
      (let ((preset-body (when contents
                           (mapconcat #'identity (nreverse contents) "\n\n---\n\n"))))
        (concat
         ;; Injected context comes first (axioms, conventions, decisions)
         (when injected-context
           (concat injected-context "\n\n---\n\n"))
         (or preset-body "")
         hive-mcp-swarm-presets--auto-shout-footer)))))

(defun hive-mcp-swarm-presets-add-custom-dir (dir)
  "Add DIR to custom preset directories and reload."
  (interactive "DPresets directory: ")
  (add-to-list 'hive-mcp-swarm-presets-custom-dirs dir)
  (hive-mcp-swarm-presets-reload))

;;;; Role Mapping:

(defcustom hive-mcp-swarm-presets-role-mapping
  '(("tester" . ("tester" "tdd"))
    ("reviewer" . ("reviewer" "solid" "clarity"))
    ("documenter" . ("documenter"))
    ("refactorer" . ("refactorer" "solid" "clarity"))
    ("researcher" . ("researcher"))
    ("fixer" . ("fixer" "tdd"))
    ("clarity-dev" . ("clarity" "solid" "ddd" "tdd"))
    ("coordinator" . ("task-coordinator"))
    ("ling" . ("ling" "minimal"))
    ("worker" . ("ling")))
  "Mapping of role names to preset lists.
Each entry is (ROLE . PRESETS-LIST)."
  :type '(alist :key-type string :value-type (repeat string))
  :group 'hive-mcp-swarm-presets)

(defcustom hive-mcp-swarm-tier-mapping
  '(("coordinator" . (:backend claude-code-ide))
    ("reviewer" . (:backend claude-code-ide))
    ("architect" . (:backend claude-code-ide))
    ("implementer" . (:backend ollama :model "devstral-small-2"))
    ("tester" . (:backend ollama :model "devstral-small-2"))
    ("fixer" . (:backend ollama :model "deepseek-r1"))
    ("documenter" . (:backend ollama :model "devstral-small-2"))
    ("refactorer" . (:backend ollama :model "devstral-small-2"))
    ("worker" . (:backend ollama :model "devstral-small-2"))
    ("ling" . (:backend ollama :model "devstral-small-2"))
    ("researcher" . (:backend claude-code-ide)))
  "Two-tier mapping of roles to backend and model.

This enables cost-effective swarm orchestration:
- Claude (premium): coordination, review, architecture decisions
- Ollama (free): implementation, testing, documentation

Backend options:
- `claude-code-ide': Claude Code via terminal (premium, high quality)
- `ollama': Local Ollama via hive-mcp-ellama (free, good enough)

Models for ollama backend:
- devstral-small-2: Mistral's coding model (15GB, good quality)
- deepseek-r1: Reasoning tasks with chain-of-thought (5.2GB)"
  :type '(alist :key-type string
                :value-type (plist :key-type symbol :value-type sexp))
  :group 'hive-mcp-swarm-presets)

(defun hive-mcp-swarm-presets-role-to-tier (role)
  "Get tier configuration for ROLE.
Returns plist with :backend and optionally :model, or nil for default."
  (cdr (assoc role hive-mcp-swarm-tier-mapping)))

(defun hive-mcp-swarm-presets-role-to-presets (role)
  "Convert ROLE to list of preset names."
  (or (cdr (assoc role hive-mcp-swarm-presets-role-mapping))
      (list role)))

;;;; Lifecycle:

(defun hive-mcp-swarm-presets-init ()
  "Initialize presets module."
  (hive-mcp-swarm-presets-reload))

(defun hive-mcp-swarm-presets-shutdown ()
  "Shutdown presets module."
  (setq hive-mcp-swarm-presets--cache nil)
  (setq hive-mcp-swarm-presets--chroma-available nil))

(provide 'hive-mcp-swarm-presets)
;;; hive-mcp-swarm-presets.el ends here
