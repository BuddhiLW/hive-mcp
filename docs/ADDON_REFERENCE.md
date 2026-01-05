# Addon Reference

Complete documentation for all emacs-mcp addons.

## chroma

**Purpose**: Semantic memory search via Chroma vector database and Ollama embeddings.

**Features**:
- Docker-compose management for Chroma container
- Automatic Ollama embedding provider configuration
- Semantic similarity search across project memory
- Fallback to local text search when Chroma unavailable
- Health monitoring and status display

**Config**:
- `emacs-mcp-chroma-host` - Chroma server host (default: "localhost")
- `emacs-mcp-chroma-port` - Chroma server port (default: 8000)
- `emacs-mcp-chroma-auto-start` - Auto-start container on init (default: t)
- `emacs-mcp-chroma-embedding-provider` - 'ollama, 'mock, or 'none
- `emacs-mcp-chroma-ollama-model` - Model for embeddings (default: "nomic-embed-text")

**Usage**:
```elisp
(require 'emacs-mcp-chroma)
(emacs-mcp-chroma-mode 1)

;; Semantic search
(emacs-mcp-chroma-search "authentication patterns")

;; Check status
M-x emacs-mcp-chroma-status

;; Transient menu
M-x emacs-mcp-chroma-transient
```

---

## cider

**Purpose**: CIDER integration for Clojure development with async nREPL and multi-session support.

**Features**:
- Async nREPL server startup (non-blocking)
- Auto-connect when nREPL becomes available
- Named sessions for parallel agent work
- Silent and explicit evaluation modes
- Memory integration for REPL history

**Config**:
- `emacs-mcp-cider-auto-start-nrepl` - Auto-start nREPL on mode enable
- `emacs-mcp-cider-nrepl-timeout` - Connection timeout in seconds
- `emacs-mcp-cider-default-deps-aliases` - Default aliases for deps.edn

**Usage**:
```elisp
(require 'emacs-mcp-cider)

;; Spawn isolated session for parallel work
(emacs-mcp-cider-spawn-session "agent-1")

;; Evaluate in specific session
(emacs-mcp-cider-eval-in-session "agent-1" "(+ 1 2)")

;; List all sessions
(emacs-mcp-cider-list-sessions)
```

---

## magit

**Purpose**: Git operations via Magit with shell command fallback.

**Features**:
- Comprehensive repo status (staged, unstaged, untracked, stashes)
- Branch management (list, create, checkout)
- Non-interactive staging and commits for MCP use
- Diff viewing (staged, unstaged, all)
- Remote operations (fetch, pull, push)
- Automatic Magit refresh after operations

**Config**:
- `emacs-mcp-magit-log-count` - Default commits in log (default: 10)
- `emacs-mcp-magit-diff-context-lines` - Context lines in diffs (default: 3)
- `emacs-mcp-magit-prefer-magit` - Use Magit functions when available (default: t)

**Usage**:
```elisp
(require 'emacs-mcp-magit)

;; Get full status
(emacs-mcp-magit-api-status)
;; => (:branch "main" :staged ("file.el") :unstaged () ...)

;; Stage and commit
(emacs-mcp-magit-api-stage 'all)
(emacs-mcp-magit-api-commit "feat: add feature")

;; Transient menu
M-x emacs-mcp-magit-transient
```

---

## swarm

**Purpose**: Multi-agent orchestration for parallel Claude Code instances.

**Features**:
- Spawn slave Claude instances in vterm/eat buffers
- Preset system (markdown system prompts)
- Task dispatch and collection
- Broadcast to all slaves
- Recursion depth limits (prevent runaway spawning)
- Rate limiting for spawn protection
- Custom preset directories

**Config**:
- `emacs-mcp-swarm-terminal` - 'vterm (recommended) or 'eat
- `emacs-mcp-swarm-max-slaves` - Maximum concurrent slaves (default: 5)
- `emacs-mcp-swarm-max-depth` - Recursion depth limit (default: 3)
- `emacs-mcp-swarm-presets-dir` - Built-in presets directory
- `emacs-mcp-swarm-custom-presets-dirs` - Additional preset directories

**Usage**:
```elisp
(require 'emacs-mcp-swarm)
(emacs-mcp-swarm-mode 1)

;; Spawn with presets
(emacs-mcp-swarm-spawn "tester" :presets '("tdd" "clarity"))

;; Dispatch task
(emacs-mcp-swarm-dispatch "swarm-tester-xxx" "Run all tests")

;; Collect result
(emacs-mcp-swarm-collect "task-tester-xxx-001" 30000)

;; Broadcast to all
(emacs-mcp-swarm-broadcast "Status report")
```

---

## projectile

**Purpose**: Project management integration via Projectile.

**Features**:
- Project info (name, root, type, file count)
- Extended type detection (npm, cargo, go-mod, deps.edn, etc.)
- File listing with glob pattern filtering
- File search by name
- Project-wide grep (ripgrep preferred)
- Recent files in project

**Config**:
- `emacs-mcp-projectile-max-files` - Max files in listings (default: 1000)
- `emacs-mcp-projectile-max-search-results` - Max search results (default: 100)
- `emacs-mcp-projectile-use-ripgrep` - Prefer rg over grep (default: t)

**Usage**:
```elisp
(require 'emacs-mcp-projectile)

;; Get project info
(emacs-mcp-projectile-api-project-info)
;; => (:name "emacs-mcp" :root "/path/to" :type "clojure-deps" ...)

;; List files with pattern
(emacs-mcp-projectile-api-project-files "*.el")

;; Search project
(emacs-mcp-projectile-api-search "defun.*api")
```

---

## docs

**Purpose**: Expose Emacs's built-in documentation to MCP.

**Features**:
- Describe functions (signature, docstring, type, source)
- Describe variables (value, docstring, custom status)
- Apropos search with type filtering
- List package functions by prefix
- Find keybindings for commands
- Extract package commentary sections

**Config**:
- `emacs-mcp-docs-max-results` - Max apropos results (default: 50)
- `emacs-mcp-docs-include-source-location` - Include file paths (default: t)

**Usage**:
```elisp
(emacs-mcp-docs-describe-function "mapcar")
;; => (:name "mapcar" :type "built-in function" :signature "(FN SEQUENCE)" ...)

(emacs-mcp-docs-apropos "json" "function")
;; => (:pattern "json" :total-matches 42 :symbols (...))
```

---

## presentation

**Purpose**: Streamline org-mode presentation creation for Beamer and Reveal.js.

**Features**:
- Pre-configured templates for Beamer (PDF) and Reveal.js (HTML)
- Slide scaffolding: code, image, quote, two-column, iframe
- Export and preview commands
- MCP memory integration for slide snippets
- Transient UI menu

**Config**:
- `emacs-mcp-presentation-default-format` - 'beamer or 'revealjs
- `emacs-mcp-presentation-beamer-theme` - Beamer color theme
- `emacs-mcp-presentation-reveal-theme` - Reveal.js theme
- `emacs-mcp-presentation-author` / `emacs-mcp-presentation-email`

**Usage**:
```elisp
;; Create new presentation
M-x emacs-mcp-presentation-create

;; Insert slides
M-x emacs-mcp-presentation-insert-code-slide
M-x emacs-mcp-presentation-insert-quote-slide

;; Export and preview
M-x emacs-mcp-presentation-refresh

;; Transient menu
M-x emacs-mcp-presentation-transient
```

---

## melpazoid

**Purpose**: MELPA submission testing via melpazoid Docker integration.

**Features**:
- Full melpazoid test suite (package-lint, byte-compile, checkdoc)
- Multiple speed modes: full Docker, cached Docker, local Python
- Auto-detect MELPA recipes from recipes/ directory
- Parse and display structured results
- Save results to MCP memory

**Config**:
- `emacs-mcp-melpazoid-path` - Path to melpazoid repo (auto-detected)
- `emacs-mcp-melpazoid-fast-mode` - nil (full), 'cached, or 'local
- `emacs-mcp-melpazoid-timeout` - Timeout in seconds (default: 300)
- `emacs-mcp-melpazoid-save-results` - Auto-save to MCP memory (default: t)

**Usage**:
```elisp
;; Run on current project
M-x emacs-mcp-melpazoid-run-current-project

;; Fast mode (no Docker)
M-x emacs-mcp-melpazoid-run-fast

;; View results
M-x emacs-mcp-melpazoid-show-results

;; Transient menu
M-x emacs-mcp-melpazoid-transient
```

---

## org-kanban

**Purpose**: Native Clojure org-mode parser for kanban boards.

**Features**:
- Parse org files to JSON structure without elisp
- Query headlines by ID, status, or type
- Move tasks between status columns
- Render visual kanban boards (terminal ASCII or org-mode)
- Statistics and progress tracking

**Usage**:
```elisp
;; Parse org file
(org_clj_parse "/path/to/kanban.org")

;; Get kanban status
(org_kanban_native_status "/path/to/kanban.org")

;; Move task
(org_kanban_native_move "/path/to/kanban.org" "task-id" "DONE")

;; Render board
(org_kanban_render "/path/to/kanban.org" :format "terminal")
```

---

## vibe-kanban

**Purpose**: Cloud task management via vibe-kanban server.

**Features**:
- NPX subprocess management for vibe-kanban server
- Task CRUD operations
- Status column management
- Sync with org-mode kanban
- Roadmap and milestone views

**Usage**:
```elisp
;; Get kanban status
(mcp_kanban_status)

;; Create task
(mcp_kanban_create_task :title "New feature" :description "Details...")

;; Move task
(mcp_kanban_move_task :task-id "xxx" :new-status "done")

;; Sync backends
(mcp_kanban_sync)
```

---

## package-lint

**Purpose**: MELPA compliance checking via package-lint.

**Features**:
- Run package-lint on current buffer
- Check all project elisp files
- Format results for MCP consumption
- Integration with memory for tracking issues

---

## claude-code

**Purpose**: Integration with claude-code.el for Claude CLI.

**Features**:
- Context injection for Claude conversations
- Memory sharing between MCP and claude-code.el

---

## claude-code-ide

**Purpose**: Swarm orchestration via claude-code-ide.el with hivemind completion tracking.

**Features**:
- Structured session management (no terminal scraping)
- Reliable prompt dispatch via claude-code-ide API
- Task completion tracking via hivemind coordinator
- Auto-sync from hivemind for completion status
- Integration with emacs-mcp-swarm presets

**Benefits over vterm-based swarm**:
- No terminal timing issues or output parsing
- Structured JSON-RPC communication
- Lings report completion via hivemind_shout (not terminal markers)
- Reliable multi-agent coordination

**Config**:
- `emacs-mcp-cci-default-timeout` - Task timeout in ms (default: 300000)
- `emacs-mcp-cci-max-lings` - Maximum concurrent lings (default: 10)
- `emacs-mcp-cci-hivemind-poll-interval` - Hivemind sync interval (default: 5s)
- `emacs-mcp-cci-auto-sync` - Auto-sync from hivemind (default: t)

**Usage**:
```elisp
(require 'emacs-mcp-claude-code-ide)
(emacs-mcp-claude-code-ide-mode 1)

;; Spawn a ling
(emacs-mcp-cci-spawn "worker" :presets '("hivemind" "tdd"))
;; => "ling-worker-123456"

;; Dispatch task (completion via hivemind)
(emacs-mcp-cci-dispatch "ling-worker-123456" "Run all tests")
;; => "task-worker-123456-001"

;; Check status
(emacs-mcp-cci-status)
;; => (:backend "claude-code-ide" :completion-mechanism "hivemind" ...)

;; Manual sync from hivemind
(emacs-mcp-cci-sync-from-hivemind)
```

See [SWARM_BACKENDS.md](SWARM_BACKENDS.md) for detailed comparison of swarm backends.

---

## org-ai

**Purpose**: Integration with org-ai for AI conversations in org-mode.

**Features**:
- Context sharing with org-ai conversations
- Memory integration for conversation history

---

## addon-template

**Purpose**: Template for creating new addons.

**Features**:
- Complete addon skeleton with lifecycle hooks
- Example customization group
- Minor mode template
- Registration boilerplate

**Usage**:
```bash
cp elisp/addons/emacs-mcp-addon-template.el elisp/addons/emacs-mcp-my-addon.el
# Edit and customize
```
