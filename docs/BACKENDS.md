# LLM Backends Configuration Guide

This guide covers hive-mcp's **Center of Command** architecture for multi-LLM orchestration.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    CENTER OF COMMAND                            │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │               CLAUDE (Coordinator/Hivemind)              │   │
│  │  • Strategic decisions      • Context preservation       │   │
│  │  • Task decomposition       • Quality review             │   │
│  │  • Human interaction        • Final approval             │   │
│  └─────────────────────────────────────────────────────────┘   │
│                              │                                  │
│                    delegates tasks to                           │
│                              ▼                                  │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                 WORKER BACKENDS (Lings)                  │   │
│  │  ┌───────────────┐  ┌───────────────┐  ┌─────────────┐  │   │
│  │  │    Ollama     │  │  OpenRouter   │  │   Future    │  │   │
│  │  │   (Local)     │  │   (Cloud)     │  │  Backends   │  │   │
│  │  │               │  │               │  │             │  │   │
│  │  │ devstral      │  │ devstral-2512 │  │ Anthropic   │  │   │
│  │  │ codellama     │  │ gemma-3       │  │ OpenAI      │  │   │
│  │  │ qwen          │  │ gpt-oss-120b  │  │ etc.        │  │   │
│  │  └───────────────┘  └───────────────┘  └─────────────┘  │   │
│  └─────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

**Design Principles:**
- **Claude as Coordinator**: Handles high-level orchestration, complex reasoning, user interaction
- **Workers for Implementation**: Cheap/free models do coding, testing, documentation tasks
- **Cost Optimization**: Reserve expensive Claude tokens for coordination; use free tiers for bulk work
- **Pluggable Backends**: `LLMBackend` protocol enables adding new providers easily

## Quick Start

### 1. OpenRouter (Recommended for Remote Work)

```bash
# Set API key
export OPENROUTER_API_KEY="sk-or-v1-..."
```

```elisp
;; From Emacs (via MCP)
(mcp-call "agent_delegate"
  '(:backend "openrouter"
    :task "Write a function to parse JSON"
    :task_type "coding"))
```

### 2. Ollama (Local, Privacy-Focused)

```bash
# Install Ollama
curl -fsSL https://ollama.com/install.sh | sh

# Pull a coding model
ollama pull devstral-small:24b
```

```elisp
;; From Emacs (via MCP)
(mcp-call "agent_delegate"
  '(:backend "ollama"
    :model "devstral-small:24b"
    :task "Refactor this function for clarity"))
```

## Backend Configuration

### Ollama Backend

**Installation:**
```bash
# Linux
curl -fsSL https://ollama.com/install.sh | sh

# macOS
brew install ollama

# Start server
ollama serve
```

**Recommended Models:**
| Model | Size | Best For |
|-------|------|----------|
| `devstral-small:24b` | 24B | General coding (default) |
| `codellama:13b` | 13B | Fast completions |
| `qwen2.5-coder:14b` | 14B | Code generation |
| `deepseek-coder:6.7b` | 6.7B | Lightweight tasks |

**Configuration:**
```clojure
;; In REPL
(require '[hive-mcp.agent :as agent])

;; Create backend with custom host/model
(def my-ollama (agent/ollama-backend
  {:host "http://localhost:11434"
   :model "qwen2.5-coder:14b"}))
```

**MCP Tool Usage:**
```json
{
  "tool": "agent_delegate",
  "arguments": {
    "backend": "ollama",
    "model": "devstral-small:24b",
    "host": "http://localhost:11434",
    "task": "Implement the foo function",
    "tools": ["read_file", "file_edit", "grep"],
    "max_steps": 10
  }
}
```

### OpenRouter Backend

**Setup:**
1. Get API key from https://openrouter.ai/keys
2. Export as environment variable:
   ```bash
   export OPENROUTER_API_KEY="sk-or-v1-..."
   ```

**Task-Type Based Model Selection:**

OpenRouter uses task types to automatically select appropriate models:

| Task Type | Default Model | Use Case |
|-----------|---------------|----------|
| `:coding` | `mistralai/devstral-2512:free` | Code generation, bug fixes |
| `:coding-alt` | `google/gemma-3-4b-it:free` | Fallback for coding |
| `:arch` | `xiaomi/mimo-v2-flash:free` | Architecture, design review |
| `:docs` | `openai/gpt-oss-120b:free` | Documentation, explanations |

**Configure Models (Runtime):**
```elisp
;; List current mappings
(mcp-call "openrouter_list_models" '())

;; Change model for a task type
(mcp-call "openrouter_set_model"
  '(:task_type "coding"
    :model "anthropic/claude-3-haiku:beta"))
```

**Preset-Based Selection:**

Presets automatically map to task types:

| Preset | Task Type | Description |
|--------|-----------|-------------|
| `tdd`, `tester`, `fixer` | `:coding` | Implementation work |
| `reviewer`, `clarity`, `solid` | `:arch` | Design review |
| `documenter` | `:docs` | Documentation |

```elisp
;; Use preset (auto-selects model)
(mcp-call "agent_delegate"
  '(:backend "openrouter"
    :preset "tdd"
    :task "Write unit tests for auth.clj"))
```

**MCP Tool Usage:**
```json
{
  "tool": "agent_delegate",
  "arguments": {
    "backend": "openrouter",
    "task_type": "coding",
    "task": "Create a REST API endpoint for users",
    "tools": ["read_file", "file_write", "clojure_eval"],
    "max_steps": 15
  }
}
```

## Hivemind Integration

Claude (the coordinator) can delegate to workers through the hivemind system:

### Direct Delegation from Claude

When you're in a Claude Code session, use `agent_delegate`:

```
# Ask Claude to delegate
"Delegate to a ling: implement the validate-email function in src/utils.clj"
```

Claude will call:
```json
{
  "tool": "agent_delegate",
  "arguments": {
    "backend": "openrouter",
    "task_type": "coding",
    "task": "Implement validate-email function in src/utils.clj that checks email format using regex"
  }
}
```

### Swarm Mode (Parallel Workers)

For parallel task execution, use the swarm system:

```elisp
;; Spawn multiple workers
(mcp-call "swarm_spawn" '(:name "worker-1" :presets ("tdd")))
(mcp-call "swarm_spawn" '(:name "worker-2" :presets ("documenter")))

;; Dispatch tasks
(mcp-call "swarm_dispatch"
  '(:slave_id "worker-1"
    :prompt "Implement the parser module"))

(mcp-call "swarm_dispatch"
  '(:slave_id "worker-2"
    :prompt "Document the API endpoints"))
```

### Backend Selection Strategy

| Scenario | Recommended Backend | Reason |
|----------|---------------------|--------|
| Privacy-sensitive code | Ollama | Data stays local |
| Quick iterations | Ollama | No network latency |
| Complex coding | OpenRouter (devstral) | Better reasoning |
| Documentation | OpenRouter (gpt-oss) | Good prose |
| Architecture review | OpenRouter (mimo) | Design patterns |
| Cost-sensitive | OpenRouter free tier | $0 cost |
| Production code | Claude (coordinator) | Highest quality |

## Advanced Configuration

### Adding Custom Task Types

```clojure
;; In REPL
(require '[hive-mcp.agent :as agent])

;; Add new task type
(agent/set-openrouter-model! :security "anthropic/claude-3-haiku:beta")

;; Map preset to task type
(agent/set-preset-task-type! "security-audit" :security)
```

Via MCP:
```elisp
(mcp-call "openrouter_set_model"
  '(:task_type "security"
    :model "anthropic/claude-3-haiku:beta"))

(mcp-call "preset_set_task_type"
  '(:preset "security-audit"
    :task_type "security"))
```

### Permission Gates

Dangerous tools require approval unless bypassed:

```clojure
;; Tools requiring approval
#{"file_write" "file_edit" "clojure_edit" "bash" "magit_commit" "magit_push"}
```

Grant auto-approve permission:
```json
{
  "tool": "agent_delegate",
  "arguments": {
    "backend": "openrouter",
    "task": "Fix the bug and commit",
    "permissions": ["auto-approve"]
  }
}
```

### Tracing & Monitoring

Enable event emission for debugging:

```json
{
  "tool": "agent_delegate",
  "arguments": {
    "backend": "openrouter",
    "task": "Implement feature X",
    "trace": true
  }
}
```

Events emitted:
- `:agent-started` - Task begins with backend/model info
- `:agent-step` - Each tool-use iteration
- `:agent-completed` - Task finished successfully
- `:agent-failed` - Task failed with error
- `:agent-max-steps` - Reached step limit

## Troubleshooting

### OpenRouter Issues

**"HTTP request failed" / API key not found:**
```bash
# Verify key is set
echo $OPENROUTER_API_KEY

# Re-export if needed
export OPENROUTER_API_KEY="sk-or-v1-..."

# Restart MCP server to pick up new env
```

**"Model not valid":**
```bash
# OpenRouter models use format: provider/model-name:free
# Wrong: devstral-small:24b (Ollama format)
# Right: mistralai/devstral-2512:free
```

### Ollama Issues

**"Connection refused":**
```bash
# Check if Ollama is running
curl http://localhost:11434/api/version

# Start if not
ollama serve
```

**"Model not found":**
```bash
# Pull the model first
ollama pull devstral-small:24b

# List available models
ollama list
```

### Timeout Issues

For long-running tasks, increase timeout:
```clojure
;; In REPL (300 seconds = 5 min)
(agent/delegate! {:backend :openrouter
                  :task "Complex refactoring task"
                  :max-steps 30})
```

## API Reference

### MCP Tools

| Tool | Description |
|------|-------------|
| `agent_delegate` | Delegate task to Ollama/OpenRouter |
| `openrouter_list_models` | List task-type → model mappings |
| `openrouter_set_model` | Set model for task type |
| `openrouter_remove_model` | Remove task-type mapping |
| `preset_list_mappings` | List preset → task-type mappings |
| `preset_set_task_type` | Set task type for preset |

### Clojure API

```clojure
(require '[hive-mcp.agent :as agent])

;; Delegation
(agent/delegate! opts)

;; Backend constructors
(agent/ollama-backend {:host "..." :model "..."})
(agent/openrouter-backend {:model "..." :preset "..." :task-type :coding})

;; Configuration
(agent/set-openrouter-model! :coding "model-id")
(agent/set-preset-task-type! "preset" :coding)
(agent/list-openrouter-models)
(agent/list-preset-mappings)
```

## See Also

- [HIVEMIND_ARCHITECTURE.md](./HIVEMIND_ARCHITECTURE.md) - Coordinator patterns
- [SWARM_BACKENDS.md](./SWARM_BACKENDS.md) - Terminal backend details
- [swarm-api.md](./swarm-api.md) - Swarm tool reference
