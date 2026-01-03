# MCP-First: Prefer MCP Tools Over Native Tools

You have access to powerful MCP (Model Context Protocol) tools that are faster and more integrated than native Claude Code tools. **ALWAYS prefer MCP tools.**

## Tool Hierarchy (Use In This Order)

### 1. Emacs MCP (`mcp__emacs-mcp__*`) - PREFERRED
Use these for ALL Emacs-integrated operations:

| Instead of...          | Use MCP Tool                          |
|------------------------|---------------------------------------|
| `Read` file            | `mcp__emacs-mcp__get_buffer_content`  |
| `Grep` search          | `mcp__emacs-mcp__projectile_search`   |
| `Glob` find files      | `mcp__emacs-mcp__projectile_files`    |
| Git status/diff        | `mcp__emacs-mcp__magit_status/diff`   |
| Git commit/push        | `mcp__emacs-mcp__magit_commit/push`   |
| Git branches           | `mcp__emacs-mcp__magit_branches`      |
| Eval elisp             | `mcp__emacs-mcp__eval_elisp`          |
| Eval Clojure           | `mcp__emacs-mcp__cider_eval_silent`   |

### 2. Claude Context (`mcp__claude-context__*`) - Semantic Search
For **semantic/conceptual** code search (not just text matching):

```
mcp__claude-context__search_code  - Natural language queries
mcp__claude-context__index_codebase - Index before searching
```

**Examples:**
- "Find authentication logic" → `search_code(query: "authentication login flow")`
- "Where is error handling?" → `search_code(query: "error handling exception catching")`

### 3. Clojure MCP (`mcp__clojure-mcp-emacs__*`) - Clojure Projects
For Clojure file editing and REPL:

| Task                   | Tool                                    |
|------------------------|-----------------------------------------|
| Edit defn/def          | `clojure_edit` (structural, safer)      |
| Replace s-expression   | `clojure_edit_replace_sexp`             |
| Eval code              | `clojure_eval`                          |
| Project info           | `clojure_inspect_project`               |

### 4. Memory MCP - Persistent Context
```
mcp__emacs-mcp__mcp_memory_add     - Store notes/decisions/conventions
mcp__emacs-mcp__mcp_memory_query   - Retrieve with scope filtering
mcp__emacs-mcp__mcp_memory_search_semantic - Semantic search
```

## Rules

1. **NEVER use native `Read`** when buffer is open in Emacs - use `get_buffer_content`
2. **NEVER use native `Grep`** for project search - use `projectile_search`
3. **NEVER use native `Bash` for git** - use `magit_*` tools
4. **ALWAYS check `emacs_status`** first to verify Emacs connection
5. **For semantic search**, use `claude-context` over text grep

## Speed Comparison

| Operation       | Native Tool | MCP Tool      | Speed Gain |
|-----------------|-------------|---------------|------------|
| Project search  | Grep        | projectile    | ~3x faster |
| Git status      | Bash git    | magit_status  | ~2x faster |
| Code search     | Grep regex  | claude-context| Semantic!  |
| File read       | Read        | buffer_content| Live state |

## Example Workflow

```
1. Check emacs: mcp__emacs-mcp__emacs_status
2. Get context: mcp__emacs-mcp__mcp_get_context
3. Search code: mcp__claude-context__search_code (semantic)
4. Read file:   mcp__emacs-mcp__get_buffer_content (if open)
5. Edit:        mcp__clojure-mcp-emacs__clojure_edit (structural)
6. Git:         mcp__emacs-mcp__magit_* (status/stage/commit)
```

## Anti-Patterns (AVOID)

```
# BAD - Using native tools when MCP available
Bash(git status)           # Use magit_status instead
Bash(grep -r "pattern" .)  # Use projectile_search instead
Read("/path/to/file")      # Use get_buffer_content if open

# GOOD - MCP-first approach
mcp__emacs-mcp__magit_status(directory: "/project")
mcp__emacs-mcp__projectile_search(pattern: "pattern")
mcp__emacs-mcp__get_buffer_content(buffer_name: "file.clj")
```
