# Memory System Enhancement Plan

Based on analysis of `memory-systems.pdf` (best practices for LLM coding assistant memory systems).

## Current Implementation

- **Duration Hierarchy**: session(0d) → short-term(7d) → long-term(90d) → permanent
- **Entry Types**: note, snippet, convention, decision, conversation
- **Operations**: add, query, update, delete, promote, demote, cleanup-expired
- **Semantic Search**: via Chroma (mcp_memory_search_semantic)

## Gap Analysis vs PDF Recommendations

| Feature | PDF Recommendation | Current State | Priority |
|---------|-------------------|---------------|----------|
| Deduplication | Hash-based duplicate detection | Not implemented | HIGH |
| Audit Trail | Track access, helpfulness | Not implemented | HIGH |
| Versioning | Keep history of changes | Not implemented | MEDIUM |
| Code Chunking | AST-aware semantic units | Line-based storage | MEDIUM |
| Knowledge Graph | Entity relationships | Tags only | LOW |
| Hybrid Search | Keyword + semantic | Semantic only | LOW |

## Implementation Tasks

### 1. Duplicate Detection (Priority: HIGH)

**Goal**: "If same fact stored twice, keep one" - PDF best practice

**Implementation**:
```elisp
;; Schema addition
:content-hash string  ;; SHA-256 of normalized content

;; New functions
(emacs-mcp-memory-content-hash content)      ;; Compute hash
(emacs-mcp-memory-find-duplicate content)    ;; Find by hash
```

**Files to modify**:
- `elisp/emacs-mcp-memory.el` - Add hash computation and duplicate check
- `src/emacs_mcp/tools.clj` - Add `mcp_memory_check_duplicate` tool

**TDD Tests**:
- `test-duplicate-detection-same-content`
- `test-duplicate-detection-different-tags-merge`
- `test-duplicate-detection-normalized-whitespace`

### 2. Audit Log for Helpfulness (Priority: HIGH)

**Goal**: "Log whether memory was helpful" - feedback loop for quality

**Schema additions**:
```elisp
:access-count integer     ;; Times retrieved (default 0)
:last-accessed timestamp  ;; When last retrieved
:helpful-count integer    ;; Positive feedback count
:unhelpful-count integer  ;; Negative feedback count
```

**New functions**:
```elisp
(emacs-mcp-memory-log-access id)           ;; Increment access-count
(emacs-mcp-memory-mark-helpful id)         ;; Increment helpful-count
(emacs-mcp-memory-mark-unhelpful id)       ;; Increment unhelpful-count
(emacs-mcp-memory-helpfulness-ratio id)    ;; helpful / (helpful + unhelpful)
```

**MCP Tools**:
- `mcp_memory_log_access` - Called when memory retrieved
- `mcp_memory_feedback` - Submit helpful/unhelpful feedback

**Cleanup Integration**:
- Low helpfulness ratio → candidate for demotion
- Zero access after 30 days → candidate for cleanup

### 3. Fact Versioning (Priority: MEDIUM)

**Goal**: "Versioning your code memory" - preserve history

**Schema additions**:
```elisp
:version integer                ;; Current version (default 1)
:previous-versions vector       ;; [{:version N :content X :updated T}]
```

**Configuration**:
```elisp
(defcustom emacs-mcp-memory-max-versions 5
  "Maximum previous versions to keep per entry.")
```

**New functions**:
```elisp
(emacs-mcp-memory-history id)              ;; Get version history
(emacs-mcp-memory-rollback id version)     ;; Restore previous version
(emacs-mcp-memory-diff id v1 v2)           ;; Diff between versions
```

**MCP Tools**:
- `mcp_memory_get_history` - Retrieve version history
- `mcp_memory_rollback` - Restore to previous version

### 4. AST-aware Code Chunking (Priority: MEDIUM)

**Goal**: Supermemory approach - "chunk code by semantic units not arbitrary lines"

**When storing snippet type with code**:
1. Detect language from content or explicit param
2. Parse to identify form boundaries (defn, defmethod, def, ns)
3. Store metadata: `:language`, `:form-type`, `:symbols-defined`, `:symbols-referenced`

**Implementation**:
```clojure
;; Use existing org-clj parser for Clojure
(require '[emacs-mcp.org-clj :as org-clj])

(defn analyze-clojure-code [code]
  {:language :clojure
   :forms (parse-top-level-forms code)
   :symbols-defined (extract-definitions code)
   :symbols-referenced (extract-references code)})
```

**For elisp**: Use Emacs `read` to parse top-level forms

**Benefits**:
- Better semantic search (function-level granularity)
- Code reuse discovery via symbol relationships
- Cleaner RAG retrieval chunks

### 5. Knowledge Graph Relations (Priority: LOW)

**Goal**: Zep's Graphiti - "connect facts via knowledge graph"

**Schema addition**:
```elisp
:relations vector  ;; [{:type :relates-to :target-id "abc123"}]
```

**Relation types**:
- `:relates-to` - Topically related
- `:supersedes` - Newer version replaces older
- `:depends-on` - References another entry
- `:contradicts` - Conflicting information (flag for review)

**New functions**:
```elisp
(emacs-mcp-memory-link id1 id2 relation-type)
(emacs-mcp-memory-unlink id1 id2)
(emacs-mcp-memory-get-related id &optional relation-type)
(emacs-mcp-memory-graph-export &optional format)  ;; DOT, JSON
```

**Auto-detection**:
- Same tags → `:relates-to`
- Updated entry with same title → `:supersedes` old entry

### 6. Semantic Search Enhancements (Priority: LOW)

**Goal**: Better RAG with hybrid search and filters

**Current**: `mcp_memory_search_semantic query`

**Enhanced**:
```clojure
(mcp_memory_search_semantic
  :query "authentication flow"
  :threshold 0.7           ;; Minimum similarity score
  :type "snippet"          ;; Filter by type
  :tags ["auth" "security"] ;; Filter by tags
  :date-after "2025-12-01" ;; Date range
  :hybrid true             ;; Combine keyword + semantic
  :limit 10)
```

**Implementation**:
1. Verify Chroma embedding provider: `bb scripts/chroma-check.bb`
2. Add metadata filters to Chroma query
3. Implement keyword scoring (TF-IDF or simple term frequency)
4. Combine scores: `final = α * semantic + (1-α) * keyword`
5. Cache embeddings in entry for faster re-indexing

## Implementation Order

1. **Duplicate Detection** - Prevents data bloat, immediate value
2. **Audit Log** - Enables data-driven cleanup decisions
3. **Fact Versioning** - Safety net for updates
4. **AST-aware Chunking** - Better code memory quality
5. **Knowledge Graph** - Enhanced context retrieval
6. **Semantic Search+** - Improved RAG accuracy

## Related Kanban Tasks

- Existing: "Add memory garbage collection and fact versioning" (id: 6bb1d851)
- New tasks created in scratch_pad under `memory_enhancements`

## References

- PDF: `~/memory-systems.pdf`
- Tools: Chroma DB, Zep, Mem0, Memori, LangChain/LlamaIndex
- Existing code: `elisp/emacs-mcp-memory.el`, `src/emacs_mcp/tools.clj`
