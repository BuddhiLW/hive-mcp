# /wrap Workflow Troubleshooting

## Overview

The `/wrap` workflow crystallizes session accomplishments into long-term memory. It consists of two phases:

1. **wrap_gather**: Collects session data (notes, tasks, commits)
2. **wrap_crystallize**: Persists a summary to memory

## Common Errors

### 1. NullPointerException: "this.text is null"

**Error message:**
```
Cannot invoke "java.lang.CharSequence.length()" because "this.text" is null
```

**Cause:** Notes with nil or map content being processed by `str/split-lines`.

**Solution:** Fixed in `crystal/core.clj` - `summarize-session-progress` now handles:
- nil content → "(no content)"
- map content → extracts :title or stringifies
- missing :content key → "(no content)"

**If you see this error:** Update to latest version or reload the crystal namespace:
```clojure
(require '[hive-mcp.crystal.core] :reload)
(require '[hive-mcp.crystal.hooks] :reload)
```

### 2. Wrong type argument: listp, ["tag1" "tag2" ...]

**Error message:**
```
*ERROR*: Wrong type argument: listp, ["session:2026-01-11" "session-summary" "wrap-generated"]
```

**Cause:** A memory entry has malformed tags stored as a cons cell with a vector cdr:
```elisp
("scope:project:x" . ["tag1" "tag2"])  ; WRONG
("scope:project:x" "tag1" "tag2")       ; CORRECT
```

This happens when tags from Clojure (a vector) are passed to elisp and `cons` is used without converting to a list first.

**Solution:**

1. **Find the corrupted entry:**
```elisp
(let* ((pid (hive-mcp-memory--project-id))
       (bad-entries '()))
  (dolist (type '("note" "snippet" "convention" "decision"))
    (dolist (entry (hive-mcp-memory--get-data pid type))
      (let ((tags (plist-get entry :tags)))
        (when (and (consp tags) (vectorp (cdr tags)))
          (push (plist-get entry :id) bad-entries)))))
  bad-entries)
```

2. **Fix the corrupted entry:**
```elisp
(let* ((entry (hive-mcp-memory-get "ENTRY-ID-HERE"))
       (tags (plist-get entry :tags))
       (fixed-tags (cons (car tags) (append (cdr tags) nil))))
  (hive-mcp-memory-update "ENTRY-ID-HERE" (list :tags fixed-tags)))
```

3. **Verify the fix:**
```elisp
(hive-mcp-memory-query 'note nil nil 5)  ; Should not error
```

**Prevention:** Fixed in `hive-mcp-memory.el` - `hive-mcp-memory--inject-project-scope` now converts vectors to lists before using `cons`.

### 3. Elisp format error with tags

**Error message:**
```
*ERROR*: Wrong type argument: listp, [...]
```
(during crystallize, not query)

**Cause:** Clojure code passing vector literal `["a" "b"]` to elisp instead of quoted list `'("a" "b")`.

**Solution:** Fixed in `crystal/hooks.clj` - uses `tags->elisp-list` helper to properly format tags for elisp.

## Diagnostic Commands

### Check wrap_gather data
```
mcp__emacs__wrap_gather
```

### Check wrap_crystallize
```
mcp__emacs__wrap_crystallize
```

### Check for corrupted memory entries
```elisp
M-x eval-expression RET
(let ((results '()))
  (maphash 
   (lambda (key entries)
     (dolist (entry entries)
       (let ((tags (plist-get entry :tags)))
         (when (and (consp tags) (vectorp (cdr tags)))
           (push (list :key key :id (plist-get entry :id)) results)))))
   hive-mcp-memory--cache)
  results)
```

## Related Files

- `src/hive_mcp/crystal/core.clj` - Session summarization logic
- `src/hive_mcp/crystal/hooks.clj` - Harvest and crystallize functions
- `src/hive_mcp/tools/crystal.clj` - MCP tool handlers
- `elisp/hive-mcp-memory.el` - Memory storage and queries
- `test/hive_mcp/crystal/core_test.clj` - TDD tests for edge cases
