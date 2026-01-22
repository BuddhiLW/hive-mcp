# Seed Memory System Architecture

## Overview

The seed memory system provides a way to bootstrap hive-mcp installations with foundational knowledge. Seeds are human-readable markdown files with YAML frontmatter that can be imported into the Chroma memory store as permanent, globally-scoped entries.

```
┌────────────────────────────────────────────────────────────┐
│                    Seed Memory Flow                        │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  seeds/conventions/scope-hierarchy.md                      │
│           │                                                │
│           ▼                                                │
│  ┌─────────────────────────────┐                          │
│  │   Parse YAML Frontmatter    │                          │
│  │   + Markdown Body           │                          │
│  └─────────────┬───────────────┘                          │
│                │                                           │
│                ▼                                           │
│  ┌─────────────────────────────┐                          │
│  │  Deduplicate (title hash)   │──► Skip if exists        │
│  └─────────────┬───────────────┘                          │
│                │                                           │
│                ▼                                           │
│  ┌─────────────────────────────┐                          │
│  │  mcp_memory_add             │                          │
│  │  - type: from frontmatter   │                          │
│  │  - tags: [..., scope:global]│                          │
│  │  - duration: permanent      │                          │
│  └─────────────────────────────┘                          │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

## Design Goals

1. **Human-editable**: Seeds are markdown files, easy to read and author
2. **Version-controlled**: Seeds live in the repository, enabling PRs and review
3. **Idempotent import**: Running import multiple times won't duplicate entries
4. **Scope-aware**: Seeds apply globally unless explicitly scoped
5. **Type-safe**: Frontmatter enforces valid memory types and durations

---

## Directory Structure

```
seeds/
├── README.md                    # User guide for seeds
├── conventions/                 # Foundational patterns and practices
│   ├── scope-hierarchy.md       # Project/global scope rules
│   ├── token-hierarchy.md       # Token budget principles
│   ├── memory-hygiene.md        # Memory management best practices
│   └── ling-protocol.md         # Worker agent conventions
├── decisions/                   # Architectural decision records
│   ├── chroma-over-datascript.md
│   ├── core-logic-for-constraints.md
│   └── dual-storage-rationale.md
└── snippets/                    # Reusable code patterns
    ├── with-chroma-macro.md     # Standard Chroma wrapper
    ├── scope-resolution.md      # Project ID resolution pattern
    └── graceful-fallback.md     # Error handling idiom
```

### Directory Semantics

| Directory       | Memory Type   | Purpose                                      |
|-----------------|---------------|----------------------------------------------|
| `conventions/`  | `convention`  | Team agreements, coding standards, protocols |
| `decisions/`    | `decision`    | ADRs, rationale for architectural choices    |
| `snippets/`     | `snippet`     | Code patterns, copy-paste templates          |

---

## Seed File Format

Each `.md` file in the seeds directory becomes one memory entry. The format uses YAML frontmatter followed by markdown content.

### Required Structure

```markdown
---
type: convention                    # Required: convention | decision | snippet
tags: [architecture, foundational]  # Required: at least one tag
duration: permanent                 # Optional: defaults to permanent
kg_implements: []                   # Optional: KG edge - implements these entry IDs
kg_supersedes: []                   # Optional: KG edge - replaces these entries
kg_depends_on: []                   # Optional: KG edge - requires these entries
kg_refines: []                      # Optional: KG edge - improves these entries
---

# Title Goes Here

Content of the memory entry. This can include:

- Markdown formatting
- Code blocks
- Lists and tables

## Sections

Organize content with headers as needed.
```

### Frontmatter Schema

```yaml
# Required fields
type: string          # One of: convention, decision, snippet
tags: string[]        # At least one tag required

# Optional fields
duration: string      # One of: ephemeral, short, medium, long, permanent
                      # Default: permanent (seeds are foundational)

# Knowledge Graph edges (optional)
kg_implements: string[]   # Entry IDs this seed implements
kg_supersedes: string[]   # Entry IDs this seed replaces
kg_depends_on: string[]   # Entry IDs this seed requires
kg_refines: string[]      # Entry IDs this seed improves
```

### Auto-Applied Attributes

The import process automatically applies:

| Attribute          | Value                              | Rationale                          |
|--------------------|------------------------------------|------------------------------------|
| `scope:global`     | Added to tags                      | Seeds work across all projects     |
| `seed`             | Added to tags                      | Identify imported seeds            |
| `duration`         | `permanent` if not specified       | Seeds are foundational             |
| `source:seed`      | Added to metadata                  | Track entry origin                 |

### Example Seed File

**File:** `seeds/conventions/scope-hierarchy.md`

```markdown
---
type: convention
tags: [architecture, foundational, memory-system]
duration: permanent
---

# Project Scope Hierarchy

Memory entries are scoped hierarchically to prevent cross-project pollution.

## Scope Levels

1. **Global** (`scope:global`) - Applies to all projects
2. **Project** (`scope:project:<id>`) - Specific to one project
3. **Submodule** (`scope:project:<parent>:<child>`) - Nested project scope

## Visibility Rules

- Global entries are always visible
- Project entries visible to that project and its submodules
- Submodule entries only visible within that submodule

## Tag Convention

Always include at least one scope tag:
- `scope:global` for cross-project knowledge
- Omit explicit scope to inherit from project context
```

---

## Import Algorithm

### Deduplication Strategy

Seeds are deduplicated by **content hash** to prevent duplicates while allowing updates:

```clojure
;; Pseudocode for import deduplication
(defn should-import? [seed-content]
  (let [content-hash (sha256 (normalize seed-content))
        existing (chroma/query-by-hash content-hash)]
    (nil? existing)))
```

### Import Process

```
1. Scan seeds/ directory recursively for .md files
2. For each file:
   a. Parse YAML frontmatter
   b. Extract markdown body (content after ---)
   c. Validate frontmatter schema
   d. Compute content hash
   e. Check for existing entry with same hash
   f. If not exists:
      - Add scope:global tag
      - Add seed tag
      - Set duration to permanent (if not specified)
      - Call mcp_memory_add
   g. Log result (imported/skipped/error)
3. Return summary: {imported: N, skipped: M, errors: []}
```

### Implementation Sketch

```clojure
(ns hive-mcp.tools.seeds
  (:require [hive-mcp.tools.memory.crud :as crud]
            [clojure.java.io :as io]
            [clj-yaml.core :as yaml]))

(defn parse-seed-file
  "Parse a seed markdown file into frontmatter + content."
  [file-path]
  (let [content (slurp file-path)
        [_ frontmatter body] (re-matches #"(?s)---\n(.+?)\n---\n(.+)" content)]
    {:frontmatter (yaml/parse-string frontmatter)
     :content (str/trim body)
     :source-file file-path}))

(defn import-seed!
  "Import a single seed file. Returns {:status :imported|:skipped|:error}."
  [{:keys [frontmatter content source-file]}]
  (let [content-hash (chroma/content-hash content)
        existing (chroma/query-by-content-hash content-hash)]
    (if existing
      {:status :skipped :reason :duplicate :file source-file}
      (do
        (crud/handle-add
          {:type (:type frontmatter)
           :content content
           :tags (-> (:tags frontmatter)
                     (conj "scope:global")
                     (conj "seed")
                     vec)
           :duration (or (:duration frontmatter) "permanent")
           :kg_implements (:kg_implements frontmatter)
           :kg_supersedes (:kg_supersedes frontmatter)
           :kg_depends_on (:kg_depends_on frontmatter)
           :kg_refines (:kg_refines frontmatter)})
        {:status :imported :file source-file}))))

(defn import-all-seeds!
  "Import all seeds from the seeds directory."
  [seeds-dir]
  (let [seed-files (->> (file-seq (io/file seeds-dir))
                        (filter #(.endsWith (.getName %) ".md"))
                        (remove #(= "README.md" (.getName %))))
        results (map (comp import-seed! parse-seed-file str) seed-files)]
    {:imported (count (filter #(= :imported (:status %)) results))
     :skipped (count (filter #(= :skipped (:status %)) results))
     :errors (filter #(= :error (:status %)) results)}))
```

---

## First-Run Experience

### Detection

On startup, detect if this is a fresh installation:

```clojure
(defn fresh-install?
  "Check if memory is empty (no conventions or decisions)."
  []
  (let [conventions (chroma/query-entries :type "convention" :limit 1)
        decisions (chroma/query-entries :type "decision" :limit 1)]
    (and (empty? conventions) (empty? decisions))))
```

### Automatic Prompt

When `fresh-install?` returns true, the system should:

1. Log: "Fresh installation detected. Run `/import-seeds` to load foundational knowledge."
2. Add to catchup context: "Seeds available - run `/import-seeds` to bootstrap memory"

### Manual Import

Users can always import seeds manually:

```
/import-seeds                    # Import from default seeds/ directory
/import-seeds path/to/seeds      # Import from custom directory
```

---

## MCP Tool Definition

```clojure
{:name "mcp_import_seeds"
 :description "Import seed memories from markdown files. Seeds are foundational
               knowledge (conventions, decisions, snippets) stored as markdown
               with YAML frontmatter. Automatically deduplicates by content hash."
 :inputSchema {:type "object"
               :properties {"directory" {:type "string"
                                         :description "Path to seeds directory (default: ./seeds)"}
                            "dry_run" {:type "boolean"
                                       :description "Preview what would be imported without importing"}
                            "force" {:type "boolean"
                                     :description "Re-import even if content hash matches (updates entries)"}}
               :required []}
 :handler handle-import-seeds}
```

---

## Contributing Seeds

### PR Process

1. **Create seed file** in appropriate directory (`conventions/`, `decisions/`, `snippets/`)
2. **Follow format**: YAML frontmatter + markdown body
3. **Include required fields**: `type`, `tags` (at least one)
4. **Write clear content**: Seeds should be self-contained and useful
5. **Submit PR** with description of why this seed is valuable

### Seed Quality Checklist

- [ ] Type matches directory (`convention` in `conventions/`, etc.)
- [ ] At least one meaningful tag
- [ ] Content is self-contained (no external dependencies)
- [ ] Title clearly describes the knowledge
- [ ] Would be useful to new hive-mcp users
- [ ] Not project-specific (seeds are global)

### Review Criteria

Seeds are reviewed for:
- **Accuracy**: Is the information correct?
- **Clarity**: Is it well-written and understandable?
- **Utility**: Will this help users?
- **Scope**: Is this truly foundational/global?

---

## Upgrade Path

### Updating Existing Seeds

When a seed needs updates:

1. Modify the seed file in the repository
2. The next import will **skip** due to content hash mismatch with stored version
3. To update stored entries:
   - Use `force: true` flag: `/import-seeds --force`
   - Or manually update via `mcp_memory_add` with `kg_supersedes` pointing to old entry

### Versioning Seeds

Seeds themselves aren't versioned, but the Knowledge Graph tracks evolution:

```markdown
---
type: decision
tags: [architecture, v2]
kg_supersedes: ["decision-abc123"]  # ID of previous version
---

# Updated Decision Title

New content that supersedes the previous version...
```

---

## Related Documentation

- [Knowledge Graph Architecture](../KNOWLEDGE_GRAPH_ARCHITECTURE.md) - How KG edges work
- [Memory System](../../src/hive_mcp/tools/memory.clj) - Memory CRUD operations
- [Scope Resolution](../../src/hive_mcp/tools/memory/scope.clj) - Project scoping logic
