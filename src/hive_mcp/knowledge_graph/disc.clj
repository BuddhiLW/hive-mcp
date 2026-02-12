(ns hive-mcp.knowledge-graph.disc
  "Disc entity management for file abstraction level.

   Disc entities track the actual state of files on disk, enabling:
   - Grounding verification without re-reading files
   - Change detection via content hash comparison
   - Git commit tracking for provenance
   - Bayesian certainty tracking with automatic event wiring

   FACADE: This namespace re-exports actively-used vars from sub-namespaces:
   - disc.hash       — Content hashing (pure)
   - disc.volatility — Volatility classification, staleness constants (pure)
   - disc.crud       — DataScript CRUD operations
   - disc.staleness  — Staleness surfacing + KG-first context
   - disc.propagation — Time decay + transitive staleness propagation"

  (:require [hive-mcp.knowledge-graph.disc.hash :as hash]
            [hive-mcp.knowledge-graph.disc.volatility :as vol]
            [hive-mcp.knowledge-graph.disc.crud :as crud]
            [hive-mcp.knowledge-graph.disc.staleness :as staleness]
            [hive-mcp.knowledge-graph.disc.propagation :as propagation]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Re-exports: Hash (disc.hash)
;; =============================================================================

(def compute-hash
  "Compute SHA-256 hash of a string."
  hash/compute-hash)

(def file-content-hash
  "Read file and compute content hash.
   Returns {:hash \"..\" :exists? true} or {:exists? false}."
  hash/file-content-hash)

;; =============================================================================
;; Re-exports: Volatility Constants (disc.volatility)
;; =============================================================================

(def base-staleness-values
  "Base staleness values by source event type."
  vol/base-staleness-values)

(def format-staleness-warnings
  "Format staleness warnings as a text block for injection into task prompts."
  vol/format-staleness-warnings)

;; =============================================================================
;; Re-exports: CRUD (disc.crud)
;; =============================================================================

(def add-disc!
  "Add or update a disc entity."
  crud/add-disc!)

(def get-disc
  "Get disc entity by file path."
  crud/get-disc)

(def remove-disc!
  "Remove a disc entity by path."
  crud/remove-disc!)

(def update-disc!
  "Update an existing disc entity by path with given attribute map."
  crud/update-disc!)

(def disc-exists?
  "Check if a disc entity exists for the given path."
  crud/disc-exists?)

(def get-all-discs
  "Get all disc entities, with optional project-id filter."
  crud/get-all-discs)

(def disc-stats
  "Return summary statistics for all disc entities."
  crud/disc-stats)

(def touch-disc!
  "Record that a file was read by an agent."
  crud/touch-disc!)

;; =============================================================================
;; Re-exports: Staleness Surfacing (disc.staleness)
;; =============================================================================

(def staleness-score
  "Compute staleness score for a disc entity (impure, reads file)."
  staleness/staleness-score)

(def staleness-warnings
  "Generate staleness warnings for a collection of file paths."
  staleness/staleness-warnings)

(def top-stale-files
  "Query top-N most stale disc entities."
  staleness/top-stale-files)

(def kg-first-context
  "Consult the KG before file reads — the Structural Differential principle."
  staleness/kg-first-context)

;; =============================================================================
;; Re-exports: Propagation & Certainty Wiring (disc.propagation)
;; =============================================================================

(def apply-time-decay-to-all-discs!
  "Apply time decay to all disc entities in DataScript."
  propagation/apply-time-decay-to-all-discs!)

(def propagate-staleness!
  "Propagate staleness from a disc to dependent labels via KG edges."
  propagation/propagate-staleness!)
