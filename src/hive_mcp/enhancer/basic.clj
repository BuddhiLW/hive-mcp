(ns hive-mcp.enhancer.basic
  "BasicEnhancer - No-op fallback implementation of IKnowledgeEnhancer.

   CLARITY-L: Layers stay pure - this is the open-source fallback.

   This implementation provides safe defaults when the proprietary
   hive-knowledge library is not available. All methods return
   neutral/empty values that allow the system to function without
   AI-powered enhancement."
  (:require [hive-mcp.enhancer.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord BasicEnhancer []
  proto/IKnowledgeEnhancer

  (enhance-query [_this query opts]
    ;; Return query unchanged with empty expansions
    {:original   query
     :expanded   query
     :terms      []
     :embeddings nil
     :enhancer   :basic
     :opts       opts})

  (compute-trust [_this _entry]
    ;; Return neutral trust score
    ;; 0.5 = "no opinion" - neither trusted nor untrusted
    0.5)

  (detect-emergence [_this _scope]
    ;; No emergence detection without ML models
    [])

  (suggest-cross-pollination [_this _entry-id]
    ;; No cross-pollination suggestions without ML models
    [])

  (extract-learnings [_this _exploration-result]
    ;; No learning extraction without ML models
    []))

(defn create-basic-enhancer
  "Create a new BasicEnhancer instance.

   This is the fallback enhancer used when hive-knowledge
   is not available on the classpath.

   Returns:
     BasicEnhancer record implementing IKnowledgeEnhancer"
  []
  (->BasicEnhancer))
