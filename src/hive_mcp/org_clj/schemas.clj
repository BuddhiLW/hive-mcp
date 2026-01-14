(ns hive-mcp.org-clj.schemas
  "Malli schemas for org-mode document structures.
   
   Provides validation schemas for:
   - Headlines (TODO keywords, priorities, tags, properties)
   - Documents (file properties, headline trees)
   
   CLARITY: Single Responsibility - only schema definitions and validation."
  (:require [malli.core :as m]
            [malli.error :as me]))

;; =============================================================================
;; Enumeration Schemas
;; =============================================================================

(def TodoKeyword
  "Valid TODO keywords recognized by the parser"
  [:enum "TODO" "DONE" "IN-PROGRESS" "IN-REVIEW" "CANCELLED" nil])

(def Priority
  "Priority markers [#A], [#B], [#C]"
  [:enum "A" "B" "C" nil])

;; =============================================================================
;; Planning Schema
;; =============================================================================

(def Planning
  "Schema for org planning info (CLOSED, SCHEDULED, DEADLINE)"
  [:map
   [:closed {:optional true} [:maybe :string]]
   [:scheduled {:optional true} [:maybe :string]]
   [:deadline {:optional true} [:maybe :string]]])

;; =============================================================================
;; Headline Schema
;; =============================================================================

(def Headline
  "Schema for an org headline.
   
   Example headline: ** TODO [#A] Task title :tag1:tag2:
   Parses to:
   {:type :headline
    :level 2
    :keyword \"TODO\"
    :priority \"A\"
    :title \"Task title\"
    :tags [\"tag1\" \"tag2\"]
    :properties {...}
    :planning {...}
    :content [...]
    :children [...]}"
  [:map
   [:type [:= :headline]]
   [:level :int]
   [:keyword {:optional true} [:maybe :string]]
   [:priority {:optional true} [:maybe :string]]
   [:title :string]
   [:tags {:optional true} [:vector :string]]
   [:properties {:optional true} [:map-of :keyword :string]]
   [:planning {:optional true} Planning]
   [:content {:optional true} [:vector :any]]
   [:children {:optional true} [:vector [:ref #'Headline]]]])

;; =============================================================================
;; Document Schema
;; =============================================================================

(def Document
  "Schema for an org document.
   
   Documents contain:
   - File-level properties (#+TITLE:, #+AUTHOR:, etc.)
   - A tree of headlines"
  [:map
   [:type [:= :document]]
   [:properties {:optional true} [:map-of :keyword :string]]
   [:headlines [:vector Headline]]])

;; =============================================================================
;; Validation Functions
;; =============================================================================

(defn validate-headline
  "Validate a headline map against the Headline schema.
   
   Returns:
   - {:valid true :data headline} on success
   - {:valid false :errors [...]} on failure with humanized errors"
  [headline]
  (if (m/validate Headline headline)
    {:valid true :data headline}
    {:valid false :errors (me/humanize (m/explain Headline headline))}))

(defn validate-document
  "Validate a document map against the Document schema.
   
   Returns:
   - {:valid true :data doc} on success  
   - {:valid false :errors [...]} on failure with humanized errors"
  [doc]
  (if (m/validate Document doc)
    {:valid true :data doc}
    {:valid false :errors (me/humanize (m/explain Document doc))}))

(defn valid-headline?
  "Predicate version of validate-headline. Returns true if valid."
  [headline]
  (m/validate Headline headline))

(defn valid-document?
  "Predicate version of validate-document. Returns true if valid."
  [doc]
  (m/validate Document doc))
