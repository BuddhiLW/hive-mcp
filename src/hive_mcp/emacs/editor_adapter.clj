(ns hive-mcp.emacs.editor-adapter
  "EmacsclientEditor — IEditor implementation wrapping emacs.client.

   Bridges the emacsclient subprocess layer into the IEditor protocol,
   returning Result ADT values instead of legacy {:success :result :error} maps."
  (:require [hive-mcp.protocols.editor :as ed]
            [hive-mcp.emacs.client :as ec]
            [hive-dsl.result :as result]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- ec-result->result
  "Convert legacy emacs.client map to Result ADT.
   {:success true :result r}  -> (ok r)
   {:success false :error e}  -> (err :editor/eval-failed ...)"
  [{:keys [success result error timed-out]}]
  (cond
    success     (result/ok result)
    timed-out   (result/err :editor/timeout {:message error})
    :else       (result/err :editor/eval-failed {:message error})))

(defrecord EmacsclientEditor []
  ed/IEditor

  (editor-id [_this] :emacsclient)

  (available? [_this]
    (ec/emacs-running?))

  (eval-expr [_this code]
    (ec-result->result (ec/eval-elisp code)))

  (eval-expr [_this code opts]
    (let [timeout-ms (:timeout-ms opts ec/*default-timeout-ms*)]
      (ec-result->result (ec/eval-elisp-with-timeout code timeout-ms))))

  (feature-available? [_this feature-name]
    (try
      (let [{:keys [success result]} (ec/eval-elisp (format "(featurep '%s)" feature-name))]
        (and success (= "t" (str/trim (or result "")))))
      (catch Exception _ false)))

  (send-to-terminal [_this terminal-id text]
    (let [escaped-text (-> text
                           (str/replace "\\" "\\\\")
                           (str/replace "\"" "\\\"")
                           (str/replace "\n" "\\n"))
          code (format "(hive-mcp-swarm-api-dispatch \"%s\" \"%s\" 60000)"
                       terminal-id escaped-text)]
      (ec-result->result (ec/eval-elisp-with-timeout code 60000)))))

(defn ->emacsclient-editor
  "Create an EmacsclientEditor instance."
  []
  (->EmacsclientEditor))
