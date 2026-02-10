(ns hive-mcp.emacs.notify
  "Desktop notifications via notify-send (freedesktop.org).
   
   Provides OS-level notifications that appear in the system notification area,
   independent of Emacs. Used for hivemind alerts that require human attention."
  (:require [clojure.java.shell :as shell]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; =============================================================================
;; Desktop Notifications via notify-send
;; =============================================================================

(def ^:private urgency-levels
  "Map notification types to notify-send urgency levels."
  {"info" "normal"
   "warning" "normal"
   "error" "critical"})

(def ^:private notification-icons
  "Icons for notification types."
  {"info" "dialog-information"
   "warning" "dialog-warning"
   "error" "dialog-error"})

(defn notify!
  "Send a desktop notification via notify-send.
   
   Options:
     :summary  - Notification title (required)
     :body     - Notification body text (optional)
     :type     - Type: \"info\", \"warning\", \"error\" (default: \"info\")
     :timeout  - Timeout in ms (default: 5000)
     :app-name - Application name (default: \"hive-mcp\")
   
   Returns true on success, false on failure."
  [{:keys [summary body type timeout app-name]
    :or {type "info"
         timeout 5000
         app-name "hive-mcp"}}]
  (try
    (let [urgency (get urgency-levels type "normal")
          icon (get notification-icons type "dialog-information")
          args (cond-> ["notify-send"
                        "-a" app-name
                        "-u" urgency
                        "-i" icon
                        "-t" (str timeout)
                        (str summary)]
                 body (conj (str body)))
          {:keys [exit err]} (apply shell/sh args)]
      (if (zero? exit)
        (do
          (log/debug "Notification sent" {:summary summary :type type})
          true)
        (do
          (log/warn "notify-send failed" {:exit exit :err err})
          false)))
    (catch Exception e
      (log/warn "Failed to send notification:" (.getMessage e))
      false)))
