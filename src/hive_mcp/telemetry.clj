(ns hive-mcp.telemetry
  "Facade for backward compatibility.
   Delegates to hive-mcp.telemetry.core after tree normalization."
  (:require [hive-mcp.telemetry.core :as core]))

(defmacro with-timing [& args] `(core/with-timing ~@args))
(defmacro with-eval-telemetry [& args] `(core/with-eval-telemetry ~@args))
(def configure-logging! core/configure-logging!)
(def log-eval-request core/log-eval-request)
(def log-eval-result core/log-eval-result)
(def log-eval-exception core/log-eval-exception)
(def emit-health-event! core/emit-health-event!)
(def health-summary core/health-summary)
(def get-recent-errors core/get-recent-errors)
(def health-error-types core/health-error-types)
(def health-severities core/health-severities)
