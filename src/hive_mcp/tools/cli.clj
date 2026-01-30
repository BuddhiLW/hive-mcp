(ns hive-mcp.tools.cli
  "CLI-style subcommand dispatcher for consolidated tools"
  (:require [clojure.string :as str]))

(defn format-help
  "Format help text listing all available commands."
  [handlers]
  (str "Available commands:\n"
       (str/join "\n" (map #(str "  - " (name %)) (keys handlers)))))

(defn make-cli-handler
  "Create a CLI-style handler that dispatches on :command param.

   handlers: map of keyword -> handler-fn
   Returns: fn that dispatches to appropriate handler"
  [handlers]
  (fn [{:keys [command] :as params}]
    (let [cmd (keyword command)]
      (if-let [handler (get handlers cmd)]
        (handler params)
        (if (= cmd :help)
          {:type "text" :text (format-help handlers)}
          {:isError true :text (str "Unknown command: " command
                                    ". Valid: " (keys handlers))})))))
