(ns modex-bb.mcp.log
  "Simple stderr logging for Babashka - replaces taoensso.timbre.")

(defn- log-msg [level & args]
  (binding [*out* *err*]
    (println (str "[" level "]") (apply str (interpose " " args)))))

(defn debug [& args]
  (apply log-msg "DEBUG" args))

(defn info [& args]
  (apply log-msg "INFO" args))

(defn warn [& args]
  (apply log-msg "WARN" args))

(defn error [& args]
  (apply log-msg "ERROR" args))
