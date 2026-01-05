(ns hive-mcp.synergy
  "Synergy functions demonstrating clojure-mcp + hive-mcp collaboration.
   
   The 'marriage' between:
   - clojure-mcp (dev-tools₁): file editing, code analysis, REPL eval
   - hive-mcp (emacs-bridge₂): visual feedback, navigation, buffer management
   
   With hive-mcp.el loaded in Emacs, Claude can now access:
   - Memory: notes, snippets, conventions, decisions
   - Context: buffer, region, project, git
   - Workflows: user-defined automations
   
   See: elisp/hive-mcp-api.el for the full API"
  (:require [hive-mcp.emacsclient :as ec]
            [clojure.string :as str]))

;; ============================================================================
;; 1. EDIT-AND-SHOW: Real-time visual feedback
;; ============================================================================

(defn refresh-buffer!
  "After editing a file with clojure-mcp, refresh it in Emacs."
  [file-path]
  (ec/eval-elisp!
   (format "(when-let ((buf (get-file-buffer %s)))
              (with-current-buffer buf
                (revert-buffer t t t)))"
           (pr-str file-path))))

(defn show-edit-diff!
  "After editing, show the diff in Emacs using ediff or diff-hl."
  [file-path]
  (ec/eval-elisp!
   (format "(progn
              (find-file %s)
              (when (fboundp 'diff-hl-mode) (diff-hl-mode 1))
              (vc-diff nil))"
           (pr-str file-path))))

;; ============================================================================
;; 2. JUMP-TO-DEFINITION: Semantic navigation
;; ============================================================================

(defn jump-to!
  "Navigate Emacs to a specific file and line. Visual feedback with pulse."
  [file-path line-num & {:keys [recenter highlight]
                         :or {recenter 5 highlight true}}]
  (ec/eval-elisp!
   (format "(progn
              (find-file %s)
              (goto-line %d)
              (recenter %d)
              %s)"
           (pr-str file-path)
           line-num
           recenter
           (if highlight
             "(when (fboundp 'pulse-momentary-highlight-one-line)
                (pulse-momentary-highlight-one-line (point)))"
             ""))))

(defn jump-to-symbol!
  "Jump to a symbol definition (uses Emacs' xref)."
  [symbol-name]
  (ec/eval-elisp!
   (format "(xref-find-definitions %s)" (pr-str symbol-name))))

;; ============================================================================
;; 3. REPL FEEDBACK: Show evaluation results in Emacs
;; ============================================================================

(defn show-in-buffer!
  "Display content in a dedicated Emacs buffer."
  [buffer-name content & {:keys [mode] :or {mode "clojure-mode"}}]
  (ec/eval-elisp!
   (format "(let ((buf (get-buffer-create %s)))
              (with-current-buffer buf
                (erase-buffer)
                (insert %s)
                (goto-char (point-min))
                (when (fboundp '%s) (%s))
                (display-buffer buf)))"
           (pr-str buffer-name)
           (pr-str content)
           mode mode)))

(defn show-eval-result!
  "Evaluate Clojure code and show result in Emacs popup."
  [result]
  (show-in-buffer! "*clojure-mcp-result*" (pr-str result)))

;; ============================================================================
;; 4. ERROR NAVIGATION: Parse errors, jump to locations
;; ============================================================================

(defn show-error-at!
  "Show an error overlay at a specific location in Emacs."
  [file-path line-num error-msg]
  (ec/eval-elisp!
   (format "(progn
              (find-file %s)
              (goto-line %d)
              (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
                (overlay-put ov 'face '(:background \"#5c0000\"))
                (overlay-put ov 'help-echo %s)
                (overlay-put ov 'evaporate t)
                (recenter 5)
                (message %s)))"
           (pr-str file-path)
           line-num
           (pr-str error-msg)
           (pr-str (str "Error at line " line-num ": " error-msg)))))

;; ============================================================================
;; 5. CODE EXPLORATION: Collapsed view in Claude, expanded in Emacs
;; ============================================================================

(defn show-function!
  "Given a function found via clojure-mcp's collapsed view,
   expand and highlight it in Emacs using imenu."
  [file-path fn-name]
  (ec/eval-elisp!
   (format "(progn
              (find-file %s)
              (goto-char (point-min))
              (when (search-forward-regexp (concat \"(defn-? \" %s) nil t)
                (beginning-of-defun)
                (recenter 3)
                (mark-defun)
                (when (fboundp 'pulse-momentary-highlight-region)
                  (pulse-momentary-highlight-region (region-beginning) (region-end)))))"
           (pr-str file-path)
           (pr-str fn-name))))

;; ============================================================================
;; NEW: hive-mcp.el API Integration
;; ============================================================================
;; These functions use the new hive-mcp.el package loaded in Emacs
;; Requires: (require 'hive-mcp) and (hive-mcp-mode 1) in Emacs

(defn get-full-context!
  "Get complete context from Emacs including memory.
   Returns buffer, region, project, git, and stored memory."
  []
  (ec/eval-elisp! "(hive-mcp-api-get-context)"))

(defn add-note!
  "Add a note to project memory."
  [content & {:keys [tags] :or {tags []}}]
  (ec/eval-elisp!
   (format "(hive-mcp-api-memory-add \"note\" %s '%s)"
           (pr-str content)
           (pr-str tags))))

(defn add-convention!
  "Record a project convention."
  [description & {:keys [example]}]
  (ec/eval-elisp!
   (format "(hive-mcp-api-memory-add \"convention\" '(:description %s :example %s))"
           (pr-str description)
           (pr-str example))))

(defn query-memory!
  "Query project memory by type.
   Type is one of: note, snippet, convention, decision, conversation"
  [type & {:keys [limit] :or {limit 20}}]
  (ec/eval-elisp!
   (format "(hive-mcp-api-memory-query %s nil %d)"
           (pr-str type)
           limit)))

(defn log-conversation!
  "Log conversation for context persistence."
  [role content]
  (ec/eval-elisp!
   (format "(hive-mcp-api-conversation-log %s %s)"
           (pr-str role)
           (pr-str content))))

(defn run-workflow!
  "Run a user-defined workflow."
  [workflow-name & {:keys [args]}]
  (if args
    (ec/eval-elisp!
     (format "(hive-mcp-api-run-workflow %s '%s)"
             (pr-str workflow-name)
             (pr-str args)))
    (ec/eval-elisp!
     (format "(hive-mcp-api-run-workflow %s)"
             (pr-str workflow-name)))))

(defn notify!
  "Show notification to user."
  [message & {:keys [type] :or {type "info"}}]
  (ec/eval-elisp!
   (format "(hive-mcp-api-notify %s %s)"
           (pr-str message)
           (pr-str type))))

(defn prompt-user!
  "Prompt user for input."
  [prompt & {:keys [default]}]
  (ec/eval-elisp!
   (format "(hive-mcp-api-prompt %s %s)"
           (pr-str prompt)
           (if default (pr-str default) "nil"))))

(defn confirm-user!
  "Ask user for yes/no confirmation."
  [prompt]
  (ec/eval-elisp!
   (format "(hive-mcp-api-confirm %s)" (pr-str prompt))))

;; ============================================================================
;; 6. TEST INTEGRATION: Run tests, show results
;; ============================================================================

(defn show-test-results!
  "Display test results in a compilation-like buffer."
  [results]
  (let [formatted (str "Test Results\n"
                       "============\n\n"
                       results)]
    (show-in-buffer! "*test-results*" formatted
                     :mode "compilation-mode")))

;; ============================================================================
;; 7. STRUCTURAL EDITING PREVIEW
;; ============================================================================

(defn preview-edit!
  "Before committing a clojure-mcp edit, preview the change in Emacs."
  [file-path old-form new-form]
  (ec/eval-elisp!
   (format "(progn
              (find-file %s)
              (goto-char (point-min))
              (when (search-forward %s nil t)
                (goto-char (match-beginning 0))
                (recenter 3)
                (let ((start (point))
                      (end (progn (forward-sexp) (point))))
                  (pulse-momentary-highlight-region start end)
                  (message \"Found form to replace. New form: %%s\" %s))))"
           (pr-str file-path)
           (pr-str (str/trim old-form))
           (pr-str (str/trim new-form)))))

;; ============================================================================
;; WORKFLOW EXAMPLES
;; ============================================================================

(comment
  ;; Workflow 1: Edit-and-See
  ;; 1. Claude uses clojure-mcp to edit a function
  ;; 2. Automatically refresh in Emacs to see the change
  (refresh-buffer! "/path/to/edited/file.clj")

  ;; Workflow 2: Navigate to Definition
  ;; 1. Claude uses clojure-mcp grep to find a function
  ;; 2. Jump to it in Emacs for context
  (jump-to! "/path/to/file.clj" 42 :highlight true)

  ;; Workflow 3: REPL-Driven Development
  ;; 1. Claude evaluates code via clojure-mcp REPL
  ;; 2. Show result in an Emacs buffer for review
  (show-eval-result! {:users [{:name "Alice"} {:name "Bob"}]})

  ;; Workflow 4: Error-Guided Fixing
  ;; 1. Claude runs tests, gets error at line 57
  ;; 2. Highlight the error in Emacs
  ;; 3. Claude reads context, proposes fix
  (show-error-at! "/path/to/file.clj" 57 "Null pointer exception")

  ;; Workflow 5: Explore-Then-Expand
  ;; 1. Claude reads collapsed view with clojure-mcp
  ;; 2. Identifies interesting function
  ;; 3. Opens and highlights it in Emacs for human review
  (show-function! "/path/to/file.clj" "process-data"))
