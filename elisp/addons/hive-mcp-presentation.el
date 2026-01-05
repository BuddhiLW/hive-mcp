;;; hive-mcp-presentation.el --- Org-mode presentation addon for hive-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedro.branquinho@usp.br>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, presentations, org-mode, beamer, reveal
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon streamlines org-mode presentation creation with support for:
;; - Beamer (LaTeX/PDF) presentations
;; - Reveal.js (HTML) presentations
;;
;; Features:
;; - Pre-configured templates for both formats
;; - Slide scaffolding functions
;; - MCP tools for AI-assisted content generation
;; - Transient UI for presentation workflow
;;
;; Usage:
;;   M-x hive-mcp-presentation-create   ; Create new presentation
;;   M-x hive-mcp-presentation-transient ; Open menu
;;   C-c m p                              ; Quick access (if configured)

;;; Code:

(require 'org)
(require 'hive-mcp-api)

(declare-function transient-define-prefix "transient")
(declare-function org-export-dispatch "ox")

;;;; Customization:

(defgroup hive-mcp-presentation nil
  "Org-mode presentation creation with hive-mcp."
  :group 'hive-mcp
  :prefix "hive-mcp-presentation-")

(defcustom hive-mcp-presentation-default-format 'beamer
  "Default presentation format."
  :type '(choice (const :tag "Beamer (LaTeX/PDF)" beamer)
                 (const :tag "Reveal.js (HTML)" revealjs))
  :group 'hive-mcp-presentation)

(defcustom hive-mcp-presentation-author user-full-name
  "Default author name for presentations."
  :type 'string
  :group 'hive-mcp-presentation)

(defcustom hive-mcp-presentation-email user-mail-address
  "Default email for presentations."
  :type 'string
  :group 'hive-mcp-presentation)

(defcustom hive-mcp-presentation-beamer-theme "magpie"
  "Default Beamer color theme."
  :type 'string
  :group 'hive-mcp-presentation)

(defcustom hive-mcp-presentation-reveal-theme "blood"
  "Default Reveal.js theme."
  :type 'string
  :group 'hive-mcp-presentation)

(defcustom hive-mcp-presentation-templates-dir
  (expand-file-name "templates/presentations"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Directory containing presentation templates."
  :type 'directory
  :group 'hive-mcp-presentation)

;;;; Beamer Template:

(defconst hive-mcp-presentation--beamer-header
  "#+STARTUP: latexpreview
#+STARTUP: imagepreview

#+LATEX_COMPILER: xelatex

#+title: %s
#+EMAIL: %s
#+BEAMER_HEADER: \\author[%s]{\\textbf{%s \\\\ \\text{\\scriptsize{%s}}}}
#+BEAMER_HEADER: \\date[]{\\textbf{\\scriptsize{%s}}}

#+BEAMER_FRAME_LEVEL: 3
#+LATEX_CLASS: beamer
#+BEAMER_COLOR_THEME: %s
#+LATEX_CLASS_OPTIONS: [bigger]
#+BEAMER_HEADER: \\useoutertheme[height=30pt]{sidebar}
#+BEAMER_HEADER: \\setbeamertemplate{frametitle}[sidebar theme]
#+BEAMER_HEADER: \\setbeamertemplate{itemize item}{\\ding{166}}
#+BEAMER_HEADER: \\setbeamercolor{item projected}{bg=magenta!90!black,fg=white}
#+BEAMER_HEADER: \\setbeamertemplate{enumerate item}[circle]
#+BEAMER_HEADER: \\setbeamerfont{block title}{size={\\centering}}
#+BEAMER_HEADER: \\setbeamercolor{block title}{bg=black!30!white,fg=white}
#+COLUMNS: %%45ITEM %%10BEAMER_ENV(Env) %%10BEAMER_ACT(Act) %%4BEAMER_COL(Col)

#+LANGUAGE: en
#+OPTIONS: H:3 num:t toc:t \\n:nil @:t ::t |:t ^:t -:t f:t *:t <:t
#+OPTIONS: TeX:t LaTeX:t skip:nil d:nil todo:t pri:nil tags:not-in-toc

#+latex_header: \\usepackage{pifont}
#+LATEX_HEADER: \\usepackage{verbatim}
#+LATEX_HEADER: \\makeatletter
#+LATEX_HEADER: \\def\\verbatim@font{\\scriptsize\\ttfamily}
#+LATEX_HEADER: \\makeatother

#+LATEX_HEADER: \\usepackage{tikz}
#+LATEX_HEADER: \\usetikzlibrary{arrows.meta}
#+LATEX_HEADER: \\usetikzlibrary{positioning}

#+LATEX_HEADER: \\usepackage{tcolorbox}
#+LATEX_HEADER: \\tcbuselibrary{skins}

#+LATEX_HEADER: \\usepackage{minted}
#+LATEX_HEADER: \\usemintedstyle{monokai}

#+LATEX_HEADER: \\newenvironment{modern-quote}{\\begin{quote}}{\\end{quote}}
#+LATEX_HEADER: \\tcolorboxenvironment{modern-quote}{blanker,before skip=6pt,after skip=6pt, borderline west={3mm}{0pt}{black!40!white}}

#+LATEX_HEADER: \\hypersetup{colorlinks, allcolors=., urlcolor=blue!70!white}

"
  "Beamer presentation header template.
Format args: title, email, short-author, full-author, email, subtitle, theme")

;;;; Reveal.js Template:

(defconst hive-mcp-presentation--revealjs-header
  ":REVEAL_PROPERTIES:
#+REVEAL_ROOT: https://cdn.jsdelivr.net/npm/reveal.js
#+REVEAL_REVEAL_JS_VERSION: 4
#+REVEAL_EXTRA_CSS: ./css/%s.css
#+REVEAL_PLUGINS: (highlight notes)
#+REVEAL_HIGHLIGHT_CSS: https://cdn.jsdelivr.net/npm/reveal.js@4.2.0/plugin/highlight/monokai.css
#+OPTIONS: reveal_global_footer:t
#+OPTIONS: timestamp:nil toc:2 num:nil
:END:

#+title: %s
#+AUTHOR: %s
#+OPTIONS: toc:nil

"
  "Reveal.js presentation header template.
Format args: theme, title, author")

;;;; Slide Templates:

(defconst hive-mcp-presentation--beamer-slide
  "*** %s
%s
"
  "Beamer slide template (level 3 heading).")

(defconst hive-mcp-presentation--beamer-two-column
  "*** %s
**** %s
:PROPERTIES:
:BEAMER_COL: 0.48
:BEAMER_ACT: <1->
:BEAMER_ENV: block
:END:
%s

**** %s
:PROPERTIES:
:BEAMER_COL: 0.48
:BEAMER_ACT: <2->
:BEAMER_ENV: block
:END:
%s
"
  "Beamer two-column slide template.")

(defconst hive-mcp-presentation--revealjs-slide
  "** %s
%s
"
  "Reveal.js slide template (level 2 heading).")

(defconst hive-mcp-presentation--revealjs-iframe
  "** %s
:PROPERTIES:
:reveal_background_iframe: %s
:reveal_background: rgb(250,250,250)
:reveal_background_opacity: 0.90
:END:
"
  "Reveal.js slide with iframe background.")

;;;; Core Functions:

(defun hive-mcp-presentation--generate-header (format title &optional subtitle)
  "Generate presentation header for FORMAT with TITLE and optional SUBTITLE."
  (pcase format
    ('beamer
     (format hive-mcp-presentation--beamer-header
             title
             hive-mcp-presentation-email
             (car (split-string hive-mcp-presentation-author))
             hive-mcp-presentation-author
             hive-mcp-presentation-email
             (or subtitle "")
             hive-mcp-presentation-beamer-theme))
    ('revealjs
     (format hive-mcp-presentation--revealjs-header
             hive-mcp-presentation-reveal-theme
             title
             hive-mcp-presentation-author))
    (_ (error "Unknown format: %s" format))))

;;;; Interactive Commands:

;;;###autoload
(defun hive-mcp-presentation-create (title format filename)
  "Create a new presentation with TITLE in FORMAT, saving to FILENAME."
  (interactive
   (list (read-string "Presentation title: ")
         (intern (completing-read "Format: " '("beamer" "revealjs") nil t
                                  (symbol-name hive-mcp-presentation-default-format)))
         (read-file-name "Save as: " nil nil nil "presentation.org")))
  (let ((header (hive-mcp-presentation--generate-header format title)))
    (find-file filename)
    (insert header)
    (insert (format "* %s\n" title))
    (pcase format
      ('beamer (insert "** Introduction\n*** Welcome\nContent here...\n"))
      ('revealjs (insert "** Welcome\nContent here...\n")))
    (save-buffer)
    (message "Created %s presentation: %s" format filename)))

;;;###autoload
(defun hive-mcp-presentation-insert-slide (title &optional content)
  "Insert a new slide with TITLE and optional CONTENT."
  (interactive "sSlide title: ")
  (let ((format (hive-mcp-presentation--detect-format)))
    (end-of-line)
    (insert "\n")
    (insert (pcase format
              ('beamer (format hive-mcp-presentation--beamer-slide
                               title (or content "")))
              ('revealjs (format hive-mcp-presentation--revealjs-slide
                                 title (or content "")))
              (_ (format "** %s\n%s\n" title (or content "")))))))

;;;###autoload
(defun hive-mcp-presentation-insert-two-column (title left-title left-content right-title right-content)
  "Insert a two-column slide with TITLE.
LEFT-TITLE and LEFT-CONTENT for left column.
RIGHT-TITLE and RIGHT-CONTENT for right column."
  (interactive
   (list (read-string "Slide title: ")
         (read-string "Left column title: ")
         (read-string "Left content: ")
         (read-string "Right column title: ")
         (read-string "Right content: ")))
  (let ((format (hive-mcp-presentation--detect-format)))
    (end-of-line)
    (insert "\n")
    (pcase format
      ('beamer
       (insert (format hive-mcp-presentation--beamer-two-column
                       title left-title left-content right-title right-content)))
      ('revealjs
       (insert (format "** %s\n#+ATTR_REVEAL: :frag (appear)\n- %s: %s\n- %s: %s\n"
                       title left-title left-content right-title right-content)))
      (_ (insert (format "** %s\n- %s: %s\n- %s: %s\n"
                         title left-title left-content right-title right-content))))))

;;;###autoload
(defun hive-mcp-presentation-insert-code-slide (title language code)
  "Insert a code slide with TITLE showing CODE in LANGUAGE."
  (interactive
   (list (read-string "Slide title: ")
         (completing-read "Language: " '("clojure" "elisp" "python" "javascript"
                                         "go" "rust" "bash" "latex" "sql"))
         (read-string "Code (or leave empty to fill later): ")))
  (let ((format (hive-mcp-presentation--detect-format)))
    (end-of-line)
    (insert "\n")
    (pcase format
      ('beamer
       (insert (format "*** %s\n#+begin_src %s :results output :exports both :eval no\n%s\n#+end_src\n"
                       title language code)))
      ('revealjs
       (insert (format "** %s\n#+begin_src %s :eval no\n%s\n#+end_src\n"
                       title language code)))
      (_ (insert (format "** %s\n#+begin_src %s\n%s\n#+end_src\n"
                         title language code))))))

;;;###autoload
(defun hive-mcp-presentation-insert-image-slide (title image-path &optional caption)
  "Insert an image slide with TITLE showing IMAGE-PATH with optional CAPTION."
  (interactive
   (list (read-string "Slide title: ")
         (read-file-name "Image: ")
         (read-string "Caption (optional): ")))
  (let ((format (hive-mcp-presentation--detect-format))
        (rel-path (file-relative-name image-path)))
    (end-of-line)
    (insert "\n")
    (pcase format
      ('beamer
       (insert (format "*** %s\n#+ATTR_LATEX: :width 0.8\\textwidth\n[[file:%s]]\n%s\n"
                       title rel-path (if (string-empty-p caption) "" caption))))
      ('revealjs
       (insert (format "** %s\n#+ATTR_HTML: :width 600px\n[[file:%s]]\n%s\n"
                       title rel-path (if (string-empty-p caption) "" caption))))
      (_ (insert (format "** %s\n[[file:%s]]\n%s\n"
                         title rel-path (if (string-empty-p caption) "" caption)))))))

;;;###autoload
(defun hive-mcp-presentation-insert-quote-slide (title quote author)
  "Insert a quote slide with TITLE showing QUOTE by AUTHOR."
  (interactive
   (list (read-string "Slide title: ")
         (read-string "Quote: ")
         (read-string "Author: ")))
  (let ((format (hive-mcp-presentation--detect-format)))
    (end-of-line)
    (insert "\n")
    (pcase format
      ('beamer
       (insert (format "*** %s
\\begin{modern-quote}
%s
\\end{modern-quote}
#+LaTeX: \\begin{raggedleft}
\\textbf{--- %s}
#+LaTeX: \\par\\end{raggedleft}
" title quote author)))
      ('revealjs
       (insert (format "** %s\n#+begin_quote\n%s\n\n--- *%s*\n#+end_quote\n"
                       title quote author)))
      (_ (insert (format "** %s\n#+begin_quote\n%s\n--- %s\n#+end_quote\n"
                         title quote author))))))

;;;###autoload
(defun hive-mcp-presentation-insert-iframe-slide (title url)
  "Insert an iframe background slide with TITLE showing URL (Reveal.js only)."
  (interactive
   (list (read-string "Slide title: ")
         (read-string "URL: ")))
  (let ((format (hive-mcp-presentation--detect-format)))
    (end-of-line)
    (insert "\n")
    (pcase format
      ('revealjs
       (insert (format hive-mcp-presentation--revealjs-iframe title url)))
      (_
       (insert (format "** %s\nSee: %s\n" title url))
       (message "Note: iframe backgrounds are only supported in Reveal.js")))))

;;;; Export and Preview Functions:

;;;###autoload
(defun hive-mcp-presentation-export ()
  "Export the current presentation to its target format."
  (interactive)
  (let ((format (hive-mcp-presentation--detect-format)))
    (pcase format
      ('beamer (org-beamer-export-to-pdf))
      ('revealjs (org-reveal-export-to-html))
      (_ (org-export-dispatch)))))

;;;###autoload
(defun hive-mcp-presentation-preview ()
  "Preview the current presentation in PDF viewer or browser."
  (interactive)
  (let ((format (hive-mcp-presentation--detect-format)))
    (pcase format
      ('beamer
       (let ((pdf-file (concat (file-name-sans-extension buffer-file-name) ".pdf")))
         (if (file-exists-p pdf-file)
             (find-file-other-window pdf-file)
           (when (y-or-n-p "PDF not found. Export first?")
             (org-beamer-export-to-pdf)
             (find-file-other-window
              (concat (file-name-sans-extension buffer-file-name) ".pdf"))))))
      ('revealjs
       (let ((html-file (concat (file-name-sans-extension buffer-file-name) ".html")))
         (unless (file-exists-p html-file)
           (if (fboundp 'org-reveal-export-to-html)
               (org-reveal-export-to-html)
             (error "ox-reveal not installed. Run: M-x package-install RET ox-reveal")))
         (browse-url-of-file html-file)
         (message "Opened in browser: %s" html-file)))
      (_ (message "Unknown format, using org-export-dispatch")
         (org-export-dispatch)))))

;;;###autoload
(defun hive-mcp-presentation-export-and-preview ()
  "Export the presentation and immediately preview it.
For Beamer: exports to PDF and opens in PDF viewer.
For Reveal.js: exports to HTML and opens in browser."
  (interactive)
  (hive-mcp-presentation-export)
  (hive-mcp-presentation-preview))

;;;###autoload
(defun hive-mcp-presentation-refresh ()
  "Re-export and refresh the presentation preview.
Use after making changes to see updates immediately."
  (interactive)
  (save-buffer)
  (let ((format (hive-mcp-presentation--detect-format)))
    (pcase format
      ('beamer
       (org-beamer-export-to-pdf)
       (let ((pdf-file (concat (file-name-sans-extension buffer-file-name) ".pdf")))
         ;; Revert PDF buffer if already open
         (when-let* ((pdf-buf (get-file-buffer pdf-file)))
           (with-current-buffer pdf-buf
             (revert-buffer t t t)))
         (message "Beamer PDF refreshed: %s" pdf-file)))
      ('revealjs
       (if (fboundp 'org-reveal-export-to-html)
           (progn
             (org-reveal-export-to-html)
             (let ((html-file (concat (file-name-sans-extension buffer-file-name) ".html")))
               (browse-url-of-file html-file)
               (message "Reveal.js refreshed and opened: %s" html-file)))
         (error "ox-reveal not installed")))
      (_ (error "Unknown presentation format")))))

;;;###autoload
(defun hive-mcp-presentation-open-file (file)
  "Open presentation FILE, export it, and preview.
FILE should be an org-mode presentation file."
  (interactive "fPresentation file: ")
  (find-file file)
  (hive-mcp-presentation-export-and-preview))

;;;###autoload
(defun hive-mcp-presentation-open-html-in-browser (file)
  "Open exported HTML FILE directly in browser.
If FILE is an .org file, opens the corresponding .html file."
  (interactive "fFile: ")
  (let ((html-file (if (string-suffix-p ".html" file)
                       file
                     (concat (file-name-sans-extension file) ".html"))))
    (if (file-exists-p html-file)
        (progn
          (browse-url-of-file html-file)
          (message "Opened in browser: %s" html-file))
      (error "HTML file not found: %s. Export first?" html-file))))

;;;###autoload
(defun hive-mcp-presentation-open-current-html ()
  "Open the HTML version of current presentation in browser."
  (interactive)
  (hive-mcp-presentation-open-html-in-browser buffer-file-name))

;;;; Helper Functions:

(defun hive-mcp-presentation--detect-format ()
  "Detect presentation format from current buffer."
  (save-excursion
    (goto-char (point-min))
    (cond
     ((re-search-forward "^#\\+LATEX_CLASS:.*beamer" nil t) 'beamer)
     ((re-search-forward "^#\\+REVEAL_ROOT:" nil t) 'revealjs)
     (t nil))))

;;;; MCP Integration:

;;;###autoload
(defun hive-mcp-presentation-generate-outline (topic)
  "Generate a presentation outline for TOPIC using MCP memory context."
  (interactive "sPresentation topic: ")
  (when (hive-mcp-api-available-p)
    (let* ((conventions (hive-mcp-api-memory-query "convention" nil 5))
           (notes (hive-mcp-api-memory-query "note" nil 5))
           (context-str (format "Topic: %s\nRelevant conventions: %s\nRelevant notes: %s"
                                topic
                                (json-encode conventions)
                                (json-encode notes))))
      (hive-mcp-api-memory-add
       "note"
       (format "Generating presentation outline for: %s" topic)
       '("presentation" "outline"))
      (message "Context prepared. Use Claude to generate outline based on: %s" context-str)
      context-str)))

;;;###autoload
(defun hive-mcp-presentation-save-slide-to-memory ()
  "Save the current slide to MCP memory as a snippet."
  (interactive)
  (when (hive-mcp-api-available-p)
    (let* ((slide-content (hive-mcp-presentation--get-current-slide))
           (title (hive-mcp-presentation--get-slide-title)))
      (hive-mcp-api-memory-add
       "snippet"
       slide-content
       (list "presentation" "slide" (or title "untitled")))
      (message "Slide saved to memory: %s" (or title "untitled")))))

(defun hive-mcp-presentation--get-current-slide ()
  "Get the content of the current slide."
  (save-excursion
    (org-back-to-heading t)
    (let ((start (point)))
      (org-end-of-subtree t t)
      (buffer-substring-no-properties start (point)))))

(defun hive-mcp-presentation--get-slide-title ()
  "Get the title of the current slide."
  (save-excursion
    (org-back-to-heading t)
    (org-element-property :title (org-element-at-point))))

;;;; Transient Menu:

(transient-define-prefix hive-mcp-presentation-transient ()
  "Presentation creation menu."
  ["hive-mcp Presentations"
   ["Create"
    ("n" "New presentation" hive-mcp-presentation-create)
    ("s" "Insert slide" hive-mcp-presentation-insert-slide)
    ("2" "Two-column slide" hive-mcp-presentation-insert-two-column)]
   ["Content"
    ("c" "Code slide" hive-mcp-presentation-insert-code-slide)
    ("i" "Image slide" hive-mcp-presentation-insert-image-slide)
    ("q" "Quote slide" hive-mcp-presentation-insert-quote-slide)
    ("f" "Iframe slide (Reveal)" hive-mcp-presentation-insert-iframe-slide)]
   ["Export & Preview"
    ("e" "Export" hive-mcp-presentation-export)
    ("p" "Preview" hive-mcp-presentation-preview)
    ("r" "Refresh (export + preview)" hive-mcp-presentation-refresh)
    ("b" "Open HTML in browser" hive-mcp-presentation-open-current-html)
    ("x" "Export and preview" hive-mcp-presentation-export-and-preview)]
   ["MCP"
    ("o" "Generate outline" hive-mcp-presentation-generate-outline)
    ("m" "Save slide to memory" hive-mcp-presentation-save-slide-to-memory)]])

;;;; MCP API Functions (for Claude to call programmatically):

(defun hive-mcp-presentation-api-refresh (file)
  "API: Open FILE, export, and preview. Returns status message.
Use this from Claude to refresh a presentation after edits."
  (find-file file)
  (hive-mcp-presentation-refresh)
  (format "Refreshed presentation: %s" file))

(defun hive-mcp-presentation-api-export-html (file)
  "API: Export FILE to HTML and return the output path."
  (find-file file)
  (let ((format (hive-mcp-presentation--detect-format)))
    (pcase format
      ('revealjs
       (org-reveal-export-to-html)
       (concat (file-name-sans-extension file) ".html"))
      ('beamer
       (error "Use api-export-pdf for Beamer presentations"))
      (_ (error "Unknown format")))))

(defun hive-mcp-presentation-api-export-pdf (file)
  "API: Export FILE to PDF and return the output path."
  (find-file file)
  (let ((format (hive-mcp-presentation--detect-format)))
    (pcase format
      ('beamer
       (org-beamer-export-to-pdf)
       (concat (file-name-sans-extension file) ".pdf"))
      ('revealjs
       (error "Use api-export-html for Reveal.js presentations"))
      (_ (error "Unknown format")))))

(defun hive-mcp-presentation-api-open-browser (file)
  "API: Open the HTML version of FILE in browser."
  (let ((html-file (if (string-suffix-p ".html" file)
                       file
                     (concat (file-name-sans-extension file) ".html"))))
    (browse-url-of-file html-file)
    (format "Opened in browser: %s" html-file)))

(defun hive-mcp-presentation-api-get-info (file)
  "API: Get presentation info for FILE as alist."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((format (hive-mcp-presentation--detect-format)))
      `((file . ,file)
        (format . ,format)
        (html-exists . ,(file-exists-p (concat (file-name-sans-extension file) ".html")))
        (pdf-exists . ,(file-exists-p (concat (file-name-sans-extension file) ".pdf")))))))

;;;; Addon Lifecycle:

(defun hive-mcp-presentation--addon-init ()
  "Initialize the presentation addon."
  (require 'ox-beamer nil t)
  (require 'ox-reveal nil t)
  (message "hive-mcp-presentation: initialized"))

(defun hive-mcp-presentation--addon-shutdown ()
  "Shutdown the presentation addon."
  (message "hive-mcp-presentation: shutdown"))

;;;; Registration:

(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'presentation
   :version "0.1.0"
   :description "Org-mode presentation creation (Beamer & Reveal.js)"
   :requires '(hive-mcp-api org)
   :provides '(hive-mcp-presentation-transient)
   :init #'hive-mcp-presentation--addon-init
   :shutdown #'hive-mcp-presentation--addon-shutdown))

(provide 'hive-mcp-presentation)
;;; hive-mcp-presentation.el ends here
