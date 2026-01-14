;;; hive-mcp-memory.el --- Persistent memory for Claude MCP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT
;; This file is part of hive-mcp.

;;; Commentary:
;;
;; Compatibility shim for hive-mcp-memory.
;; The actual implementation has been decomposed into submodules in elisp/memory/.
;; This file loads the decomposed modules and provides backward compatibility.

;;; Code:

;; Add memory submodule directory to load path
(add-to-list 'load-path (expand-file-name "memory" (file-name-directory
                                                     (or load-file-name buffer-file-name))))

;; Load the decomposed facade (which loads all submodules)
(require 'hive-mcp-memory)

;; Re-provide to satisfy (require 'hive-mcp-memory) from this file path
(provide 'hive-mcp-memory)
;;; hive-mcp-memory.el ends here
