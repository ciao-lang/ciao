;;; ciao-testing.el --- Interface for Ciao testing features
;; Copyright (C) 1986-2012 Free Software Foundation, Inc. and
;; M. Hermenegildo and others (herme@fi.upm.es, UPM-CLIP, Spain).

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'ciao-loading)
                        ; ciao-send-compiler-command
(require 'ciao-process) ; ciao-proc-get-buffer, ciao-proc-prompt,
			; ciao-send-command

;; ---------------------------------------------------------------------------
;; Default query (for debugging and testing)
;; ---------------------------------------------------------------------------

(defvar ciao-query ""
  "Query to use in Ciao. Setting this is useful when using the
  debugger or running tests if the query is long or complicated
  because it saves from having to type it (or search back for it)
  over and over again. Furthermore, it is possible to set things
  so that this query will be issued any time a program
  is (re)loaded.")

(defun ciao-get-query () ciao-query)

(defun ciao-set-query ()
  "Set a default query. This may be useful specially during debugging
or testing sessions. However, as mentioned elsewhere, note that
commands that repeat previous queries are also available. 

This query can be recalled at any time using \\<ciao-mode-map>
\\[ciao-load-query].  It is also possible to set things up so
that this query will be issued automatically any time a program
is (re)loaded. The functionality is available in the major
mode (i.e., from a buffer containing a source file) and in the
inferior mode (i.e., from the buffer running the top-level
shell). When called from the major mode (i.e., from window
containing a source file) then the user is prompted in the
minibuffer for the query. When called from the inferior
mode (i.e., from a top-level window) then the query on the
current line, following the Ciao prompt, is taken as the default
query.

To clear the default query use \\<ciao-mode-map> \\[ciao-clear-query]
or simply set it to an empty query: i.e., in a source buffer select
\\[ciao-set-query] and enter an empty query. In an inferior mode
simply select \\[ciao-set-query] on a line that contains only the
system prompt."

  (interactive)
  (let (beg query)
    (cond ((and (eq major-mode 'ciao-inferior-mode)
		(string= (current-buffer) (ciao-proc-get-buffer 'ciaosh-cproc)))
	   (setq ciao-query (ciao-get-last-query)))
	  ((eq major-mode 'ciao-mode)
	   (setq query (read-string "Set default query to: " ciao-query))
	   (setq ciao-query query)
	   )))
  (if (string= ciao-query "")
      (message "Default query cleared")
    (message (concat "Default query set to: '" ciao-query "'" ))))

(defun ciao-get-last-query ()
  "Obtain the last query issued in the current Ciao toplevel
buffer."
  (save-excursion
    ;; MH This approach does not work in 21.1
    ;; (beginning-of-line) 
    (if (not (search-backward-regexp
	      (ciao-proc-prompt-pattern 'ciaosh-cproc) nil t))
	"" ;; no query found
      (goto-char (match-end 0))
      (setq beg (point))
      (end-of-line)
      (buffer-substring-no-properties beg (point)))))

(defun ciao-clear-query ()
  "Clear the default query."
  (interactive)
  (setq ciao-query "")
  (message "Default query cleared"))

;;;###autoload
(defun ciao-load-query ()
  "Issue predefined query."
  (interactive)
  (ciao-send-compiler-command (ciao-get-query)))
;  (ciao-send-command 'ciaosh-cproc (ciao-get-query) t))

;; JF: Remove?
;;;###autoload
(defun ciao-load-query-ask ()
  "Issue predefined query (asking the user first)."
  (interactive)
  (if (y-or-n-p (concat "Do you wish call the query '" (ciao-get-query) "'? "))
      (ciao-send-command 'ciaosh-cproc ciao-query t)
    t))

;;------------------------------------------------------------
;; Testing modules
;;
;; NOTE: Testing involves loading programs and running them
;;   (automatically) on a selection of input queries.
;; ------------------------------------------------------------

(defvar ciao-testing-library-command "use_module(library(unittest))."
  "Command to load library that is needed in order to run tests
  from the top level.")

;; TODO: This should be local to the inferior process
(defvar ciao-testing-lib-loaded nil
  "Stores whether testing library has been loaded or not (see
ciao-send-testing-command-on-buffer).")

(defvar ciao-testing-command-passed nil
  "Stores the next command passed during testing.")

;;;###autoload
(defun ciao-run-tests-in-buffer ()
  "Run the tests in the current buffer.

The tests should be specified using test assertions in the module."
  (interactive)
  (setq ciao-testing-command-passed "run_tests_in_module")
  (ciao-send-testing-command-on-buffer))

;;;###autoload
(defun ciao-run-tests-in-buffer-check-exp-assrts ()
  "Run the tests in the current buffer and check the assertions of exported predicates.

The tests should be specified using test assertions in the module."
  (interactive)
  (setq ciao-testing-command-passed "run_tests_in_module_check_exp_assrts")
  (ciao-send-testing-command-on-buffer))

;;;###autoload
(defun ciao-run-tests-in-buffer-and-related ()
  "Run the tests in the current buffer and all related modules.

The tests should be specified using test assertions in those modules."
  (interactive)
  (setq ciao-testing-command-passed "run_tests_related_modules")
  (ciao-send-testing-command-on-buffer))
  
;;;###autoload
(defun ciao-show-untested-preds-buffer ()
  "Show any exported predicates that do not have test assertions.

This is an aid towards ensuring that all exported predicates have tests."
  (interactive)
  (setq ciao-testing-command-passed "show_untested_exp_preds")
  (ciao-send-testing-command-on-buffer))

;; (defun ciao-send-testing-command-on-buffer (predname) 
;;   (ciao-send-compiler-command
;;    (concat predname "('" (buffer-file-name) "')."))

(defun ciao-send-testing-command-on-buffer () 
  ;; TODO: do not use current-buffer here... (use orig-buffer at least?)
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (if (not (ciao-proc-alive 'ciaosh-cproc))
      ;; Ciao process is not alive
      (progn
	(ciao-start-inferior-process 'ciaosh-cproc)
	(ciao-show-inferior-process (ciao-proc-get-buffer 'ciaosh-cproc))
	(ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-load-testing-lib)
	(ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-do-send-testing-command))
    ;; Ciao process is alive
    (if ciao-assrt-lib-loaded ;; if lib loaded
	(ciao-do-check-buffer-syntax)
      ;; lib not loaded
      (ciao-load-testing-lib)
      (ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-do-send-testing-command))))

(defun ciao-load-testing-lib ()
  (ciao-send-command 'ciaosh-cproc ciao-testing-library-command t)
  (setq ciao-testing-lib-loaded t))

(defun ciao-do-send-testing-command ()
  (ciao-send-command 'ciaosh-cproc 
		     (concat ciao-testing-command-passed
			     "('"
			     (buffer-file-name ciao-last-source-buffer-used)
			     "').")
		     t)
  (ciao-toplevel-continuation-hooks))


;; Provide ourselves:

(provide 'ciao-testing)

;;; ciao-testing.el ends here

