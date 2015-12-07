;;; ciao-syntax.el --- Syntax definitions for Ciao language
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

(require 'ciao-config) ; ciao-get-config
(require 'ciao-common) ; ciaoide (group)

(defgroup ciaolang nil
  "The Ciao language and syntax."
  :tag "Ciao Language"
  :group 'ciao)

;;------------------------------------------------------------
;; Syntax and movement
;;------------------------------------------------------------

(defvar ciao-mode-syntax-table nil)
(if ciao-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))

    (modify-syntax-entry ?_ "w" table) ; word constituent
    (modify-syntax-entry ?\\ "." table) ; punctuation
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
;;  1 means CHAR is the start of a two-char comment start sequence.
;;  2 means CHAR is the second character of such a sequence.
;;  3 means CHAR is the start of a two-char comment end sequence.
;;  4 means CHAR is the second character of such a sequence.
    (modify-syntax-entry ?/ "." table) ; punctuation
    (modify-syntax-entry ?* "." table) ; punctuation
    (modify-syntax-entry ?+ "." table) ; punctuation
    (modify-syntax-entry ?- "." table) ; punctuation
    (modify-syntax-entry ?= "." table) ; punctuation
    (modify-syntax-entry ?% "<" table) ; comment starter
    (modify-syntax-entry ?\n ">" table); comment ender
    (modify-syntax-entry ?\^m ">" table); ; comment ender
    (modify-syntax-entry ?< "." table) ; punctuation
    (modify-syntax-entry ?> "." table) ; punctuation
    (modify-syntax-entry ?\' "\"" table) ; escape
    (setq ciao-mode-syntax-table table)))

(defvar ciao-mode-abbrev-table nil)
(define-abbrev-table 'ciao-mode-abbrev-table ())

(defun ciao-syntax-mode-variables ()
  (setq case-fold-search nil)
  (set-syntax-table ciao-mode-syntax-table)
  ;;
  (setq local-abbrev-table ciao-mode-abbrev-table)
  (set (make-local-variable 'paragraph-start) (concat "^%%\\|^$\\|" page-delimiter)) ;'%%..'
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'indent-line-function) 'ciao-indent-line)
  (set (make-local-variable 'comment-start) "%")
  (set (make-local-variable 'comment-start-skip) "%+ *")
  (set (make-local-variable 'comment-column) 48)
;; Obsolete since before 19.5
;;   (set (make-local-variable 'comment-indent-hook) 'ciao-comment-indent)
  (set (make-local-variable 'comment-indent-function) 'ciao-comment-indent)
  )

;;------------------------------------------------------------
;; Indentation
;;------------------------------------------------------------

(defcustom ciao-first-indent-width 8 ;; some people prefer 4
  "First level indentation for a new goal."
  :group 'ciaolang
  :type 'integer) ;; it was 'tab-width' before

(defcustom ciao-indent-width 4
  "Indentation for a new goal."
  :group 'ciaolang
  :type 'integer)

(defun ciao-indent-line (&optional whole-exp)
  "Indent current line as Ciao code.
With argument, indent any additional lines of the same clause
rigidly along with this one."
  (interactive "p")
  (let ((indent (ciao-indent-level))
	(pos (- (point-max) (point))) beg)
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")

    (if (zerop (- indent (current-column)))
	nil
      (delete-region beg (point))
      (indent-to indent))

    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))))

;; JA 890605
(defun ciao-indent-level ()
  "Compute Ciao indentation level."
  (save-excursion
    (beginning-of-line)
      (skip-chars-forward " \t")
      (cond
       ((bobp) 0)                            ;Beginning of buffer
       ((looking-at "\n")                    ;a new fresh line
        (ciao-indent-for-new-clause))
       (t                                    ;indent existing clause
        (forward-line -1)
	(ciao-indent-for-new-clause)))))

;; JA 890601
(defun ciao-search-for-prev-goal ()
  "Search for the most recent Ciao symbol (in head or in body)."
  (while (and (not (bobp)) (or (looking-at "%") (looking-at "\n")))
    (forward-line -1)
    (skip-chars-forward " \t")))

;; JA 890601
(defun ciao-indent-for-new-clause ()
  "Find column for a new goal."
  (skip-chars-forward " \t")
  (ciao-search-for-prev-goal)
  (let ((prevcol (current-column)))
    (ciao-end-of-clause)
    (forward-char -1)
    (cond ((bobp) 0)
	  ((looking-at "[.]") 0)
	  ((zerop prevcol) ciao-first-indent-width)
	  ((looking-at "[\[{(;]")
	   (max ciao-first-indent-width (+ ciao-indent-width (ciao-column-of-um-lparen))))
	  ((looking-at "[,>]") (ciao-column-of-prev-term))
	  (t (ciao-column-of-um-lparen)))))

;; JA 890601
(defun ciao-column-of-prev-term ()
  (beginning-of-line)
  (skip-chars-forward " \t\[{(;")
  (current-column))

;; JA 890601
(defun ciao-column-of-um-lparen ()
  (let ((pbal 0))
    (while (and (>= pbal 0)
		(or (> (current-column) 0)
		    (looking-at "[ \t]")))
      (cond ((looking-at "[\]})]")
	     (setq pbal (1+ pbal))
	     (forward-char -1))
	    ((looking-at "[\[{(]")
	     (setq pbal (1- pbal))
	     (forward-char -1))
	    ((looking-at "'")
	     (search-backward "'" nil t)
	     (forward-char -1))
	    ((looking-at "\"")
	     (search-backward "\"" nil t)
	     (forward-char -1))
	    (t (forward-char -1)))))
  (forward-char 1)  ;; Reset buffer pointer to prev column
  (current-column))

(defun ciao-end-of-clause ()
  "Go to end of clause in this line."
  (beginning-of-line)
  (let* ((eolpos (save-excursion (end-of-line) (point))))
    (if (re-search-forward comment-start-skip eolpos 'move)
	(goto-char (match-beginning 0)))
    (skip-chars-backward " \t")))

;; (defun ciao-comment-indent ()
;;   "Compute Ciao comment indentation."
;;   (ciao-indent-level))
(defun ciao-comment-indent ()
  "Compute Ciao comment indentation."
  (cond ((looking-at "%%%") 0)
	((looking-at "%%") (ciao-indent-level))
	(t
	 (save-excursion
	       (skip-chars-backward " \t")
	       (max (1+ (current-column)) ;Insert one space at least
		    comment-column)))))

;; ---------------------------------------------------------------------------

;; TODO: Use with care, this is experimental.
(defun ciao-indent-file ()
  "Indent a Ciao or Prolog file using `plindent'."
  (interactive)
  (setq tmp_file_1 (concat (buffer-file-name) ".1.tmp"))
  (setq current-point (point))
  (setq current-start (window-start))
  (setq source-buffer (current-buffer))
  (with-temp-file tmp_file_1
    (insert-buffer source-buffer))
  (shell-command (concat (ciao-get-config :plindent-bin) " " tmp_file_1 " -") source-buffer)
  (delete-file tmp_file_1)
  (set-window-start (selected-window) current-start)
  (goto-char current-point)
  )

;;------------------------------------------------------------
;; Some aid for inserting program elements (very limited for now)
;;------------------------------------------------------------

;; TODO: It does not seem the right place for this.

(defun ciao-insert-script-header ()

  "Insert a (Unix) header at the top of the current buffer so that the
Ciao script interpreter will be called on this file if @em{run} from
the command line. It also makes the file ``executable'' (e.g.,
'@tt{chmod +x <file>}' in Unix). See @ref{The script interpreter} for
details."

  (interactive)
  (goto-char (point-min))
  (insert 
   (concat "#!/usr/bin/env ciao-shell\n"
           "% -*- mode: ciao; -*-\n"
	   "\n"))
  (set-file-modes (buffer-file-name) 448))

;;------------------------------------------------------------
;; Syntactic definitions for inferior modes
;;------------------------------------------------------------

;; TODO: Add pattern for 'bash'. Better idea: change the prompt of
;;   external shell used for LPdoc.
;; (defcustom ciao-os-shell-prompt-pattern "\\(\\[[0-9]+\\]> \\|.*\\$ \\|.*> \\|[A-Z]:.*>\\)"
;; Refined inclusion of current Ubuntu default. Basically, 
;; eliminated spaces in (...>) prompt. May still need work.
;; This is a side effect of using this pattern also when looking for
;;   errors in ciao buffers because LPdoc still uses an OS shell
;;   instead of a Ciao top level: this is what needs to be fixed
;;   really.  --MH
(defcustom ciao-os-shell-prompt-pattern "\\(\\[[0-9]+\\]> \\|.*\\$ \\|[^ ]*> \\|[A-Z]:.*>\\)"
  "Regular expression used to describe typical shell prompt
patterns (csh and bash), so that error location works in inferior
shells. This is useful for example so that errors are located
when generating documentation, and also when using the embedded
debugger or any other application in a shell. It is best to be as
precise as possible when defining this so that the standard Ciao
error location does not get confused."
  :group 'ciaolang
  :type 'string)

;; Prompt patterns -- left some out of custom because you need to be
;;                    really careful when changing these...

;; TODO: allow generic names in prompts?
(defun ciao-any-prompt-pattern ()
  "Matching any Ciao or other inferior process prompt"
  (concat
   "\\("
   "^\\(\\(\\|[0-9]+ \\|ciaopp \\|lpdoc \\|| \\)\\?-\\)" 
   "\\|" 
   ciao-os-shell-prompt-pattern 
   "\\)"))
;; "\\(^|* *\\?- *\\)\\|\\(^ciaopp \\?- *\\)") Old Ciao/SICStus prompt patterns

(defcustom ciao-locate-also-note-messages nil
  "If set, also when errors of type NOTE are detected the
corresponding file is visited and the location marked. It is set to
nil by default because sometimes the user prefers not to take any
action with respect to these messages (for example, many come from the
documenter, indicating that adding certain declarations the
documentation would be improved)."
  :group 'ciaoide
  :type 'boolean)

(defun ciao-error-or-prompt-pattern ()
  (concat 
  "\\("				      
  (if ciao-locate-also-note-messages
      "^\\({?WARNING.*:\\|{?ERROR.*:\\|{?NOTE.*:\\)"
    "^\\({?WARNING.*:\\|{?ERROR.*:\\)")
  "\\|"
  (ciao-any-prompt-pattern)
  "\\)"))


;; Provide ourselves:

(provide 'ciao-syntax)

;;; ciao-syntax.el ends here

