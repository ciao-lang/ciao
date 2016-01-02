;;; ciao-parsing.el --- Parsing of Ciao source and messages
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

;; ==========================================================================
;; This file contains several predicates to do (somethimes
;; approximate) parsing of Ciao source code and Ciao messages produced
;; by the Ciao inferior processes.
;;
;; Some parsing functions do navigate on the code and may modify the
;; cursor position.
;; ==========================================================================

(require 'ciao-aux) ; ciao-what-line, ciao-replace-regexp-in-string,
		    ; fix-cygwin-drive-letter,
		    ; match-string-no-properties
(require 'ciao-syntax) ; ciao-os-shell-prompt-pattern,
		       ; ciao-error-or-prompt-pattern
(require 'ciao-process) ; ciao-proc-any-prompt-pattern,
			; ciao-proc-prompt

;; -----------------------------------------------------------
;; Parsing of source code (approximate)
;; -----------------------------------------------------------

;; This is, as so many other things, an approximation...
(defun ciao-get-module-name ()
  "Extract the module name (of the current buffer)"
  (save-excursion 
    (goto-char (point-min))
    (let ((mbeg 0) (mend 0) (module-name nil))
      (setq module-name
	    (if (eq (search-forward-regexp 
		     "^[ \t]*:-[ \t\n]*\\(module\\|class\\)([ \t\n]*" 
		     20000 t) nil)
		"user"
	      (goto-char (match-end 0))
	      (setq mbeg (match-end 0))
	      (search-forward-regexp "[ \t\n]*\\(,\\|)\\)")
	      (setq mend (match-beginning 0))
	      (goto-char (match-beginning 0))
	      (buffer-substring-no-properties mbeg mend)))
      (if (eq (string-match "_" module-name) 0)
	  ;; if _ take the file name
	  (file-name-nondirectory 
	   (file-name-sans-extension
	    (buffer-file-name (current-buffer))))
	;; else, module-name, but eliminate quotes if they appear
	(ciao-replace-regexp-in-string 
	 "'[ \t\n]*$" "" 
	 (ciao-replace-regexp-in-string "^[ \t\n]*'" "" module-name)))
      )))

;; TODO: document
(defun ciao-module-name ()
  (let ((module-name (ciao-get-module-name)))
    (if (and (> (length module-name) 3)
             (or (string= (substring module-name -3) ".pl")
	         (string= (substring module-name -4) ".pls")
	         (string= (substring module-name -4) ".cgi")))
	;; The name has some Ciao-related extension, remove it
	(file-name-sans-extension module-name)
      module-name)))

;; TODO: use with-current-buffer, etc.?
;; TODO: implement a simpler 'module kind' operation
(defun ciao-get-module-kind ()
  "Determines the kind of module (of the current buffer) (user,
class, or normal module)."
  (save-excursion 
    (goto-char (point-min))
    (if (string= (ciao-get-module-name) "user")
	'user-module
      (goto-char (point-min))
      (if (eq (search-forward-regexp 
	       "^[ \t]*:-[ \t\n]*class([ \t\n]*" 10000 t) nil)
	  'normal-module
	'class-module))))

;; MH save-excursion
;; Must be improved. Cannot handle predicates with clauses
;; separated by newlines...
;; PO 890606
(defun predicate-boundaries ()
  ;; Find "beginning" of predicate
  (beginning-of-line)
  (save-excursion 
    (while (and (not (looking-at "\n")) (not (bobp)))
      (forward-line -1)
      (skip-chars-forward " \t"))
    (let ((start (point)))
	 ;; Find "end" of predicate
	 (forward-line 1)
	 (skip-chars-forward " \t")
	 (while (and (not (looking-at "\n")) (not (eobp)))
	   (forward-line 1)
	   (skip-chars-forward " \t"))
	 (cons start (point)))))

;; TODO: Why are those different?
(defun ciao-debug-predicate-boundaries (point)
  (let ((start) 
	(bound)
	(begin)
	(test t))
    ;; Find the beginning of the predicate boundary
    (save-excursion
      (search-backward-regexp "^[^ \t]" 1 t)
      (setq start (ciao-what-line)))
    ;; Find the end of the predicate boundary
    (save-excursion 
      ;; Search line to line to establish limits
      (setq test t)
      (setq begin (point))
      (end-of-line)
      (setq bound (point))
      (goto-char begin)
      (while test
	(while (not (search-forward-regexp "\\.[ \t]*\\(%\\|$\\)" bound t))
	  (forward-line 1)
	  (setq begin (point))
	  (end-of-line)
	  (setq bound (point))
	  (goto-char begin))
	;; We reach this point just when find the regexp. Are we in a
	;; comment?
	(if (not (search-backward "%" begin t))
	    (setq test nil)
	  (forward-line 1)
	  (setq begin (point))
	  (end-of-line)
	  (setq bound (point))
	  (goto-char begin)))	  
      (cons start (ciao-what-line)))))

;; -------------------------------------------------------------
;; Locating errors in inferior processes (just parsing)
;;
;; NOTE: Those make use of an error finding session, which has to be
;; conveniently created. Currently there is a single global session.
;; -------------------------------------------------------------

;; TODO: Associate a single 'last process buffer' for each source
;;   buffer and associate a single error session per each process
;;   buffer. In this way, checking errors with different processes
;;   (different toplevels, or a toplevel and a preprocessor, etc.)
;;   would not interfere. (JFMC)
;;
;; TODO: Use markers instead of line numbers? That would make error
;;   highlight more robust.

;; The error structure

(defun ciao-error-new (beginline endline filename infline)
  "Create a new error structure."
  (list beginline endline filename infline))

(defun ciao-error-get (err prop)
  "Get the property `prop' of error structure `err'."
  (cond
   ((eq prop 'ln0) (car err)) ;; begin line
   ((eq prop 'ln1) (car (cdr err))) ;; end line
   ((eq prop 'file) (car (cdr (cdr err)))) ;; source
   ((eq prop 'infln) (car (cdr (cdr (cdr err))))) ;; line of error at
						  ;; inferior buffer
   (t (error "Unknown property of error data %s" prop))))

(defun ciao-error-has-lines (err)
  "The error structure `err' contains valid line numbers."
  (> (ciao-error-get err 'ln0) 0))

;; State for the finding error session

(defvar ciao-error-session-orig-buffer nil
  "Non nil if we are in the middle of the process of finding errors.
   In that case it contains the original buffer to return to in
   the end.")
(defvar ciao-current-error nil
  "The current located error.")
(defvar ciao-error-session-marker nil
  "Marker for the current located error at the process buffer.")

;; TODO: Each compilation process could have its own session.
(defun ciao-error-session-active-p ()
  "True if the finding error session is active."
  (not (eq ciao-error-session-orig-buffer nil)))

(defun ciao-error-session-begin (procbuffer cproc)
  "Begin a finding errors session for errors in `procbuffer'"
  ;; Rmember the original buffer from which the session was started
  (setq ciao-error-session-orig-buffer (current-buffer))
  ;; In `procbuffer', go back to previous prompt or beginning of
  ;; buffer.
  (with-current-buffer procbuffer
    (save-excursion
      (goto-char (point-max))
      (move-to-column 0) ;; skip prompt if at prompt
      (search-backward-regexp (ciao-proc-any-prompt-pattern cproc) nil t)
      (end-of-line)
      ;; Move or create the session marker
      (if (not ciao-error-session-marker)
	  (setq ciao-error-session-marker (make-marker)))
      (set-marker ciao-error-session-marker (point) (current-buffer)))))

(defun ciao-error-session-end (procbuffer) ; procbuffer unused at this
					   ; moment
  "Finish a finding error session"
  (if ciao-error-session-marker
      (set-marker ciao-error-session-marker nil))
  (setq ciao-error-session-orig-buffer nil))

;;

;; TODO: Remove all marks and get the session as parameter (currently,
;;   this may be hard).
(defun ciao-reset-error-state ()
  t
  ;; (setq ciao-current-error nil)
  )

(defun ciao-any-errors (procbuffer cproc)
  "True if there were any errors in `procbuffer'."
  (with-current-buffer procbuffer
    (save-excursion
      ;; Go to process buffer
      (goto-char (point-max))
      (move-to-column 0) ;; skip prompt if at prompt
      ;; Go back to previous prompt or beginning of buffer
      (search-backward-regexp (ciao-proc-any-prompt-pattern cproc) nil t)
      (end-of-line)
      (not (ciao-no-more-errors cproc)))))

(defun ciao-no-more-errors (cproc)
  "No more errors, from this point onwards."
  (or (not (search-forward-regexp (ciao-error-or-prompt-pattern) nil t))
      (string=  (buffer-substring-no-properties 
		 (- (point) (length (ciao-proc-prompt cproc))) (point))
		(ciao-proc-prompt cproc))
      (eq (string-match ciao-os-shell-prompt-pattern
		    (buffer-substring-no-properties 
		     (match-beginning 0) (match-end 0))
		    ) 0)
      ))

(defun ciao-error-session-next (procbuffer cproc)
  "Locates next error, and highlights it. Returns:
     nil -- if no more errors
     '(beginline endline file) -- if an error found, where
        beginline/endline = location of error in process buffer
        file = source file containing error (if nil: no file was located)"
;;; ALT:
;;;         beginline/endline = can also contain predicate name / clause number
;;;             (this is a temporary kludge while preprocessor error
;;;              reporting is improved)
  (with-current-buffer procbuffer
    (save-excursion
      (if ciao-current-error
	  ;; Move to the previous error found in the inferior buffer
	  (let ((infline (ciao-error-get ciao-current-error 'infln)))
	    (goto-line infline))
	;; Or to the beginning of the errors
	(goto-char (marker-position ciao-error-session-marker)))
      ;; From 21.1 on , this does not go over the prompt. Using column instead:
      ;;  (beginning-of-line)
      ;;  (move-to-column 0)
      (end-of-line)
      (if (ciao-no-more-errors cproc)
	  ;; No (more) errors found, return null error structure
	  nil
	(let ((messpoint (point)) beginline endline openpoint filename infline)
	  (move-to-column 0)
	  (if (not (search-forward "lns " (+ (point) 80) t))
;;; MH OLD
	      ;; No line number info: -1 -1
	      (progn
		(setq beginline -1)
		(setq endline -1))
;;; MH ALT
;;;  	  (if (not (search-forward " at " (+ (point) 80) t))
;;; 	      ;; No line number info: -1 -1
;;; 	      (progn
;;; 		(setq beginline -1)
;;; 		(setq endline -1))
;;; 	    ;; locate by e.g. "at partition/4/3/1" 
;;; 	    ;; This is a kludge while messages from preprocessor improve
;;;  	    (let ((beg (point)) predicate clausenumber)
;;;  	      (search-forward "/")
;;;  	      (backward-char 1)
;;;  	      (setq predicate (buffer-substring-no-properties beg (point)))
;;;  	      (forward-char 1)
;;;  	      ;; ignore arity (approximation)
;;;  	      (search-forward "/")
;;;  	      (setq beg (point))
;;;  	      (search-forward "/")
;;;  	      (setq clausenumber
;;;  		    (string-to-number (buffer-substring-no-properties beg
;;;  								   (point))))
;;; 	      ;; MH ***
;;; 	      (message (append "ERROR DATA: " predicate " "
;;; 			       (int-to-string clausenumber )))
;;; 	      ;; This typically done elsewhere, but kludge to get line numbers
;;; 	      (save-excursion 
;;; 		(find-file-other-window filename)
;;; 		(search-forward-regexp (concat "^" beginline) nil t endline)
;;; 		(setq beginline (point))
;;; 		(search-forward-regexp (concat "\\(^" beginline "\\|^$\\)")  nil t)
;;; 		(setq endline (point)))
;;; 	      )
;;; 	    (progn 
;;; 	      (setq beginline -1)
;;; 	      (setq endline -1))
;;; 	    )
	    ;; Get line number info.
;;;	(search-forward "lns " (+ (point) 80) t)
	    (let ((beg (point)))
	      (search-forward "-")
	      (backward-char 1)
	      (setq beginline 
		    (string-to-number (buffer-substring-no-properties beg (point)))))
	    (forward-char 1)
	    (let ((beg (point)))
	      (search-forward ")")
	      (backward-char 1)
	      (setq endline 
		    (string-to-number (buffer-substring-no-properties beg (point)))))
	    )
	  ;; Beginning of ERROR/WARNING/... line
	  (move-to-column 0)
	  (setq infline (ciao-what-line))
	  
	  ;; Try to find opening "{" by inserting a "}"
	  (insert "}")
	  ;; Change syntax of parenthesis
	  (modify-syntax-entry ?( "_")
			       (modify-syntax-entry ?) "_")
	  (modify-syntax-entry ?[ "_")
			       (modify-syntax-entry ?] "_")
	  ;; Scan to "{"
	  (condition-case nil
	      (setq openpoint (scan-sexps (point) -1))
	    (error (setq openpoint 0)))
	  ;; Return syntax of parenthesis
	  (modify-syntax-entry ?( "()")
			       (modify-syntax-entry ?) ")(")
	  (modify-syntax-entry ?[ "(]")
			       (modify-syntax-entry ?] ")[")      
	  ;; Delete the "}" inserted
	  (delete-char -1)
	  (if (= openpoint 0)
	      (setq filename nil)
	    (goto-char openpoint)
	    (search-forward "/")
	    (backward-char 1)
	    (let ((beg (point)))
	      (search-forward-regexp 
	       "\\(\\.\\(po\\|itf\\|asr\\|ast\\|testout\\|pls\\|pl\\|cgi\\)\\>\\|$\\)")
	      (setq filename 
		    (fix-cygwin-drive-letter
		     (concat (buffer-substring-no-properties 
			      beg (match-beginning 0)) 
			     ;; MH cygdrive case for .pls, fixed bug
			     (cond
			      ((string= (match-string-no-properties 0) ".po") 
			       ".pl")
			      ((string= (match-string-no-properties 0) ".itf") 
			       ".pl")
			      ((string= (match-string-no-properties 0) ".asr") 
			       ".pl")
			      ((string= (match-string-no-properties 0) ".ast") 
			       ".pl")
			      ((string= (match-string-no-properties 0) ".testout") 
			       ".pl")
			      ((string= (match-string-no-properties 0) ".pls") 
			       ".pls")
			      ((string= (match-string-no-properties 0) ".pl") 
			       ".pl")
			      ((string= (match-string-no-properties 0) "cgi") ;; TODO: .cgi?
			       ".cgi")
			      ((string= (match-string-no-properties 0) "") 
			       "")
			      )))))
	    (goto-char messpoint)
	    ;; (beginning-of-line)
	    (move-to-column 0)
	    )
	  ;; Create the error structure
	  (ciao-error-new beginline endline filename infline))))))


;; Provide ourselves:

(provide 'ciao-parsing)

;;; ciao-parsing.el ends here

