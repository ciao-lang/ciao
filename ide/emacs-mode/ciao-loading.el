;;; ciao-loading.el --- Loading code in the Ciao toplevel
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

(require 'ciao-common) ; ciaoide (group)
(require 'ciao-scratchpad) ; ciao-scratchpad-last-code-file,
			   ; ciao-scratchpad-clean
(require 'ciao-process) ; ciao-start-inferior-process,
			; ciao-show-inferior-process,
			; ciao-last-process-buffer-used,
			; ciao-last-process-cproc,
			; ciao-proc-enqueue-w,
			; ciao-proc-alive, ciao-send-command,
			; ciao-proc-get-buffer
(require 'ciao-parsing) ; predicate-boundaries, ciao-get-module-kind
(require 'ciao-aux) ; ciao-color, ciao-uncolor,
		    ; ciao-replace-regexp-in-string,
		    ; ciao-switch-other-window,
		    ; ciao-switch-this-window, ciao-write-region

;;===========================================================================
;; Generic commands and functions for loading of code in a Ciao
;; toplevel. Many of those functions are shared by the CiaoPP and
;; LPdoc inferior modes.
;;
;; This also includes:
;;  - interactive location of errors (using ciao-parsing)
;;
;; TODO: Split again this file or make it more generic?
;;===========================================================================

;;------------------------------------------------------------
;; State of the loader
;;------------------------------------------------------------

(defvar ciao-last-source-buffer-used nil ; (of bufferp type)
  "Used to contain sometimes the last source buffer used (useful
for returning to it after processing).")

;; 'ignore' is because custom passes id of symbol
(defun ciao-do-set-library-path (ignore ciaolib) 
  (if (string= ciaolib "") 
      (progn
	(setenv "CIAOLIB" nil)
	(setq ciao-library-path ""))
    (setenv "CIAOLIB" ciaolib)
    (setq ciao-library-path ciaolib)))

;;------------------------------------------------------------
;; Library path for Ciao processes
;;------------------------------------------------------------

(defun ciao-initialize-library-path (ignorea ignoreb) 
  (ciao-do-set-library-path nil (or (getenv "CIAOLIB") "")))

(defcustom ciao-library-path ""
  "Path to the Ciao System libraries (reads/sets the CIAOLIB
environment variable ). Typically left empty, since ciao
executables know which library to use."
  :group 'ciaocore
  :type 'string
  :initialize 'ciao-initialize-library-path
  :set 'ciao-do-set-library-path
  )

(defun ciao-set-library-path () 
  "Change the location of the Ciao library paths (changes the
   environment variable @tt{CIAOLIB})."
  (interactive)
  (ciao-do-set-library-path nil
   (read-file-name "Change Ciao library path? " 
		   "" (getenv "CIAOLIB") nil (getenv "CIAOLIB"))))

;;------------------------------------------------------------
;; Main file
;;
;; TODO: Generalize for bundles (Manifest.pl)
;;------------------------------------------------------------

(defvar ciao-main-filename ""
  "Name of main file in a multiple module program. Setting this
is very useful when working on a multi-module program because it
allows issuing a load command after working on an inferior module
which will reload from the main module, thus also reloading
automatically all dependent modules.")

(defun ciao-set-main-filename ()
  "Set the current buffer as the principal file in a multiple module
programming environment."
  (interactive)
  (setq ciao-main-filename
	(read-file-name "Change Ciao main file? " 
			"" (buffer-file-name) t (buffer-file-name)))
  ;; TODO: Only ciaopp?
  (ciao-send-command 'ciaopp-cproc
		     (concat "set_prolog_flag(main_module,\'" ciao-main-filename "\').")
		     t))
;;   (setq ciao-main-filename 
;; 	(read-file-name "Change Ciao main module? " 
;; 			"" (ciao-get-module-name) nil (ciao-get-module-name))))

;;------------------------------------------------------------
;; Queries for the Ciao toplevel process, with parsing of errors from
;; the command output.
;; 
;; Note: these commands are useful for implementation of interactive
;; commands. This includes remembering the current buffer for later
;; management of windows.
;; ------------------------------------------------------------

(defcustom ciao-locate-errors-after-run t
  "If set, location of any errors produced when running Ciao
tools (loading or preprocessing code, running the documenter,
etc.) will be initiated automatically. I.e., after running a
command, the system will automatically highlight any error
messages and the corresponding areas in source files if
possible. If set to nil this location will only happen after
typing \\<ciao-mode-map> \\[ciao-find-last-run-errors] or
accessing the corresponding menu or tool bar button."
  :group 'ciaoide
  :type 'boolean)

(defun ciao-send-compiler-command (command)
  "Sends a command to the toplevel, instructing the inferior mode
  to parse errors and warnings from the output."
  ;; TODO: do not use current-buffer here... (use orig-buffer at least?)
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (ciao-send-command 'ciaosh-cproc command t)
  (ciao-toplevel-continuation-hooks))

(defun ciao-toplevel-continuation-hooks ()
  "Setting up hooks to run after a compilation-like command
execution on the toplevel."
  (if ciao-locate-errors-after-run
      (ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-launch-find-last-run-errors-from-orig-buffer)))

;;------------------------------------------------------------
;; Loading
;;------------------------------------------------------------

;; TODO: this should be local to the inferior buffer
(defvar ciao-objects-lib-loaded nil
  "Stores whether objects library has been loaded or not (see
ciao-load-command).")

;; TODO: use with-current-buffer, etc.?
(defun ciao-load-command (filename)
  "Determines the loading command necessary for `filename'"
  (save-excursion 
    (find-file filename)
    (let ((kind (ciao-get-module-kind)))
      (concat
       (if (eq kind 'user-module)
	   "ensure_loaded('"
	 (if (eq kind 'normal-module)
	     "use_module('"
	   ;; (eq kind 'class-module)
	   (concat
	    ;; Use objects package if necessary
	    ;; TODO: this is a kludge...
	    (if ciao-objects-lib-loaded
		""
	      (setq ciao-objects-lib-loaded t)
	      "use_package(objects).\n")
	    "use_class('"
	    )))
;; 	     (if (boundp 'xemacs-logo)
;; 		 (replace-in-string filename "\\\\" "\\\\" t)
;; 	       (replace-regexp-in-string "\\\\" "\\\\" filename t t))
       (ciao-replace-regexp-in-string "\\\\" "\\\\" filename t t)
       "')."))))

;;;###autoload
(defun ciao-load-buffer ()
  "Load the current buffer (and any auxiliary files it may use) into the
top level. 

The type of compilation performed (@index{compiling} or
@index{interpreting}) is selected automatically depending on whether the
buffer has been marked for debugging or not -- see below. In case you try
to load a file while in the middle of the debugging process the debugger is
first aborted and then the buffer is loaded. Also, if there is a defined
query, the user is asked whether it should be called."
  (interactive)
  (ciao-unmark-last-run-errors)
  (ciao-load-buffer-current-or-main nil))

;;;###autoload
(defun ciao-load-from-main-module ()
  "Load the module designated as @index{main module} (and all related files
that it uses) into the top level. If no main module is defined it will load
the current buffer. 

The type of compilation performed (@index{compiling} or
@index{interpreting}) is selected automatically depending on whether
the buffer has been marked for debugging or not -- see below. In case
you try to load a file while in the middle of the debugging process
the debugger is first aborted and then the buffer is loaded. Also, if
there is a defined query, the user is asked whether it should be 
called."
  (interactive)
  (ciao-unmark-last-run-errors)
  (let ((main-defined (string= ciao-main-filename "")))
    (ciao-load-buffer-current-or-main main-defined)))

(defun ciao-load-buffer-current-or-main (main)
  (setq ciao-last-source-buffer-used (current-buffer))
  (if (not (ciao-proc-alive 'ciaosh-cproc))
      ;; If Ciao buffer doesn't exist then go directly to load
      (ciao-real-load-buffer-current-or-main main)
    ;; Abort while debugging and then continue the normal process
    (let ((column
           (save-excursion
             (set-buffer (ciao-proc-get-buffer 'ciaosh-cproc))
             (goto-char (point-max))
             (current-column))))
      (if (< column 10)
	  (ciao-real-load-buffer-current-or-main main)
	(ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-enable-trace)
	(if main
	    (ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-real-load-from-main-module)
	  (ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-real-load-buffer))
	(ciao-send-command 'ciaosh-cproc "a" t)))))

(defun ciao-real-load-buffer ()
  "This function really loads the buffer. And in case a default query has been
defined it asks the user if this query should be called."
  (interactive) ;; JF: really interactive?
  (ciao-real-load-buffer-current-or-main nil))

(defun ciao-real-load-from-main-module ()
  (interactive) ;; JF: really interactive?
  (ciao-real-load-buffer-current-or-main t))

(defun ciao-real-load-buffer-current-or-main (main)
  ;; SEE ABOVE
  ;; (ciao-unmark-errors ciao-last-process-buffer-used)
  (let ((filename (if main ciao-main-filename (buffer-file-name))))
    (ciao-send-command 'ciaosh-cproc
		       (ciao-load-command filename)
		       t))
  (ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-errors-or-load-query))

(autoload 'ciao-get-query "ciao-testing") ;; TODO: this should be a hook

(defun ciao-errors-or-load-query ()
  (if ciao-locate-errors-after-run
      (ciao-launch-find-last-run-errors-from-orig-buffer))
  (if (or (string= (ciao-get-query) "")
	  (ciao-last-run-any-errors))
      t
    (ciao-load-query)))

;;---------------------------------------------------------------------------

;;;###autoload
(defun ciao-load-region (start end)
  "Load the current region (between the cursor and a previous mark)
into the top level. Since loading a region of a file is typically done
for debugging and/or testing purposes, this command always loads the
region in debugging mode (interpreted)." 
  (interactive "r")
  (ciao-write-region start end (ciao-scratchpad-last-code-file))
  (ciao-send-command 'ciaosh-cproc 
		     (concat "debug_module(user), "
			     "ensure_loaded('" ciao-scratchpad-last-file "')." )
		     t)
  (ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-scratchpad-clean))

;;;###autoload
(defun ciao-load-predicate ()
  "Load the predicate around the cursor into the top level. Since loading a 
single predicate is typically done for debugging and/or testing purposes,
this command always loads the predicate in debugging mode (interpreted)."
  (interactive)
  (let ((boundaries (predicate-boundaries)))
    (ciao-load-region (car boundaries) (cdr boundaries))))

;;------------------------------------------------------------
;; Traditional commands: Consulting
;;------------------------------------------------------------
;; These and the following commands reuse the same temp file, which is
;; left at /tmp in the end. This eliminates the  need for
;; synchronization with the Ciao process, which is complicated by
;; the SICStus "The procedure xxx/yyy is being redefined" messages
;; (but unfortunately leaves  garbage behind, in the same way as the
;; ususal prolog.el mode).

;;;###autoload
(defun ciao-consult-buffer ()
  "Consult the entire buffer."
  (interactive)
  (ciao-send-command 'ciaosh-cproc
		     (concat "consult('" (buffer-file-name) "').")
		     t))

;;;###autoload
(defun ciao-consult-region (start end)
  "Consult a given region."
   (interactive "r")
  (ciao-write-region start end (ciao-scratchpad-last-code-file))
  (ciao-send-command 'ciaosh-cproc 
		     (concat "consult('" ciao-scratchpad-last-file "')." )
		     t)
  (ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-scratchpad-clean))

;;;###autoload
(defun ciao-consult-predicate ()
  "Consult the predicate around point."
  (interactive)
  (let ((boundaries (predicate-boundaries)))
    (ciao-consult-region (car boundaries) (cdr boundaries))))

;;---------------------------------------------------------------------------

;; TODO: see ciao-find-errors-or-show-output
(defun ciao-find-errors-or-load-buffer ()
  (ciao-switch-this-window ciao-last-source-buffer-used)
  (if (and ciao-locate-errors-after-run
	   (ciao-last-run-any-errors))
      (ciao-find-last-run-errors)
    ;; No errors found (or locating errors disabled)
    (ciao-load-buffer)
    ;; In this case, probably best to stay in original buffer
    (ciao-switch-other-window ciao-last-source-buffer-used)))

(defun ciao-launch-find-last-run-errors-from-orig-buffer ()
  (ciao-switch-this-window ciao-last-source-buffer-used)
  (ciao-find-last-run-errors))

;;------------------------------------------------------------
;; Assertions and syntax cheking
;;------------------------------------------------------------

;; TODO: This should be local to the inferior process
(defvar ciao-assrt-lib-loaded nil
  "Stores whether assertion library has been loaded or not (see
ciao-check-buffer-syntax).")

;; TODO: ciao-assrt-lib-loaded must be associated to the process
;; TODO: Simplify this code with a process queue

(defun ciao-check-buffer-syntax ()

  "Check the @em{syntax} of the code and assertions in the current
buffer, as well as imports and exports.  This uses the standard top
level (i.e., does not call the preprocessor and thus does not require
the preprocessor to be installed). Note that full (semantic) assertion
checking must be done with the preprocessor."

  (interactive)
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (if (not (ciao-proc-alive 'ciaosh-cproc))
      ;; Ciao process is not alive
      (progn
	(ciao-start-inferior-process 'ciaosh-cproc)
	(ciao-show-inferior-process (ciao-proc-get-buffer 'ciaosh-cproc))
	(ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-load-assrt-lib)
	(ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-do-check-buffer-syntax))
    ;; Ciao process is alive
    (if ciao-assrt-lib-loaded ;; if lib loaded
	(ciao-do-check-buffer-syntax)
      ;; lib not loaded
      (ciao-load-assrt-lib)
      (ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-do-check-buffer-syntax))))

(defun ciao-load-assrt-lib ()
  (ciao-send-command 'ciaosh-cproc 
		     "use_module(library(assertions/assrt_lib))."
		     t)
  (setq ciao-assrt-lib-loaded t))

(defun ciao-do-check-buffer-syntax ()
  (ciao-send-command 'ciaosh-cproc 
		     (concat "prolog_flag(verbose_compilation,_Old,off),"
			     "check_code_and_assrt_syntax('"
			     (buffer-file-name ciao-last-source-buffer-used)
			     "'),"
			     "prolog_flag(verbose_compilation,_,_Old).")
		     t)
  (ciao-toplevel-continuation-hooks))

;;===========================================================================

;;------------------------------------------------------------
;; Locating errors on the last process buffer (that is,
;; `ciao-last-process-buffer-used')
;;------------------------------------------------------------

(require 'ciao-parsing) ; ciao-any-errors, ciao-current-error,
			; ciao-error-session-orig-buffer,
			; ciao-error-session-active-p,
			; ciao-error-session-begin,
			; ciao-error-session-end,
			; ciao-error-session-next

(defun ciao-last-run-any-errors ()
  "True if there were any errors in the previous run."
  (let ((procbuffer ciao-last-process-buffer-used))
    (if (and procbuffer (buffer-live-p procbuffer))
	;; buffer still exists
	(ciao-any-errors procbuffer ciao-last-process-cproc))))

(defun ciao-find-last-run-errors ()
  "Go to the location in the source file containing the next
error reported by the last Ciao subprocess (preprocessor or
toplevel) which was run."
  (interactive)
  (let ((procbuffer ciao-last-process-buffer-used))
    (if (and procbuffer (buffer-live-p procbuffer))
	;; buffer still exists
	(ciao-do-find-errors procbuffer ciao-last-process-cproc)
      (message "No recent program processing active."))))

(defun ciao-do-find-errors (procbuffer cproc)
  "Go to the location in the source file containing the next
error reported `procbuffer'."
  (if (not (ciao-error-session-active-p))
      ;; Begin new session
      (ciao-error-session-begin procbuffer cproc)
      ;; Only before we start a new finding errors session, delete
      ;; other windows to maximize frame space.
      (if (not (eq procbuffer (current-buffer)))
	  (delete-other-windows)))
  (ciao-switch-other-window procbuffer)
  (ciao-find-and-highlight-error procbuffer cproc))

;; TODO: Circular dependency! Use hooks to solve it.
(autoload 'ciao-debug-remove-marks "ciao-debugger")

(defun ciao-unmark-last-run-errors ()
  "Remove error marks from last run (and also debugging marks if
present). This finish the error finding session."
  (interactive)
  (let ((procbuffer ciao-last-process-buffer-used))
    (if (and procbuffer (buffer-live-p procbuffer))
	;; buffer still exists
	(progn
	  (ciao-unmark-errors procbuffer)
	  (ciao-error-session-end procbuffer)
	  ;; This returns nil if not debugging, so it does not hurt and
	  ;; is handy
	  (ciao-debug-remove-marks))
      (message "No recent program processing active."))))

(defun ciao-unmark-errors (procbuffer)
  "Remove error marks from `procbuffer' (and its companion
source buffer)."
  (if (not ciao-current-error)
      t ; Do nothing
    (ciao-unmark-error procbuffer ciao-current-error)
    (setq ciao-current-error nil)))

(defun ciao-find-and-highlight-error (procbuffer cproc)
  "Go to location in source file containing next error, highlight."
  ;; First, remove error previous marks in the source and process
  ;; buffers. No need to do anything if file is not being visited
  ;; any more.
  (if ciao-current-error
      (ciao-unmark-error procbuffer ciao-current-error))
  (let (err beginline endline filename infline)
    ;; In process buffer, get next error data
    (setq err (ciao-error-session-next procbuffer cproc))
    (setq ciao-current-error err)
    (if (eq err nil)
	;; There are no (more) errors
	(progn
	  (ciao-finish-find-error procbuffer)
	  (message "There were no (more) errors."))
      ;; Error located
      (ciao-goto-error procbuffer err) ; go to the error in the buffers
      (ciao-mark-error procbuffer err) ; mark the error in the buffers
      )))

(defun ciao-finish-find-error (procbuffer)
  "Finish error finding. Return to the original buffer."
  (set-buffer procbuffer)
  (goto-char (point-max)) ;; goto end of process buffer
  ;; Return to original buffer if not already there
  (ciao-switch-other-window ciao-error-session-orig-buffer)
  ;; MH Put this back in to return to single original window 
  ;; *** (delete-other-windows) ***
  (setq ciao-error-session-orig-buffer nil))

;;------------------------------------------------------------
;; Moving to errors in source and inferior process
;;------------------------------------------------------------

(require 'ciao-parsing) ; ciao-error-get, ciao-error-has-lines

(defun ciao-goto-error (procbuffer err)
  "Move the cursor in the source and inferior buffers to point to
the error `err'. It opens the source file in a new buffer if it
is not loaded."
  ;; Go to the current error in the inferior buffer
  (let ((infline (ciao-error-get err 'infln)))
    (if infline ;; TODO: This is never nil
	(progn
	  (set-buffer procbuffer)
	  ;; Move the cursor and recenter the buffer
	  (goto-line infline)
	  (recenter 1))))
  ;; Go to the current error in the source buffer (if known)
  ;; Keep that window selected.
  (let ((beginline (ciao-error-get err 'ln0))
	(endline (ciao-error-get err 'ln1))
	(filename (ciao-error-get err 'file))
	(error-buffer nil))
    ;; First, determine the error buffer from `filename'
    (if (eq filename nil)
	(message "No corresponding file could be determined.")
      (if (not (file-exists-p filename))
	  ;; Do not exist!
	  (message "Corresponding file %s does not exist." filename)
	;; Exists, open it in a new buffer
	(setq error-buffer (find-file-noselect filename))))
    ;; Then, go to error in error buffer
    (if (eq error-buffer nil)
	() ; No error buffer for some of the reason above, do nothing
      (ciao-switch-other-window error-buffer)
      (if (not (ciao-error-has-lines err))
	  ;; No line numbers: just visit file
	  (progn 
	    (goto-char (point-min))
	    (message "Error within this file."))
	;; Go to the right point in opened file...
	(push-mark (point) t)
	(goto-line beginline)
	(recenter 0)
	(goto-line (+ endline 1))
	(backward-char 1)
	t ;; (message "Mark set")
	))))

;;------------------------------------------------------------
;; Marking errors in source and inferior process
;;------------------------------------------------------------

(defun ciao-mark-error (procbuffer err)
  "Set the error mark for the error `err' found at
`procbuffer'."
  (ciao-p-mark-error procbuffer err ciao-face-highlight-code))

(defun ciao-unmark-error (procbuffer err)
  "Remove error mark for the error `err' found at
`procbuffer'."
  (ciao-p-mark-error procbuffer err nil))

(defun ciao-p-mark-error (procbuffer err face-or-uncolor)
  "Mark or unmark the error with `face-or-uncolor' (see
`ciao-p-color')."
  ;; Color/uncolor mark in the source buffer
  (let ((error-buffer (ciao-error-get-buffer err)))
    (if (not error-buffer)
	() ; buffer does not exist any more
      (with-current-buffer error-buffer
	(if (ciao-error-has-lines err)
	    (ciao-p-color (ciao-error-get err 'ln0)
			  (ciao-error-get err 'ln1)
			  face-or-uncolor
			  'ciao-error)))))
  ;; Color/uncolor mark in the inferior buffer
  (let ((infline (ciao-error-get err 'infln)))
    (if infline ;; TODO: In principle, this is never nil
	(if (buffer-live-p procbuffer) ;; else already deleted
	    (with-current-buffer procbuffer
	      (ciao-p-color infline
			    infline
			    face-or-uncolor
			    'ciao-error))))))

(defun ciao-p-color (begin end face-or-uncolor over)
  "Behaves like `ciao-color' if `face-or-uncolor' is not nil, or
like `ciao-uncolor' if it is nil."
  (if (eq face-or-uncolor nil)
      (ciao-uncolor begin end over)
    (ciao-color begin end face-or-uncolor over)))

;; TODO: Open the file if it is not opened?
(defun ciao-error-get-buffer (err)
  "Get the buffer for the source file to which `err'
points. Returns nil if it is not opened in any buffer."
  (let ((filename (ciao-error-get err 'file)))
    (if (eq filename nil)
	nil ; nil if file could not be determined
      (get-file-buffer filename))))

;;===========================================================================

;; M.H. 
;; In distributed execution, need to halt siblings...
;; (setq kill-buffer-hook 'ciao-halt-process)

;;;###autoload
(defun ciao-halt-process ()
  (if (not (ciao-proc-alive 'ciaosh-cproc))
      ;; Ciao process not alive
      ()
    (progn 
      (process-send-string (ciao-proc-name 'ciaosh-cproc) "halt.")
      (sleep-for 2))
    ))

;;===========================================================================

(defun ciao-reset-loading-state ()
  (setq ciao-objects-lib-loaded nil)
  (setq ciao-assrt-lib-loaded nil)
  (setq ciao-testing-lib-loaded nil))

;;===========================================================================

(require 'ciao-process) ; ciao-ensure-inferior-process,
			; ciao-proc-enqueue-w,
			; ciao-proc-get-buffer

;;;###autoload
(defun ciao ()
  "Like \\<ciao-mode-map> \\[run-ciao-toplevel], but starts with a
single window."
  (interactive)
  (run-ciao-toplevel))

;;;###autoload
(defun prolog ()
  "Start up Ciao."
  (interactive)
  (ciao))

;;;###autoload
(defun run-ciao-toplevel ()

  "Ensure that an inferior Ciao top-level process is running. 

   This opens a top-level window (if one did not exist already)
where queries can be input directly. Programs can be loaded into
this top level by typing the corresponding commands in this
window (such as use_module, etc.), or, more typically, by opening
the file to be loaded in an emacs window (where it can be edited)
and issuing a load command (such as \\<ciao-mode-map>
\\[ciao-load-buffer] or \\[ciao-load-from-main-module]) directly
from there (see the loading commands of this mode and their
bindings).

   Note that many useful commands (e.g., to repeat and edit previous
commands, interrupt jobs, locate errors, automatic completions, etc.)
are available in this top-level window (see @ref{Commands available in
toplevel and preprocessor buffers}).

   Often, it is not necessary to use this function since
execution of any of the other functions related to the top
level (e.g., loading buffers into the top level) ensures that a
top level is started (starting one if required)."

  (interactive)
  (ciao-ensure-inferior-process 'ciaosh-cproc)
  (ciao-switch-this-window (ciao-proc-get-buffer 'ciaosh-cproc)))

;;------------------------------------------------------------
;; Recenter last process buffer
;;------------------------------------------------------------

(defun ciao-recenter-last-ciao-buffer () 
  "Recenter the most recently used Ciao inferior process
buffer (e.g., top level, preprocessor, etc.)."
  (interactive)
  (let ((procbuffer ciao-last-process-buffer-used))
    (if (and procbuffer (buffer-live-p procbuffer))
	;; buffer still exists
	(ciao-switch-this-window procbuffer)
      (message "No recent program processing active."))))


;; Provide ourselves:

(provide 'ciao-loading)

;;; ciao-loading.el ends here

