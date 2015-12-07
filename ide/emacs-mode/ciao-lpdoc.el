;;; ciao-lpdoc.el --- LPdoc Interface

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

(require 'comint) ; comint-output-filter

(require 'ciao-config) ; ciao-get-config
(require 'ciao-aux) ; ciao-completion-choice,
		    ; ciao-completing-read
(require 'ciao-scratchpad) ; ciao-scratchpad-source-assoc-dir
(require 'ciao-process) ; ciao-proc-if-prompt-run-hook
(require 'ciao-loading) ; ciao-last-source-buffer-used,
			; ciao-unmark-last-run-errors,
			; ciao-proc-enqueue-w,
			; ciao-send-command,
			; ciao-locate-errors-after-run

;; ---------------------------------------------------------------------------
;; LPdoc variables
;; ---------------------------------------------------------------------------

(defcustom ciao-lpdoc-system (or (getenv "LPDOC") (ciao-get-config :lpdoc-bin))
  "Name of LPdoc auto-documenter executable."
  :group 'lpdoc
  :type 'string)

;;;###autoload
(defun ciao-set-lpdoc-system () 
  "Change the executable used to run the LPdoc auto-documenter. It is
set by default to @tt{lpdoc} or to the environment  
variable @tt{LPDOC} if it is defined. @cindex{lpdoc command, setting}
@cindex{auto-documenter command, setting}"
  (interactive)
  (setq ciao-lpdoc-system
	(read-file-name "Change Ciao LPdoc auto-documenter executable? "
   		        "" ciao-lpdoc-system nil ciao-lpdoc-system))) 

(defcustom ciao-lpdoc-system-args (or (getenv "LPDOCARGS") "")
  "Arguments passed to LPdoc executable."
  :group 'lpdoc
  :type 'string)

;;;###autoload
(defun ciao-set-lpdoc-system-args () 
  "Change the arguments passed to the LPdoc auto-documenter. They are
set by default to none or to the environment variable @tt{LPDOCARGS} if it
is defined. @cindex{lpdoc command args, setting}
@cindex{auto-documenter command args, setting}" 
  (interactive)
  (setq ciao-lpdoc-system-args
     (read-file-name "Change args passed to LPdoc auto documenter executable? "
	        "" ciao-lpdoc-system-args nil ciao-lpdoc-system-args))) 

;; TODO: Ask Ciao what are the docformats
(defvar ciao-lpdoc-docformats
  '(;; "dvi"
    ;; "ps"
    "pdf"
    "html"
    "info"
    "manl"
    "ascii"))

;; TODO: Ask Ciao what are the formats
(defcustom ciao-lpdoc-docformat (or (getenv "LPDOCFORMAT") "html")
  "Name of default output format used by LPdoc."
  :group 'lpdoc
  :type (ciao-completion-choice
	 ciao-lpdoc-docformats))

;;;###autoload
(defun ciao-set-lpdoc-docformat () 
  "Change the default output format used by the LPdoc auto-documenter. It
is set by default to @tt{html} or to the environment variable
@tt{LPDOCFORMAT} if it is defined. @cindex{lpdoc default format, setting}
@cindex{auto-documenter default format, setting}"
  (interactive)
  (ciao-completing-read 'ciao-lpdoc-docformat
			"Change default doc format used by LPdoc auto-documenter?"
			ciao-lpdoc-docformats))

(defcustom ciao-lpdoc-libpath (or (getenv "LPDOCLIB") (ciao-get-config :lpdoc-lib-dir))
  "Path in which the LPdoc library is installed."
  :group 'lpdoc
  :type 'directory)

;;;###autoload
(defun ciao-set-lpdoc-libpath () 
  "Change the path in which the LPdoc library is installed. It is
set by default to @tt{/home/clip/lib} or to the environment  
variable @tt{LPDOCLIB} if it is defined. @cindex{lpdoc lib path, setting}
@cindex{auto-documenter lib path, setting}"
  (interactive)
  (setq ciao-lpdoc-libpath
	(read-file-name "Change path in which LPdoc lib is installed? "
   		        "" ciao-lpdoc-libpath nil ciao-lpdoc-libpath))) 

;;------------------------------------------------------------
;; Communication with LPdoc process of the inferior mode

;; (see ciao-send-compiler-command in ciao-loading.el for TODO list)

(defun ciao-lpdoc-send-command-at-dir (dir command)
  "Send command `command' under `dir' directory to the LPdoc
inferior process"
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  ;; Send command to move to `dir'
  (ciao-lpdoc-goto-dir dir)
  ;; Then, execute LPdoc command `command'
  (ciao-proc-enqueue-w 'lpdoc-cproc
		       `(lambda () (ciao-lpdoc-do-command ,command)))
  ;; And setup continuation hooks (for errors)
  (ciao-lpdoc-continuation-hooks))

(defun ciao-lpdoc-continuation-hooks ()
  "Setting up hooks to run after LPdoc command execution"
  (if ciao-locate-errors-after-run
      (ciao-proc-enqueue-w 'lpdoc-cproc 'ciao-launch-find-last-run-errors-from-orig-buffer)))

;;  ;; Execute `exit'
;;  (ciao-send-command 'lpdoc-cproc 
;;		     "exit"
;;		     t)
;; Does not work in windows:
;; 	(ciao-send-command 'lpdoc-cproc 
;; 	 (concat "cd \"" dir "\" ; \"" 
;; 		 ciao-lpdoc-system "\" " 
;; 		 (if (string= ciao-lpdoc-docformat "dvi")
;; 		     ;; "large" Optional, for demos
;; 		     "")
;; 		 ciao-lpdoc-docformat "view")
;; 	 t)

(defun ciao-lpdoc-goto-dir (dir)
  (ciao-send-command 'lpdoc-cproc 
		     (concat "cd \"" dir "\"")
		     t))

(defun ciao-lpdoc-do-command (command)
  (ciao-send-command 'lpdoc-cproc 
		     (concat "\"" ciao-lpdoc-system "\" " command)
		     t))

(defun ciao-lpdoc-filter (proc string)
  ;; Here's where the actual buffer insertion is done
  (if (buffer-name (process-buffer proc))
      ;; Was (incorrectly) save-excursion (EG fix)
      ;; We must allow Ciao to affect the point so that we
      ;; return to the end of output.
      (save-current-buffer
	(set-buffer (process-buffer proc))
	(comint-output-filter proc string)
	(ciao-proc-if-prompt-run-queue 'lpdoc-cproc string))))

;;------------------------------------------------------------
;; Non interactive LPdoc commands (generate and view documentation)

(defun ciao-lpdoc-gen-doc (sourcefile no-settings-behaviour)
  "Generate the documentation for `sourcefile'. The argument
  `no-settings-behaviour' controls the behaviour if no
  SETTINGS.pl file is found. It can be:

   - 'error: prompts an error

   - 'create-buffer-default: create a default SETTINGS.pl file
     appropriate for just `sourcefile'."
  
  (let
      ((settings-ok ; First, locate SETTINGS.pl
	(cond
	  ((eq no-settings-behaviour 'error)
	   (if (file-exists-p (ciao-lpdoc-settings-file sourcefile))
	       t ;; Found
	     ;; Not found
	     (message 
	      "You need to first visit/create a SETTINGS.pl and perhaps choose options")
	     nil))
	  (t ;; Otherwise, ensure that a SETTINGS.pl file is generated
	   (ciao-lpdoc-gen-buffer-settings sourcefile)
	   t))))
    (if settings-ok
	(let ((dir (ciao-lpdoc-settings-dir sourcefile)))
	  (ciao-lpdoc-send-command-at-dir
	   dir
	   (concat "\"" ciao-lpdoc-docformat "\""))
	  t) ;; TODO: Detect errors?
      nil)))

(defun ciao-lpdoc-view (sourcefile)
  "View the documentation (in the format specified by
   `ciao-lpdoc-docformat') for `sourcefile'"
  (let* ((sourcefile (buffer-file-name (current-buffer)))
	 (thisfileroot (file-name-sans-extension (file-name-nondirectory sourcefile)))
	 (dir (ciao-lpdoc-settings-dir sourcefile)))
    (if (not (file-exists-p (ciao-lpdoc-settings-file sourcefile)))
	(message "You need to first choose options in SETTINGS.pl")
      (cond
       ((string= ciao-lpdoc-docformat "ascii") 
	(find-file-other-window 
	 (concat dir "/" thisfileroot ".ascii")))
       ((string= ciao-lpdoc-docformat "info") 
	(info-other-window
	 (concat dir "/" thisfileroot ".info")))
       (t
	(ciao-lpdoc-send-command-at-dir
	 dir
	 (concat ciao-lpdoc-docformat "view")
	 ))))))

;;------------------------------------------------------------
;; Locate, create, and modify SETTINGS.pl files for LPdoc
;;
;; TODO: the creation/patching part could be done in Prolog

;; Location of SETTINGS.pl

(defun ciao-lpdoc-settings-file (sourcefile)
  "Locate the @tt{SETTINGS.pl} file for `sourcefile'."
  (let ((dir (ciao-scratchpad-source-assoc-dir sourcefile)))
    (concat dir "/SETTINGS.pl")))

(defun ciao-lpdoc-settings-dir (sourcefile)
  "Locate the directory for the @tt{SETTINGS.pl} associated to
`sourcefile'."
  (ciao-scratchpad-source-assoc-dir sourcefile))

;; Creation and modification of SETTINGS.pl files

(defun ciao-lpdoc-default-settings-file ()
  "The default settings file (@tt{SETTINGS_DEFAULT.pl})."
  (concat ciao-lpdoc-libpath "/lib/SETTINGS_DEFAULT.pl"))  

(defun ciao-lpdoc-gen-default-settings (sourcefile)
  "Creates a simple @tt{SETTINGS.pl} file for `sourcefile',
patching the default settings file. The mainfile and paths will
point to the source name and directory. Returns the name of that
settings file."
  (let ((dir (ciao-lpdoc-settings-dir sourcefile))
	(settings (ciao-lpdoc-settings-file sourcefile)))
    (if (file-exists-p settings)
	;; Do nothing, the file already existed
	settings
      ;; Copy the SETTINGS.pl file from the default one
      (make-directory dir t)
      (copy-file (ciao-lpdoc-default-settings-file) settings t)
      ;; Patch the file
      (find-file settings)
      (ciao-lpdoc-adjust-settings-paths sourcefile)
      (ciao-lpdoc-adjust-settings-mainfile sourcefile)
      ;; Save the buffer
      (goto-char (point-min))
      (save-buffer)
      (bury-buffer)
      ;; Return the complete path to the SETTINGS.pl file
      settings)))

(defun ciao-lpdoc-gen-buffer-settings (sourcefile)
  "Creates a simple @tt{SETTINGS.pl} file for `sourcefile'. The
mainfile and paths will point to the source name and directory."
  (let ((settings (ciao-lpdoc-gen-default-settings sourcefile)))
    (message (concat "Settings is: " settings))
    ;; Load the SETTINGS.pl file
    (find-file settings)
    ;; Patch the file with the entries for the buffer
    (ciao-lpdoc-adjust-settings-mainfile sourcefile) ; TODO: really necessary?
    ;; Save the buffer
    (goto-char (point-min))
    (save-buffer)
    (bury-buffer)))

(defun ciao-lpdoc-adjust-settings-paths (sourcefile)
  "Adjust the paths of a @tt{SETTINGS.pl} file (in the current
buffer) for the given `sourcefile'."
  (goto-char (point-min))
  (search-forward "filepath := ")
  (kill-line)
  (insert "\'")
  ;; Use source directory as path to locate the source
  (insert (directory-file-name
	   (file-name-directory sourcefile)))
  (insert "\'."))

(defun ciao-lpdoc-adjust-settings-mainfile (sourcefile)
  "Adjust the mainfile of a @tt{SETTINGS.pl} file (in the current
buffer) for the given `sourcefile'."
  (goto-char (point-min))
  (search-forward "doc_structure := ")
  (kill-line)
  (insert "\'")
  ;; The name of the module: file name without directory and extension
  (insert (file-name-sans-extension
	   (file-name-nondirectory sourcefile)))
  (insert "\'.")
  (goto-char (point-min))
  (search-forward "htmldir := ")
  (kill-line)
  (insert "\'\'.")
  (goto-char (point-min))
  (search-forward "output_name := ")
  (kill-line)
  (insert "_ :- fail."))

;;------------------------------------------------------------
;; Interactive commands

;;;###autoload
(defun ciao-visit-lpdoc-settings ()
  "Visit, or create, the @tt{SETTINGS.pl} file (which controls
all auto-documenter options) for the current buffer."
  (interactive)
  (let* ((sourcefile (buffer-file-name (current-buffer)))
         (settings (ciao-lpdoc-gen-default-settings sourcefile)))
    (find-file-other-window settings)))

;;;###autoload
(defun ciao-gen-doc ()
  "Generate the documentation according to @tt{SETTINGS.pl} in
the default format. This allows generating complex documents but
it assumes that @tt{SETTINGS.pl} exists and that the options that
it contains (main file, component files, paths, etc.) have been
set properly. Documentation is generated in a temporary
directory. Note however that for generating complex manuals the
best approach is to set up a permanent documentation directory
with the appropriate @tt{SETTINGS.pl} (see the LPdoc manual)."
  (interactive)
  (ciao-gen-doc-custom "Generating documentation" 'error))

;;;###autoload
(defun ciao-gen-buffer-doc ()
  "Generate the documentation for the current buffer in the
default format. This allows generating a simple document for the
current buffer. Basically, it creates a simple, default
@tt{SETTINGS.pl} file, sets @tt{mainfile} in @tt{SETTINGS.pl} to
the current buffer file and then generates the documentation in a
temporary directory. This is useful for seeing how the
documentation of a file will format. Note that for generating
manuals the best approach is to set up a permanent documentation
directory with the appropriate @tt{SETTINGS.pl} file (see the
LPdoc manual)."
  (interactive)
  (ciao-gen-doc-custom "Generating documentation for buffer"
		       'create-buffer-default))

;;;###autoload
(defun ciao-gen-doc-custom (msg no-settings-behaviour)
  "Generate the documentation associated with the current buffer."
  (message (concat msg "... "))
  (let ((sourcefile (buffer-file-name (current-buffer))))
    (if (ciao-lpdoc-gen-doc sourcefile no-settings-behaviour)
	;; LPdoc could run (even with errors or warnings)
	(progn
	  (message (concat msg "... done.")))
      ;; There was some serious problem (e.g, SETTINGS.pl was not
      ;; found and could not be generated)
      nil)))

;;;###autoload
(defun ciao-start-viewer ()
  "Start a viewer on the documentation for the current buffer in the
   default format." 
  (interactive)
  (let ((sourcefile (buffer-file-name (current-buffer))))
    (ciao-lpdoc-view sourcefile)))


;; Provide ourselves:

(provide 'ciao-lpdoc)

;;; ciao-lpdoc.el ends here

