;;; ciao-debugger.el --- Interface with Ciao debugger

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

(require 'ciao-faces)
(require 'ciao-parsing) ; ciao-module-name,
			; ciao-debug-predicate-boundaries
(require 'ciao-aux) ; ciao-color, ciao-uncolor, ciao-what-line,
		    ; fix-cygwin-drive-letter,
		    ; match-string-no-properties
(require 'ciao-process) ; ciao-proc-enqueue-nw,
			; ciao-proc-enqueue-nw-once,
			; ciao-proc-enqueue-w,
			; ciao-proc-alive, ciao-send-command,
			; ciao-proc-get-buffer,
			; ciao-proc-if-prompt-run-queue

;; ---------------------------------------------------------------------------
;; Source debugger variables
;; ---------------------------------------------------------------------------

;; TODO: Move the rest of debugger variables here (it needs more
;; changes)

(defvar ciao-debug-marker-acc ""
  "Text to search for ciao-debug-marker-prompt.")
(make-variable-buffer-local 'ciao-debug-marker-acc)

(defvar ciao-debug-marker-regexp nil
  "Regular expression for looking for file position info.")
(setq ciao-debug-marker-regexp
      (concat 
	      "         In "
	      "\\(.*\\)"         ; Src file
	      " ("
	      "\\([0-9]+\\)"     ; Start line
	      "-"
	      "\\([0-9]+\\)"     ; End line
              ") "
	      "\\(.*\\)"         ; Pred name
	      "-"
	      "\\([0-9]+\\)\n"    ;)) ; n-th pred
	      ".*[\*0-9]+  [\*0-9]+"
	      "  \\([CERF][a-z]+\\):.* ? "))

;; CHANGE
(defvar ciao-debug-filter-defer-flag nil
  "Non-nil means don't process anything form the debugger 
right now. It is saved for when flag is not set.")

(defvar ciao-debug-filter-pending-text nil
  "Non-nil means this is text that has been saved for later in
'ciao-debug-filter'.")

(defvar ciao-debug-delete-prompt-marker nil)

(defvar ciao-debug-last-frame nil 
  "Last file over which we have drawn.")

(defvar ciao-debug-last-line nil 
  "Temporary storage of last line (coloring). Con pair buffer, line number.")

;; TODO: why is ciao-debug-last-line not local?
(defun ciao-debugger-init ()
  "Initialize the debugger state"
  (setq ciao-debug-last-line nil))

(defun ciao-debugger-mode-variables ()
  "Initialize mode variables for the debugger."
  (set (make-local-variable 'ciao-debug-last-frame) nil)
  (set (make-local-variable 'ciao-debug-delete-prompt-marker) (make-marker)))

;;------------------------------------------------------------
;; Interactive commands
;;------------------------------------------------------------

(defun ciao-select-debug-mode ()
  "Mark, or unmark, the current buffer for debugging (traditional
debugging or source debugging). Note that if the buffer has already been
loaded while it was unmarked for debugging (and has therefore been loaded
in ``compile'' mode) it has to be loaded again. The minibuffer shows how
the module is loaded now and allows selecting another mode for it. There
are three posibilities: N for no debug, S for source debug and D for
traditional debug."
  (interactive)
  (ciao-proc-enqueue-nw 'ciaosh-cproc 'ciao-real-select-debug-mode)
  (ciao-send-command 'ciaosh-cproc "display_debugged." t))

(defun ciao-real-select-debug-mode (&optional list)
  (let ((end 0) 
	(buffers-debug)
	(module (ciao-module-name))
	(actually "N")
	(string)
	(default)
	(option))
    (if list
	(setq buffers-debug list)
      (setq buffers-debug (ciao-how-debugged)))
    (if (string-match (concat "\\<" module "\\>") (car buffers-debug))
	(setq actually "D"))
    (if (string-match (concat "\\<" module "\\>") (cdr buffers-debug))
	(setq actually "S"))
    (cond ((string= actually "N")
	   (setq string "Module not selected for debug. ")
	   (setq default "S"))
	  ((string= actually "D")
	   (setq string "Module selected for trad debug. ")
	   (setq default "N"))
	  ((string= actually "S")
	   (setq string "Module selected for source debug. ")
	   (setq default "N")))
    (setq string (concat string "Select debug mode (N/S/D)? "))
    (setq option
 	  (read-string string default nil))
    (if (string= option "") (setq option default))
    ;; Was simply:  (but xemacs does not support the last argument)
    ;;	  (read-string string default nil default))
    ;; Send the appropiate command to Ciao
    (cond ((and (or (string= actually "N")
		    (string= actually "S"))
		(string= option "D"))
	   (ciao-send-command 'ciaosh-cproc
			      (concat "debug_module('" module "').") t))
	  ((and (or (string= actually "N")
		    (string= actually "D"))
		(string= option "S"))
	   (ciao-send-command 'ciaosh-cproc
			      (concat "debug_module_source('" module "').") t))
	  ((and (or (string= actually "S")
		    (string= actually "D"))
		(string= option "N"))
	   (ciao-send-command 'ciaosh-cproc
			      (concat "nodebug_module('" module "').") t)))))

;; TODO: does it need to be interactive?
(defun ciao-mark-buffer-source-debug ()
  "Mark a module for source debug."
  (interactive)
  (ciao-send-command 'ciaosh-cproc 
		     (concat "debug_module_source('" (ciao-module-name) "').")
		     t))

;; TODO: does it need to be interactive?
(defun ciao-unmark-buffer-debug ()
  "Unmark a module for debug."
  (interactive)
  (ciao-send-command 'ciaosh-cproc
		     (concat "nodebug_module('" (substring (buffer-name) 0 -3) "').")
		     t))

(defun ciao-enable-trace ()
  "Set the debugger to the trace state. In this state, the program is
executed step by step."
  (interactive)
  (ciao-send-command 'ciaosh-cproc "trace." t))

(defun ciao-enable-debug ()
  "Set the debugger to the debug state. In this state, the program will
only stop in breakpoints and spypoints. Breakpoints are specially supported
in @apl{emacs} and using source debug."
  (interactive)
  (ciao-send-command 'ciaosh-cproc "debug." t))

(defun ciao-no-debug ()
  "Set the debugger to the no debug state. In this state, the program will
execute until the end, without stopping in any step of the program."
  (interactive)
  (ciao-send-command 'ciaosh-cproc "nodebug." t))

(defun ciao-debug-buffer ()
  "Debug (or stop debugging) buffer source. This is a shortcut which
is particularly useful when using the source debugger on a single
module. It corresponds to several lower-level actions.  Those
lower-level actions depend on how the module was selected for
debugging. In case the module was not marked for source-level
debugging, it marks the module corresponding to the current buffer for
source-level debugging, reloads it to make sure that it is loaded in
the correct way for debugging (same as \\<ciao-mode-map>
\\[ciao-load-buffer]), and sets the debugger in trace mode (i.e.,
issues the @tt{trace.} command to the top-level shell). Conversely, if
the module was already marked for source-level debugging then it will
take the opposite actions, i.e., it unmarks the module for
source-level debugging, reloads it, and sets the debugger to non-debug
mode."
  (interactive)
  (ciao-send-command 'ciaosh-cproc "display_debugged." t)
  (ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-real-debug-buffer))

(defun ciao-real-debug-buffer ()
  (let ((end 0) 
	(buffers-debug)
	(module (ciao-module-name))
	(actually "N"))
    (setq buffers-debug (cdr (ciao-how-debugged)))
    (if (string-match (concat "\\<" module "\\>") buffers-debug)
	(setq actually "S"))
    (cond ((string= actually "S")
	   ;; Buffer is marked for source debug
	   (ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-no-debug)
	   (ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-load-buffer)
	   (ciao-unmark-buffer-debug))
	  ((string= actually "N")
	   ;; Buffer is marked for traditional debug or not marked for
	   ;; debug.
	   (ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-enable-trace)
	   (ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-load-buffer)
	   (ciao-mark-buffer-source-debug)))))

(defun ciao-select-buffers-for-debug ()
  "Visits all Ciao files which are currently open in a buffer
allowing selecting for each of them whether to debug them or not and
the type of debugging performed. When working on a multiple module
program, it is possible to have many modules open at a time. In this
case, you will navigate through all open Ciao files and select
the debug mode for each of them (same as doing \\<ciao-mode-map>
\\[ciao-select-debug-mode] for each)."
  (interactive)
  (ciao-send-command 'ciaosh-cproc "display_debugged." t)
  (ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-real-select-buffers-for-debug))

(defvar ciao-buffers nil)

(defun ciao-real-select-buffers-for-debug ()
  (let* ((buffers (ciao-how-debugged))
	 (ciao-select-ciao-buffers
	  (function (lambda (buffer)
		      (set-buffer buffer)
		      (if (eq major-mode 'ciao-mode)
			  (setq ciao-buffers (cons buffer ciao-buffers))))))
	 (select-debug-module 
	  (function (lambda (buffer)
		      (set-buffer buffer)
		      (switch-to-buffer buffer t)
		      (ciao-real-select-debug-mode buffers))))
	 module)

    (if (not ciao-buffers)
	(mapcar ciao-select-ciao-buffers (buffer-list)))
    
    (setq module (car ciao-buffers))
    (setq ciao-buffers (cdr ciao-buffers))
    (funcall select-debug-module module)
    (if ciao-buffers
	(ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-real-select-buffers-for-debug))))

(defun ciao-how-debugged ()
  "Return a pair containning buffers selected for traditional debug and
buffers selected for source debug."
  (let (buffers-debug end)
    (save-excursion
      (set-buffer (ciao-proc-get-buffer 'ciaosh-cproc))
      (search-backward "display_debugged.")
      ;; Match all tradicional debugged modules
      (forward-line)
      (end-of-line)
      (setq end (point))
      ;; (beginning-of-line)
      (move-to-column 0)
      (if (search-forward-regexp "\\[\\(.*\\)\\]" end t)
	  (setq buffers-debug (match-string-no-properties 1))
	(setq buffers-debug ""))
      ;; Match all source debug modules
      (forward-line)
      (end-of-line)
      (setq end (point))
      ;; (beginning-of-line)
      (move-to-column 0)
      (if (search-forward-regexp "\\[\\(.*\\)\\]" end t)
	  (setq buffers-debug (cons buffers-debug 
				    (match-string-no-properties 1)))
	(setq buffers-debug (cons buffers-debug ""))))))

;; TODO: Detection of a debugging session is based on the comparison
;;   the column number with 6. This is a fragile method.
(defun ciao-debug-maybe-escape ()
  "In case we are debugging send a @ and then continue with the
   normal process."
  (if (ciao-proc-alive 'ciaosh-cproc)
      (let ((column))
	(save-excursion
	  (set-buffer (ciao-proc-get-buffer 'ciaosh-cproc))
	  (setq column (current-column)))
	(if (< column 6) ;; TODO: kludge?
	    t
	  (ciao-send-command 'ciaosh-cproc "@" t)
	  (sleep-for 0.01)))))

(defun ciao-debug-breakon ()
  "Set a breakpoint on the current literal (goal). This can be done at any
time (while debugging or not). The cursor must be @em{on the predicate
symbol of the literal}. Breakpoints are only useful when using source-level
debugging."
  (interactive)
  (ciao-debug-maybe-escape)
  (ciao-color (ciao-what-line)
	      (ciao-what-line)
	      ciao-face-debug-breakpoint
	      'ciao-break)
  (ciao-send-command 'ciaosh-cproc
		     (concat "breakpt(" (ciao-debug-breakparams (point)) ").")
		     t))
  
(defun ciao-debug-breakoff ()
  "Remove a breakpoint from the current literal (goal). This can be done
at any time (while debugging or not). The cursor must be @em{on the predicate
symbol of the literal}."
  (interactive)
  (ciao-debug-maybe-escape)
  (ciao-uncolor (ciao-what-line)
		(ciao-what-line)
		'ciao-break)
  (ciao-send-command 'ciaosh-cproc
		     (concat "nobreakpt(" (ciao-debug-breakparams (point)) ").")
		     t))

(defun ciao-debug-all-breakoff ()
  "Remove all breakpoints. This can be done at any time (while debugging
or not)."
  (interactive)
  (ciao-debug-maybe-escape)
  (ciao-send-command 'ciaosh-cproc "nobreakall." t)
  (ciao-debug-uncolor-all-breakpt))
  
(defun ciao-debug-breakparams (point)
  (let* ((boundaries (ciao-debug-predicate-boundaries point))
	(pred-name (find-tag-default)) 
	(src-file (expand-file-name (buffer-name (current-buffer))))
	(begin-line (car boundaries))
	(end-line (cdr boundaries)) 
	(number 0)
	string)
    (save-excursion
      (goto-line begin-line)
      (while (< (point) point)
	(if (re-search-forward
	     (concat "\\<" (regexp-quote pred-name) "\\>") nil nil)
	    (setq number (+ number 1)))))
    (concat  "'" pred-name "','" src-file "',"
			 (int-to-string begin-line) "," 
			 (int-to-string end-line) "," 
			 (int-to-string number) "," 
			 (int-to-string (ciao-what-line)))))

(defun ciao-debug-uncolor-all-breakpt ()
  "Remove breakpoint coloring in all Ciao files."
  (interactive)
  (save-excursion
    (mapcar (function (lambda (buffer)
			(set-buffer buffer)
			(if (eq major-mode 'ciao-mode)
			    (ciao-debug-uncolor-buffer))))
	    (buffer-list))))

(defun ciao-debug-uncolor-buffer ()
  "Remove breakpoint faces color in a Ciao buffer"
  (let (beg end)
    (setq beg (point-min))
    (setq end (point-max))
    (mapcar (function (lambda (over)
			(and (overlay-get over 'ciao-break)
			     (delete-overlay over))))
	    (overlays-in beg end))))
  
(defun ciao-debug-display-breakpt ()
  "Redisplay breakpoints in all Ciao buffers. This ensures that the marks
in the source files and the Ciao toplevel are synchronized."

  (interactive)
  (ciao-debug-uncolor-all-breakpt)
  (if (ciao-proc-alive 'ciaosh-cproc)
      (progn
	(ciao-proc-enqueue-nw 'ciaosh-cproc 'ciao-debug-redisplay-breakpt)
	(ciao-send-command 'ciaosh-cproc "list_breakpt." t))))

(defun ciao-debug-redisplay-breakpt ()
    (let ((buffer (current-buffer)))
      (save-excursion
	(let ((file 0) (l0 0) (l1 0) (pred 0) (numpred 0) (bound 0))
	  (set-buffer (ciao-proc-get-buffer 'ciaosh-cproc))
	  (setq bound (point))
	  (search-backward "list_breakpt.")
	  (while (search-forward-regexp 
		  (concat "Breakpoint in file \\(.*\\)" 
			  " \\([0-9]+\\)-\\([0-9]+\\) "
			  "on literal \\(.*\\)-\\([0-9]+\\)")
		  bound t)
	    (setq file (buffer-substring-no-properties (match-beginning 1)
						       (match-end 1))
		  l0 (string-to-number (buffer-substring-no-properties 
				     (match-beginning 2) (match-end 2)))
		  l1 (string-to-number (buffer-substring-no-properties 
				     (match-beginning 3) (match-end 3)))
		  pred (buffer-substring-no-properties (match-beginning 4)
						       (match-end 4))
		  numpred (string-to-number (buffer-substring-no-properties 
					  (match-beginning 5) (match-end 5))))
	    (save-excursion
	      (set-buffer (get-file-buffer file))
	      (goto-line l0)
	      ;; To change when considering comments in clause
	      (search-forward pred nil t numpred)
	      (ciao-color (ciao-what-line)
			  (ciao-what-line)
			  ciao-face-debug-breakpoint
			  'ciao-break)))))
    (switch-to-buffer buffer)))

;;------------------------------------------------------------
;; Commands related to the source code debugger
;;------------------------------------------------------------

(defun ciao-debug-display-frame (buffname)
  (if ciao-debug-last-frame
      (progn
	;; (ciao-debug-set-buffer)
	(let ((port    (car ciao-debug-last-frame))
	      (file    (car (cdr ciao-debug-last-frame)))
	      (l0      (car (cdr (cdr ciao-debug-last-frame))))
	      (l1      (car (cdr (cdr (cdr ciao-debug-last-frame)))))
	      (numpred (car (cdr (cdr (cdr (cdr ciao-debug-last-frame))))))
	      (pred    (cdr (cdr (cdr (cdr (cdr ciao-debug-last-frame)))))))

	  ;; (setq file (ciao-debug-transform-file-name file))
	  (ciao-debug-display-line file l0 l1 pred numpred port buffname) 
	  (setq ciao-debug-last-frame nil)))))

(defun ciao-debug-display-line (file start end pred numpred port buffname)
  (let* ((count 0) (init 0) (finish 0) (test t) (pos 0)
	 (last-nonmenu-event t)  ; Prevent use of dialog box for questions.
	 ;; Problem for embedded debugger
	 (buffer
	  (save-excursion
	    (or (string= (buffer-name) buffname) ; was (current-buffer) and eq!
		(set-buffer buffname))
	    (ciao-debug-find-file file)))
	 (window (and buffer (or (get-buffer-window buffer)
				 (display-buffer buffer)))))

    ; Unmark the last region marked
    (ciao-debug-uncolor-line)

    (if buffer
	(progn
	  (save-excursion
	    (set-buffer buffer)
	    (save-restriction
	      (widen)
	      ;; (goto-line start)
	      ;; Due to impression in detecting the start line of a clause
	      ;; we move to the end and clause and then search backward
	      ;; until find the beginning of the clause.
	      (goto-line end)
	      (end-of-line)
	      (re-search-backward "^[a-z']" (point-min) t)

	      ;; Search the numpred-th pred and put the marker at the
	      ;; beginning of the line. Doesn't consider PRED in
	      ;; comment
 	      (end-of-line)
 	      (setq finish (point))
 	      (beginning-of-line)
 	      (setq init (point))
 	      (while (and test (not (eq count numpred)))
 		(while (and test (not (search-forward pred finish t)))
 		  (forward-line)
		  (if (or (< end (ciao-what-line))
			  (and (eq init (point)) (eq (point) finish)))
		      (setq test nil))
 		  (end-of-line)
 		  (setq finish (point))
 		  (beginning-of-line)
 		  (setq init (point)))
 		;; Found a PRED, search if it is in a comment
 		(if (and test (not (search-backward "%" init t)))
 		    (setq count (+ count 1))
 		  (forward-line)
 		  (end-of-line)
 		  (setq finish (point))
 		  (beginning-of-line)
 		  (setq init (point))))
	      
	      (if (< count numpred) 
		  ;; Not found pred, overlay the whole region
		  (progn
		    (setq overlay-arrow-string "")
		    (goto-line end)
		    (end-of-line)
		    (re-search-backward "^[a-z']" (point-min) t)
		    (ciao-color (ciao-what-line)
				end
				ciao-face-debug-expansion
				'ciao-debug)
		    ;; Save information for uncoloring the last line
		    (setq ciao-debug-last-line
			  (cons (current-buffer)
				(ciao-what-line)))

		    )
		;; Save information for uncoloring the last line
		(setq ciao-debug-last-line
		      (cons (current-buffer)
			    (ciao-what-line)))
		
		;; Color line
		(ciao-color (ciao-what-line)
			    (ciao-what-line)
			    (ciao-debug-obtain-color port)
			    'ciao-debug)
		(setq overlay-arrow-string (ciao-debug-transform-port port))

		)
	      ;; Arrow position
	      (beginning-of-line)
	      (setq pos (point))
	      (or overlay-arrow-position
		  (setq overlay-arrow-position (make-marker)))
	      (set-marker overlay-arrow-position (point) (current-buffer)))
	    (cond ((or (< pos (point-min)) (> pos (point-max)))
		   (widen)
		   (goto-char pos))))
	  (set-window-point window overlay-arrow-position)))))

(defun ciao-debug-transform-port (port)
  "Arrow to show in source file. It's determines from PORT."
  (cond ((string= "Call" port) "C=>")
	((string= "Exit" port) "E=>")
	((string= "Fail" port) "F=>")
	((string= "Redo" port) "R=>")))

(defun ciao-debug-obtain-color (port)
  (cond ((string= "Call" port) ciao-face-debug-call)
	((string= "Exit" port) ciao-face-debug-exit)
	((string= "Fail" port) ciao-face-debug-fail)
	((string= "Redo" port) ciao-face-debug-redo)))

(defun ciao-debug-uncolor-line ()
  (if (and ciao-debug-last-line
	   (buffer-name (car ciao-debug-last-line))) ;; And buffer not killed
      (save-excursion
	(set-buffer (car ciao-debug-last-line))
	(ciao-uncolor (cdr ciao-debug-last-line)
		      (cdr ciao-debug-last-line)
		      'ciao-debug))))
  
(defun ciao-debug-remove-marks ()
  (ciao-debug-uncolor-line)
  (setq overlay-arrow-position nil))

;; TODO: This includes two filters indeed:
;;   - debug events
;;   - warning/error messages after compilation
(defun ciao-debug-filter (proc string)
  ;; Here's where the actual buffer insertion is done
  (let (output process-window)
    (if (buffer-name (process-buffer proc))
	(if ciao-debug-filter-defer-flag
	    ;; If we can't process any text now,
	    ;; save it for later
	    (setq ciao-debug-filter-pending-text 
		  (concat (or ciao-debug-filter-pending-text "") string))

	  (let ((ciao-debug-filter-defer-flag t))
	    ;; Process now any text we previously saved up
	    (if ciao-debug-filter-pending-text
		(setq string (concat ciao-debug-filter-pending-text string)
		      ciao-debug-filter-pending-text nil))
            ;; Was (incorrectly) save-excursion (EG fix)
            ;; We must allow Ciao to affect the point so that we
	    ;; return to the end of output.
	    (save-current-buffer
	      (set-buffer (process-buffer proc))
	      ;; If we have been so requested, delete the debugger prompt.
	      (if (marker-buffer ciao-debug-delete-prompt-marker)
		  (progn
		    (delete-region (process-mark proc)
				   ciao-debug-delete-prompt-marker)
		    (set-marker ciao-debug-delete-prompt-marker nil)))
	      
	      ; Here we obtain the output to show in the buffer
	      (setq output (ciao-debug-marker-filter string))
	      
	      (setq process-window
		    (and ciao-debug-last-frame
			 (>= (point) (process-mark proc))
			 (get-buffer-window (current-buffer))))

	      ;; Let the comint filter do the actual insertion.
	      ;; That lets us inherit various comint features.
	      (comint-output-filter proc output))

	    ;; TODO: this looks like a hack
	    (ciao-proc-enqueue-nw-once 'ciaosh-cproc 'ciao-debug-remove-marks)

	    (ciao-proc-if-prompt-run-queue 'ciaosh-cproc output)

	    ;; Put the arrow on the source line.
	    ;; This must be outside of the save-excursion 
	    ;; in case the source file is our current buffer.
	    (if process-window
		(save-selected-window
		 (select-window process-window)
		 (ciao-debug-display-frame (buffer-name)))   
	      ;; We have to be in the proper buffer, (process-buffer proc),
	      ;; but not in a save-excursion, because that would restore
	      ;; point.
	      (let ((old-buf (current-buffer)))
		(set-buffer (process-buffer proc))
		(unwind-protect
		    (ciao-debug-display-frame (buffer-name))
		  (set-buffer old-buf)))))
	  ;; If we deferred text that arrived during this processing
	  ;; handle it now.
	  (if ciao-debug-filter-pending-text
	      (ciao-debug-filter proc "")))))) 
	  
(defun ciao-debug-find-file (file)
  (save-excursion
    (let ((buf (find-file-noselect (fix-cygwin-drive-letter file))))
      (set-buffer buf)
      buf)))

(defun ciao-debug-marker-filter (string)
  "Search the string for the debugging information"
  (setq ciao-debug-marker-acc (concat ciao-debug-marker-acc string))
  (let ((output ""))
    ; Process all the complete markers in this chunk
    (while (string-match ciao-debug-marker-regexp ciao-debug-marker-acc)
      (setq
       ;; Extract the frame position from the marker
       ciao-debug-last-frame
       (cons (substring ciao-debug-marker-acc (match-beginning 6)
			(match-end 6))
	     (cons (substring ciao-debug-marker-acc 
			      (match-beginning 1) (match-end 1))
	     (cons (string-to-number (substring ciao-debug-marker-acc
				       (match-beginning 2) (match-end 2)))
	     (cons (string-to-number (substring ciao-debug-marker-acc
				       (match-beginning 3) (match-end 3)))
	     (cons (string-to-number (substring ciao-debug-marker-acc
				       (match-beginning 5) (match-end 5)))
		   (substring ciao-debug-marker-acc 
			      (match-beginning 4) (match-end 4)))))))
	            
       ;; Append Any Text Before the marker to the output we're going to
       ;; return - we don't include the marker in this text
       output (concat output 
		      (substring ciao-debug-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text
       ciao-debug-marker-acc (substring ciao-debug-marker-acc (+ (match-end
							       5) 1))))

    ;; Does the remaining text look like it might end with the beginning of
    ;; another marker? If it does, the keep it in ciao-debug-marker until
    ;; we receive the rest of it. Since we know the full marker regexp
    ;; above failed, it's pretty simple to test for marker starts.
    (if (string-match "         In " ciao-debug-marker-acc)
	(progn
	  ;; Everything before the potential marker start can be output
	  (setq output (concat output (substring ciao-debug-marker-acc 0
						 (match-beginning 0))))
	  (setq ciao-debug-marker-acc (substring ciao-debug-marker-acc
						 (match-beginning 0))))
      (setq output (concat output ciao-debug-marker-acc)
	    ciao-debug-marker-acc ""))
    output))


;; Provide ourselves:

(provide 'ciao-debugger)

;;; ciao-debugger.el ends here

