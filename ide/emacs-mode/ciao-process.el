;;; ciao-process.el --- Ciao (toplevel) processes
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

(require 'comint) ; comint-mode, comint-highlight-prompt,
		  ; make-comint-in-buffer, comint-input-filter,
		  ; comint-prompt-regexp, comint-send-input,
		  ; comint-proc-query, comint-check-proc
(require 'ciao-common) ; ciao-system, ciao-system-args,
		       ; ciao-ciaopp-system, ciao-ciaopp-system-args,
		       ; ciao-lpdoc-system, ciao-lpdoc-system-args
		       ; ciao-toplevel-buffer-name,
		       ; ciao-ciaopp-buffer-name,
		       ; ciao-lpdoc-buffer-name,
		       ; ciao-ciaopp-gmenu-buffer-name
(require 'ciao-syntax) ; ciao-any-prompt-pattern
(require 'ciao-aux) ; ciao-switch-other-window

;; ==========================================================================

;; ---------------------------------------------------------------------------
;; Definitions for the several Ciao toplevel processes
;;
;; TODO: Move out of this file (clients of ciao-process.el should
;;   register this)
;; ---------------------------------------------------------------------------

;; Images for logos
(defvar ciao-logos
  '(:ciao "ciao-logo.png"
    :ciaopp "ciaopp-logo.png"
    :java "java-logo.png"
    ))
(defun ciao-get-logo (key) (plist-get ciao-logos key))

; (private)
(defvar ciao-ciaosh-prompt "?-" 
  "Ciao prompt (simplest)")
(defvar ciao-ciaopp-prompt "ciaopp ?-" 
  "CiaoPP prompt (simplest)")
(defvar ciao-lpdoc-prompt "lpdoc ?-" 
  "LPdoc prompt (simplest)")

; (private)
(defvar ciao-ciaosh-prompt-pattern "\n\\?- " 
  "Matching the Ciao prompt")
(defvar ciao-ciaopp-prompt-pattern "\nciaopp \\?- " 
  "Matching the CiaoPP prompt")
;; Set to ciao-os-shell-prompt-pattern:
(defun ciao-lpdoc-prompt-pattern ()
  "Matching the lpdoc prompt"
  (ciao-any-prompt-pattern))
;; *** For future lpdoc versions (with top level):
;; (defun ciao-lpdoc-prompt-pattern ()
;;   "Matching the lpdoc prompt"
;;   "\n\\lpdoc ?- ")

; (private)
;; TODO: merge with the previous one
(defvar ciao-any-ciaosh-prompt-pattern "^\\(\\(\\|[0-9]+ \\)\\?-\\)")
(defvar ciao-any-ciaopp-prompt-pattern "^\\(ciaopp \\?-\\)")
(defvar ciao-any-lpdoc-prompt-pattern "^\\(lpdoc \\?-\\|.*\\$ \\)")

;; ---------------------------------------------------------------------------
;; State for communication with Ciao toplevel processes (ciaosh,
;; ciaopp, lpdoc, etc.) via inferior buffers.
;; ---------------------------------------------------------------------------

;; TODO: allow creation of many custom Ciao inferior buffers

;; queue: A function queue for things to do in the source (emacs
;;   buffer) or Ciao process buffer.  A special value
;;   'prompt-wait-mark indicates that the previous function must wait
;;   for a prompt.
(defun ciao-proc-queue (cproc)
  (cond ((eq cproc 'ciaosh-cproc)
	 'ciao-ciaosh-queue)
	((eq cproc 'ciaopp-cproc)
	 'ciao-ciaopp-queue)
	((eq cproc 'lpdoc-cproc)
	 'ciao-lpdoc-queue)
	(t (error "Unknown Ciao process %s" cproc))))

;; marker-acc: Keep the last line written in the Ciao inferior
;;   buffer. It is used to search for the prompt since the prompt
;;   should be after a newline.
(defun ciao-proc-marker-acc (cproc)
  (cond ((eq cproc 'ciaosh-cproc)
	 'ciao-ciaosh-prompt-marker-acc)
	((eq cproc 'ciaopp-cproc)
	 'ciao-ciaopp-prompt-marker-acc)
	((eq cproc 'lpdoc-cproc)
	 'ciao-lpdoc-prompt-marker-acc)
	(t (error "Unknown Ciao process %s" cproc))))

; (private)
(defvar ciao-ciaosh-queue nil)
(defvar ciao-ciaosh-prompt-marker-acc "")
(make-variable-buffer-local 'ciao-ciaosh-prompt-marker-acc)

; (private)
(defvar ciao-ciaopp-queue nil)
(defvar ciao-ciaopp-prompt-marker-acc "")
(make-variable-buffer-local 'ciao-ciaopp-prompt-marker-acc)

; (private)
(defvar ciao-lpdoc-queue nil)
(defvar ciao-lpdoc-prompt-marker-acc "")
(make-variable-buffer-local 'ciao-lpdoc-prompt-marker-acc)

(defun ciao-proc-reset-queue-all ()
  "Reset the queue of of the all Ciao processes"
  (ciao-proc-reset-queue 'ciaosh-cproc)
  (ciao-proc-reset-queue 'ciaopp-cproc)
  (ciao-proc-reset-queue 'lpdoc-cproc))

;; ---------------------------------------------------------------------------

(defun ciao-cproc-for-buffer ()
  "Infer the right Ciao inferior process (cproc) corresponding to
this buffer name."
  ;; IMPORTANT: Do not use the buffer here, but the buffer name.
  (cond ((string= (buffer-name) (ciao-proc-buffer-name 'ciaosh-cproc))
	 'ciaosh-cproc)
	((string= (buffer-name) (ciao-proc-buffer-name 'ciaopp-cproc))
	 'ciaopp-cproc)
	((string= (buffer-name) (ciao-proc-buffer-name 'lpdoc-cproc))
	 'lpdoc-cproc)
	;; This case is usually used in normal shell. The filter is
	;; to handle source-level embedded debugger messages
	(t 'ciaosh-cproc)))

(defun ciao-proc-name (cproc)
  "Process name of `cproc'"
  (cond ((eq cproc 'ciaosh-cproc) "ciaosh")
	((eq cproc 'ciaopp-cproc) "ciaopp")
	((eq cproc 'lpdoc-cproc) "lpdoc")
	(t (error "Unknown Ciao process %s" cproc))))

(defun ciao-proc-desc (cproc)
  (cond ((eq cproc 'ciaosh-cproc) "Ciao toplevel")
	((eq cproc 'ciaopp-cproc) "Ciao preprocessor")
	((eq cproc 'lpdoc-cproc) "LPdoc auto-documenter")
	(t (error "Unknown Ciao process %s" cproc))))

(defun ciao-proc-logo (cproc)
  (cond ((eq cproc 'ciaosh-cproc) (ciao-get-logo :ciao))
	((eq cproc 'ciaopp-cproc) (ciao-get-logo :ciaopp))
	((eq cproc 'lpdoc-cproc) nil)
	(t (error "Unknown Ciao process %s" cproc))))

(defun ciao-proc-system (cproc)
  (cond ((eq cproc 'ciaosh-cproc) ciao-system)
	((eq cproc 'ciaopp-cproc) ciao-ciaopp-system)
	((eq cproc 'lpdoc-cproc) nil) ;; ciao-lpdoc-system
	(t (error "Unknown Ciao process %s" cproc))))

(defun ciao-proc-system-args (cproc)
  (cond ((eq cproc 'ciaosh-cproc) ciao-system-args)
	((eq cproc 'ciaopp-cproc) ciao-ciaopp-system-args)
	((eq cproc 'lpdoc-cproc) nil) ;; ciao-lpdoc-system-args
	(t (error "Unknown Ciao process %s" cproc))))

(defun ciao-proc-filter (cproc)
  "The right filter for this Ciao buffer"
  (cond ((eq cproc 'ciaosh-cproc) 'ciao-debug-filter)
	((eq cproc 'ciaopp-cproc) 'ciao-ciaopp-filter)
	((eq cproc 'lpdoc-cproc) 'ciao-lpdoc-filter)
	(t (error "Unknown Ciao process %s" cproc))))

;; TODO: Try to use ciao-proc-buffer instead, whenever it is possible.
(defun ciao-proc-buffer-name (cproc)
  (cond ((eq cproc 'ciaosh-cproc) ciao-toplevel-buffer-name)
	((eq cproc 'ciaopp-cproc) ciao-ciaopp-buffer-name)
	((eq cproc 'lpdoc-cproc) ciao-lpdoc-buffer-name)
	(t (error "Unknown Ciao process %s" cproc))))

(defun ciao-proc-prompt (cproc)
  (cond ((eq cproc 'ciaosh-cproc) ciao-ciaosh-prompt)
	((eq cproc 'ciaopp-cproc) ciao-ciaopp-prompt)
	((eq cproc 'lpdoc-cproc) ciao-lpdoc-prompt)
	(t (error "Unknown Ciao process %s" cproc))))

(defun ciao-proc-prompt-pattern (cproc)
  (cond ((eq cproc 'ciaosh-cproc) ciao-ciaosh-prompt-pattern)
	((eq cproc 'ciaopp-cproc) ciao-ciaopp-prompt-pattern)
	((eq cproc 'lpdoc-cproc) (ciao-lpdoc-prompt-pattern))
	(t (error "Unknown Ciao process %s" cproc))))

;; TODO: merge with the previous one
(defun ciao-proc-any-prompt-pattern (cproc)
  (cond ((eq cproc 'ciaosh-cproc) ciao-any-ciaosh-prompt-pattern)
	((eq cproc 'ciaopp-cproc) ciao-any-ciaopp-prompt-pattern)
	((eq cproc 'lpdoc-cproc) ciao-any-lpdoc-prompt-pattern)
	(t (error "Unknown Ciao process %s" cproc))))

;; ---------------------------------------------------------------------------

(defun ciao-proc-get-buffer (cproc)
  "Find the buffer associated to `cproc'. Return `nil' if there
is no such buffer."
  (get-buffer (ciao-proc-buffer-name cproc)))

;; ---------------------------------------------------------------------------
;; Sending commands to the inferior process

;; TODO: Split into a low-level version (which do not change windows),
;;   and a high-level version (which does).

(defvar ciao-last-process-buffer-used nil
  "The last Ciao toplevel process buffer used (preprocessor,
  toplevel, ...).")
(defvar ciao-last-process-cproc nil
  "Contains which is the type of Ciao toplevel process used.")

;; ---------------------------------------------------------------------------

;; TODO: Circular dependencies! Use hooks to solve it. Define a
;;   hook-less version for cheap processes where only communication is
;;   necessary.
(autoload 'ciao-inferior-mode-internal "ciao")
(autoload 'ciao-reset-loading-state "ciao-loading")
(autoload 'ciao-reset-error-state "ciao-parsing")
(autoload 'ciao-debug-remove-marks "ciao-debugger")

(require 'ciao-aux) ; ciao-queue-clear, ciao-queue-empty,
		    ; ciao-queue-enqueue, ciao-queue-dequeue

(defun ciao-start-inferior-process (cproc &optional buffname)
  "Start in `buffname' an inferior Ciao process of type
`cproc'. Return the buffer."
  (let (system system-args newbuff)
    ;; If buffname is nil, obtain the default value
    (if (eq buffname nil)
	(setq buffname (ciao-proc-buffer-name cproc)))
    ;;
    (setq system (ciao-proc-system cproc)) 
    (setq system-args (ciao-proc-system-args cproc))
    ;; TODO: (JF) make the buffname optional
    (setq 
     newbuff
     (if (eq cproc 'lpdoc-cproc)
	 ;; It is an lpdoc buffer, use normal shell.
	 (save-window-excursion ; Do not allow 'shell' disorder our windows
	   (shell buffname))
       (if (equal ""
		  ;; Done differently because of filenames with blanks...
		  ;; (ciao-get-string-after-blank system)
		  system-args
		  )
	   (progn 
	     (make-comint-in-buffer
	      (ciao-proc-name cproc)
	      buffname
	      ;; Done differently because of filenames with blanks...
	      ;; (ciao-get-string-before-blank system)
	      system
	      ))
	 (make-comint-in-buffer
	  (ciao-proc-name cproc)
	  buffname 
	  ;; Done differently because of filenames with blanks...
	  ;; (ciao-get-string-before-blank system) ; command name
	  system
	  nil                                   ; filename
	  ;; Done differently because of filenames with blanks...
	  ;; (ciao-get-string-after-blank system)  ; arguments
	  system-args
	  ))))

    (with-current-buffer newbuff
      (ciao-inferior-mode-internal cproc)
      (goto-char (point-max)))

    (setq ciao-last-process-buffer-used newbuff)
    (setq ciao-last-process-cproc cproc)
    ;; Return the buffer
    newbuff))

;; Make sure that the inferior process buffer is visible
(defun ciao-show-inferior-process (procbuff)
  (let (origbuff (current-buffer))
    (ciao-switch-other-window procbuff)
    (ciao-switch-other-window origbuff)))

(defun ciao-proc-mode (cproc)
  "Simple initialization of a Ciao inferior mode (just
communication, no syntax, font-lock, menus, keybindings). This is
used as basis for more complex inferior modes."
  (comint-mode)
  ;; Unfortunately variable not in emacs-22 :-(
  (setq comint-highlight-prompt nil) ; avoid unwanted coloring
  (setq mode-line-process '(": %s"))
  (setq comint-input-filter 'ciao-input-filter)
  (let ((proc (get-buffer-process (current-buffer))))
    (set-process-filter proc (ciao-proc-filter cproc))
    (set-process-sentinel proc 'ciao-inferior-process-sentinel))
  ;; 
  (setq comint-prompt-regexp (ciao-proc-any-prompt-pattern cproc)))

(defun ciao-input-filter (str)
  ;; putting "[ \t]*" instead of " *" breaks in xemacs...
  (cond ((string-match "\\`\\s *\\'" str) nil) ;whitespace
	((not (eq major-mode 'ciao-inferior-mode)) t)
	((= (length str) 1) nil)	;one character
	((string-match "\\`[rf][ \t]*[0-9]*\\'" str) nil) ;r(edo) or f(ail)
	(t t)))

;; (invoked from the process filter)
(defun ciao-proc-if-prompt-run-queue (cproc string)
  (let ((marker-acc (ciao-proc-marker-acc cproc))
	(prompt-pattern (ciao-proc-prompt-pattern cproc)))
    ;; (message "JF: (%S) prompt pattern is %S" cproc prompt-pattern)
    (set marker-acc (concat (symbol-value marker-acc) string))
    (if (or 
	 (string-match prompt-pattern
		       (symbol-value marker-acc))
	 (and (eq cproc 'ciaopp-cproc) ;; Special case ;; TODO: why?
	      (string-match "\nCiao Listener finished"
			    (symbol-value marker-acc))))
	(progn
	  ;; We found a prompt then remove it from accumulator so don't call
	  ;; again hook.
	  ;; Wrong. Search until last \n or \n\\?-
	  ;; (message "JF: prompt found")
	  (set marker-acc (substring (symbol-value marker-acc)
				     (match-end 0)))
	  (ciao-proc-run-queue-next cproc)))))

;; Execute whenever there is a prompt, which means that the Ciao
;; process is idle waiting for more commands.
(defun ciao-proc-run-queue-next (cproc)
  (let ((queue (ciao-proc-queue cproc))
	(endloop nil)
	hook)
    ;; Run hooks in queue, stop at each 'prompt-wait-mark
    (while (and (not endloop)
		(not (ciao-queue-empty queue)))
      ;; (message "queue hook looks like %S" (symbol-value queue))
      (setq hook (ciao-queue-dequeue queue))
      (if (eq hook 'prompt-wait-mark)
	  ;; Do not run more hooks, wait for a prompt.
	  ;; Note: we assume that the previous hook will send some
	  ;; command to the inferior Ciao process. That command will
	  ;; finish with a prompt. Once the prompt is captured, the
	  ;; next hook will be executed.
	  (setq endloop t)
	;; Run the hook and loop
	;; (message "Running source hook %S" hook)
	(funcall hook)))
    ))

;(public)
(defun ciao-proc-enqueue-nw (cproc hook)
  "Enqueue `hook' in the command queue of Ciao process
`cproc'. The hook runs entirely on the emacs side, thus it does
not require to wait for a prompt on the Ciao toplevel side before
executing the next command."
  (let ((queue (ciao-proc-queue cproc)))
    (ciao-queue-enqueue queue hook)))

;(public)
;; TODO: This should not be necessary (see ciao-loading.el)
(defun ciao-proc-enqueue-nw-once (cproc hook)
  "Like `ciao-proc-enqueue-nw', but makes sure that `hook' only
appears once."
  (let ((queue (ciao-proc-queue cproc)))
    (if (member hook (symbol-value queue))
	;; (message "Already in queue %S" hook)
	t ; Already in the queue
      (ciao-queue-enqueue queue hook))
    ;;(message "queue hook looks like %S" (symbol-value queue))
    ))

;(public)
(defun ciao-proc-enqueue-w (cproc hook)
  "Enqueue `hook' in the command queue of Ciao process
`cproc'. The hook must send exactly one command to the Ciao
toplevel side. The command queue execution will wait for a prompt
on the Ciao toplevel side before executing the next command."
  (let ((queue (ciao-proc-queue cproc)))
    (ciao-queue-enqueue queue hook)
    (ciao-queue-enqueue queue 'prompt-wait-mark)))

(defun ciao-proc-reset-queue (cproc)
  "Reset the command queue of the Ciao process `cproc'"
  (ciao-queue-clear (ciao-proc-queue cproc))
  (set (ciao-proc-marker-acc cproc) ""))

;; ---------------------------------------------------------------------------

;; TODO: Document
(defun ciao-inferior-process-sentinel (proc msg)
  (cond
   ((null (buffer-name (process-buffer proc)))
    ;; buffer killed (e.g. C-c k)
    (ciao-reset-inferior)
    (set-process-buffer proc nil))
   ((memq (process-status proc) '(signal exit))
    ;; process terminated
    (ciao-reset-inferior)
    (ciao-fix-mode-line proc msg)
    )))

(defun ciao-reset-inferior ()
  ;; Need to reload certain things if needed.
  (ciao-reset-loading-state)
  (ciao-reset-error-state)
  (setq ciao-debug-filter-pending-text "")

  ;; Stop displaying an arrow in a source file.
  (ciao-debug-remove-marks)

  ;; Reset stuff needed for prompt hook in ciao, ciaopp and lpdoc
  ;; TODO: probably we should not reset all the processes (just one)
  (ciao-proc-reset-queue-all))

;; TODO: Document
(defun ciao-fix-mode-line (proc msg)	 
  ;; Fix the mode line.
  (setq mode-line-process
	(concat ":"
		(symbol-name (process-status proc))))
  (let* ((obuf (current-buffer)))
    ;; save-excursion isn't the right thing if
    ;;  process-buffer is current-buffer
    (unwind-protect
	(progn
	  ;; Write something in *compilation* and hack its mode line,
	  (set-buffer (process-buffer proc))
	  (force-mode-line-update)
	  (if (eobp)
	      (insert ?\n mode-name " " msg)
	    (save-excursion
	      (goto-char (point-max))
	      (insert ?\n mode-name " " msg)))
	  ;; If buffer and mode line will show that the process
	  ;; is dead, we can delete it now.  Otherwise it
	  ;; will stay around until M-x list-processes.
	  (delete-process proc))
      ;; Restore old buffer, but don't restore old point
      ;; if obuf is the gud buffer.
      (set-buffer obuf))))

;; ---------------------------------------------------------------------------

;; Had to do this differently because of filenames with blanks...
;; (defun ciao-get-string-before-blank  (string)
;;   (if (string-match " " string) 
;;       (substring string 0 (string-match " " string))
;;   string))
;; 
;; (defun ciao-get-string-after-blank  (string)
;;   (if (string-match " " string) 
;;       (substring string (+ (string-match " " string) 1) nil)
;;   nil))

(defun ciao-proc-alive (cproc)
  "The Ciao process `cproc' is a living process (see
`comint-check-proc')"
  (let ((procbuffer (ciao-proc-get-buffer cproc)))
    (not (eq (comint-check-proc procbuffer) nil))))

;; General interface to subprocess
(defun ciao-send-command (cproc command recenter-opt)
  ;; remember the buffer we are at
  (let ((procbuffer (ciao-proc-get-buffer cproc))
	(origbuffer (current-buffer)))
    (save-some-buffers) ;; TODO: This is not always necessary
    (if (not (ciao-proc-alive cproc))
	;; The buffer or process may not exist
	(progn
	  (setq procbuffer (ciao-start-inferior-process cproc))
	  (ciao-show-inferior-process procbuffer)
	  ;; Send command using the hook
	  (ciao-proc-enqueue-w
	   cproc
	   `(lambda ()
	      (ciao-do-send-command ,origbuffer ,procbuffer ,command nil))))
      ;; Send the command directly
      (ciao-do-send-command origbuffer procbuffer command recenter-opt))
    ;; MH Added to improve tracking of last inferior buffer used.
    (setq ciao-last-process-buffer-used procbuffer)
    (setq ciao-last-process-cproc cproc)))

;; TODO: Do not switch buffers here (define other command to do it)
(defun ciao-do-send-command (origbuffer procbuffer command recenter-opt)
  (let ((at-menu nil))
    (if (string= (buffer-name) ciao-ciaopp-gmenu-buffer-name)
	(progn
	  ;; (set-buffer procbuffer)
	  (switch-to-buffer procbuffer)
	  (setq at-menu t))
      (ciao-switch-other-window procbuffer))
    ;; (set-buffer procbuffer)
    (goto-char (point-max))
    (if (eq recenter-opt t) 
	(recenter 0))
    ;; Send the command
    (insert command)
    ;; (setq comint-process-echoes t)
    (comint-send-input nil t)
    (if at-menu
	; ()
	(switch-to-buffer ciao-ciaopp-gmenu-buffer-name)
      (ciao-switch-other-window origbuffer))
    ))

;; MH Alternative (but doesn't work?)
;; (defun ciao-send-command (buffername command)
;;   (comint-proc-query buffername command))

;; ---------------------------------------------------------------------------
;; Inferior modes with logos
;;
;; TODO: Move to the higher-level part? (probably to font-lock as an
;; overlay?)
;; ---------------------------------------------------------------------------

(require 'ciao-aux) ; ciao-insert-image

;; Mainly used to start toplevels (it shows a logo and it shows some
;; verbose messages to let the user know when the process has started)
(defun ciao-ensure-inferior-process (cproc)
  "Ensure there is a buffer running an inferior Ciao process of
type `cproc'. Return the buffer."
  (let ((procbuffer (ciao-proc-get-buffer cproc)))
    (if (ciao-proc-alive cproc)
	;; Already a process, do nothing
	t
      ;; Start a new process and show the logo
      (message "Starting %s... " (ciao-proc-desc cproc))
      (setq procbuffer (ciao-start-inferior-process cproc))
      (ciao-proc-enqueue-w cproc 'ciao-ensure-inferior-done))
    procbuffer))

;; Show the logo and print the 'done' message
(defun ciao-ensure-inferior-done ()
  (let ((cproc ciao-last-process-cproc))
    (ciao-insert-logo cproc
		      (ciao-proc-get-buffer cproc))
    (message "Starting %s... done" (ciao-proc-desc cproc))))

(defun ciao-insert-logo (cproc procbuffer)
  "Insert the logo the Ciao program development system at the
beginning of the current buffer (or just after the listener
finish message)."
  (set-buffer procbuffer)
  ;; If running in a graphical system, show the logo at the right
  ;; place (note that the toplevel process may be already running).
  (if window-system
      (progn
	(goto-char (point-max))
	(if (re-search-backward
	     "^Ciao Listener finished" (point-min) t)
	    (next-line 1) ; where last listener finished
	  (goto-char (point-min))) ; beginning of buffer
	;; Show the logo.
	(open-line 2) ; (open-line 3)
	(next-line 1)
	(ciao-insert-image 'png (ciao-proc-logo cproc) "Ciao")
	))
  ;; Move to the last point
  (goto-char (point-max)))


;; Provide ourselves:

(provide 'ciao-process)

;;; ciao-process.el ends here

