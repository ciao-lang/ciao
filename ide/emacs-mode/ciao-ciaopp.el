;;; ciao-ciaopp.el --- CiaoPP Interface

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

(require 'comint) ; comint-send-input, comint-output-filter

(require 'ciao-common) ; ciao-ciaopp-gmenu-buffer-name
(require 'ciao-process) ; ciao-proc-enqueue-w,
			; ciao-send-command, ciao-proc-get-buffer,
			; ciao-proc-prompt-pattern,
			; ciao-proc-if-prompt-run-queue
(require 'ciao-loading) ; ciao-last-source-buffer-used,
			; ciao-last-run-any-errors,
			; ciao-find-last-run-errors,
			; ciao-launch-find-last-run-errors-from-orig-buffer,
			; ciao-unmark-last-run-errors,
			; ciao-locate-errors-after-run
(require 'ciao-widgets)
(require 'ciao-aux) ; ciao-switch-this-window,
		    ; ciao-switch-other-window

;;------------------------------------------------------------
;; Preprocess buffer (including CiaoPP graphical menu)
;;------------------------------------------------------------

(defcustom ciao-ciaopp-use-graphical-menu t
  "If set, an interactive graphical menu is used for controlling
CiaoPP, instead of asking ascii questions in the CiaoPP buffer."
  :group 'ciaopp
  :type 'boolean)

;; (defun ciao-preprocess-buffer-menu ()
;;   "Preprocess the buffer, selecting options. Instructs the
;; preprocessor to load the current buffer and start an interactive
;; dialog in which the different options available in the preprocessor
;; can be set. "
;;   (interactive)
;;   (ciao-do-preprocess-buffer 'menu nil))
;; 
;; (defun ciao-preprocess-buffer ()
;;   "Preprocess the buffer, using the previously selected options. If no
;; options were set previously, then the preprocessor defaults are used."
;;   (interactive)
;;   (ciao-do-preprocess-buffer 'nomenu nil))
;; 
;; (defun ciao-preprocess-buffer-and-show-output ()
;;   "Preprocess the buffer, using the previously selected (or default)
;; options, waits for preprocessing to finish and displays the
;; preprocessor output (leaving the cursor at the same point if already
;; on a preprocessor output file). This allows running the preprocessor
;; over and over and watching the output while modifying the source
;; code."
;;   (interactive)
;;   (ciao-do-preprocess-buffer 'nomenu t))
;; 
;; (defun ciao-check-types-modes ()
;;   "Uses the preprocessor to perform compile-time checking of types and
;; modes (pptypesfd and shfr analyses). "
;;   (interactive)
;;   (message "Checking types and modes... ")
;;   (ciao-do-preprocess-buffer 'typesmodes nil))

;;;###autoload
(defun ciao-analyze-buffer ()
  "Call the preprocessor to perform a number of pre-selected analyses
on the current buffer (and related modules)."
  (interactive)
  (message "Analyzing buffer... ")
  (ciao-do-preprocess-buffer 'analyze t))

;;;###autoload
(defun ciao-load-and-check-buffer ()
  "Load CiaoPP and then the current buffer (and any auxiliary files it
may use) into the top level. Use CiaoPP auto_check_assrt predicate to
check current buffer assertions and then load the buffer if there was
no error."
  (interactive)
  (ciao-unmark-last-run-errors)
  (ciao-check-assertions)
  (ciao-ciaopp-continuation-hooks-special))

;;;###autoload
(defun ciao-check-assertions ()
  "Call the preprocessor to perform compile-time checking of the
assertions (types, modes, determinacy, nonfailure, cost, ...) in the
current buffer (and against those in related modules)."
  (interactive)
  (message "Checking assertions... ")
  (ciao-do-preprocess-buffer 'checkassrt nil))

;;;###autoload
(defun ciao-optimize-buffer ()
  "Uses the preprocessor to perform optimizations (partial evaluation,
abstract specialization, parallelization, ...) on the current
buffer (and related modules)."
  (interactive)
  (message "Optimizing buffer... ")
  (ciao-do-preprocess-buffer 'optimize t))

;;;###autoload
(defun ciao-browse-preprocessor-options ()
  "Browse and select (using the preprocessor menus) the actions to be
performed by the preprocessor when performing analisys used by
\\<ciao-mode-map> \\[ciao-] \\[ciao-analyze-buffer],
\\[ciao-check-assertions], \\[ciao-optimize-buffer], and the
corresponding toolbar buttons."
  (interactive)
  (message "Browsing preprocessor options... ")
  (ciao-remember-last-menu-key  -1)
  (setq ciao-ciaopp-prog-lang 0)
  (ciao-do-preprocess-buffer 'customize t))

;; DTM: Added this new "hook"
(defvar ciao-ciaopp-gmenu-hook nil
  "Things to do in CiaoPP once a ciaopp menu string is found in
the CiaoPP buffer.")

(defun ciao-do-preprocess-buffer (action showoutput)
  "Main function to call the preprocessor. Implements the others via options."
  (message "Preprocessing buffer... ")
  (if (not (string= (buffer-name (current-buffer)) ciao-ciaopp-gmenu-buffer-name)) 
      (progn
;	(setq ciao-gm-recovering 0)
	(setq ciao-last-source-buffer-used (current-buffer))
	(setq ciao-g-showoutput showoutput)))
  (setq ciao-gm-recovering 0)
  (ciao-unmark-last-run-errors)
  (ciao-send-command 'ciaopp-cproc
   ;; Command
   (cond
;   ((eq action 'menu)        (ciao-build-ciaopp-command "[]"))
;   ((eq action 'nomenu)      (ciao-build-ciaopp-command nil ))
;   ((eq action 'typesmodes)  (ciao-build-ciaopp-specific-command "ctcheck"))
    ; a) check assertions
    ((eq action 'checkassrt)  (ciao-build-ciaopp-specific-command 
			       "auto_check_assert"))
    ; b) analyze
    ((eq action 'analyze)     (ciao-build-ciaopp-specific-command
			       "auto_analyze"))
    ; c) optimize
    ((eq action 'optimize)    (ciao-build-ciaopp-specific-command
			       "auto_optimize"))
    ; d) browse options, using graphical menu
    ((and ciao-ciaopp-use-graphical-menu
	  (or
	   (eq action 'customize)
	   (eq action 'customize-no-set-hook)))
     (if (eq ciao-ciaopp-prog-lang 0)           
	 "customize(all)."
       "customize_java(all)."
       )
     )

    ; e) browse options, not using graphical menu 
    ; this action is meant to be called only from the OK button hook
    ; of the graphical menu AND only after having executed a customize
    ; action
    ((or (eq action 'customize-and-exec) 
	  (and (eq action 'customize)
	       (not ciao-ciaopp-use-graphical-menu)))

      (if (not (eq ciao-last-source-buffer-used nil))

	  (if (eq ciao-ciaopp-prog-lang 0)           
	    (concat "customize_and_preprocess('" 
		  (buffer-file-name ciao-last-source-buffer-used) "').")
 
	    (concat "customize_and_preprocess_java('" 
		  (buffer-file-name ciao-last-source-buffer-used) "').")
	    )
	(error "INTERNAL ERROR: cannot find the last source buffer used!")
	nil))

    ; f) failsafe default: return nil (empty command)    
    (t nil))
    ; last argument of ciao-send-command: recenter = true
   t)

  (if ciao-ciaopp-use-graphical-menu
      (if (eq action 'customize-and-exec)
	  (progn 
	    (setq ciao-widget-str-list   nil)
	    (setq ciao-widget-id           0)
	    (setq ciao-ciaopp-gmenu-hook 
		  'ciao-ciaopp-process-gmenu-just-write-answers))
	(if (or (eq action 'customize) (eq action 'customize-no-set-hook))
	    (progn 
	      (setq ciao-widget-str-list   nil)
	      (setq ciao-widget-id           0)
	  
	      (if (eq action 'customize)
		  (progn
		    (setq ciao-ciaopp-gmenu-hook 
			  'ciao-ciaopp-process-graphical-menu)
		    (ciao-proc-enqueue-w 'ciaopp-cproc 'ciao-ciaopp-show-graphic-menu))))
	  (ciao-ciaopp-continuation-hooks showoutput)))
    ; this means: if cannot use graphical menu, then put the hooks
    ; (switch-to-buffer (ciao-proc-get-buffer 'ciaopp-cproc))
    (ciao-ciaopp-continuation-hooks showoutput)))

;;;###autoload
(defun java-browse-preprocessor-options ()
  "Browse and select (using the preprocessor menus) the actions to be
performed by the preprocessor when performing analisys used by the
corresponding toolbar buttons."
  (interactive)
  (message "Browsing preprocessor options... ")
  (ciao-remember-last-menu-key  -1)
  (setq ciao-ciaopp-prog-lang 1 )
  (message "Sets the ciao-ciaopp-prog-lang variable to Java")
  (ciao-do-preprocess-buffer 'customize t)
  )

(defun ciao-toggle-ciaopp-use-graphical-menu ()
  "Toggle graphical/textual menu."
  (interactive)
  (setq ciao-ciaopp-use-graphical-menu
	(if ciao-ciaopp-use-graphical-menu
	    (progn
	      (message "CiaoPP option browsing will use text-based menus")
	      nil)
	  (message "CiaoPP option browsing will use graphical menus")
	  t)))

;;; **** Needs fixing properly.
(defun ugly-filter (line-str)
  "Total kludge to compensate for emacs bug in mac: include text in wrong place"
  (replace-regexp-in-string "customize_java(all)." "" 
			    (replace-regexp-in-string "customize(all)." "" line-str)))

(defun ciao-ciaopp-process-graphical-menu (line-str-orig)
  (setq line-str (ugly-filter line-str-orig))
  ;; (message "*** After filter: " line-str)
  (let* ( (new-line (concat ciao-widget-str-list line-str))
	  widget-mark
	  default_opt)
    
    (if (<= ciao-widget-id ciao-widget-last-changed-option)
	(setq default_opt (ciao-get-graphic-menu-option ciao-widget-id))
      (setq default_opt nil))
    
    (save-excursion
      ; we do have to create the buffer (INIT process)
      (if (and (eq ciao-widget-id 0)
	       (is_a_menu new-line)
;;	       (eq (ciao-proc-get-buffer 'ciaopp-cproc) nil)
	       )
	  (ciao-create-widgets-buffer)
	(set-buffer (get-buffer-create ciao-ciaopp-gmenu-buffer-name)))
      
      (goto-char (point-max))
      (setq widget-mark 
	    (ciao-create-widget ciao-widget-id new-line default_opt))
      (setq ciao-widget-str-list (substring new-line widget-mark)))
    
      ; TRICK: As menu is always waiting for an input, menu is always
      ; the last thing we can expect
      (if (is_a_menu new-line)
	  (let ( (def_opt (ciao-get-toplevel-menu-default-option 
			   (match-string 3 new-line))) )
		; we do have to rewrite the options because in the new pass
	        ; they do not have meaning anymore. For example:
	        ; (Format: QX-> Question X. AY -> AnswerY (Widget ID))
	        ; QA: AA (1)
		; QB: AB (2)
	        ; QC: AC (3)
	        ; if we change AB (Answer B), with the widget id 2, on the 
	        ; new menu, 3 will be probably QD, that is not QC.

	    (goto-char (point-max))
	  
	    (if (and (eq ciao-gm-recovering 0)
		     (<= ciao-widget-id ciao-widget-last-changed-option))
		(insert (ciao-remove-all-spaces default_opt))
	      (ciao-change-graphic-menu-option ciao-widget-id def_opt))
	      
;; 	      (prin1 (list "*** Current id:" ciao-widget-id " last id: "
;;                      ciao-widget-last-changed-option 
;; 		      " values: " ciao-widget-values " Rec: "
;;                      ciao-gm-recovering "\n")) 

	    (setq ciao-widget-id (1+ ciao-widget-id))
	    (comint-send-input)))))

(defun ciao-ciaopp-process-gmenu-just-write-answers (line-str-orig)
  (setq line-str (ugly-filter line-str-orig))
  (let* ((new-line       (concat ciao-widget-str-list line-str))
	 widget-mark
	 default_opt)

 ; TRICK: As menu is always waiting for an input, menu is always the
 ; last thing we can expect (Note *other things* are notes or errors
    (if (is_a_menu new-line)
	(let ( (the_mark (match-end 3))
	       (def_opt (ciao-get-toplevel-menu-default-option 
			 (match-string 3 new-line))))

	  (setq ciao-widget-str-list 
		(substring new-line (+ 3 the_mark)))
		; we do have to rewrite the options because in the new pass
	        ; they dont have meaning anymore. For example:
	        ; (Format: QX-> Question X. AY -> AnswerY (Widget ID))
	        ; QA: AA (1)
		; QB: AB (2)
	        ; QC: AC (3)
	        ; if we change AB (Answer B), with the widget id 2, on the 
	        ; new menu, 3 will be probably QD, that is not QC.

	  (goto-char (point-max))
	  
	  (if (<= ciao-widget-id ciao-widget-last-changed-option)
	      (insert (ciao-remove-all-spaces 
		(ciao-get-graphic-menu-option ciao-widget-id)))
	    (insert nil))
	  
	  (setq ciao-widget-id (1+ ciao-widget-id))

	  ; if we have answered all the questions => Stop the hooks
	  (if (eq ciao-widget-id ciao-widget-last-changed-option)
	      (progn
		; *** (switch-to-buffer-other-window (ciao-proc-buffer 'ciaopp-cproc)) 
		(set-buffer (ciao-proc-get-buffer 'ciaopp-cproc))
		(kill-buffer ciao-ciaopp-gmenu-buffer-name)
		(setq ciao-ciaopp-gmenu-hook nil)
		(setq ciao-cancel-widget-values nil)

		(ciao-ciaopp-end-graphic-menu)
		(comint-send-input))
	    (comint-send-input)))
      (setq ciao-widget-str-list new-line))))

(defun ciao-ciaopp-show-graphic-menu ()
  "Shows the graphic ciaopp menu process hook"
  (setq ciao-ciaopp-gmenu-hook nil)

  (if (eq (window-buffer (selected-window)) 
	  (ciao-proc-get-buffer 'ciaopp-cproc))
      (switch-to-buffer ciao-ciaopp-gmenu-buffer-name)
    (set-buffer     (get-buffer-create ciao-ciaopp-gmenu-buffer-name)) 
    (display-buffer (get-buffer-create ciao-ciaopp-gmenu-buffer-name)) 

    (if (eq ciao-cancel-widget-values nil)
	(progn
	  (setq ciao-cancel-widget-values 
		(copy-sequence ciao-widget-values)))))
  (goto-char (point-max))
  (ciao-create-widgets-buttons)
  (ciao-run-widget-buffer))

(defun ciao-ciaopp-end-graphic-menu ()
  "Removes the graphic ciaopp menu process hook"
  (setq ciao-ciaopp-gmenu-hook nil)
  (ciao-ciaopp-continuation-hooks ciao-g-showoutput))

(defun ciao-ciaopp-continuation-hooks (showoutput)
  "Setting up hooks to run after CiaoPP command execution"
  (if showoutput
      (ciao-proc-enqueue-w 'ciaopp-cproc 'ciao-find-errors-or-show-output)
    (if ciao-locate-errors-after-run
	(ciao-proc-enqueue-w 'ciaopp-cproc 'ciao-launch-find-last-run-errors-from-orig-buffer))))

;; TODO: Does it make sense? It was in ciao-load-and-check-buffer
(defun ciao-ciaopp-continuation-hooks-special ()
  "Setting up hooks to run after CiaoPP command execution"
  (ciao-proc-enqueue-w 'ciaopp-cproc 'ciao-find-errors-or-show-output)
  (if ciao-locate-errors-after-run
      (ciao-proc-enqueue-w 'ciaopp-cproc 'ciao-launch-find-last-run-errors-from-orig-buffer)))

(defun ciao-widget-hook (widget &rest ignore)
  "This funcion is invoked whenever a widget is changed."
  (let* ((value     (widget-value widget))
	 (key       (widget-get   widget :widgetid)))
    (ciao-change-graphic-menu-option key value)
    (ciao-remember-last-menu-key     key)
  
    ; FOR TESTING!!!
    ; (if (eq ciao-gm-recovering 0)
    ;     (ciao-change-graphic-menu-option key "potatoe"))

    ; Now invokes the CiaoPP menu till the question numbered key
    (ciao-do-preprocess-buffer 'customize nil)))

(defun ciao-ok-button-widget-hook (widget &rest ignore)
  "This funcion is invoked when the OK button is released."

   ; Remember the number of answers we have to write
   (ciao-remember-last-menu-key ciao-widget-id)
 
   ; Now invokes the CiaoPP menu until the question numbered key
   (ciao-do-preprocess-buffer 'customize-and-exec nil))


(defun ciao-cancel-button-widget-hook (widget &rest ignore)
  "This funcion is invoked when the OK button is released."
  
  ; Remember the number of answers we have to write
  (ciao-remember-last-menu-key ciao-widget-id)
  
  (setq ciao-ciaopp-gmenu-hook
	'ciao-ciaopp-process-gmenu-just-write-answers)
  
  ; Remember the number of answers we have to write
  (ciao-remember-last-menu-key (length ciao-cancel-widget-values))

  ; Restore the values of the menu before starting to play with it
  (setq ciao-widget-values (copy-sequence ciao-cancel-widget-values))
  
  ; Now invokes the CiaoPP menu till the question numbered key
  (ciao-do-preprocess-buffer 'customize-no-set-hook nil))

(defun ciao-change-graphic-menu-option (key newvalue)
  "We have an alist in the variable ciao-widget-values. Whenever we
change a value, we create a new alist with the new key an newvalue and
save it in the same symbol."
  (let ((par (cons key newvalue)))
    (if (boundp 'ciao-widget-values)
	(setq ciao-widget-values
	      (cons par (assq-delete-all  key ciao-widget-values)))
      (setq ciao-widget-values (list par)))))

(defun ciao-get-graphic-menu-option (key)
  "Returns the value of the widget-id 'key'."
  (if (boundp 'ciao-widget-values)
      (cdr (assoc key ciao-widget-values))
    nil))

(defun ciao-remember-last-menu-key (key)
  (setq ciao-widget-last-changed-option key))

;; TODO: ciao-find-errors-or-load-buffer
(defun ciao-find-errors-or-show-output ()
  (switch-to-buffer ciao-last-source-buffer-used)
  (if (and ciao-locate-errors-after-run
	   (ciao-last-run-any-errors))
      (ciao-find-last-run-errors)
    (ciao-show-preprocessor-output)
    ;; In this case, probably best to go back to original buffer
    (ciao-switch-other-window ciao-last-source-buffer-used)))

;; The following are obsolete with new option browser.
;; 
;; (defun ciao-set-ciaopp-output-pred ()
;;   "Make ciaopp output only predicate-level analysis information."
;;   (interactive)
;;   (ciao-send-command 'ciaopp-cproc "dump_ai(pred)." t))
;; 
;; (defun ciao-set-ciaopp-output-full ()
;;   "Make ciaopp output both literal- and predicate-level analysis information."
;;   (interactive)
;;   (ciao-send-command 'ciaopp-cproc "dump_ai(yes)." t))
;; 
;; (defun ciao-set-ciaopp-output-none ()
;;   "Make ciaopp output no analysis information."
;;   (interactive)
;;   (ciao-send-command 'ciaopp-cproc "dump_ai(no)." t))

(defun ciao-build-ciaopp-command (options)
  (concat "precompile('" (buffer-file-name)
	  (if (string= options nil)
	      "')."
	    (concat "'," options ").") )))

(defun ciao-build-ciaopp-specific-command (command-name)
  (concat command-name "('" (buffer-file-name) "').") )

;;;###autoload
(defun ciao-show-preprocessor-output ()
  "Show last output file produced by Ciao preprocessor. The preprocessor
works by producing a file which is a transformed and/or adorned (with
assertions) version of the input file. This command is often used after
running the preprocessor in order to visit the output file and see the
results from running the preprocessor."
  (interactive)
  (let ((ciaoppbuff (ciao-proc-get-buffer 'ciaopp-cproc))
	(origbuffer (current-buffer)))
    (if (not ciaoppbuff)
	(message "Preprocessor buffer not active.")
      (ciao-switch-other-window ciaoppbuff)
      (let ((file (ciao-ciaopp-get-output-file)))
	(if file
	    (progn
	      (if (get-file-buffer file)
		  ;; The complication is to not complain if disk more recent!
		  (progn 
		    (switch-to-buffer (get-file-buffer file))
		    (let ((local-buff-point (point)))
		      (kill-buffer (get-file-buffer file))
		      (find-file file)
		      (goto-char local-buff-point)))
		(find-file file)
		))
	  (message "No output file written out by preprocessor.")
	  ;; If not output to visit, get cursor back to original buffer
	  (ciao-switch-other-window origbuffer))))))

;; TODO: Add a special parsing for CiaoPP or include that in
;;   ciao-parsing.el?

(defun ciao-ciaopp-get-output-file ()
  "Parse latest written output file from CiaoPP output"
  (save-excursion
    (let ((mbeg 0) (mend 0) (file nil))
      (goto-char (point-max))
      (move-to-column 0) ;; skip prompt if at prompt
      ;; (search-backward-regexp (ciao-proc-any-prompt-pattern 'ciaopp-cproc)
      ;;                         nil t)
      ;; It is safe (and more precise) to be more specific here:
      (search-backward-regexp (ciao-proc-prompt-pattern 'ciaopp-cproc)
			      nil t)
      (end-of-line)
      (if (search-forward-regexp "written file " nil t)
	  (progn
	    (setq mbeg (match-end 0))
	    (goto-char mbeg)
	    (search-forward-regexp "}")
	    (setq mend (match-beginning 0))
	    (setq file (buffer-substring-no-properties mbeg mend))
	    file)
	nil))))

(defun ciao-ciaopp-filter (proc string)
  ;; Here's where the actual buffer insertion is done
  (if (buffer-name (process-buffer proc))
      ;; Was (incorrectly) save-excursion (EG fix)
      ;; We must allow Ciao to affect the point so that we
      ;; return to the end of output.
;      (with-current-buffer (process-buffer proc)
      (save-current-buffer
	(set-buffer (process-buffer proc))
	(comint-output-filter proc string)
	(ciao-proc-if-prompt-run-queue 'ciaopp-cproc string)
	;; DTM: I need a new hook for the graphical menu :S
	(ciao-ciaopp-if-gmenu-run-hook string))))

(defun ciao-ciaopp-if-gmenu-run-hook (string)
    (if (not (equal ciao-ciaopp-gmenu-hook nil))
	  (funcall ciao-ciaopp-gmenu-hook string)))

;;===========================================================================

(require 'ciao-process) ; ciao-ensure-inferior-process

;;;###autoload
(defun ciaopp ()
  "Same as \\<ciao-mode-map> \\[run-ciao-preprocessor]."
  (interactive)
  (run-ciao-preprocessor))

;;;###autoload
(defun run-ciao-preprocessor ()
  "Ensure that an inferior Ciao preprocessor process is running. 

   This opens a preprocessor top-level window (if one did not exist
already) where preprocessing commands and preprocessing menu options
can be input directly. Programs can be preprocessed by typing commands
in this window, or, more typically, by opening the file to be
preprocessed in an emacs window (where it can be edited) and issuing a
command (such as \\<ciao-mode-map> \\[ciao-analyze-buffer],
\\[ciao-check-assertions], \\[ciao-optimize-buffer], or
\\[ciao-browse-preprocessor-options]) directly from there (see the
preprocessing commands of this mode and their bindings).

   Note that many useful commands (e.g., to repeat and edit previous
commands, interrupt jobs, locate errors, automatic completions, etc.)
are available in this top-level window (see @ref{Commands available in
toplevel and preprocessor buffers}).

   Often, it is not necessary to use this function since
execution of any of the other functions related to the top
level (e.g., loading buffers into the top level) ensures that a
top level is started (starting one if required)."
  (interactive)
  (ciao-ensure-inferior-process 'ciaopp-cproc)
  (ciao-switch-this-window (ciao-proc-get-buffer 'ciaopp-cproc)))


;; Provide ourselves:

(provide 'ciao-ciaopp)

;;; ciao-ciaopp.el ends here

