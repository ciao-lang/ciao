;;; ciao.el --- Ciao Mode for Emacs
;; Copyright (C) 1986-2012 Free Software Foundation, Inc. and
;; M. Hermenegildo and others (herme@fi.upm.es, UPM-CLIP, Spain).

;; Authors: 1986-2011 M. Hermenegildo <herme@fi.upm.es> and others 
;;          2012      M. Hermenegildo <herme@fi.upm.es> and 
;;                    Jose F. Morales <jfran@clip.dia.fi.upm.es>

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

;; ===========================================================================
;;        The major and inferior mode for the Ciao system and tools
;;        *********************************************************
;;
;; The description of the architecture of the mode is described
;; here. The mode is composed of several files with a precise
;; structure in several levels. In general, there are no dependencies
;; between modules in the same level, and level I+1 depends on some
;; modules of level I. The directed acyclic graph of the components
;; can be draw as follows:
;;
;; Level 0 -- Configuration and support code
;;
;;   ciao-common.el: common definitions (basically, the top group for ciao)
;;   ciao-aux.el: auxiliary and helper definitions
;;   ciao-config.el(.skel): configuration parameters from installation
;;   ciao-scratchpad.el: a scratchpad for temporary files
;;
;; Level 1 -- Syntax-related definitions (language and communication)
;;
;;   ciao-faces.el: faces for font-lock
;;   ciao-syntax.el: language and toplevel syntax definitions
;;
;; Level 2 -- Syntax highlight and process abstraction (emacs only)
;;
;;   ciao-font-lock.el: font-lock for the language
;;   ciao-process.el: core functionality to create inferior Ciao
;;                    processes
;;
;; Level 3 -- Parsing and more complex interaction (may use
;;            external processes)
;;   ciao-parsing.el: module parsing and navigation
;;   ciao-widgets.el: widget library (for graphical menus)
;; 
;; Level 4 -- Interaction with Ciao tools
;;
;;   ciao-loading.el: loading modules (based on ciao-process)
;;                    treatment of mainfile
;;   ciao-compile.el: compiling modules (based on ciao-process)
;;   ciao-debugger.el: debuging modules (based on ciao-process)
;;   ciao-testing.el: tesging modulescommands for testing modules
;;                    treatment of default query
;;   ciao-ciaopp.el: preprocessing modules (based on ciao-process)
;;   ciao-lpdoc.el: documenting modules (based on ciao-process)
;;   ciao-help.el: (info) manual search (based on word-help.el)
;;   ciao-vc.el: support for version control comments
;;
;; Level 5 -- All user interaction stuff
;;
;;   ciao-bindings.el: setup keybindings, tool bar, and menu bar of
;;                     most interactive Ciao commands offered at
;;                     previous levels
;;   ciao-splash.el: the splash screen
;;
;; Level 6 -- Definition of the Ciao modes (main file)
;;
;;   ciao.el: the Ciao major and inferior modes (enables bindings
;;            (keyboard, menu and tool bar), font-lock, syntactic
;;            definitions, inferior process communication, etc.
;;
;; Additionally, there is a separate file which is not linked when
;; using the Ciao mode, but useful during installation:
;;
;;   ciao-documentation.el: generate LPdoc documentation for mode
;;
;; Other modes are based on functionality described here:
;;
;;   java-ciaopp.el: mode for preprocessing of Java
;;
;; ===========================================================================

;; ---------------------------------------------------------------------------
;; Required packages
;; ---------------------------------------------------------------------------

;; (require 'calendar)
(require 'etags)

;; We use FSF Emacs overlays. XEmacs uses extents instead, but comes
;; with a package to emulate overlays.
(if (boundp 'xemacs-logo)
  (require 'overlay))

;; ---------------------------------------------------------------------------
;; The actual major and inferior modes
;; ---------------------------------------------------------------------------

;; Set load-path so that it points to the directory where this file is
;; stored.
;;;###autoload
(add-to-list 'load-path
             (or (file-name-directory load-file-name) (car load-path)))

;; All required
(require 'ciao-config)
(require 'ciao-common)
(require 'ciao-bindings) ; ciao-setup-bindings
(require 'ciao-syntax) ; ciao-syntax-mode-variables
(require 'ciao-font-lock) ; ciao-font-lock-defaults-create
(require 'ciao-debugger) ; ciao-debugger-mode-variables

;; TODO: Make it parametric to support corrent syntax for literate
;;   Ciao (.lpdoc) modules.
(defun ciao-mode-internal ()
  "Internal ininitialization of the Ciao major mode."
  (kill-all-local-variables)
  (setq major-mode 'ciao-mode)
  (setq mode-name "Ciao")
  (ciao-syntax-mode-variables)
  (ciao-setup-bindings)
  (ciao-font-lock-defaults-create)
  (ciao-debugger-mode-variables)    
  (run-hooks 'ciao-mode-hook))

(require 'ciao-process) ; ciao-proc-mode
(require 'ciao-font-lock) ; ciao-inferior-font-lock-defaults-create
(require 'ciao-debugger) ; ciao-debugger-mode-variables,
			 ; ciao-debugger-init
(require 'ciao-bindings) ; ciao-setup-inferior-bindings

(defun ciao-inferior-mode-internal (cproc)
  "Internal initialization of the Ciao inferior mode. The
argument `cproc' specifies the Ciao process to run."
  (if (not (eq major-mode 'ciao-inferior-mode))
      (progn
	(kill-all-local-variables)
	(ciao-proc-mode cproc)
	;;
	(setq major-mode 'ciao-inferior-mode)
	(setq mode-name "Ciao Listener")
	;;
	(ciao-syntax-mode-variables)
	(ciao-setup-inferior-bindings)
	(ciao-inferior-font-lock-defaults-create)
	(ciao-debugger-mode-variables)
	(ciao-debugger-init)
	(run-hooks 'ciao-inferior-mode-hook))))

;; Major Ciao mode
;;;###autoload
(defun ciao-mode ()
  "
   This is a major mode for 
   editing / debugging / documenting / compiling / running / ...
   Ciao code.

See the Ciao manuals (you can use \\<ciao-mode-map>
\\[ciao-goto-manuals]) for full information on the many features
of this mode.

The following is a summary of the keyboard commands available in the
mode (see also the mode-specific entries in the menu-bar if enabled):

\\{ciao-mode-map}

Entry to this mode calls the value of ciao-mode-hook if that value is
non-nil." 

  (interactive)
  (if (get-buffer-process (current-buffer))
      ; Exit if this is a process buffer (very likely an error)
      (message 
      "Ciao mode not for process buffers, use M-x ciao-inferior-mode instead.")
    (ciao-mode-internal)))

;; Associate file extensions to the Ciao mode

;;;###autoload(add-to-list 'auto-mode-alist '("\\.pl\\'" . ciao-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.pls\\'" . ciao-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.lpdoc\\'" . ciao-mode))

;;;###autoload(add-to-list 'completion-ignored-extensions ".itf")
;;;###autoload(add-to-list 'completion-ignored-extensions ".po")
;;;###autoload(add-to-list 'completion-ignored-extensions ".asr")
;;;###autoload(add-to-list 'completion-ignored-extensions ".ast")
;;;###autoload(add-to-list 'completion-ignored-extensions ".cpx")

(require 'ciao-process) ; ciao-cproc-for-buffer

;; Inferior Ciao mode
;;;###autoload
(defun ciao-inferior-mode ()
  "Inferior mode for interaction with the Ciao top-level.


This is a major emacs mode used in Ciao-related interactive
buffers, i.e., buffers in which it is possible to interact with
an inferior process running a Ciao top-level (including the
preprocessor).

You can talk to the top-level by typing commands directly in the
corresponding buffer as if in a normal shell. You can also send
files or parts of files to be preprocessed or compiled by the
processes running under this inferior mode from any buffer which
is in ciao-mode (see the emacs commands available in such
buffers).

All commands available in standard emacs shell packages (comint)
are available in these interactive buffers. In addition, there
are many new commands which are specific to this mode.  The
following is a list of all the available commands:

\\{ciao-inferior-mode-map}

Entry to this mode calls the value of ciao-inferior-mode-hook
with no arguments, if that value is non-nil.  Likewise with the
value of comint-mode-hook.  ciao-inferior-mode-hook is called
after comint-mode-hook.
"
  (interactive)
  (ciao-inferior-mode-internal (ciao-cproc-for-buffer)))

;; ==========================================================================

(require 'ciao-font-lock) ; ciao-emacs-can-do-font-lock-p

;; Turn on font lock (if not used globally)
(if (ciao-emacs-can-do-font-lock-p)
    (progn
      (add-hook 'ciao-mode-hook 'turn-on-font-lock)
      (add-hook 'ciao-inferior-mode-hook 'turn-on-font-lock)))

;; Specific to Windows installation
(if (equal (ciao-get-config :ciao-emacs-type) "Win32")
    (progn
      ;; Make things nicer (but check if you are already doing it)
      (global-font-lock-mode)
      (transient-mark-mode t)
      ;; Help for using the Windows command.com as your shell
      ;; (comment out if you use bash, etc.):
      (setq process-coding-system-alist '(("cmdproxy" . (raw-text-dos . raw-text-dos))))
      ;; Preventing ctrln-m's from being printed in the shell
      (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)))

;; ==========================================================================
;; Backward compatibility for Emacs 22

;; ;; (borrowed from ProofGeneral, proof-compat.el)
;; (or (fboundp 'declare-function)
;; ;; taken from Emacs 22.2, not present in 22.1:
;; (defmacro declare-function (&rest args)
;;   "In Emacs 22, does nothing.  In 23, it will suppress byte-compiler warnings.
;; This definition is so that packages may take advantage of the
;; Emacs 23 feature and still remain compatible with Emacs 22."
;;   nil))


;; Provide ourselves:

(provide 'ciao)

;;; ciao.el ends here

