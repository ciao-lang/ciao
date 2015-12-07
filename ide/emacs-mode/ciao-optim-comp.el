;;; ciao-optim-comp.el --- Interface for Ciao optim comp
;; Copyright (C) 1986-2012 Free Software Foundation, Inc.

;; Authors: 2012      Jose F. Morales <jfran@clip.dia.fi.upm.es>

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

;;===========================================================================
;;
;; **Experimental** interface for commands and functions for optim_comp.
;;
;; TODO: To a smooth merge of this functionality.
;; TODO: This is too complex, with too many options. Integrate with bundles.

(defun ciao-get-optim-comp-proc-buffer ()
  (get-buffer-create "*Ciao Optim Comp*"))

;; TODO: use ciao-builder-command, make sure that bash-env is not needed
(defun ciao-optim-comp-command (cmd)
  "Execute the `cmd' ciao command"
  (async-shell-command
   (concat
    "eval `INSIDE_EMACS=t "
    ciao-bin-dir "/ciao oc:bash-env`; INSIDE_EMACS=t "
    ciao-bin-dir "/ciao " cmd)
   (ciao-get-optim-comp-proc-buffer)))

;;;###autoload
(defun ciao-optim-comp-build ()
  "Build the complete (optim-comp) system (build-comp and build-all)"
  (interactive)
  (ciao-optim-comp-command "oc:build"))

;;;###autoload
(defun ciao-optim-comp-build-all ()
  "Build (optim-comp) loader and apps"
  (interactive)
  (ciao-optim-comp-command "oc:build-all"))

;;;###autoload
(defun ciao-optim-comp-build-loader ()
  "Build the (optim-comp) dynamic executable loader"
  (interactive)
  (ciao-optim-comp-command "oc:build-loader"))

;;;###autoload
(defun ciao-optim-comp-build-cmds ()
  "Build misc (optim-comp) commands (ciaosh, ...)"
  (interactive)
  (ciao-optim-comp-command "oc:build-cmds"))

;;;###autoload
(defun ciao-optim-comp-build-comp ()
  "Build the (optim-comp) compiler"
  (interactive)
  (ciao-optim-comp-command "oc:build-comp"))

;;;###autoload
(defun ciao-optim-comp-build-comp-js ()
  "Build the (optim-comp) JS backend compiler)"
  (interactive)
  (ciao-optim-comp-command "oc:build-comp-js"))

;;;###autoload
(defun ciao-optim-comp-js-check-all ()
  "JS backend regression: that all modules are compiled correctly"
  (interactive)
  (ciao-optim-comp-command "oc:js-backend check-all"))

;;;###autoload
(defun ciao-optim-comp-js-regr-all ()
  "JS backend regression: check that all tests modules produce the correct output"
  (interactive)
  (ciao-optim-comp-command "oc:js-backend regr-all"))

;;;###autoload
(defun ciao-optim-comp-tests-full ()
  "optim-comp regression: Full tests (comp + inccomp + (build-all) + runexec + check)"
  (interactive)
  (ciao-optim-comp-command "oc:tests full"))

;;;###autoload
(defun ciao-optim-comp-tests-comp ()
  "optim-comp regression: Compiler check"
  (interactive)
  (ciao-optim-comp-command "oc:tests comp"))

;;;###autoload
(defun ciao-optim-comp-tests-briefcompare ()
  "optim-comp regression: Show a summary of compilation results that differs"
  (interactive)
  (ciao-optim-comp-command "oc:tests briefcompare"))

;;;###autoload
(defun ciao-optim-comp-tests-compare ()
  "optim-comp regression: Compare saved results (invoking @app{meld} for each pair of files)"
  (interactive)
  (ciao-optim-comp-command "oc:tests compare"))

;;;###autoload
(defun ciao-optim-comp-tests-save ()
  "optim-comp regression: Save results"
  (interactive)
  (ciao-optim-comp-command "oc:tests save"))

;; ---------------------------------------------------------------------------
;; (JS backend)

;; TODO: Ask Ciao what are the platforms
(defvar ciao-optim-comp-js-platforms
  '("nodejs"
    "v8"
    "safari"
    "chrome"
    "firefox"
    "opera"))

(defcustom ciao-optim-comp-js-platform "chrome"
  "Name of default platform used by JS backend."
  :group 'ciaocore
  :type (ciao-completion-choice
	 ciao-optim-comp-js-platforms))

;;;###autoload
(defun ciao-optim-comp-set-js-platform () 
  "Change the default platform used by the JS backend"
  (interactive)
  (ciao-completing-read 'ciao-optim-comp-js-platform
			"Change default platform used by JS backend?"
			ciao-optim-comp-js-platforms))

;;;###autoload
(defun ciao-optim-comp-js-compile-buffer ()
  "optim-comp: compile buffer to JS"
  (interactive)
  (ciao-optim-comp-command
   (concat "oc:js-backend --target-platform "
	   ciao-optim-comp-js-platform
	   " check-exec "
	   (buffer-file-name))))

;;;###autoload
(defun ciao-optim-comp-js-compile-and-run-buffer ()
  "optim-comp: compile buffer to JS and run on browser"
  (interactive)
  (ciao-optim-comp-command
   (concat "oc:js-backend --target-platform "
	   ciao-optim-comp-js-platform
	   " try-exec "
	   (buffer-file-name))))

;;;###autoload
(defun ciao-optim-comp-js-run-buffer ()
  "optim-comp: compile buffer to JS and run on browser"
  (interactive)
  (ciao-optim-comp-command
   (concat "oc:js-backend --target-platform "
	   ciao-optim-comp-js-platform
	   " run-exec "
	   (buffer-file-name))))


;; Provide ourselves:

(provide 'ciao-optim-comp)

;;; ciao-optim-comp.el ends here

