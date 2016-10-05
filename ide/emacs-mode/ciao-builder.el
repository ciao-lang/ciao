;;; ciao-builder.el --- Interface to Ciao Builder
;; Copyright (C) 2012-2015 Jose F. Morales <jfran@clip.dia.fi.upm.es>

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
;; NOTE: Keep it simple (everything should be reproducible from the
;;   command line or toplevel)
;;
;; TODO:
;;   - Alpha state (it need many more commands (but not too many).
;;   - Entries in emacs menus and some documentation is missing.
;;   - Processes (like toplevels) need manual restart on update.
;;   - Remove dependencies from shell scripts and POSIX tools (as much
;;     as possible)
;;
;;===========================================================================

(defun ciao-get-builder-proc-buffer ()
  (get-buffer-create "*Ciao Builder Process*"))

;; TODO: define 'ciao-builder-build-all'?
;; TODO: define 'ciao-builder-update'? (should it download anything?)

(defun ciao-builder-cmdstr (cmd)
  (concat "INSIDE_EMACS=t " (ciao-get-config :ciaosh-bin) " " cmd))

(defun ciao-builder-command (cmd)
  "Execute the `cmd' ciao builder command"
  (async-shell-command
   (ciao-builder-cmdstr cmd)
   (ciao-get-builder-proc-buffer)))

;; TODO: Missing sub- and special targets (e.g., ciaobase, core/engine, etc.)
(defun ask-bundle (msg)
  (let
      ((bundle-list
	(split-string
	 (ciao-trim-end
	  (shell-command-to-string (ciao-builder-cmdstr "list")))
	 "\n")))
    (completing-read
     msg
     bundle-list
     nil t "")))

(defun ciao-trim-end (str)
  (replace-regexp-in-string (rx (* (any " \t\n")) eos) "" str))

;;;###autoload
(defun ciao-info () 
  "Show info about the specified Ciao bundle"
  (interactive)
  (let 
      ((target (ask-bundle "Show info about the specified Ciao bundle: ")))
    (ciao-builder-command (concat "info " target))))

;;;###autoload
(defun ciao-build () 
  "(Re)Build the specified Ciao bundle"
  (interactive)
  (let 
      ((target (ask-bundle "(Re)Build the specified Ciao bundle: ")))
    (ciao-builder-command (concat "build " target))))

;;;###autoload
(defun ciao-build-nodocs () 
  "(Re)Build the specified Ciao bundle (no docs)"
  (interactive)
  (let 
      ((target (ask-bundle "(Re)Build the specified Ciao bundle (no docs): ")))
    (ciao-builder-command (concat "build_nodocs " target))))

;;;###autoload
(defun ciao-build-docs () 
  "(Re)Build the specified Ciao bundle (only docs)"
  (interactive)
  (let 
      ((target (ask-bundle "(Re)Build the specified Ciao bundle (only docs): ")))
    (ciao-builder-command (concat "build_docs " target))))

;;;###autoload
(defun ciao-install () 
  "Install the specified Ciao bundle"
  (interactive)
  (let 
      ((target (ask-bundle "Install the specified Ciao bundle: ")))
    (ciao-builder-command (concat "install " target))))

;;;###autoload
(defun ciao-uninstall () 
  "Uninstall the specified Ciao bundle"
  (interactive)
  (let 
      ((target (ask-bundle "Uninstall the specified Ciao bundle: ")))
    (ciao-builder-command (concat "uninstall " target))))

;;;###autoload
(defun ciao-clean () 
  "Clean the specified Ciao bundle"
  (interactive)
  (let 
      ((target (ask-bundle "Clean the specified Ciao bundle: ")))
    (ciao-builder-command (concat "clean " target))))

;; TODO: provide a minor mode for this; add "info"
;;;###autoload
(defun ciao-list-bundles () 
  "List the available bundles."
  (interactive) 
  (ciao-builder-command "list"))

;; ---------------------------------------------------------------------------
;; Grep on bundles source
;; TODO: only works in instype=local installation
;; TODO: add tags-search

;; TODO: only works in instype=local installation
(defun ciao--root-dir ()
  "Guess a value for CIAOROOT"
  (expand-file-name (concat ciao-bin-dir "/../..")))

;;;###autoload
(defun ciao-grep-root ()
  "Run grep on Ciao source files (at root directory)"
  (interactive)
  (let ((re (read-from-minibuffer "Search Ciao code at root directory (Regexp): ")))
    (ciao--grep-common re (list (ciao--root-dir)))))

;;;###autoload
(defun ciao-grep-all ()
  "Run grep on Ciao source files (at root directory and workspaces given by CIAOPATH)"
  (interactive)
  (let ((dirs (append (mapcar 'directory-file-name
			      (parse-colon-path (getenv "CIAOPATH")))
		      (list (ciao--root-dir))))
	(re (read-from-minibuffer "Search Ciao code at CIAOPATH workspaces and root directory (Regexp): ")))
    (ciao--grep-common re dirs)))

;;;###autoload
(defun ciao-grep ()
  "Run grep on Ciao source files (under the default directory)"
  (interactive)
  (let ((re (read-from-minibuffer "Search Ciao code under the default directory (Regexp): ")))
      (ciao--grep-common re (list (expand-file-name default-directory)))))

(defun ciao--grep-common (regexp dirs)
  "Run grep with REGEXP on Ciao source files at directory DIR"
  (let* ((grep-cmd (concat (ciao--root-dir) "/core/cmds/grep-source.bash"))
	 (args (append (list grep-cmd "-e" regexp) dirs))
	 (cmdstr (mapconcat 'shell-quote-argument args " ")))
      (grep cmdstr)))

;; ---------------------------------------------------------------------------
;; Server process for IDE (experimental)
;;
;; TODO: move to a different file, configure

(defvar ciao-ide-server-buffer-name "*Ciao-IDE-Server*")

(defun ciao-ide-server-started ()
  (processp (get-buffer-process ciao-ide-server-buffer-name)))

(defun ciao-ide-ensure-server-start ()
  (if (not (ciao-ide-server-started))
      (ciao-ide-server-start)))

;;;###autoload
(defun ciao-ide-server-start ()
  "Start the IDE server"
  (interactive)
  (if (ciao-ide-server-started)
      (message "IDE server already started")
    (let ((runideserver (expand-file-name 
		      (concat ciao-bin-dir "/../../ide/web/run_ciao_ide.bash"))))
      (start-process "ciao-ide-server" 
		     ciao-ide-server-buffer-name 
		     runideserver)
      (message "IDE server started"))))

;;;###autoload
(defun ciao-ide-server-stop ()
  "Stop the IDE server"
  (interactive)
  (let ((p (get-buffer-process ciao-ide-server-buffer-name)))
    (if (processp p)
	(progn
	  (kill-process p)
	  (message "IDE server stop")))))

;;;###autoload
(defun ciao-browse ()
  "Open Ciao code browser"
  (interactive)
  (ciao-ide-ensure-server-start)
  (eww "http://localhost:8001"))

;;;###autoload
(defun ciao-browse-external ()
  "Open Ciao code browser (in an external web browser)"
  (interactive)
  (ciao-ide-ensure-server-start)
  (shell-command "open http://localhost:8001"))


;; Provide ourselves:

(provide 'ciao-builder)

;;; ciao-builder.el ends here

