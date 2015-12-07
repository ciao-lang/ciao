;;; ciao-scratchpad.el --- A scratchpad for temporary (source) Ciao
;;;   files and directories.

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

;;------------------------------------------------------------
;; Variables
;;------------------------------------------------------------

;; Prefix for scratchpad file names
(defvar ciao-scratchpad-prefix "ciao")

(defcustom ciao-scratchpad-root
  (or (getenv "CIAOSCRATCHDIR") "/tmp")
  "Name of root directory of the scratchpad for temporary source
files and directories."
  :group 'lpdoc
  :type 'directory)

;;;###autoload
(defun ciao-set-scratchpad-root () 
  "Change the root directory of the scratchpad for temporary source files.
This is used, e.g., by the LPdoc auto-documenter when generating
temporary configuration files and documentation for buffers. It is
set by default to a new dir under @tt{/tmp} or to the environment
variable @tt{CIAOSCRATCHDIR} if it is defined. @cindex{scratchpad
directory} @cindex{auto-documenter working dir, setting}"
  (interactive)
  (let ((dir
	 (read-file-name
	  (format (concat
		   "Change root directory of the scratchpad for "
		   "temporary source files: (currently %s) ")
		  ciao-scratchpad-root)
	  nil nil t)))
    ;; Remove trailing '/' (required later)
    (setq ciao-scratchpad-root (directory-file-name dir))))

;;------------------------------------------------------------
;; Creation of (new) temporary file and directory names
;;------------------------------------------------------------

;; MH Changed to do it in current dir (so that slaves can see it, etc.!)
(defvar ciao-scratchpad-file-counter 0)

(defvar ciao-scratchpad-last-file nil)

(defun ciao-scratchpad-last-code-file ()
  "Returns the name of a the last created temporary file in the
current dir (or creates one)."
  (if (eq ciao-scratchpad-last-file nil)
      (setq ciao-scratchpad-last-file (ciao-scratchpad-code-file "."))
    ciao-scratchpad-last-file))

(defun ciao-scratchpad-code-file (from-dir)
  "Returns the name of a temporary file in dir given in argument."
  (concat (expand-file-name (concat from-dir "/")) 
	  ciao-scratchpad-prefix 
	  (int-to-string ciao-scratchpad-file-counter) "_" (make-temp-name "")))

(defun ciao-scratchpad-new-code-file (from-dir)
  "Builds new temporary file names in the current dir."
  (setq ciao-scratchpad-file-counter (+ ciao-scratchpad-file-counter 1))
  (ciao-scratchpad-code-file from-dir))

(defun ciao-scratchpad-new-code-dir (filename)
  "Builds new temporary dir names in lpdoc root dir."
  (setq ciao-scratchpad-file-counter (+ ciao-scratchpad-file-counter 1))
  (concat (expand-file-name ciao-scratchpad-root) "/lpdoc_" filename "_"
	  (int-to-string ciao-scratchpad-file-counter) "_" (make-temp-name "")))

;;------------------------------------------------------------
;; Association of temporary directories to source file names.
;;------------------------------------------------------------

;; TODO: This list always grows. Fix.
(defvar ciao-scratchpad-source-assoc-dir-list nil
  "Assoc. list relating filenames and their temporary doc dirs.")

;; TODO: This only takes the module name; there will be conflicts with
;; the hierarchical module system.
(defun ciao-scratchpad-source-assoc-dir (sourcefile)
  "The temporary directory associated to `sourcefile'."
  (let* ((filename (file-name-nondirectory sourcefile))
	 (tmpdir (cdr (assoc filename ciao-scratchpad-source-assoc-dir-list))))
    (if tmpdir
	tmpdir
      (setq tmpdir (ciao-scratchpad-new-code-dir filename))
      (setq ciao-scratchpad-source-assoc-dir-list
	    (cons 
	     (cons filename tmpdir)
	     ciao-scratchpad-source-assoc-dir-list)))
      tmpdir
    ))

;;------------------------------------------------------------
;; Cleaning of the scrachpad
;;------------------------------------------------------------

;; TODO: Only ciao-scrachpad-last-file is cleaned, is that correct?

(defun ciao-scratchpad-clean ()
  "Delete files (and possible compilation output files) in the `scrachpad'"
  (delete-file-if-possible ciao-scratchpad-last-file)
  (delete-file-if-possible (concat ciao-scratchpad-last-file ".err"))
  (delete-file-if-possible (concat ciao-scratchpad-last-file ".asr"))
  (delete-file-if-possible (concat ciao-scratchpad-last-file ".ast"))
  (delete-file-if-possible (concat ciao-scratchpad-last-file ".itf"))
  (delete-file-if-possible (concat ciao-scratchpad-last-file ".po")))

(defun delete-file-if-possible (file)
  (if (and (file-exists-p file) (file-writable-p file))
      (delete-file file)
    nil))


;; Provide ourselves:

(provide 'ciao-scratchpad)

;;; ciao-scrachpad.el ends here

