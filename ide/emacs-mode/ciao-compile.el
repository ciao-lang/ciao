;;; ciao-compile.el --- Interface with Ciao compiler
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

(require 'ciao-parsing) ; predicate-boundaries
(require 'ciao-process) ; ciao-proc-enqueue-w,
			; ciao-send-command
(require 'ciao-loading) ;; TODO: replace by toplevel?
(require 'ciao-aux) ; ciao-write-region
(require 'ciao-scratchpad) ; ciao-scratchpad-last-code-file

;;------------------------------------------------------------
;; Compiler/Top-level, file based.
;;------------------------------------------------------------

;;;###autoload
(defun ciao-make-exec ()
  "Make an executable from the code in the current buffer. The buffer
must contain a @pred{main/0} or @pred{main/1} predicate. Note that
compiler options can be set to determine whether the libraries and
auxiliary files used by the executable will be statically linked,
dynamically linked, auto-loaded, etc."
  (interactive)
  (ciao-send-compiler-command
   (concat "make_exec('" (buffer-file-name) "',_)." 
;; This was useful but now 'make_exec(FILE,_)' works (better!)
;; 	   (substring (buffer-name) 0 (string-match ".pl" (buffer-name))) 
;; 	   "')." 
	   )))

;;;###autoload
(defun ciao-make-po ()
  "Make a Ciao object (.po) file from the code in the current
buffer.  This is useful for example while debugging during development
of a very large application which is compiled into an excutable, and
only one or a few files are modified. If the application executable is
dynamically linked, i.e., the component .po files are loaded
dynamically during startup of the application, then this command can
be used to recompile only the file or files which have changed, and
the correct version will be loaded dynamically the next time the
application is started. However, note that this must be done with care
since it only works if the inter-module interfaces have not changed.
The recommended, much safer way is to generate the executable again,
letting the Ciao compiler, which is inherently incremental, determine
what needs to be recompiled."
  (interactive)
  (ciao-send-compiler-command
   (concat "make_po('" (buffer-file-name) "').")))

;;;###autoload
(defun ciao-make-activemod ()
  "Make an active module executable from the code in the current
buffer. An active module is a remote procedure call server (see the
@lib{activemod} library documentation for details)."
  (interactive)
  (let ((method (read-string "Address publishing method: " 
			     "actmods/filebased_publish")))
    (ciao-send-compiler-command
     (concat "make_actmod('" (buffer-file-name) "','" method "')." ))))

;;------------------------------------------------------------
;; Traditional commands: Compiling
;;------------------------------------------------------------

(defun ciao-compile-query (filename)
  "Ciao query for compiling `filename'"
  (concat "compile('" (buffer-file-name) "')."))

;;;###autoload
(defun ciao-compile-buffer ()
  "Compile the entire buffer."
  (interactive)
  (ciao-send-command 'ciaosh-cproc
		     (ciao-compile-query (buffer-file-name))
		     t))

;;;###autoload
(defun ciao-compile-region (start end)
  "Compile a given region."
  (interactive "r")
  (ciao-write-region start end (ciao-scratchpad-last-code-file))
  (ciao-send-command 'ciaosh-cproc
		     (ciao-compile-query ciao-scratchpad-last-file)
		     t)
  (ciao-proc-enqueue-w 'ciaosh-cproc 'ciao-scratchpad-clean))

;; PO 890606
;;;###autoload
(defun ciao-compile-predicate ()
  "Compile the predicate around point."
  (interactive)
  (let ((boundaries (predicate-boundaries)))
    (ciao-compile-region (car boundaries) (cdr boundaries))))

;; Original version: JA 890531
;; (defun build-ciao-command (commstring)
;;   (concat "ciao:zap_file('"
;;   (concat "zap_file('"
;; 	  (ciao-scratchpad-code-file) "', '"
;; 	  (or (buffer-file-name) "user") "', " commstring ")."))


;; Provide ourselves:

(provide 'ciao-compile)

;;; ciao-compile.el ends here

