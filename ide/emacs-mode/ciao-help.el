;;; ciao-help.el --- Help for the Ciao environment
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
;; Help (locating manuals, calling word-help, etc.)
;;------------------------------------------------------------

(require 'word-help)
(require 'ciao-config) ; ciao-get-config

;; List of manuals for word-help (help on symbol under cursor). These
;; info files must be accessible from the paths in the environment
;; variable 'INFOPATH' (or in the emacs variable Info-default-directory-list).
;; 
;; Unfortunately, this is very brittle... (Stallman seems to have
;; plans for a better word-help in new versions of emacs)

(defun ciao-help-info-entry (loc)
  "A `word-help' entry for the LPdoc-generated info manual at
`loc' (which includes the info file and indices)."
  `(;; info file
    ,(concat loc ".info")
    ;; Indices currently in manuals: concept lib pred prop regtype decl usage
    ;; Later entries take precedece, so this is probably the right order!
    "Global Index"
    "Author Index"
    "Concept Index" 
    "Library/Module Index" 
    "Predicate/Method Index" 
    "Property Index" 
    "Regular Type Index" 
    "Declaration Index" 
    )
  )

;; TODO: This may not be necessary if stored in the bundle data
;; structure
(defun ciao-help-info-entry-exists-p (inf)
  "Detects if the (Ciao) info manual `inf' actually exists."
  (file-exists-p
   (concat (ciao-get-config :lpdoc-dir) "/" (car inf))))

;; The list of manuals for the available bundles

(defun ciao-help-existing-manuals (bases)
  "Add the (existing) manuals with the given `bases'."
  (let (manuals)
    (dolist (base bases)
      (let
	  ((inf (ciao-help-info-entry base)))
	(if (ciao-help-info-entry-exists-p inf)
	    (add-to-list 'manuals inf))))
    (nreverse manuals)))

(defun ciao-help-add-manual (mode manuals)
  "Associate a list of `manuals' with a `mode' name in the
`word-help-mode-alist' variable."
  (add-to-list 'word-help-mode-alist
	       `(,mode . (,manuals
			  (("[A-Za-z_]+" 0)
			   ("[A-Za-z_][A-Za-z0-9_^/]+" 0))
			  nil
			  (("[A-Za-z_]+" 0))
			  ))))

;;---------------------------------------------------------------------------  

;; TODO: Reevaluate if bundles change
(defvar ciao-manuals
  (ciao-help-existing-manuals (ciao-get-config :manual-bases)))

;; Then associate Ciao manuals with each Ciao mode
(ciao-help-add-manual "Ciao" ciao-manuals)
(ciao-help-add-manual "Ciao Listener" ciao-manuals)

(defvar ciao-info-dir (ciao-get-config :lpdoc-dir)
  "Where the actual Ciao (LPdoc) info directory is.")

;; (previous code)
;; ;(if (boundp 'xemacs-logo)
;;     (progn
;;       (load-library "info")             ; Info creates Info-directory-list
;;       ;; (require 'info)
;;       (if (null Info-directory-list)    ; but it is not initialized there
;;           (setq Info-default-directory-list ; Will be initialized from here
;;                 (cons ciao-info-dir Info-default-directory-list))
;;         (setq Info-directory-list (cons ciao-info-dir Info-directory-list))
;;         )
;;       ) 
;; ;  )

;;;###autoload
(defun ciao-update-info-dir ()
  "Update the info directory list to include Ciao manuals"
  (progn
    ;; (message "Updating Ciao info")
    (add-hook 'Info-mode-hook		; After Info-mode has started
	      (lambda ()
		(setq Info-additional-directory-list Info-default-directory-list)
		))
    ;; (require 'info)
    (if (null Info-directory-list)    ; but it is not initialized there
	(progn
	  (setq Info-default-directory-list ; Will be initialized from here
		(cons ciao-info-dir Info-default-directory-list))
	  (load-library "info")             ; Info creates Info-directory-list
	  )
      (setq Info-directory-list (cons ciao-info-dir Info-directory-list)))
    ))

;;;###autoload(ciao-update-info-dir)
  
;;---------------------------------------------------------------------------  

(defun ciao-help-on-current-symbol () 

  "Find help for the symbol (e.g., predicate, directive, declaration,
property, type, etc.) that is currently under the cursor. Opens
a (hopefully) relevant part of the Ciao manuals in @apl{info}
mode. Requires that the Ciao manuals in @apl{info} format be
installed and accessible to @apl{emacs} (i.e., they should appear
somewhere in the info directory when typing @tt{M-x info}). It
also requires @file{word-help.el}, which is provided with
Ciao. Refer to the installation instructions if this is not the
case."

  (interactive) 
  (call-interactively 'word-help))

(defun ciao-complete-current-symbol () 

  "Find a completion for the symbol (e.g., predicate, directive,
declaration, type, etc.) that is currently under the cursor. Uses for
completion the contents of the indices of the Ciao manuals. Same
requirements as for finding help for the symbol."

  (interactive) 
  (call-interactively 'word-help-complete))

(defun ciao-goto-manuals () 
  "Go to the part of the info directory containing the Ciao manuals."
  (interactive) 

  (ciao-locate-manual-in-info-dir "Ciao system"))

;; (defun ciao-goto-ciaocore-manual () 
;;   "Go to the part of the info directory containing the Ciao
;; system manual."
;;   (interactive) 
;;   (ciao-goto-particular-manual (concat (ciao-get-config :ciaocore-name-version) ":")))
;; 
;; (defun ciao-goto-ciaopp-manual () 
;;   "Go to the part of the info directory containing the Ciao
;; preprocessor manual."
;;   (interactive) 
;;   (ciao-goto-particular-manual (concat (ciao-get-config :ciaopp-name-version) ":")))
;; 
;; (defun ciao-goto-lpdoc-manual () 
;;   "Go to the part of the info directory containing the
;; lpdoc (automatic documenter) manual."
;;   (interactive) 
;;   (ciao-goto-particular-manual (concat (ciao-get-config :lpdoc-name-version) ":")))

(defun ciao-locate-manual-in-info-dir (text) 
  "Locate a manual entry in the info dir"
  (info) 
  (Info-directory)
  (if (search-forward text nil t) 
      (recenter 0)
    (error (concat "Could not find " text " manual in info dir"))))

;; (Unused now)
(defun ciao-goto-particular-manual (manual) 
  "Go to a particular manual."
  (ciao-locate-manual-in-info-dir manual)
  (if (not (boundp 'xemacs-logo))
      (Info-follow-nearest-node)
    (backward-char 3)
    (Info-follow-nearest-node (point))
    ))

;;;###autoload
(defun ciao-describe-mode () 
  "Show a short description of the Ciao emacs mode, including all key
bindings." 
  (interactive) 
  (describe-mode))


;; Provide ourselves:

(provide 'ciao-help)

;;; ciao-help.el ends here

