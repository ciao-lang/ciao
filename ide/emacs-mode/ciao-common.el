;;; ciao-common.el --- Common definitions for the Ciao mode
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

(require 'ciao-config) ; ciao-get-config

;; ---------------------------------------------------------------------------
;; Basic Ciao mode variables
;; ---------------------------------------------------------------------------

(defgroup ciao nil
  "The Ciao programming environment." 
  :tag "Ciao"
  :group 'emacs)

(defgroup ciao nil
  "The Ciao programming environment." 
  :tag "Ciao"
  :group 'languages)

;;---------------------------------------------------------------------------

(defgroup ciaoide nil
  "Ciao IDE behaviour customization."
  :tag "Ciao IDE"
  :group 'ciao)

;;---------------------------------------------------------------------------

(defgroup ciaocore nil
  "The Ciao core (compiler, toplevel, debugger, and core
libraries)."
  :tag "Ciao core"
  :group 'ciao)

(defgroup ciaopp nil
  "The Ciao preprocesor."
  :tag "CiaoPP"
  :group 'ciao)

(defgroup lpdoc nil
  "The LPdoc documentation generator."
  :tag "LPdoc"
  :group 'ciao)

(defcustom ciao-system (or (getenv "CIAO") (ciao-get-config :ciaosh-bin))
  "Name of Ciao executable which runs the classical top level."
  :group 'ciaocore
  :type 'string)

(defun ciao-set-ciao-system () 
  "Change the Ciao executable used to run the top level. It is set by
default to @tt{ciao} or, to the environment variable @tt{CIAO} if it
is defined. @cindex{toplevel command, setting}" 
  (interactive)
  (setq ciao-system
	(read-file-name "Change Ciao top-level executable? " 
			"" ciao-system nil ciao-system)))

(defun ciao-system-args-interactive ()
    (if (string= system-type "windows-nt") "-i" ""))

(defcustom ciao-system-args 
  (or (getenv "CIAOARGS") 
      (ciao-system-args-interactive))
  "Arguments passed to Ciao toplevel executable."
  :group 'ciaocore
  :type 'string)

(defun ciao-set-ciao-system-args () 
  "Change the arguments passed to the Ciao executable. They are
set by default to none or, to the environment variable @tt{CIAOARGS} if it
is defined. @cindex{toplevel command args, setting}"
  (interactive)
  (setq ciao-system-args
	(read-string "Change args passed to Ciao executable? " 
		     ciao-system-args nil)))

;; ---------------------------------------------------------------------------
;; TODO: Redistribute

(defcustom ciao-toplevel-buffer-name "*Ciao*"
  "Basic name of the buffer running the Ciao toplevel inferior process."
  :group 'ciaocore
  :type 'string)

(defcustom ciao-lpdoc-buffer-name "*LPdoc*"
  "Basic name of the buffer running the auto-documenter inferior process."
  :group 'lpdoc
  :type 'string) 

(defcustom ciao-ciaopp-buffer-name "*Ciao Preprocessor*"
  "Basic name of the buffer running the Ciao preprocessor inferior process."
  :group 'ciaopp
  :type 'string) 

;; ---------------------------------------------------------------------------
;; CiaoPP variables
;; ---------------------------------------------------------------------------

;; (defcustom ciao-ciaopp-system (or (getenv "CIAOPP") "ciaopp-1.0")
(defcustom ciao-ciaopp-system (or (getenv "CIAOPP") (ciao-get-config :ciaopp-bin))
  "Name of Ciao preprocessor executable."
  :group 'ciaopp
  :type 'string)

(defun ciao-set-ciaopp-system () 
  "Change the executable used to run the Ciao Preprocessor
toplevel. It is set by default to @tt{ciaopp} or, to the environment 
variable @tt{CIAOPP} if it is defined. @cindex{preprocessor command, setting}"
  (interactive)
  (setq ciao-ciaopp-system
	(read-file-name "Change Ciao preprocessor executable? "
   		        "" ciao-ciaopp-system nil ciao-ciaopp-system))) 

(defun ciao-ciaopp-system-args-interactive ()
    (if (string= system-type "windows-nt") "-T -i" "-T"))

(defcustom ciao-ciaopp-system-args 
  (or (getenv "CIAOPPARGS") 
      (ciao-ciaopp-system-args-interactive))
  "Arguments passed to Ciao preprocessor executable."
  :group 'ciaopp
  :type 'string)

(defun ciao-set-ciaopp-system-args () 
  "Change the arguments passed to the Ciao preprocessor executable. They are
set by default to none or to the environment variable @tt{CIAOPPARGS} if it
is defined. @cindex{preprocessor command args, setting}"
  (interactive)
  (setq ciao-ciaopp-system-args
	(read-string "Change args passed to Ciao preprocessor executable? "
		     ciao-ciaopp-system-args nil))) 

(defcustom ciao-ciaopp-gmenu-buffer-name "*CiaoPP Interface*"
  "Name of the buffer running the Ciao preprocessor graphical
menu interface."
  :group 'ciaopp
  :type 'string) 


;; Provide ourselves:

(provide 'ciao-common)

;;; ciao-common.el ends here


