;;; ciao-splash.el --- Splash screen for Ciao Mode for Emacs
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

(require 'ciao-common) ; ciaoide (group)
(require 'ciao-loading) ; run-ciao-toplevel
(require 'ciao-process) ; ciao-proc-get-buffer
(require 'ciao-scratchpad) ; ciao-scratchpad-new-code-file,
			   ; ciao-scratchpad-root

;;------------------------------------------------------------
;; Startup 
;;------------------------------------------------------------

;; Problem is, environment usually opened with -q -> needs a special 
;; config file for this case...
;; (i.e., typically by double-clicking on an icon) 
(defcustom ciao-create-sample-file-on-startup t
  "When starting the Ciao environment using ciao-startup two buffers
are opened: one with a toplevel and another with a sample file. This
toggle controls whether the sample file, meant for novice users, is
created or not. Set by default, non-novice users will probably want to
turn it off."
  :group 'ciaoide
  :type 'boolean)

;;;###autoload
(defun ciao-startup ()
  "Like \\<ciao-mode-map> \\[run-ciao-toplevel], but starts with a
window in Ciao mode, ready to edit, and another one with the Ciao
toplevel. Useful as splash screen for the Ciao program development
system, for example when launching from a desktop (launch emacs,
calling this function)."
  (interactive)
  (let ((tmpfile 
	(concat (ciao-scratchpad-new-code-file ciao-scratchpad-root) ".pl")))
    (delete-other-windows)
    (run-ciao-toplevel)
    (if ciao-create-sample-file-on-startup
	(progn
	  (find-file tmpfile)
	  (goto-char (point-min))
	  (insert 
"% You can type code in this buffer. 
% Save with \"File->Save Buffer As...\" or \"C-x C-s\".
% Load into toplevel with \"C-c l\"
% Explore menus and buttons above.
% See also Section \"Using Ciao inside GNU emacs\" of the Ciao manual
% (\"CiaoHelp->Ciao system manual\") 

:- module(_,_).

main(Arg) :- 
	write(Arg).

")
	  )
      (switch-to-buffer (ciao-proc-get-buffer 'ciaosh-cproc))
      (delete-other-windows)
    )))


;; Provide ourselves:

(provide 'ciao-splash)

;;; ciao-splash.el ends here


