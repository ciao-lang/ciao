;;; ciao-vc.el --- Support for version control for the Ciao mode
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

(require 'ciao-aux) ; ciao-switch-other-window

;;------------------------------------------------------------
;; On-line comments and changelog management
;;------------------------------------------------------------

(defcustom ciao-ask-for-version-maintenance-type "no"
  "If turned to yes the system asks prompts to set version control
when saving files that do not set a version control system
explicitly within the file."
  :group 'lpdoc
  :type 'string) 

(defvar update-version-comments 0 ; 0 means "uninitialized"
  "Controls for each buffer whether version maintenance is performed
or not and if it is, what type.")
(make-variable-buffer-local 'update-version-comments)

;; Note: this comment was commented on the Ciao mode and Ciao inferior
;; mode initialization code:
;;
;; ;; Using make-variable-buffer-local above
;; ;;  (set (make-local-variable 'update-version-comments) 0) ; 0 means "uninitialized"

;;;###autoload
(defun ciao-new-version () 

  "Force a move to a new major/minor version number (the user will be
prompted for the new numbers). Only applicable if using
directory-based version maintenance. Note that otherwise it suffices
with introducing a changelog entry in the file and changing its
version number by hand."

  (interactive)
  (ciao-handle-version-control-option)
  (if (or (string= (ciao-version-maint-type) "off") 
	  (string= (ciao-version-maint-type) "on"))
      (error "Only supported if using version directory")
    (if (not (string= 
	      (read-string "Change major/minor version? (y/n) " "n")
	      "y"))
	nil
     
      (message "Will first delete current Version/Patch files")
      (sleep-for 2)
      (delete-file (concat (ciao-version-maint-type) "/GlobalVersion"))
      (delete-file (concat (ciao-version-maint-type) "/GlobalPatch"))
      (message "Current Version/Patch files deleted")
      (sleep-for 2)
      (ciao-update-version (ciao-version-maint-type))
      )
    )
  )

;;;###autoload
(defun ciao-set-version-control-for-buffer ()
"Used to turn on or off version control for the file being visited in
the current buffer.  The user will be prompted to choose among the
following options:

   @begin{description} 

   @item{@key{y}} Turn version control on for this file. 

   @item{@key{n}} Turn version control off for this file. A version
control comment such as:

@tt{:- doc(version_maintenance,off).}

@noindent will be added to the buffer and the file saved. No version
control will be performed on this file until the line above is removed
or modified (i.e., from now on \\<ciao-mode-map> \\[ciao-save-buffer]
simply saves the buffer).

   @item{@key{q}} Turn off prompting for the introduction of changelog
entries for now. @apl{emacs} will not ask again while the buffer is
loaded, but it may ask again when saving after the next time you load
the buffer (if @tt{ciao-ask-for-version-maintenance-type} is set to
@tt{yes}).

   @end{description}

   If @key{y} is selected, then the system prompts again regarding how
and where the version and patch number information is to be
maintained. The following options are available:

   @begin{description}

   @item{@tt{on}} All version control information will be
contained within this file. When saving a buffer
\\<ciao-mode-map> (\\[ciao-save-buffer]) emacs will ask if a
changelog entry should be added to the file before saving. If a
comment is entered by the user, a new patch number is assigned to
it and the comment is added to the file. This patch number will
be the one that follows the most recent changelog entry already
in the file. This is obviously useful when maintaining version
numbers individually for each file.

   @item{@tt{<directory_name>}} Global version control will be
performed coherently on several files. When saving a buffer
\\<ciao-mode-map> (\\[ciao-save-buffer]) emacs will ask if a changelog
entry should be added to the file before saving. If a comment is
given, the global patch number (which will be kept in the file:
@tt{<directory_name>/GlobalPatch}) is atomically incremented and the
changelog entry is added to the current file, associated to that patch
number. Also, a small entry is added to a file
@tt{<directory_name>/GlobalChangeLog} which points to the current
file. This allows inspecting all changes sequentially by visiting all
the files where the changes were made (see \\<ciao-mode-map> 
\\[ciao-fetch-next-changelog-entry]). This is obviously useful when
maintaining a single thread of version and patch numbers for a set of
files.

   @item{@tt{off}} Turns off version control: \\[ciao-save-buffer] then simply
   saves the file as usual. 

   @end{description}

@bf{Some useful tips:} 

@begin{itemize}

@item If a changelog entry is in fact introduced, the cursor is left
at the point in the file where the comment was inserted and the mark
is left at the original file point. This allows inspecting (and
possibly modifying) the changelog entry, and then returning to the
original point in the file by simply typing
\\[exchange-point-and-mark].

@item @cindex{moving changelog entries} The first changelog entry is
entered by default at the end of the buffer. Later, the changelog
entries can be moved anywhere else in the file. New changelog entries
are always inserted just above the first changelog entry which appears
in the file.

@item The comments in changelog entries can be edited at any time. 

@item If a changelog entry is moved to another file, and version
numbers are shared by several files through a directory, the
corresponding file pointer in the
@tt{<directory_name>/GlobalChangeLog} file needs to be changed also,
for the entry to be locatable later using
\\[ciao-fetch-next-changelog-entry].

@end{itemize}

"
  (interactive)
  (let ((option-was) (maint-type))
    (setq option-was ciao-ask-for-version-maintenance-type)
    (setq ciao-ask-for-version-maintenance-type "yes")
    (cond
     ((string= (ciao-version-maint-type) "on")
      (message "File already under version control. Edit file for changes."))
     ((string= (ciao-version-maint-type) "off")
      (message "Version control already disabled. Revisit or edit the file."))
     (t
      (setq update-version-comments 0)
      (ciao-handle-version-control-option)))
    (setq ciao-ask-for-version-maintenance-type option-was)
  ))

;;;###autoload
(defun ciao-save-buffer ()

  "This is the standard @apl{emacs} command that saves a buffer by
writing the contents into the associated @tt{.pl} file.  However, in
the Ciao mode, if version control is set to on for ths file, then this
command will ask the user before saving whether to introduce a
changelog entry documenting the changes performed.

In addition, if: 

@begin{itemize}

@item the buffer does not already contain a comment specifying the
@concept{type of version control} to be performed,

@item and the customizable variable
@tt{ciao-ask-for-version-maintenance-type} is set to @tt{yes} (go to
the Ciao options menu, LPdoc area to change this, which is by default
set to @tt{no}),

@end{itemize} 

@noindent then, before saving a buffer, the user will be also
automatically asked to choose which kind of version control is desired
for the file, as in \\<ciao-mode-map>
\\[ciao-set-version-control-for-buffer].

"
  (interactive)
  (ciao-save-buffer-option nil))

;;;###autoload
(defun ciao-add-comment-and-save ()

  "Same as \\<ciao-mode-map> \\[ciao-save-buffer] except that it
forces prompting for inclusion of a changelog entry even if the buffer
is unmodified."

  (interactive)
  (ciao-save-buffer-option t))

(defun ciao-save-buffer-option (save-option)
  "Same as above, but allows forcing save / minor version change."
  (if (and (eq (buffer-modified-p) nil) (eq save-option nil))
      ;; will do nothing -- just for printing the usual message
      (save-buffer) 
    (ciao-handle-version-control-option)
    (if (and (string= (ciao-version-maint-type) "off") (eq save-option nil))
	;; just normal save
	(save-buffer)
      (if (and (eq save-option t) 
	       (not (string= (ciao-version-maint-type) "off")))
	  ;; no need to ask
	  (ciao-update-version (ciao-version-maint-type))
	(if (string= (ciao-version-maint-type) "off")
	    ;; will do nothing -- just for printing the usual message
	    (save-buffer) 
	  ;; ask 
	  (if (not (string= 
		    (read-string "Insert changelog entry? (y/n) " "n")
		    "y"))
	      (save-buffer);; normal save and return
	    ;; update version and save
	    (ciao-update-version (ciao-version-maint-type))
	    ))))))

;;;###autoload
(defun ciao-update-version (version-dir) 
  "Inserts a changelog entry (comment and patch number change). If a
  comment is in fact introduced, the buffer is left at the file point
  of the entry for inspection and the mark is left at the original
  file point for easy return."  
  (interactive)
  (let (original-point 
	original-buffer 
	version-file 
	version-major 
	version-minor
	no-previous-version
	patch-file
	patch-buffer
	patch-number
	keep-version
	comment
	month day year time
	old-version-control
	change-file
	tmp-point)
  (setq original-point (point))
  (goto-char (point-min))
  (cond
   ((not (or (string= version-dir "on") (string= version-dir "off")))
    ;; Previous version is in external file - get it
    ;; For locking, we are taking advantage of emacs file locking by
    ;; modifying the buffer right away.
    (setq original-buffer (current-buffer))
    (setq version-file (concat version-dir "/GlobalVersion"))
    (if (file-readable-p version-file)
	(progn 
	  (find-file version-file)
	  (goto-char (point-min))
	  (setq tmp-point (point))
	  (search-forward-regexp "\\.")
	  (backward-char 1)
	  ;; kill-region modifies and sets lock...
	  (setq version-major
		(buffer-substring-no-properties tmp-point (point)))
	  (forward-char 1)
	  (setq tmp-point (point))
	  (end-of-line)
	  (setq version-minor
		(buffer-substring-no-properties tmp-point (point)))
	  (setq no-previous-version nil)
	  (kill-buffer (current-buffer))
	  )
      (if (string= 
	   (read-string 
	    (concat "Could not find " version-file ", create? ") "y")
	   "y")
	  (progn
	    (setq no-previous-version t))
	(error "No version file")))

    (setq patch-file (concat version-dir "/GlobalPatch"))
    (if no-previous-version
	nil
      ;; There is a previous version
      (if (file-readable-p patch-file)
	  ;; Readable patch file: get patch number
	  (progn 
	    (switch-to-buffer original-buffer) ;; So that relative paths work!
	    (find-file patch-file)
	    (goto-char (point-min))
	    (setq patch-buffer (current-buffer))
	    (setq tmp-point (point))
	    (end-of-line)
	    (setq patch-number 
		  (buffer-substring-no-properties tmp-point (point)))
	    (kill-buffer (current-buffer))
	    )
	;; No patch file: new patch number
	(setq patch-number "-1")))

    (switch-to-buffer original-buffer))
   ((search-forward-regexp "^[ \t]*:-[ \t\n]*\\(comment\\|doc\\)([ \t\n]*version(" nil t)
    ;; A previous version exists in the file: get it
    (setq tmp-point (point))
    (search-forward-regexp "\\*")
    (backward-char 1)
    (setq version-major
	  (buffer-substring-no-properties tmp-point (point)))
    (forward-char 1)
    (setq tmp-point (point))
    (search-forward-regexp "\\+")
    (backward-char 1)
    (setq version-minor
	  (buffer-substring-no-properties tmp-point (point)))
    (forward-char 1)
    (setq tmp-point (point))
    (search-forward-regexp "[ \t\n]*,")
    (backward-char 1)
    (setq patch-number 
	  (buffer-substring-no-properties tmp-point (point)))
    (setq no-previous-version nil)
    )
   (t
    ;; No previous version exists: set it to 0.1+-1
    (setq no-previous-version t)
    )
   )

  (if no-previous-version
      (progn 
	(setq keep-version "n")
	(setq version-major "0")
	(setq version-minor "1")
	)
       (setq keep-version "y")
    )
	
  ;; If we keep the version or no comment
  (if (string= keep-version "y")
        ;; Version and patch number stay as they are
	nil
    ;; Else, get new version
    (setq version-major
	  (read-string "Major version? " version-major))
    (setq version-minor
	  (read-string "Minor version? " version-minor))
    ;; and reset patch number
    (setq patch-number "-1"))
   
  (setq comment (read-string (concat 
			      "Type a comment for new version "
			      version-major "." 
			      version-minor "#" 
			      (int-to-string 
			       (+ (string-to-number patch-number) 1))
			      ":"
			      ) 			     
			     ""))

  (if (string= comment "")
      nil
    ;; Increment patch number (will be 0 if new version)
    (setq patch-number (int-to-string (+ (string-to-number patch-number) 1))))

  ;; Hey, why not set them right here
  (setq month (format-time-string "%m"))
  (setq day   (format-time-string "%d"))
  (setq year  (format-time-string "%Y"))
  (setq time  (format-time-string "%H:%M*%S+'%Z'"))

  ;; If version came from changelog file in a directory, update the
  ;; version files 
  (if (or (string= version-dir "on") (string= comment ""))
      nil

    (switch-to-buffer original-buffer) ;; So that relative paths work!
    (find-file version-file)
    (goto-char (point-min))
    (setq tmp-point (point))
    (end-of-line)
    (delete-region tmp-point (point))
    (insert (concat version-major "." version-minor))
    (setq old-version-control version-control)
    (setq version-control 'never)
    (save-buffer (current-buffer))
    (setq version-control old-version-control)
    (kill-buffer (current-buffer))

    (switch-to-buffer original-buffer) ;; So that relative paths work!
    (find-file patch-file)
    (goto-char (point-min))
    (setq tmp-point (point))
    (end-of-line)
    (delete-region tmp-point (point))
    (goto-char (point-min))
    (insert patch-number)
    (setq old-version-control version-control)
    (setq version-control 'never)
    (save-buffer (current-buffer))
    (setq version-control old-version-control)
    (kill-buffer (current-buffer))

    (switch-to-buffer original-buffer) ;; So that relative paths work!
    (setq change-file (concat version-dir "/GlobalChangeLog"))
    (if (file-readable-p change-file)
	  (find-file change-file)
      (find-file change-file)
      (goto-char (point-min))
;;    Sets buffer in Ciao mode: necessary for bindings!  
      (insert "\n:- module(_,_,[assertions]).\n\n")
      (ciao-insert-version-control-off-comment) 
      ;;    This one would be visible by a Ciao program (not needed)
      ;;     (insert "\n:- doc(version_maintenance,off).\n\n")
      )
    (goto-char (point-min))
    (ciao-insert-version-comment 
     version-major version-minor patch-number month day year time 
     (file-relative-name (buffer-file-name original-buffer)))
    (setq old-version-control version-control)
    (setq version-control 'never)
    (save-buffer (current-buffer))
    (setq version-control old-version-control)
    (kill-buffer (current-buffer))
    (switch-to-buffer original-buffer)
    )

  (if (string= comment "")
      ;; If user gave no input comments, do nothing
      (progn 
	(message "Blank comment -- no version change")
	(if (string= version-dir "on")
	    nil
	  (set-mark original-point)
	  (goto-char original-point)
          (save-buffer))
	)
    ;; Else, insert new version
    ;; in current buffer.
    ;; Position ourselves
    (ciao-goto-first-version-comment)
    ;; We are positioned: insert new comment
    (ciao-insert-version-comment 
     version-major version-minor patch-number month day year time 
     (concat comment "\n    (" user-full-name ")") )
    (fill-paragraph nil)
    (set-mark original-point)
    (save-buffer)
    )
  ))

(defun ciao-insert-version-comment 
  (version-major version-minor patch-number month day year time comment)
  "Insert a Ciao changelog entry in file at current point."
  (insert (concat 
		  ":- doc(version(" version-major "*" version-minor "+"
		      patch-number "," year "/" month "/" day ","
		      time "),\n   \"" comment "\").\n\n"))
  (search-backward-regexp "^[ \t]*:-[ \t\n]*\\(comment\\|doc\\)([ \t\n]*version(")
  )

(defun ciao-goto-first-version-comment ()
  "Position ourselves at first changelog entry if it exists"
  (goto-char (point-min))
  ;; If previous version exists
  (if (search-forward-regexp "^[ \t]*:-[ \t\n]*\\(comment\\|doc\\)([ \t\n]*version(" nil t)
      (beginning-of-line)
    ;; If no previous version exists
;;     (goto-char (point-min))
;;     (if (search-forward-regexp "^[ \t]*:-[ \t\n]*module(" nil t) t t)
;;     (ciao-next-blank-line-or-eof)
    (goto-char (point-max))))

(defun ciao-insert-version-control-off-comment ()
  (insert (concat
		  "\n%% Local Variables: \n"
		  "%% mode: CIAO\n"
		  "%% update-version-comments: \"off\"\n"
		  "%% End:\n\n")))

(defun ciao-insert-assertions-package-reminder ()
  (insert 
   (concat
    "\n"
    "%% *** Delete this comment after reading: it is only a reminder! ***\n"
    "%% \n" 
    "%% The \"assertions\" library needs to be included in order to support\n"
    "%% \":- doc(...,...).\" declarations such as below, i.e., insert: \n"
    "%% \n" 
    "%% :- module(_,_,[assertions]).\n" 
    "%% \n" 
    "%% At the beginning of the file:\n" 
    "%% The following version comment(s) can be moved elsewhere in the \n"
    "%% file. Subsequent version comments will always be placed above \n"
    "%% the last one inserted.\n\n"
    )))

;; (defun ciao-next-blank-line-or-eof ()
;;   (if (search-forward-regexp "^[ \t]*$" nil t)
;;       t
;;     nil))

;;;###autoload
(defun ciao-fetch-next-changelog-entry () 

   "When a unique version numbering is being maintained across several
files, this command allows inspecting all changes sequentially by
visiting all the files in which the changes were made:

    @begin{itemize}

    @item If in a source file, find the next changelog entry in the
source file, open in another window the corresponding
@file{GlobalChangeLog} file, and position the cursor at the
corresponding entry. This allows browsing the previous and following
changes made, which may perhaps reside in other files in the system.

   @item If in a @file{GlobalChangeLog} file, look for the next entry
in the file, and open in another window the source file in which the
corresponding comment resides, positioning the corresponding comment
at the top of the screen. This allows going through a section of the
@file{GlobalChangeLog} file checking all the corresponding comments in
the different files in which they occur.

    @end{itemize}

"

  (interactive)
  (let ((mbeg 0) (mend 0) original-buffer (version nil))
    (setq original-buffer (current-buffer))
    (if (not (search-forward-regexp 
	      "^[ \t]*:-[ \t]*\\(comment\\|doc\\)([ \t\n]*version("
	      nil t))
	(message "No (more) changelog entries found.")
      (setq mbeg (match-end 0))
      (recenter 0)
      (goto-char mbeg)
      (search-forward-regexp ")[ \t\n]*,")
      (setq mend (- (match-beginning 0) 1))
      (goto-char mend)
      (setq version (buffer-substring-no-properties mbeg mend))
      (if (string-match "GlobalChangeLog" (buffer-name))
	  ;; It is a changelog buffer: find matches in files
	  (progn
	    (search-forward "\"")
	    (setq mbeg (match-end 0))
	    (goto-char mbeg)
	    (search-forward "\"")
	    (setq mend (match-beginning 0))
	    (find-file-other-window (buffer-substring-no-properties mbeg mend))
	    (goto-char (point-min))
	    (search-forward version)
	    (beginning-of-line)
	    (recenter 0)
	    (ciao-switch-other-window original-buffer))
	;; It is a normal buffer: find entry in changelog buffer
	(if (or (string= (ciao-version-maint-type) "on") 
		(string= (ciao-version-maint-type) "off"))
	    (error "No GlobalChangeLog file is associated with this file")
	  (find-file-other-window (concat (ciao-version-maint-type) "/GlobalChangeLog"))
;;	  (ciao-mode-internal) 
;;	  ;; set buffer to Ciao mode so that bindings are active!
	  (goto-char (point-min))
	  (search-forward version)
	  (beginning-of-line)
	  (recenter 0)
	  (ciao-switch-other-window original-buffer))
	))))

;; (autoload 'ciao-mode-internal "ciao")

(defun ciao-handle-version-control-option ()
  "Look and see if there is a local variable designating whether
version control should be performed. If not (and global flag
ciao-ask-for-version-maintenance-type is set to yes) ask the user for
which type and add code to the buffer to set it up."
  (save-excursion
    (let ((option nil) (option-dir nil))
      (cond
       ((not (string= (ciao-version-maint-type) nil))
	;; local var already present: just return
	;; (message (concat "Local var found;value: "
	;;  (ciao-version-maint-type)))
	)
       ((string= ciao-ask-for-version-maintenance-type "yes")
	;; no local var: ask for it (if global flag allows)
	(setq option 
	      (read-string 
	       "Turn on changelog prompting on this file? (y/n/q) " "q"))
	(if (string= option "q")
	    (setq update-version-comments "off")
	  (goto-char (point-max))
	  (cond
	   ((string= option "n")
	    ;; do not maintain control
	    (ciao-insert-version-control-off-comment)
	    (message "Off - see comments inserted at end of file")
	    (setq update-version-comments "off"))
	   (t 
	    ;; maintain control - normal file
	    (setq option-dir
		  (read-file-name
		   "Name of directory with version file? (ret = this file) " 
		   "" "on" nil "on"))
	    ;; MR Added to avoid the bug when having control version in a
	    ;; directory which doesn't exist.
	    (if (string= option-dir "on")
		t
	      ;; Make sure the directory exists. If it doesn't exist then
	      ;; create the directory
	      (if (file-directory-p option-dir)
		  t
		(make-directory option-dir)))
	    
	    (ciao-insert-assertions-package-reminder)
	    (insert 
	     (concat
	      "\n:- doc(version_maintenance," 
	      (if (or (equal option-dir "on") (equal option-dir "off"))
		  option-dir
		(concat "dir('" option-dir "')" ))
	      ").\n\n"))
	    (message "On - see comments inserted at end of file")
	    (setq update-version-comments option-dir))
	   )))
       (t (setq update-version-comments "off")
	  )))))

;;;###autoload
(defun ciao-version-maint-type ()
"Determine the type of version control being done for the file."
  (interactive)
  (if (not (eq update-version-comments 0))
	update-version-comments
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp 
         "^[ \t]*:-[ \t\n]*\\(comment\\|doc\\)([ \t\n]*version_maintenance[ \t\n]*,[ \t]*" 
	   nil t)
	  (let ((begin 0))
	    (search-forward-regexp "dir([ \t\n]*'*" nil t)
	    (setq begin (point))
	    (search-forward-regexp "'*[ \t]*)" nil t)
	    (goto-char (match-beginning 0))
	    (setq update-version-comments 
		  (buffer-substring-no-properties begin (point)))
	    (message (concat "DIR: " update-version-comments))
	    )
	(setq update-version-comments nil)
	update-version-comments
        ))))

;;------------------------------------------------------------
;; Kludge to fix old version maintenance entries...
;;------------------------------------------------------------

; Probably does not work in xemacs...
;;;###autoload
(defun ciao-fix-old-version-maintenance ()
  (interactive)
  (goto-char (point-min))
  (if (search-forward "%% Control version comment prompting for" nil t)
      (let (tmp)
	(beginning-of-line)
	(kill-line 3)
	(next-line 1)
	(kill-line 1)
	(previous-line 1)
	(beginning-of-line)
	(set-mark (point))
	(search-forward "version-comments:")
	(search-forward "\"")
	(kill-region (mark) (point))
	(set-mark (point))
	(search-forward "\"")
	(backward-char 1)
	(setq tmp (buffer-substring-no-properties (mark) (point)))
	(kill-region (mark) (point))
	(kill-line 1)
	(insert 
	 (concat
	  ":- doc(version_maintenance,"
	  (cond
	   ((equal tmp "on") "on")
	   ((equal tmp "off") "off")
	   (t (concat "dir('" tmp "')")))
	  ").\n"
	  )))
    (error "Could not find version maintenance comment")))


;; Provide ourselves:

(provide 'ciao-vc)

;;; ciao-vc.el ends here

