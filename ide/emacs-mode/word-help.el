;; word-help.el -- keyword help/completion for languages doc'd in TeXinfo.

;; Copyright (c) 1996-97 Free Software Foundation, Inc.

;; Maintainer: Jens T. Berger Thielemann, <jensthi@ifi.uio.no>
;; Keywords: help, keyword, languages, completion

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file GPL.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides a rather general interface for doing keyword
;; help in most languages.  In short, it'll determine which TeXinfo
;; file which is relevant for the current mode; cache the index and
;; use regexps to give you help on the keyword you're looking at.
;;
;; In addition, it will add keyword completion for the same modes.

;; Installation
;; ************

;; For the default setup to work for all supported modes, make sure
;; the Texinfo files from the following packages are installed:

;; Texinfo file   | Available in archive or URL | Notes
;; autoconf.info  | autoconf-2.10.tar.gz        | -
;; bison.info     | bison-1.25.tar.gz           | -
;; libc.info      | glibc-1.09.1.tar.gz         | -
;; elisp.info     | elisp-manual-19-2.4.tar.gz  | -
;; latex.info     | ftp.dante.de:pub/tex/info/latex2e-help-texinfo/latex2e.texi
;; groff.info     | groff-1.10.tar.gz           | -
;; m4.info        | m4-1.4.tar.gz               | -
;; make.info      | make-3.75.tar.gz            | -
;; perl.info      | http://www.perl.com/CPAN/doc/manual/info/
;; simula.info    | Mail bjort@ifi.uio.no       | Written in Norwegian
;; texinfo.info   | texinfo-3.9.tar.gz          | -

;; BTW: We refer to Texinfo files by just their last component, not
;; with an absolute file name.  You must thus set up
;; `Info-directory-list' and `Info-default-directory-list' so that
;; these can automatically be located.

;; Manual installation of the package is done by adding the following
;; lines to your .emacs file:

;;    (autoload 'set-help-file "word-help"
;;      "Sets the set of Texinfo files used for `word-help' to HELPFILE."
;;      t nil)
;;    (autoload 'word-help "word-help"
;;      "Find documentation on the KEYWORD under the cursor (or passed)."
;;      t nil)
;;    (autoload 'word-help-complete "word-help"
;;      "Perform completion on the symbol preceding the point." t nil)
;;    (autoload 'word-help-add-keywords "word-help"
;;      "Add user keywords to a certain Info file." nil nil)
;;    (define-key help-map [?\C-i] 'word-help)
;;    (global-set-key [\C-tab] 'word-help-complete)

;; Usage
;; *****
;;
;; Place the cursor over the function/variable/type/whatever you want
;; help on.  Type "C-h C-i".  `word-help' will then make a suggestion
;; to an index topic; press return to accept this.  If not, you may use
;; tab-completion to find the topic you're interested in.

;; `word-help' is also able to do symbol completion via the
;; `word-help-complete' function (normally bound to C-TAB).
;; Note that some modes automatically override this key; you may
;; therefore wish to either put the above statement in a hook or
;; associate the function with an other key.

;; Usually, `word-help' is able to determine the relevant Texinfo
;; file from looking at the buffer's `mode-name'; if not, you can use
;; the interactive function `set-help-file' to set this.

;; If you wish to temporarily get help on another language, give
;; `word-help' a prefix arg (e.g. press "C-u C-h C-i").

;; Customizing
;; ***********
;;
;; User interface
;; --------------
;;
;; Two variables control the behaviour of the user-interface of
;; `word-help': `word-help-split-window-flag' and
;; `word-help-magic-index-flag'. Do C-h v to get more information on
;; these.

;; Adding more Texinfo files
;; -------------------------
;;
;; Associations between mode-names and Texinfo files can be done
;; through the `word-help-mode-alist' variable, which defines an
;; `alist' making `set-help-file' able to initialize the necessary
;; variable.

;; Contacting the author
;; *********************
;;
;; If you wish to contact me for any reason, please feel free to write
;; to:

;; Jens Berger
;; Spektrumveien 4
;; N-0666 Oslo
;; Norway
;;
;; E-mail: <jensthi@ifi.uio.no>

;; Have fun.

;; RCS $Id: word-help.el,v 1.31 1997/12/29 13:09:41 jensthi Exp $

;;
;;; Code:
;;

(require 'info)

;;--------------------
;;    USER OPTIONS
;;--------------------


(defvar word-help-split-window-flag t
  "*Non-nil means that the info buffer will pop up in a separate window.
If nil, we will just switch to it.")

(defvar word-help-magic-index-flag t
  "*Non-nil means that the keyword will be searched for in the requested node.
This is done by determining whether the line the point is positioned
on after using `Info-goto-node', actually contains the keyword.  If
not, we will search for the first occurence of the keyword.  This may
help when the info file isn't correctly indexed.")

;;; ---- end of user configurable variables

;;;-------------------------
;;   ADVANCED USER OPTIONS
;;-------------------------

(defconst :files ':files)
(defconst :keyword ':keyword)
(defconst :ignore-case	':ignore-case)
(defconst :index-map ':index-map)
(defconst :complete ':complete)
(defconst :magic-index ':magic-index)

(defvar word-help-load-hook nil
  "Hooks to run when the `word-help' package is loaded.")

(defvar word-help-mode-alist nil
  "Assoc list between `mode-name' and Texinfo files.
The variable should be initialized with a list of elements with the
following form:

\(mode-name (word-help-info-files) (word-help-keyword-regexps)
	   `word-help-ignore-case' `word-help-index-mapper'
           `word-help-complete-list' `word-help-magic-index')

where `word-help-info-files', `word-help-keyword-regexps' and so forth
are the values to put in the respective variables¹ for the mode
specified.  Note that `mode-name' doesn't have to be a legal `mode-name';
the user may use the call `set-help-file', where `mode-name' will be
used in a `completing-read'.

A less macho way of initing this structure is to use the
`word-help-add-mode' call.

Example entry (for C):

\(\"C\" ((\"libc\" \"Type Index\" \"Function Index\" \"Variable Index\"))
       ((\"[A-Za-z_][A-Za-z0-9]+\")))

The two first variables must be initialized; the remaining will
get default values if you omit them or set them to nil.  The default
values are:

word-help-keyword-regexps: ((\"[A-Za-z_][A-Za-z0-9]+\"))
word-help-ignore-case:     nil
word-help-index-mapper:    ((\"^\\\\([^ \\t\\n]+\\\\)\" 1))
word-help-complete-list:   ((\"[A-Za-z_][A-Za-z0-9]+\"))
word-help-magic-index:     ((\"^ - [A-Za-z ]+: \" keyword)
  			    (\"^ - [A-Za-z ]+: [A-Za-z ]+\" keyword)
			    (\"^[`\\\"]\" keyword \"['\\\"]\")
			    (\"^[^A-Za-z \\t]+\" keyword)
			    (\"[`\\\"\\(]\" keyword \"['\\\"\\)]\")
			    (\"^\" keyword)
			    (keyword))


More settings may be defined in the future.

You may also define aliases, if there are several relevant mode-names
to a single entry.  These should be of the form:

\(MODE-NAME-ALIAS . MODE-NAME-REAL)

For C++, you would use the alias

\(\"C++\" . \"C\")

to make C++ mode use the same help files as C files do.  Please note
that you can shoot yourself in the foot with this possibility by
defining recursive aliases.

¹ Now, these are functions doing lookup. But they still contain the
  necessary documentation.")

;;;###autoload
(defun word-help-add-mode (mode params)
  "Associates info files and stuff with a given mode.
This is just a user-friendly way to update the `word-help-mode-alist'
variable. MODE is the mode you wish to update, while PARAMS is
the parameters for the current mode, given in a key-value fashion.

Example:

 (word-help-add-mode \"Bison\"
	 	     '( :files    ((\"bison\" \"Index\")
				   (\"libc\" \"Type Index\" \"Function Index\"
                                    \"Variable Index\"))
		        :keyword  ((\"%[A-Za-z]*\")
				   (\"[A-Za-z_][A-Za-z0-9_]*\"))
		        :complete ((\"%[A-Za-z]*\" nil nil ((\"^%\")))
				   (\"[A-Za-z_][A-Za-z0-9_]*\" nil nil
                                    ((\"[A-Za-z_][A-Za-z0-9_]*\")))
				   word-help-wrapt-complete-tag)))

Currently supported  keys are (with the \"variables\" they refer to - do
\\[describe-function] to get more help on each keyword). The nice thing
about this interface is that you don't have to remember the order of
the items and that nil's are filled in automagically.

Keyword		Documentation item
:files		`word-help-info-files'
:keyword	`word-help-keyword-regexps'
:ignore-case	`word-help-ignore-case'
:index-map	`word-help-index-mapper'
:complete	`word-help-complete-list'
:magic-index	`word-help-magic-index'

See also `word-help-add-alias'."
  (let (entry old key value assoc-list)
    ;; Make it into an assoc list
    (while params
      (setq key    (car params)
	    value  (car (cdr params))
	    params (cdr (cdr params)))
      (setq assoc-list (cons (cons key value) assoc-list)))
    (setq entry (list (cdr (assq :files       assoc-list))
		      (cdr (assq :keyword     assoc-list))
		      (cdr (assq :ignore-case assoc-list))
		      (cdr (assq :index-map   assoc-list))
		      (cdr (assq :complete    assoc-list))
		      (cdr (assq :magic-index assoc-list))))
    (setq old (assoc mode word-help-mode-alist))
    (if old
	(setcdr old entry)
        (setq word-help-mode-alist (cons (cons mode entry)
					 word-help-mode-alist)))))


;;;###autoload
(defun word-help-add-alias (new-mode old-mode)
  "Adds an alias for a mode for the `word-help' system.
NEW-MODE is the alias while OLD-MODE is the system to use. This is
useful when there are more than one mode available for a given
programming language."
  (let ((old (assoc new-mode word-help-mode-alist)))
    (if old
	(setcdr old old-mode)
        (setq word-help-mode-alist (cons (cons new-mode old-mode)
					 word-help-mode-alist)))))



;;; Wrappers for external routines

(defmacro word-help-t-wrapper (function &optional load-file args)
  "Creates function named `word-help-wrapt-FUNCTION' which calls FUNCTION.
This wrapper always returns t. Useful for completion, where such
interfaces may be necessary. Takes note of interactive calls.

In order to avoid warnings, you may specify LOAD-FILE. We will then do
an autoload statement, which will load the file whenever this function
is called. The function will in such case be marked as interactive.

You may specify an argument list ARGS which will be passed on."
  (list 'eval-when-compile (if load-file (autoload 'function load-file "Not loaded yet." t)))
  `(progn
     (if ,load-file
	 (autoload (quote ,function) ,load-file "Not loaded yet." t))
     (defun
       ,(make-symbol (concat "word-help-wrapt-" (symbol-name function)))
       ,args
       ,(concat "Wrapper for " (symbol-name function) " returning t.")
       (interactive)
       (let ((prefix-arg current-prefix-arg))
	 (if (interactive-p)
	     (call-interactively ',function)
	   (,function ,@args)
	   t)))))

(word-help-t-wrapper lisp-complete-symbol)
(word-help-t-wrapper makefile-complete "make-mode")
(word-help-t-wrapper ispell-complete-word "ispell")
(word-help-t-wrapper complete-tag)


;;; --- end of advanced user options


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; Lookup functions & defaults ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro word-help-lookup (index &optional default)
  "Lookup macro for `word-help-mode-alist'.
INDEX is list element, DEFAULT is obvious."
  `(cond
    ((nth ,index (assoc help-mode word-help-mode-alist)))
    (t ,default)))

(defmacro word-help-lookup-sym (index &optional default)
  "Lookup macro for `word-help-mode-alist'.
INDEX is list element, DEFAULT is obvious. Returns (result)
if result is a symbol."
  `(let ((result (word-help-lookup ,index ,default)))
     (if (symbolp result) (list result) result)))
 

(defsubst word-help-info-files (help-mode)
  "List of relevant info files with respective nodes for HELP-MODE.

This should be a list of the following form:

\((INFO-FILE-1 NODE-NAME-1 NODE-NAME-2 ...)
 (INFO-FILE-1 NODE-NAME-1 NODE-NAME-2 ...)
      :           :           :
 (INFO-FILE-1 NODE-NAME-1 NODE-NAME-2 ...))

An example entry for e.g. C would be:

\((\"/local/share/gnu/info/libc\" \"Function Index\" \"Type Index\"
  \"Variable Index\"))

The files and nodes will be searched/cached in the order specified.
This variable is usually set by the `word-help-switch-help-file'
function, which utilizes  the `word-help-mode-alist'."
  (word-help-lookup-sym 1))

(defsubst word-help-keyword-regexps (help-mode)
  "Regexps for finding keywords in HELP-MODE.

This is constructed as a list of the following form:

\((REGEXP SUBMATCH-LOOKUP SUBMATCH-CURSOR)
 (REGEXP SUBMATCH-LOOKUP SUBMATCH-CURSOR)
       :          :          :
 (REGEXP SUBMATCH-LOOKUP SUBMATCH-CURSOR))

The regexps will be searched in order, until a match is found.

SUBMATCH-LOOKUP is the submatch number which will be looked for in the
index.  May be omitted; defaults to 0 (e.g. the entire pattern).  This is
useful for ignoring prefixes or similar, for instance the optional \"m4_\"
prefix in m4. The following regexp will do this:

 (\"\\\\(m4_\\\\|)\\\\([A-Za-z0-9]+\\\\)\" 2)

SUBMATCH-cursor is the part of the match which the cursor must be within.
May be omitted; defaults to 0 (e.g. the entire pattern)."
  (word-help-lookup 2 '(("[A-Za-z_][A-Za-z_0-9]*"))))

(defsubst word-help-ignore-case (help-mode)
  "Non-nil means that case is ignored when doing lookup in mode HELP-MODE."
  (word-help-lookup 3))

(defsubst word-help-index-mapper (help-mode)
  "Regexps to use for massaging index-entries into keywords in mode HELP-MODE.
This variable should contain a list of regexps with sub-expressions.
Only the submatch specified will be entered as a keyword into our
hashtable.

The regexp list should be formatted as:

  ((REGEXP SUBEXP) (REGEXP SUBEXP) ... )

If the index entry does not match any of the regexps, it will be ignored.

Example:

Perl has index entries of the following form:

* abs VALUE:                    perlfunc.
* accept NEWSOCKET,GENERICSOCKET: perlfunc.
* alarm SECONDS:                perlfunc.
* atan2 Y,X:                    perlfunc.
* bind SOCKET,NAME:             perlfunc.
         :             :           :

We will thus try to extract the first word in the index entry -
\"abs\" from \"abs VALUE\", etc.  This is done by the following entry:

\((\"^\\\\([^ \\t\\n]+\\\\)\" 1))

This value is the default one, and works with most Texinfo files"
  (word-help-lookup 4 '(("^\\([^ \t\n]+\\)" 1))))


(defsubst word-help-complete-list (help-mode)
  "Regexps or function to use for completion of symbols in mode HELP-MODE.
The list should have the following format:

  ((REGEXP SUBMATCH TEXT-APPEND (RE-FILTER-1 RE-FILTER-2 ...)
           :               :             :               :      :
   (REGEXP SUBMATCH TEXT-APPEND (RE-FILTER-1 RE-FILTER-2 ...))

The two first entries are similar to `word-help-keyword-regexps',
REGEXP is a regular expression which should match any relevant
expression, and where SUBMATCH should be used for look up. By
specifying non-nil REGEXP-FILTERs, we'll only include entries in the
index which matches the regexp specified.

If the contents of this variable is a symbol of a function, this
function will be called instead. This is useful for modes providing
a more intelligent function (like `lisp-complete-symbol' in Emacs Lisp mode).

You may also mix the two types, e.g.

 ((REGEXP SUBMATCH TEXT-APPEND (RE-FILTER-1 REG-FILTER-2 ...)
    :
  SYMBOL
    :
  (REGEXP SUBMATCH TEXT-APPEND (RE-FILTER-1 REG-FILTER-2 ...))

where we we'll try all until a match is found (functions should return
a non-nil value if they produced a match). If you are using callback
functions, you may use `word-help-t-wrapper' to make a function which
always returns t. This is intended for backward compatibility.

Non-nil TEXT-APPEND means that this text will be inserted after the
completion, if we manage to do make a completion."
  (word-help-lookup-sym 5 '(("[A-Za-z_][A-Za-z_0-9]*"))))

(defsubst word-help-magic-index (help-mode)
  "Transformations to apply to a keyword in order to make it searchable.
HELP-MODE is the string representing the current value of `mode-name'.
`word-help' will take each sublist, substitute the symbol
`keyword' with a quoted version of the keyword to find, and concatenate
each entry in the list.

That is, for autoconf files, where each entry starts with \"AC_\", you
could for instance use the following value:

  ((\"AC_\" keyword)))

We use significantly more magic by default."
  (word-help-lookup 6 '(("^ - [A-Za-z ]+: " keyword)
			("^ - [A-Za-z ]+: [A-Za-z ]+" keyword)
			("^[`\"]" keyword "['\"]")
			("^[^A-Za-z \t]+" keyword)
			("[`\"\(]" keyword "['\"\)]")
			("^" keyword)
			(keyword))))


;;; Work variables

(defvar word-help-history nil
  "History for `word-help' minibuffer queries.")
(make-local-variable 'word-help-history)

(defvar word-help-index-alist nil
  "An assoc list mapping help files to info indexes.")

(defvar word-help-complete-alist nil
  "An Assoc list mapping help modes to completion words.")

(defvar word-help-info-extra-alist nil
  "An assoc list specifying extra keywords for an Info file.
Use `word-help-add-keywords' to initialize. Format:

 ((info-file [words]) ... )")

(defvar word-help-help-mode nil
  "Which mode the help system is bound to for the current mode.")
(make-variable-buffer-local 'word-help-help-mode)

(defvar word-help-last-help nil
  "Which language we gave help on last time.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; User Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Debugging

(defun reset-word-help ()
  "Clears various information in the `word-help' system."
  (interactive)
  (setq word-help-index-alist nil
	word-help-complete-alist nil))


;;; Main user interface

;;;###autoload
(defun word-help (keyword &optional help-system)
  "Find documentation on the KEYWORD under the cursor (or passed).
The determination of which language the keyword belongs to, is based
upon matching `mode-name' (the major mode) against the assoc list
`word-help-mode-alist'. You may also supply HELP-SYSTEM in order to
override this.

If this is not possible, `set-help-file' will be invoked for selecting
the relevant info file.  `set-help-file' may also be invoked
interactively by the user.

If you supply a prefix arg via \\[universal-argument], we'll invoke
`set-help-file'anyway, and do a *temporary* change of subject.

If the keyword you are looking at is not available in any index, no
default suggestion will be presented."
  (interactive
   (let (myguess guess this-help
		 (old-help word-help-help-mode)
		 (completion-ignore-case t))
     (if current-prefix-arg
	 (progn
	   (setq old-help word-help-help-mode)
	   (call-interactively 'set-help-file)))
     (while (not word-help-help-mode)
       (word-help-find-help-file))
     (setq this-help word-help-help-mode)
     (setq myguess (word-help-guess this-help))
     (setq guess (completing-read
		  (if myguess
		      (format "Look up keyword (default %s): " myguess)
		      "Look up keyword: ")
		  (word-help-get-index this-help)
		  'word-help-only-helpable t nil 'word-help-history))
     (if (equal guess "")
	 (setq guess myguess))
   (if (or old-help current-prefix-arg) (set-help-file old-help))
   (list guess this-help)))
  ;; Start of non-interactive part

  (let (index-info prev-help)
    (and
     (or
      keyword
      (not (message "Help aborted."))
      nil)
     (or
      help-system
      (and (setq prev-help word-help-help-mode) nil)
      (not (set-help-file help-system))
      (not (message "Help not defined for '%s'." help-system))
      nil)
     (let ((this-help (or help-system word-help-help-mode)))
       (word-help-show-keyword keyword this-help)
       (setq word-help-last-help this-help)
       (if prev-help (set-help-file prev-help))
       t))))


;;;###autoload
(defun word-help-complete ()
  "Perform completion on the symbol preceding the point.
The determination of which language the keyword belongs to, is based upon
The relevant info file is selected by matching `mode-name' (the major
mode) against the assoc list `word-help-mode-alist'.

If this is not possible, `set-help-file' will be invoked for selecting
the relevant info file.  `set-help-file' may also be invoked
interactively by the user.

The keywords are extracted from the index of the info file defined for
this mode, by using the `word-help-complete-list' variable."
  (interactive "*")
  (while (not word-help-help-mode)
    (word-help-find-help-file))
  (let* ((cmpl-idx (word-help-get-complete word-help-help-mode))
	 (cmpl-this nil) (completion nil)
	 (close nil) (words nil) (completed nil)
	 (all-match (word-help-guess-all
		     cmpl-idx (word-help-ignore-case word-help-help-mode) t))
	 (completion-ignore-case (word-help-ignore-case
				  word-help-help-mode))
	 (this-match nil))

    ;; Loop over and try to find a match wor
    (while (and all-match (not completed))
      (setq this-match (car all-match)
	    all-match (cdr all-match)
	    cmpl-this (car cmpl-idx)
	    cmpl-idx (cdr cmpl-idx))
      (cond
       ;; Ignore non-matches
       ((null this-match))
       ;; Use backend?
       ((symbolp this-match)
	(setq completed
	      (if (interactive-p)
		  (call-interactively this-match)
		(eval (list this-match)))))
       (this-match
	(setq close (nth 3 cmpl-this)
	      words (nth 4 cmpl-this)
	      ;; Find the maximum completion for this word
	      completion (try-completion this-match words))

	(cond
	 ;; Was the match exact
	 ((eq completion t)
	  (and close
	       (not (looking-at (regexp-quote close)))
	       (insert close))
	  (setq completed t))

	 ;; Silently ignore non-matches
	 ((not completion))

	 ;; May we complete more unambiguously
	 ((not (string-equal completion this-match))
	  (delete-region (- (point) (length this-match))
			 (point))
	  (insert completion)
	  ;; Was the completion full?
	  (if (eq t (try-completion completion words))
	      (progn
		(and close
		     (not (looking-at (regexp-quote close)))
		     (insert close))))
	  (setq completed t))

	 ;; Just part-match found. Show completion list
	 (t
	  (message "Making completion list...")
	  (let ((list (all-completions this-match words nil)))
	    (setq completed list)
	    (with-output-to-temp-buffer "*Completions*"
	      (display-completion-list list))
	  (message "Making completion list...done")))))))
    (if (not completed) (message "No match."))))

;;;###autoload
(defun word-help-add-keywords (file words)
  "Add user keywords to a certain Info file.
FILE is the info file (this may in turn be used by multiple Emacs modes),
while WORDS is a list of lists of the following form:

  ((KEYWORD NODE) (KEYWORD NODE) ... (KEYWORD NODE))

If you don't specify a node, no help will be available; however, the keyword
will still be available for completion."
  (let ((extra-list (assoc file word-help-info-extra-alist)))
    (if extra-list
      (setcdr extra-list (append (cdr extra-list) words))
      (setq word-help-info-extra-alist
	    (cons (cons file words)
		  word-help-info-extra-alist)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; Switch mode files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Mode lookup

;;;###autoload
(defun set-help-file (helpfile)
  "Sets the set of Texinfo files used for `word-help' to HELPFILE.

`word-help' maintains a list over which Texinfo files which are
relevant for each programming language (`word-help-mode-alist').  It
usually selects the correct one, based upon the value of `mode-name'.
If this guess is incorrect, you may also use this function manually to
instruct future `word-help' calls which Texinfo files to use.

Returns nil if no mode was set."
  (interactive
   (list
    (let (helpfile helpguess (completion-ignore-case t))
;; Try to make a guess
      (setq helpguess (word-help-guess-help-file))
;; Ask the user
      (setq helpfile (completing-read
		      (if helpguess
			  (format "Select help mode (default %s): " helpguess)
			"Select help mode: ")
		      word-help-mode-alist
		      nil t nil nil))
      (if (equal "" helpfile)
	  (setq helpfile helpguess))
      helpfile)))
  (let ((helpdesc (assoc helpfile word-help-mode-alist)))
    (cond
     ((string-equal helpfile word-help-help-mode))
     ((stringp (cdr-safe helpdesc))
      (set-help-file (cdr helpdesc)))
     (helpdesc
      (setq word-help-help-mode helpfile))
     (t
      (message "No help defined for \"%s\"." helpfile)
      nil))))

(defun word-help-find-help-file ()
  "Finds and sets a relevant help file for the current mode.
May ask the user interactively."
  (let (helpguess)
    (cond
     ((and word-help-last-help (string-equal mode-name "Info"))
      (set-help-file word-help-last-help))
     ((not word-help-help-mode)
      (if (setq helpguess (word-help-guess-help-file))
	  (set-help-file helpguess)
	  (call-interactively 'set-help-file))))))

(defun word-help-guess-help-file ()
  "Guesses a relevant help file based on mode name.
Returns nil if no guess could be made.  Uses `word-help-mode-alist'."
  (car-safe (assoc mode-name word-help-mode-alist)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Index mapping ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun word-help-map-index-entries (str re-list)
  "Transform the Info index entry `STR' into a programming keyword.
Uses this by mapping the entries through RE-LIST (from
`word-help-index-mapper'). Nil RE-LIST will just return the str."
  (let ((regexp (car (car re-list)))
	(subexp (car (cdr (car re-list))))
	(next (cdr re-list)))
    (cond
     ((not re-list) str)
     ((string-match regexp str)
      (substring str (match-beginning subexp) (match-end subexp)))
     (next
      (word-help-map-index-entries str next)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Cache management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun word-help-keyword-insert (ob-array keyword file index
				 node ignore-case)
  "Inserts into OB-ARRAY the word KEYWORD, found in info file FILE.
`word-help' supports two ways of finding a help node, either via first
going to the node INDEX in file and then jumping to the index entry
NODE, or by going directly to the node NODE via `Info-goto-node'. The
latter is done if INDEX is nil.

In any case, FILE must be set. IGNORE-CASE decides whether we'll later
support case-insensitive matching."
  (if (and keyword file)
      (let ((cmd (if ignore-case (downcase keyword) keyword)))
	(if (or index node)
	    (progn
	      (put (intern      cmd ob-array) 'word-help-file file)
	      (put (intern-soft cmd ob-array) 'word-help-index index)
	      (put (intern-soft cmd ob-array) 'word-help-node node))
	    (if (not (intern-soft cmd ob-array))
	        (intern cmd ob-array))))))

(defun word-help-keyword-file (ob-array keyword ignore-case)
  "Finds the entry in OB-ARRAY of KEYWORD, and returns the associated file.
Case-insensitive if IGNORE-CASE is non-nil."
  (let* (cmd sym)
    (and
     (setq cmd (if ignore-case (downcase keyword) keyword))
     (setq sym (intern-soft cmd ob-array))
     (get (intern-soft cmd ob-array) 'word-help-file))))

(defun word-help-keyword-index (ob-array keyword ignore-case)
  "Finds the entry in OB-ARRAY of KEYWORD, and returns the associated index.
Case-insensitive if IGNORE-CASE is non-nil."
  (let* (cmd sym)
    (and
     (setq cmd (if ignore-case (downcase keyword) keyword))
     (setq sym (intern-soft cmd ob-array))
     (get (intern-soft cmd ob-array) 'word-help-index))))

(defun word-help-keyword-node (ob-array keyword ignore-case)
  "Finds the entry in OB-ARRAY of KEYWORD, and returns the associated node.
Case-insensitive if IGNORE-CASE is non-nil."
  (let* (cmd sym)
    (and
     (setq cmd (if ignore-case (downcase keyword) keyword))
     (setq sym (intern-soft cmd ob-array))
     (get (intern-soft cmd ob-array) 'word-help-node))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Index collection ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Extract index from file and index, and insert into INDEX-OB. FILE
;; is the name of the info file, while INDEX is a list of node-names
;; to search. Case will be ignored if IGNORE-CASE is non-nil. Keywords
;; found will be processed through INDEX-MAP, as documented in
;; `word-help-index-mapper'."

(defun word-help-extract-index (index-ob filename indexname ignore-case index-map)
  (let (topic keyword)
    (message "Processing \"%s\" in %s..." indexname filename)
    (save-window-excursion
      (Info-goto-node (concat "(" filename ")" indexname))
      (goto-char (point-max))
      (while (re-search-backward "\\* \\([^\n:]+\\):" nil t)
	(setq topic (buffer-substring (match-beginning 1) (match-end 1)))
	(setq keyword (word-help-map-index-entries topic index-map))
	(word-help-keyword-insert index-ob keyword filename indexname topic ignore-case)))))

(defun word-help-get-index (help-mode)
  "Process all the entries in the global variable `word-help-info-files'.
Returns an ob-array of all the symbols found, with the following
properties on each symbol:

  word-help-file:  The file name where the symbol is to be found
  word-help-index: Which index we must walk through in order to look
                   up the symbol
  word-help-node:  If `word-help-index' is nil, this specifies the
                   node to jump to. If not, this will be the menu item
                   in the node to select.


If `word-help-file' is not set, it is assumed that it isn't possible
to get info on this particular topic.

Note that we use `word-help-index-alist' to speed up the process. Note
that `word-help-help-mode' must be set must have been called before this
function.

If no help could be found for the HELP-MODE passed, nil is returned."
  (let ((ignore-case (word-help-ignore-case help-mode))
	(index-map (word-help-index-mapper help-mode))
	index-ob index-ass array)
    (cond
     ;; See whether we have a copy cached
     ((setq index-ass (assoc help-mode word-help-index-alist))
      (setq index-ob (car (cdr index-ass))))
     ;; Try to build it ourselves
     ((word-help-info-files help-mode)
      (setq index-ob (make-vector 307 0))
      (mapcar (lambda (file-node-list)
		(let ((file (car file-node-list))
		      (nodes (cdr file-node-list)))
		  ;; First do the indexes in the file itself
		  (mapcar (lambda (node)
			    (word-help-extract-index index-ob file node ignore-case index-map))
			  nodes)
		  ;; Then the user-supplied indexes
		  (mapcar (lambda (name-node)
			    (word-help-keyword-insert index-ob (car name-node) file
						      nil (car (cdr name-node))
						      ignore-case))
			  (cdr (assoc file word-help-info-extra-alist)))))
		(word-help-info-files help-mode))
      ;; Update the cache
      (setq word-help-index-alist
	    (cons (list help-mode index-ob)
		  word-help-index-alist))
      index-ob)
     (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Keyword guess ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun word-help-guess-all (re-list ignore-case &optional copy-to-point)
  "Guesses *all* keywords the user possibly may be looking at.
Returns a list of all possible keywords.

RE-LIST should be a list of lists using the format

  ((regexp submatch cursmatch)
   (regexp submatch cursmatch)
      :       :         :
   (regexp submatch cursmatch))

You may also place symbols as list elements (they will simply be returned).
IGNORE-CASE makes us, uh, ignore case. If COPY-TO-POINT is non-nil, we'll
just return everything up to the point, and not the complete match."
  (let ((cur-point (point))
	(case-fold-search ignore-case)
	end-point result)
    (save-excursion
      (end-of-line)
      (setq end-point (point)
	    result (mapcar
		    (lambda (reglist)
		      (cond
		       ((symbolp reglist)
			reglist)
		       ((string-match (car reglist) "")
			(error "The regexp \"%s\" matches the empty string" (car reglist))
			nil)
		       (t
			(let ((regexp (car reglist))
			      (submatch  (cond ((nth 1 reglist)) (t 0)))
			      (cursmatch (cond ((nth 2 reglist)) (t 0)))
			    guess)
			  ;; Start at the beginning
			  (beginning-of-line)
			  (while (and (not guess) (re-search-forward regexp end-point t))
			    ;; Look whether the cursor is within the match
			    (if (and (<= (match-beginning cursmatch) cur-point)
				     (>= (match-end cursmatch) cur-point))
				(if (or (not copy-to-point) (<= cur-point (match-end submatch)))
				    (setq guess (buffer-substring (match-beginning submatch)
								  (if copy-to-point
								      cur-point
								    (match-end submatch)))))))
			  guess))))
		    re-list))
      result)))
  
  

(defun word-help-guess (help-mode)
   "Guesses what keyword the user is looking at, and returns that.
See also `word-help-keyword-regexps' and `word-help-mode-alist' for
ways to customize this.

Look up in these variables is done by using HELP-MODE."
  (let* ((ignore-case (word-help-ignore-case help-mode))
	 (all-matches (word-help-guess-all (word-help-keyword-regexps help-mode)
					   ignore-case))
	 (index-ob (word-help-get-index help-mode)))
    (word-help-first-match (lambda (keyword)
			     (word-help-keyword-file index-ob keyword ignore-case))
			   all-matches)))

(defun word-help-first-match (ok-func match-list)
  "Applies OK-FUNC to each elem in MATCH-LIST, returns first OK element.
That is, returns the first element where OK-FUNC returns a non-nil value."
  (cond
   ((eval (list ok-func (car match-list)))
    (car match-list))
   ((cdr match-list)
    (word-help-first-match ok-func (cdr match-list)))
   (t nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;	Show node for keyword ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun word-help-show-keyword (keyword help-mode)
  "Shows help for KEYWORD in the mode HELP-MODE.
This is a lowlevel routine, please use `word-help' instead."
  (let* ((popit (and (not (string-equal mode-name "Info")) word-help-split-window-flag))
	 (index-ob (word-help-get-index help-mode))
	 (ignore-case (word-help-ignore-case help-mode))
	 (node-name (word-help-keyword-node index-ob keyword ignore-case))
	 (file-name (word-help-keyword-file index-ob keyword ignore-case))
	 (index-name (word-help-keyword-index index-ob keyword ignore-case))
	 (buffer (current-buffer)))
    (if (not file-name)
	(message "Can not find help on '%s' in '%s'." keyword help-mode)
      (if popit (pop-to-buffer nil))
      (if (not index-name)
	  (Info-goto-node (concat "(" file-name ")" node-name))
	  (Info-goto-node (concat "(" file-name ")" index-name))
	  (Info-menu node-name))
      ;; Do magic keyword search
      (if word-help-magic-index-flag
	  (let (regs this-re found entry-re)
	    ;; Quote the actual keyword
	    (setq entry-re (concat (if (string-match "^[A-Za-z]" keyword) "\\<")
				   (regexp-quote keyword)
				   (if (string-match "[A-Za-z]$" keyword) "\\>")))
	    ;; Apply the magic indexing stuff
	    (setq regs (mapcar
			(lambda (reg-expr)
			  (mapconcat
			   (lambda (str-or-sym)
			     (cond
			      ((symbolp str-or-sym)
			       entry-re)
			      ((stringp str-or-sym)
			       str-or-sym)
			      (t
			       (error "Not symbol or string in word-help-magic-index"))))
			   reg-expr ""))
			(word-help-magic-index help-mode)))
	    ;; Try to find it.
	    (while (and (not found) (car regs))
	      (setq this-re (car regs)
		    regs (cdr regs)
		    found (re-search-forward this-re nil t)))
	    (recenter 0)))
      (if popit (pop-to-buffer buffer)))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun word-help-get-complete (help-mode)
  "Returns the ob-array for completion associated with the HELP-MODE passed."
  (let ((ignore-case (word-help-ignore-case help-mode))
	cmpl-idx index-ass cmpl-list)
    (cond
     ;; See whether we have a copy cached
     ((setq index-ass (assoc help-mode word-help-complete-alist))
      (car (cdr index-ass)))
     ;; Try to build it ourselves
     ((setq cmpl-list (word-help-complete-list help-mode))
      (setq cmpl-idx (word-help-build-complete (word-help-get-index help-mode) cmpl-list))
      ;; Update the cache
      (setq word-help-complete-alist
	    (cons (list help-mode cmpl-idx)
		  word-help-complete-alist))
      cmpl-idx)
     (t nil))))

(defun word-help-build-complete (index-ob cmpl-list)
  (mapcar (lambda (cmpl)
	    (cond
	     ((symbolp cmpl)
	      cmpl)
	     (t
	      (let ((regexp (car cmpl))
		    (subm (cond ((nth 1 cmpl)) (0)))
		    (cursm (cond ((nth 1 cmpl)) (0)))
		    (app (cond ((nth 2 cmpl)) ("")))
		    (re-list (cond ((nth 3 cmpl)) ('(("."))))))
		(list regexp subm cursm app (word-help-extract-matches index-ob re-list))))))
	  cmpl-list))

(defun word-help-extract-matches (from-ob re-list)
  "Filters atoms from FROM-OB through the regexps in RE-LIST.
The atoms that matched are returned in a new obarray."
  (let ((dest-ob (make-vector 47 0)))
    (mapcar (lambda (re)
	      (let ((regexp (car re)))
		(mapatoms (lambda (x)
			    (if (or (not regexp) (string-match regexp (symbol-name x)))
				(intern (symbol-name x) dest-ob)))
			  from-ob)))
    re-list)
    dest-ob))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Misc. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun word-help-only-helpable (symbol)
  "Returns non-nil if SYMBOL can be documented in the current mode."
  (get symbol 'word-help-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Mode definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(word-help-add-mode "autoconf"
		    '( :files    (("autoconf" "Macro Index") ("m4" "Macro index"))
		       :keyword  (("AC_\\([A-Za-z0-9_]+\\)" 1)
				  ("[a-z]+"))
		       :complete (("AC_\\([A-Za-z0-9_]+\\)" 1 nil (("^[A-Z_]+$")))
				  ("[a-z_][a-z_]*" 0 nil (("^[a-z_]+$"))))))

(word-help-add-mode "AWK"
		    '( :files	  (("gawk" "Index"))
		       :index-map (("`\\([^']+\\)' +\\(special pattern\\|statement\\)" 1)
				   ("^[A-Za-z0-9_]+$" 0)
				   ("\\([A-Za-z0-9_]+\\) +\\(special pattern\\|statement\\)" 1))))



(word-help-add-mode "Bison"
		    '( :files    (("bison" "Index")
				  ("libc" "Type Index" "Function Index" "Variable Index"))
		       :keyword  (("%[A-Za-z]*")
				  ("[A-Za-z_][A-Za-z0-9_]*"))
		       :complete (("%[A-Za-z]*" nil nil (("^%")))
				  ("[A-Za-z_][A-Za-z0-9_]*" nil nil (("[A-Za-z_][A-Za-z0-9_]*")))
				  word-help-wrapt-complete-tag)))

(word-help-t-wrapper complete-tag)

(word-help-add-alias "YACC" "Bison")

(word-help-add-mode "C"
		    '( :files    (("libc" "Type Index" "Function Index" "Variable Index"))
		       :complete (("struct[^A-Za-z_]+\\([^ \t\n]+\\)" 1)
				  ("^\\([^ \t\n]+\\)" 1)
				  word-help-wrapt-complete-tag)))
(word-help-add-alias "C++" "C")


(word-help-add-mode "Emacs-Lisp"
		    '( :files    (("elisp" "Index")
				  ("emacs" "Command Index"))
		       :keyword  (("[^][ ()\n\t.\"'#]+"))
		       :complete word-help-wrapt-lisp-complete-symbol))

(word-help-add-mode "LaTeX"
		    '( :files    (("latex" "Command Index"))
		       :keyword  (("\\\\\\(begin\\|end\\){\\([^}\n]+\\)\*?}" 2 0)
				  ("\\\\[A-Za-z]+")
				  ("\\\\[^A-Za-z]")
				  ("[A-Za-z]+"))
		       :complete (("\\\\begin{\\([A-Za-z]*\\)" 1 "}" (("^[A-Za-z]+\*?$")))
				  ("\\\\end{\\([A-Za-z]*\\)" 1 "}" (("^[A-Za-z]+\*?$")))
				  ("\\\\renewcommand{\\(\\\\?[A-Za-z]*\\)" 1 "}" (("^\\\\[A-Za-z]+")))
				  ("\\\\renewcommand\\(\\\\?[A-Za-z]*\\)" 1 "" (("^\\\\[A-Za-z]+")))
				  ("\\\\renewenvironment{?\\([A-Za-z]*\\)" 1  "}"(("^[A-Za-z]+$")))
				  ("\\\\[A-Za-z]*" 0 "" (("^\\\\[A-Za-z]+"))))))

(word-help-add-alias "latex" "LaTeX")

(word-help-add-mode "Nroff"
		    '( :files    (("groff" "Macro Index" "Register Index" "Request Index"))
		       :keyword  (("\\.[^A-Za-z]")
				  ("\\.[A-Za-z]+")
				  ("\\.\\([A-Za-z]+\\)" 1))
		       :complete (("\\.[A-Za-z]*" nil nil (("^\\.[A-Za-z]+$")))
				  ("\\.\\([A-Za-z]*\\)" 1 nil (("^[A-Za-z]+$"))))))

(word-help-add-alias "Groff" "Nroff")

(word-help-add-mode "m4"
		    '( :files    (("m4" "Macro index"))
 		       :keyword  (("\\([mM]4_\\)?\\([A-Za-z_][A-Za-z_0-9]*\\)" 2))
		       :complete (("[mM]4_\\([A-Za-z_]?[A-Za-z_0-9]*\\)" 1)
				  ("[A-Za-z_][A-Za-z_0-9]*"))))

(word-help-add-mode "Makefile"
		    '( :files    (("make" "Name Index"))
		       :keyword  (("\\.[A-Za-z]+") ;; .SUFFIXES
				  ("\\$[^()]")  ;; $@
				  ("\\$([^A-Za-z].)") ;; $(<@)
				  ("\\$[\(\{]\\([a-zA-Z+]\\)" 1) ;; $(wildcard)
				  ("[A-Za-z]+")) ;; foreach
		       :complete (("\\.[A-Za-z]*" nil nil ":" (("^\\.[A-Za-z]+$")))
				  ("\\$\(\\([a-z]*\\)" 1 nil ")" (("^[a-z]")))
				  ("[a-z]+" nil nil "" (("^[a-z]+$")))
				  word-help-wrapt-makefile-complete)))
(word-help-add-mode "Perl"
		    '( :files    (("perl" "Variable Index" "Function Index"))
		       :keyword  (("\\$[^A-Za-z^]") ;; $@
				  ("\\$\\^[A-Za-z]?") ;; $^D
				  ("\\$[A-Za-z][A-Za-z_0-9]+") ;; $foobar
				  ("[A-Za-z_][A-Za-z_0-9]+")) ;; dbmopen
		       :complete (("\\$[A-Za-z]*" nil nil (("^\\$[A-Za-z]+$"))) ;; $variable
				  ("[A-Za-z_][A-Za-z_0-9]*" nil nil
				   (("^[A-Za-z_][A-Za-z_0-9]*$")))))) ;; function

(word-help-add-mode "Simula"
		    '( :files    (("simula" "Index"))
		       :ignore-case t))

(word-help-add-alias "Ifi Simula" "Simula")
(word-help-add-alias "SIMULA" "Simula")

(word-help-add-mode "Texinfo"
		    '( :files    (("texinfo" "Command and Variable Index"))
		       :keyword  (("@\\([A-Za-z]+\\)" 1))
		       :complete (("@\\([A-Za-z]*\\)" 1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Extra keywords for completion/etc. ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keywords from latex.el.
;;
;; Based upon work by Per Abrahamsen <auc-tex@sunsite.auc.dk> and
;; Kresten Krab Thorup.
;;
;; Copyright 1991 Kresten Krab Thorup
;; Copyright 1993, 1994, 1995, 1996 Per Abrahamsen

(word-help-add-keywords "latex"
 ;; LaTeX environments
   '(("abstract") ("array") ("center") ("description")
     ("displaymath") ("document") ("enumerate") ("eqnarray")
     ("eqnarray*") ("equation") ("figure") ("figure*") ("flushleft")
     ("flushright") ("itemize") ("list") ("math") ("minipage")
     ("picture") ("picture") ("quotation") ("quote") ("sloppypar")
     ("tabbing") ("table") ("table*") ("tabular") ("tabular*")
     ("thebibliography") ("theindex") ("titlepage") ("trivlist")
     ("verbatim") ("verbatim*") ("verse")

   ;; LaTeX commands

     ("\\LaTeX") ("\\SLiTeX") ("\\\\") ("\\\\*") ("\\addcontentsline")
     ("\\addtocontents") ("\\addtocounter") ("\\addtolength")
     ("\\addvspace") ("\\alph") ("\\and") ("\\arabic")
     ("\\arraystretch") ("\\author") ("\\bibitem") ("\\bibliography")
     ("\\bibliographystyle") ("\\bigskip") ("\\caption") ("\\centering")
     ("\\circle") ("\\circle*") ("\\cite") ("\\cleardoublepage")
     ("\\clearpage") ("\\cline") ("\\cline") ("\\dashbox") ("\\date")
     ("\\documentclass") ("\\documentstyle") ("\\extracolsep")
     ("\\fbox") ("\\flushbottom") ("\\fnsymbol") ("\\footnote")
     ("\\footnotemark") ("\\footnotetext") ("\\frac") ("\\frame")
     ("\\framebox") ("\\fussy") ("\\glossary") ("\\hline") ("\\hspace")
     ("\\hspace*") ("\\hyphenation") ("\\include") ("\\includeonly")
     ("\\index") ("\\input") ("\\item") ("\\kill") ("\\label")
     ("\\left") ("\\lefteqn") ("\\line") ("\\linebreak")
     ("\\linethickness") ("\\makebox") ("\\makeglossary")
     ("\\makeindex") ("\\marginpar") ("\\markboth") ("\\markright")
     ("\\mbox") ("\\medskip") ("\\multicolumn") ("\\multiput")
     ("\\newcommand") ("\\newcommand") ("\\newcounter")
     ("\\newenvironment") ("\\newfont") ("\\newlength") ("\\newline")
     ("\\newpage") ("\\newsavebox") ("\\newtheorem") ("\\nocite")
     ("\\nolinebreak") ("\\nonumber") ("\\nopagebreak")
     ("\\normalmarginpar") ("\\numberline") ("\\onecolumn") ("\\oval")
     ("\\overbrace") ("\\overline") ("\\pagebreak") ("\\pagenumbering")
     ("\\pageref") ("\\pagestyle") ("\\parbox") ("\\poptabs")
     ("\\protect") ("\\pushtabs") ("\\put") ("\\raggedbottom")
     ("\\raggedleft") ("\\raggedright") ("\\raisebox") ("\\ref")
     ("\\refstepcounter") ("\\renewcommand") ("\\renewcommand")
     ("\\renewenvironment") ("\\reversemarginpar") ("\\roman")
     ("\\rule") ("\\samepage") ("\\savebox") ("\\sbox") ("\\setcounter")
     ("\\setlength") ("\\settowidth") ("\\shortstack") ("\\sloppy")
     ("\\smallskip") ("\\sqrt") ("\\stackrel") ("\\stepcounter")
     ("\\stretch") ("\\thanks") ("\\thicklines") ("\\thinlines")
     ("\\thispagestyle") ("\\title") ("\\twocolumn") ("\\typein")
     ("\\typeout") ("\\underbrace") ("\\underline") ("\\usebox")
     ("\\usecounter") ("\\usepackage") ("\\value") ("\\vector")
     ("\\verb") ("\\verb*") ("\\vline") ("\\vspace") ("\\vspace*")))

;; Keywords from func-doc.el
;;
;; Based upon work by Peter S. Galbraith <P_Galbraith@Qc.DFO.ca>.
;;
;; Copyright (C) 1996 Peter S. Galbraith

(word-help-add-keywords "gawk"
      '(("int" "Numeric Functions") ("sqrt" "Numeric Functions")
        ("exp" "Numeric Functions") ("log" "Numeric Functions")
        ("sin" "Numeric Functions") ("cos" "Numeric Functions")
        ("atan2" "Numeric Functions") ("rand" "Numeric Functions")
        ("srand" "Numeric Functions")))

(run-hooks 'word-help-load-hook)

(provide 'word-help)
;;; word-help ends here
