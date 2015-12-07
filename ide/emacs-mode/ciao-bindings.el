;;; ciao-bindings.el --- Keyboard bindings, menu bar, and tool bar.
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

;;===========================================================================
;; This is the final part of the Ciao emacs mode, which defines
;; keyboard bindings, menu bar, and toolbar for accessing most interactive
;; commands of the major mode and inferior mode of Ciao source and
;; Ciao processes.
;;===========================================================================

(require 'comint)
(require 'ciao-config) ; ciao-get-config
(require 'ciao-common) ; ciaoide (group), ciao-set-ciao-system,
		       ; ciao-set-ciao-system-args,
		       ; ciao-set-ciaopp-system,
		       ; ciao-set-ciaopp-system-args
(require 'ciao-aux) ; ciao-find-icon

;; This is to avoid warnigns in fsf from xemacs vars and functions.
(eval-when-compile
  (if (boundp 'xemacs-logo)
      ()
    ;; fsf
    (defconst default-toolbar nil)
    (defconst right-toolbar-visible-p nil)
    (defconst right-toolbar-width nil)
    (defconst right-toolbar nil)
    (defun toolbar-make-button-list (&rest foo))
    (defun console-on-window-system-p (&rest foo))
    (defun set-specifier (&rest foo))
    (defun specifier-specs (&rest foo))
    ))

;;------------------------------------------------------------
;; TODO: move somewhere else (ciao-common.el?)

;;;###autoload
(defun ciao-customize-all () 
  (interactive)
  "Enter interface customization option browser."
  (customize-group 'ciao))

;; Do not change the two lines below (Patched by installation!):
(defconst ciao-mode-version (ciao-get-config :version)
  "This is the version number of the Ciao emacs mode")

;;;###autoload
(defun ciao-report-mode-version ()
  "Report the version of the emacs Ciao mode."
  (interactive)
  (message (concat "Ciao mode version: " ciao-mode-version)))

(defconst ciao-mode-emacs-version 

  "This mode is currently being developed within @apl{GNU emacs}
version 24.1. It should also (hopefully) work with all other
23.XX, 22.XX, 21.XX, 20.XX, and later 19.XX versions. We also try
our best to keep things working under @apl{xemacs} and under some
emacs native ports for the mac."

  "This is a comment describing for which emacs version this Ciao
   emacs mode has been developed.")

;;---------------------------------------------------------------------------
;; The interactive commands that will be accessed from keyboard
;; bindings, menu bars, or tool bars.
;; ---------------------------------------------------------------------------

(require 'ciao-help) ; ciao-goto-manuals, ciao-help-on-current-symbol
(require 'ciao-splash) ; ciao-startup
(require 'ciao-syntax) ; ciao-indent-line, ciao-indent-file,
		       ; ciao-insert-script-header
(require 'ciao-font-lock) ; ciao-fontify-buffer,
			  ; ciao-font-lock-defaults-create,
			  ; ciao-inferior-font-lock-defaults-create
(require 'ciao-loading) ; run-ciao-toplevel, ciao-set-main-filename,
			; ciao-set-library-path,
			; ciao-check-buffer-syntax, ciao-load-buffer,
			; ciao-load-from-main-module,
			; ciao-load-region, ciao-load-predicate,
			; ciao-consult-buffer, ciao-consult-region,
			; ciao-consult-predicate,
			; ciao-find-last-run-errors,
			; ciao-unmark-last-run-errors,
			; ciao-recenter-last-ciao-buffer
(require 'ciao-compile) ; ciao-make-exec, ciao-make-po,
			; ciao-make-activemod, ciao-compile-buffer,
			; ciao-compile-region, ciao-compile-predicate
(require 'ciao-testing) ; ciao-load-query,
			; ciao-run-tests-in-buffer-and-related,
			; ciao-run-tests-in-buffer,
			; ciao-run-tests-in-buffer-check-exp-assrts,
			; ciao-show-untested-preds-buffer
(require 'ciao-debugger) ; ciao-select-debug-mode,
			 ; ciao-mark-buffer-source-debug,
			 ; ciao-unmark-buffer-debug,
			 ; ciao-enable-trace, ciao-enable-debug,
			 ; ciao-no-debug, ciao-debug-buffer,
			 ; ciao-select-buffers-for-debug,
			 ; ciao-debug-breakon, ciao-debug-breakoff,
			 ; ciao-debug-all-breakoff,
			 ; ciao-debug-breakparams,
			 ; ciao-debug-uncolor-all-breakpt,
			 ; ciao-debug-display-breakpt
(require 'ciao-ciaopp) ; run-ciao-preprocessor,
		       ; ciao-load-and-check-buffer,
		       ; ciao-analyze-buffer, ciao-check-assertions,
		       ; ciao-optimize-buffer,
		       ; ciao-toggle-ciaopp-use-graphical-menu,
		       ; ciao-browse-preprocessor-options,
		       ; ciao-show-preprocessor-output,
		       ; ciao-ciaopp-filter
(require 'ciao-lpdoc) ; ciao-set-lpdoc-system,
		      ; ciao-set-lpdoc-system-args,
		      ; ciao-set-lpdoc-docformat,
		      ; ciao-set-lpdoc-libpath,
		      ; ciao-visit-lpdoc-settings, ciao-gen-doc,
		      ; ciao-gen-buffer-doc, ciao-start-viewer
(require 'ciao-vc) ; ciao-new-version,
		   ; ciao-set-version-control-for-buffer,
		   ; ciao-save-buffer, ciao-add-comment-and-save,
		   ; ciao-update-version,
		   ; ciao-fetch-next-changelog-entry,
		   ; ciao-version-maint-type

;;------------------------------------------------------------
;; Key and menu bindings + documentation sections
;; These nifty functions allow autodocumenting using lpdoc! MH
;;------------------------------------------------------------

(defvar ciao-mode-map (make-sparse-keymap))

(defvar ciao-inferior-mode-map nil)

(if ciao-inferior-mode-map
    nil
  ;; HB: 930205: Use the "correct" function 'copy-keymap'
  ;; to copy a keymap.
  ;; Inherit the commands from comint.
  (setq ciao-inferior-mode-map (copy-keymap
				comint-mode-map)))

;; ---------------------------------------------------------------------------
;; TODO: This is generic, move to ciao-lpdoc-elisp-reflect.el, which
;;   generates LPdoc documentation from elisp comments.

(defvar ciao-documented-commands nil
 "Stores the list of commands which will appear in the documentation
  for the main mode, preceded by section comments.")

(defun ciao-define-key (map binding function)
  "A call to define-key, but we store stuff in our own format, which
  is used later to generate the documentation."
  (setq ciao-documented-commands
	(cons (list binding function) ciao-documented-commands))
  (define-key map binding function))

;; (defun ciao-report-defined-key (map function &optional comment)
;;   "Store the info for an already defined key. Used to generate the
;; documentation. Optional comment overrides the function's default
;; comment."
;;   (setq ciao-documented-commands
;; 	(cons (list (substitute-command-keys 
;; 		     (concat "\\[" (symbol-name function) "]"))
;; 	       (or comment function)) ciao-documented-commands)))

(defun ciao-report-defined-key (map function &optional comment binding)
  "Store the info for an already defined key. Used to generate the
documentation. Optional comment overrides the function's default
comment. Optional binding sets and reports a different binding."
  (let ((desc 
	 (where-is-internal function map t t)
	 )) ;; used: overriding-local-map 
    (setq ciao-documented-commands
	  (cons (list (if (or desc binding)
			  (if (stringp binding)
				binding
			    (key-description desc))
			(progn
			  (message "Warning: in `ciao-report-defined-key', no bindings found for function `%s'" function)
			  (format "M-x %s" function)))
		      (or comment function))
		ciao-documented-commands)))
  (if (stringp binding)
      (define-key map binding function)
    nil)
  )

(defun ciao-documentation-section (sec-title sec-intro)
  "We store a section title and intro, used later to generate documentation."
  (setq ciao-documented-commands
	(cons (list 'section sec-title sec-intro) ciao-documented-commands)))

(defun ciao-documentation-paragraph (paragraph-contents)
  "We store paragraph, used later to generate documentation."
  (setq ciao-documented-commands
	(cons (list 'paragraph paragraph-contents) ciao-documented-commands)))

;; ---------------------------------------------------------------------------

;; Should start with a section!
(defun ciao-mode-commands (map inferior-map)

  (ciao-documentation-section
    "Syntax coloring and syntax-based editing"
    
    "Syntax-based highlighting (coloring) of code is provided
automatically when opening Ciao files.  This includes also the
assertions used by the preprocessor and the documentation strings used
by the Ciao auto-documenter, @apl{lpdoc}.  The mode should be set to
Ciao and the Ciao mode menus should appear on the menu bar. The
colors and fonts used can be changed through the @index{customize}
options in the help menu (see @ref{Customization}).

During editing this coloring may be refreshed by calling the
appropriate function (see below).

Limited syntax-based auto-indentation and auto-fill of code and
comments is also provided. Syntax highlighting and coloring is also
available for the error and warning messages produced by the top
level, preprocessor, and auto-documenter, and, in general, for the
output produced by these tools.

@noindent
Commands:
  ")

  (ciao-define-key map "\C-ch" 'ciao-fontify-buffer)
  (ciao-define-key map "\t" 'ciao-indent-line)

  (ciao-documentation-section 
   "Getting on-line help" 

   "The following commands are useful for getting on-line help. This
is done by accessing the @apl{info} version of the Ciao manuals or the
@apl{emacs} built-in help strings. Note also that the @apl{info}
standard @tt{search} command (generally bound to @key{s}) can be used
inside @apl{info} buffers to search for a given string.
   ")

  (ciao-define-key map "\C-c\C-i" 'ciao-help-on-current-symbol)
  (ciao-define-key map "\C-c/"    'ciao-complete-current-symbol)
  (ciao-define-key map "\C-c\C-m" 'ciao-goto-manuals)
  (ciao-define-key map "\C-hm"    'ciao-describe-mode)

  (ciao-documentation-section 
   "Loading and compiling programs" 

   "These commands allow @index{loading programs}, @index{creating
executables}, etc. by issuing the appropriate commands to a Ciao top
level shell, running in its own buffer as a subprocess. See @ref{The
interactive top-level shell} for details. The following commands
implement the communication with the Ciao top level:
   ")

  (ciao-define-key map "\C-ct" 'run-ciao-toplevel)
  (ciao-define-key map "\C-cl" 'ciao-load-buffer)
  (ciao-define-key map "\C-cf" 'ciao-load-and-check-buffer)

  (ciao-define-key map "\C-cx" 'ciao-make-exec)
  (ciao-define-key map "\C-co" 'ciao-make-po)
  (ciao-define-key map "\C-ca" 'ciao-make-activemod)

  (ciao-define-key map "\C-cs" 'ciao-set-main-filename)
  (ciao-define-key map "\C-cL" 'ciao-load-from-main-module)

  (ciao-documentation-section
   "Commands available in toplevel and preprocessor buffers"
    
   "The interactive top level and the preprocessor both are typically
run in an iteractive buffer, in which it is possible to communicate
with them in the same way as if they had been started from a standard
shell. These interactive buffers run in the so-called @em{Ciao
inferior mode}. This is a particular version of the standard emacs
shell package (comint) and thus all the commands typically available
when running shells inside emacs also work in these buffers.  In
addition, many of the commands and key bindings available in buffers
containing Ciao source code are also available in these interactive
buffers, when applicable.  The Ciao-specific commands available
include:
    ")

  ;; Not such a good idea: completion is better (see
  ;; 'comint-dynamic-complete above) 
  ;; (ciao-define-key map "\t" 'ciao-indent-line)
  (ciao-define-key inferior-map "\C-c\C-i" 'ciao-help-on-current-symbol)
  (ciao-define-key inferior-map "\C-c/"    'ciao-complete-current-symbol)
  (ciao-define-key inferior-map "\C-c`"    'ciao-find-last-run-errors)
  (ciao-define-key inferior-map "\C-ce" 'ciao-unmark-last-run-errors)
  (ciao-define-key inferior-map "\C-cq" 'ciao-set-query)
;; (ciao-define-key map "???" 'ciao-clear-query)
  (ciao-define-key inferior-map "\C-cQ" 'ciao-load-query)
  (ciao-define-key inferior-map "\C-c\C-v" 'ciao-show-preprocessor-output)
  (ciao-define-key inferior-map "\C-cv" 'ciao-report-mode-version)

  (ciao-documentation-paragraph
   (substitute-command-keys "@noindent The following are some of
the commands from the comint shell package which may be specially
useful (type \\<ciao-mode-map> @tt{\\[describe-mode]} while in a
Ciao interactive buffer for a complete list of commands):"))

  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-previous-input)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-next-input)
  (if (version<= "23.2" emacs-version)
      ;; Since emacs 23.2, M-r is bound to this function
      (ciao-report-defined-key ciao-inferior-mode-map
			       'comint-history-isearch-backward-regexp)
    (ciao-report-defined-key ciao-inferior-mode-map
			     'comint-previous-matching-input))
  (ciao-report-defined-key ciao-inferior-mode-map 
 			   'comint-dynamic-complete
			   "Dynamically find completion of the item at
point. Note that this completion command refers generally to filenames
 (rather than, e.g., predicate names, as in the previous functions)."
			   "\t")
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-dynamic-list-filename-completions
                           "List all (filename) completions of the
item at point."
			   "\M-?")
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-send-input
			   "Return at any point of the a line at the
end of a buffer sends that line as input. Return not at end copies the
rest of the current line to the end of the buffer and sends it as
input.")
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-delchar-or-maybe-eof)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-kill-input)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'backward-kill-word)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-interrupt-subjob)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-stop-subjob)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-quit-subjob)

  (ciao-documentation-section 
   "Locating errors and checking the syntax of assertions" 

   "These commands allow locating quickly the point in the source code
corresponding to errors flagged by the compiler or preprocessor as
well as performing several syntactic checks of assertions:
@cindex{locating errors} 
   ")

  (ciao-define-key map "\C-c`"    'ciao-find-last-run-errors)
  (ciao-define-key map "\C-ce" 'ciao-unmark-last-run-errors)
  (ciao-define-key map "\C-cE"    'ciao-check-buffer-syntax)

  (ciao-documentation-section 
   "Commands which help typing in programs" 

   "The following commands are intended to help in the process of
writing programs: @cindex{script header, inserting automatically}
   ")

  (ciao-define-key map "\C-cIS" 'ciao-insert-script-header)
  (ciao-define-key map "\C-ci"  'ciao-indent-file)

  (ciao-documentation-section
   "Debugging programs"

   "These commands allow marking modules for @index{debugging} by
issuing the appropiate commands to a Ciao top level shell, running in
its own buffer as a subprocess. There are two differents types of
debugging: traditional debugging (using the @concept{byrd-box model}
and @concept{spy-points}) and @index{source-level debugging} (same as
traditional debugging plus source tracing and
@concept{breakpoints}). @cindex{debugging, source-level} In order to
use @index{breakpoints}, source debugging must be on. The following
commands implement comunication with the Ciao top level:
   ")

  (ciao-define-key map "\C-cd" 'ciao-debug-buffer)
  (ciao-define-key map "\C-cm" 'ciao-select-debug-mode)
  (ciao-define-key map "\C-c\M-m" 'ciao-select-buffers-for-debug)

  (ciao-define-key map "\C-cSb" 'ciao-debug-breakon)
  (ciao-define-key map "\C-cSv" 'ciao-debug-breakoff)
  (ciao-define-key map "\C-cSn" 'ciao-debug-all-breakoff)
  (ciao-define-key map "\C-cSl" 'ciao-debug-display-breakpt)
  (ciao-define-key map "\C-cSr" 'ciao-debug-uncolor-all-breakpt)

  (ciao-define-key map "\C-cSt" 'ciao-enable-trace)
  (ciao-define-key map "\C-cSd" 'ciao-enable-debug)

  (ciao-define-key map "\C-cr" 'ciao-load-region)
  (ciao-define-key map "\C-cp" 'ciao-load-predicate)

  (ciao-documentation-section
   "Testing programs"
   
   ;; TODO: Improve documentation
   "These commands allow testing predicates and modules, based on
   interactively defined queryies or more sophisticated tests
   specified within the source code.")

  (ciao-define-key map "\C-cq" 'ciao-set-query)
;; (ciao-define-key map "???" 'ciao-clear-query)
  (ciao-define-key map "\C-cQ" 'ciao-load-query)

  (ciao-define-key map "\C-cu" 'ciao-run-tests-in-buffer)
  (ciao-define-key map "\C-cU" 'ciao-run-tests-in-buffer-check-exp-assrts)

  (ciao-documentation-section 
   "Preprocessing programs" 

   "These commands allow @index{preprocessing programs} with
@apl{ciaopp}, the @index{Ciao preprocessor}.

@include{README_CIAOPP.lpdoc}

See the preprocessor manual for details. The following commands
implement the communication with the Ciao preprocessor:
  ")

;;   (ciao-define-key map "\C-cM" 'ciao-preprocess-buffer-menu)
;;   (ciao-define-key map "\C-cP" 'ciao-preprocess-buffer)
;;   (ciao-define-key map "\C-cT" 'ciao-check-types-modes)
  (ciao-define-key map "\C-cA" 'ciao-analyze-buffer)
  (ciao-define-key map "\C-cT" 'ciao-check-assertions)
  (ciao-define-key map "\C-cO" 'ciao-optimize-buffer)
  (ciao-define-key map "\C-cM" 'ciao-browse-preprocessor-options)
;;   (ciao-define-key map "\C-c\C-p" 'ciao-set-ciaopp-output-pred)
;;   (ciao-define-key map "\C-c\C-f" 'ciao-set-ciaopp-output-full)
;;   (ciao-define-key map "\C-c\C-x" 'ciao-set-ciaopp-output-none)
  (ciao-define-key map "\C-c\C-v" 'ciao-show-preprocessor-output)
;;  (ciao-define-key map "\C-cV" 'ciao-preprocess-buffer-and-show-output)
  (ciao-define-key map "\C-c\C-r" 'run-ciao-preprocessor)

  (ciao-documentation-section 
   "Version control" 

   "The following commands can be used to carry out a simple but
effective form of @concept{version control} by keeping a @concept{log
of changes} on a file or a group of related files. Interestingly, this
log is kept in a format that is understood by @apl{lpdoc}, the Ciao
documenter @cite{lpdoc-tr}. As a result, if these version comments are
present, then @apl{lpdoc} will be able to automatically assign up to
date version numbers to the manuals that it generates. This way it is
always possible to identify to which version of the software a manual
corresponds. Also, @apl{lpdoc} can create automatically sections
describing the changes made since previous versions, which are
extracted from the comments in the changelog entries.

The main effect of these commands is to automatically associate the
following information to a set of changes performed in the file and/or
in a set of related files:

@begin{itemize}

@item a @index{version number} (such as, e.g., @tt{1.2}, where @tt{1}
is the @concept{major version number} and @tt{2} is the @concept{minor
version number}),

@item a @concept{patch number} (such as, e.g., the @tt{4} in
@tt{1.2#4}), 

@item a @concept{time stamp} (such as, e.g.,
@tt{1998/12/14,17:20*28+MET}),

@item the author of the change, @cindex{change, author} and

@item a comment explaining the change. @cindex{change, comment}
@end{itemize}

The @concept{version numbering} used can be local to a single file or
common to a number of related files. A simple version numbering policy
is implemented: when a relevant change is made, the user typically
inserts a @concept{changelog entry} for it, using the appropriate
command (or selecting the corresponding option when prompted while
saving a file). This will cause the @em{patch number} for the file (or
for the whole system that the file is part of) to be incremented
automatically and the corresponding machine-readable comment to be
inserted in the file. Major and minor version numbers can also be
changed, but this is always invoked by hand (see below).

The changelog entry is written in the form of a @decl{comment/2}
declaration.  As mentioned before, the advantage of using this kind of
changelog entries is that these declarations can be processed by the
@apl{lpdoc} automatic documenter (see the @apl{lpdoc} reference
manual @cite{lpdoc-tr} or the @lib{assertions} library documentation
for more details on these declarations). 

Whether the user is asked or not to introduce such changelog entries,
and how the patch and version numbers should be increased is
controlled by the presence in the file of a @pred{comment/2}
declaration of the type:

@tt{:- doc(version_maintenance,<type>).}

@noindent (note that this requires including the @lib{assertions}
library in the source file).  These declarations themselves are also
typically introduced automatically when using this mode (see below).

The version maintenance mode can also be set alternatively by
inserting a comment such as:

@begin{verbatim}
%% Local Variables: 
%% mode: ciao
%% update-version-comments: \"off\"
%% End:
@end{verbatim}

The lines above instruct emacs to put the buffer visiting the file in
@concept{emacs Ciao mode} and to turn version maintenance off.
Setting the version maintenance mode in this way has the disadvantage
that @apl{lpdoc}, the auto-documenter, and other related tools will
not be aware of the type of version maintenance being performed (the
lines above are comments for Ciao). However, this can be useful in
fact for setting the @index{version maintenance mode for packages} and
other files meant for inclusion in other files, since that way the
settings will not affect the file in which the package is included.

The following commands implement the version control support:
   ")

  (ciao-define-key map "\C-c\C-a" 'ciao-set-version-control-for-buffer)
  (ciao-define-key map "\C-x\C-s" 'ciao-save-buffer)
  (ciao-define-key map "\C-c\C-s" 'ciao-add-comment-and-save)
  (ciao-define-key map "\C-cn"    'ciao-new-version)
  (ciao-define-key map "\C-c\C-n" 'ciao-fetch-next-changelog-entry)

  (ciao-documentation-section 
   "Generating program documentation"

   "These commands provide some bindings and facilities for generating
and viewing the documentation corresponding to the current buffer. The
documentation is generated in a temporary directory, which is created
automatically.  This is quite useful while modifying the documentation
for a file, in order to check the output that will be produced,
whithout having to set up a documentation directory by hand or to
regenerate a large manual of which the file may be a part. 
   ")

  (ciao-define-key map "\C-cDB" 'ciao-gen-buffer-doc)
  (ciao-define-key map "\C-cDF" 'ciao-set-lpdoc-docformat)
  (ciao-define-key map "\C-cDS" 'ciao-visit-lpdoc-settings)
  (ciao-define-key map "\C-cDG" 'ciao-gen-doc)
  (ciao-define-key map "\C-cDV" 'ciao-start-viewer)
  (ciao-define-key map "\C-cDW" 'ciao-set-scratchpad-root)

  (ciao-documentation-section 
   "Setting top level preprocessor and documenter executables"
 
   "These commands allow @index{changing the executables used} when
starting the top-level, the preprocessor, or the auto-documenter. They
also allow changing the arguments that these executables take, and
changing the path where the libraries reside. In the case of the
top-level and preprocessor, this should be done only by users which
understand the implications, but it is very useful if several versions
of Ciao or the preprocessor are available in the system. All these
settings can be changed through the @index{customize} options in the
help menu (see @ref{Customization}).
   ")

  (ciao-define-key map "\C-cSA"    'ciao-customize-all)
  (ciao-define-key map "\C-cSC"    'ciao-set-ciao-system)
  (ciao-define-key map "\C-cS\C-c" 'ciao-set-ciao-system-args)
  (ciao-define-key map "\C-cSP"    'ciao-set-ciaopp-system)
  (ciao-define-key map "\C-cS\C-p" 'ciao-set-ciaopp-system-args)
  (ciao-define-key map "\C-cSL"    'ciao-set-library-path)
  (ciao-define-key map "\C-cSD"    'ciao-set-lpdoc-system)
  (ciao-define-key map "\C-cS\C-d" 'ciao-set-lpdoc-system-args)
  (ciao-define-key map "\C-cS\C-l" 'ciao-set-lpdoc-libpath)

  (ciao-documentation-section 
   "Other commands" 
   "Some other commands which are active in the Ciao mode:
   ") 

  (ciao-define-key map "\C-c\C-l" 'ciao-recenter-last-ciao-buffer)

  (ciao-documentation-section 
   "Traditional Prolog Mode Commands" 

   "These commands provide some bindings and facilities for
loading programs, which are present in emacs Prolog modes of
traditional Prolog systems (e.g., SICStus). This is useful mainly
if the Ciao emacs mode is used with such Prolog systems.  Note
that these commands (@pred{compile/1} and @pred{consult/1}) are
deprecated in Ciao (due to the more advanced, separate
compilation model in Ciao) and their use in the Ciao top-level is
not recommended.
   ")

  (ciao-define-key map "\C-cK" 'ciao-compile-buffer)
  (ciao-define-key map "\C-ck" 'ciao-compile-region)
  (ciao-define-key map "\C-c\C-k" 'ciao-compile-predicate)
  (ciao-define-key map "\C-cC" 'ciao-consult-buffer)
  (ciao-define-key map "\C-cc" 'ciao-consult-region)
  (ciao-define-key map "\C-c\C-c" 'ciao-consult-predicate)

  (ciao-documentation-section 
   "Coexistence with other Prolog-like interfaces" 

   "As mentioned previously, the Ciao @apl{emacs} interface can
also be used to work with traditional Prolog or CLP systems. Also, the
Ciao @apl{emacs} interface (@em{mode}) can coexist with other
Prolog-related @apl{emacs} interfaces (@em{modes}) @cindex{emacs mode,
loading several} (such as, e.g., the @apl{SICStus} Prolog
interface). Only one of the interfaces can be active at a time for a
given buffer (i.e., for each given file opened inside @apl{emacs}). In
order the change a buffer to a given interface, move the cursor to
that buffer and type @tt{M-x ...-mode} (e.g., for the Ciao
mode, @tt{M-x ciao-mode}).

If several Prolog-related @apl{emacs} interfaces are loaded, then
typically the @em{last} one to be loaded takes precedence, in the
sense that this will be the interface in which @apl{emacs} will be set
when opening files which have a @tt{.pl} ending (this depends a bit on
how things are set up in your @tt{.emacs} file).")

  (ciao-documentation-section 
   "Getting the Ciao mode version" 
   "@cindex{Ciao mode version}")

  (ciao-define-key map "\C-cv" 'ciao-report-mode-version)

  )

(ciao-mode-commands ciao-mode-map ciao-inferior-mode-map)

;;---------------------------------------------------------------------------
;; Menu bars
;;---------------------------------------------------------------------------

(require 'easymenu)

(defconst ciao-mode-menus-sys
  (list "CiaoSys"
;;      "----"
;;      "TOP-LEVEL/COMPILER"
     ["(Re)Start Ciao top level"                 run-ciao-toplevel t]
     ["(Re)Load buffer into top level"           ciao-load-buffer  t]
     ["Check assertions and (re)load into top level" 
                                                 ciao-load-and-check-buffer t]
     ["(Re)Load main and related modules"        ciao-load-from-main-module t]
     ["Make executable from buffer as main"      ciao-make-exec t]
     "----"
     ["Go to (next) preproc/compiler error msg"  ciao-find-last-run-errors t]
     ["Remove error (and dbg) marks in buffers"  ciao-unmark-last-run-errors t]
     "----"
     ["(Un)Set main module"                      ciao-set-main-filename t]
     "----"
     ["Check buffer syntax (incl. assertions)"   ciao-check-buffer-syntax t]
     "----"
     ["Update syntax-based coloring"             ciao-fontify-buffer t]
     ["Indent and format file"                   ciao-indent-file t]
     ["Insert script header"                     ciao-insert-script-header t]
     "----"
     ["Make active module from buffer"           ciao-make-activemod t]
     ["Make object file (.po) from buffer"       ciao-make-po t]
;;
;; These versions with nice comments do not work yet in xemacs --need to port.
;; 
;;      ["(Re)Start Ciao top level"                 run-ciao-toplevel
;;       :help "Starts the Ciao interactive top level."]
;;      ["(Re)Load into top level"                  ciao-load-buffer
;;       :help "Load code in current open file (+ all dependent files) into Ciao."]
;;      ["Check assertions and (re)load into top level" ciao-load-and-check-buffer
;;       :help "Checks assertions in current open file and loads into Ciao."]
;;      ["(Re)Load main and related modules"        ciao-load-from-main-module
;;       :help "Loads file declared as main + all dependent files into Ciao."]
;;      ["Make executable from buffer as main"      ciao-make-exec
;;       :help "Compile an executable with code in current open file + all dependent files."]
;;      "----"
;;      ["Go to (next) preproc/compiler/lpdoc error msg"  ciao-find-last-run-errors
;;       :help "Locate (next) error reported in the last run or the compiler, preprocessor, or documenter."]
;;      ["Remove error (and dbg) marks in buffers"  ciao-unmark-last-run-errors
;;       :help "Clear any error highlighting marks left in different buffers."]
;;      "----"
;;      (list "Query and main file"
;; 	   ["(Un)Set main module"                ciao-set-main-filename
;;       :help "Define or undefine the main module of the current project (from which compilation will start)."]
;; 	   ["Set default query"                  ciao-set-query
;;             :help "Define a default query which be called automatically on load."]
;; 	   ["Call default query"                 ciao-load-query
;;             :help "Call the default query defined above."]            
;; 	   ["Clear default query"                ciao-clear-query
;;             :help "Clear the default query so that it is not called on load."]
;; 	   )
;;      ["Check buffer syntax (incl. assertions)"   ciao-check-buffer-syntax
;;       :help "Just check file syntax, including assertions, without loading."]
;;      "----"
;;      ["Update syntax-based coloring"        ciao-fontify-buffer
;;       :help "Refresh the syntax-based coloring (useful for complex code or if syntax-based coloring gets out of sync)."]
;;      ["Insert script header"                     ciao-insert-script-header
;;       :help "Insert appropriate header so that this file is treated as a script (so that it can be run directly without compilation)."]
;;      "----"
;;      ["Make active module from buffer"           ciao-make-activemod
;;       :help "Compile an active module (active process that serves remotely the exported predicates) from this file."]
;;      ["Make object file (.po) from buffer"       ciao-make-po
;;       :help "Force compilation of a relocatable object file for this file (normally done automatically by the compiler when needed). "]
;; 
;; Deprecated and not recommended:
;;      "----"
;;      (list "TRADITIONAL PROLOG COMMANDS (also for SICStus)"
;;            ["Compile buffer"    ciao-compile-buffer  t]
;;            ["Compile region"    ciao-compile-region  t]
;;            ["Compile predicate" ciao-compile-predicate t]
;;            ["Consult buffer"    ciao-consult-buffer  t]
;;            ["Consult region"    ciao-consult-region  t]
;;            ["Consult predicate" ciao-consult-predicate t]
;;      )
     )
  "Menus for the Ciao mode.")

(defconst ciao-mode-menus-debug
  (list "CiaoDbg" 
;;      "----"
;;      "TOP-LEVEL/DEBUGGER"
     ["(Un)Debug buffer source"               ciao-debug-buffer t]
     "----"
     ["Select debug mode"                     ciao-select-debug-mode t]
     ["Select multiple buffers for debug"     ciao-select-buffers-for-debug t]
     (list "Breakpoints"
	   ["Set breakpoint on current literal pred symb" ciao-debug-breakon t]
 	   ["Remove breakpoint from current literal"  ciao-debug-breakoff t]
 	   ["Remove all breakpoints"             ciao-debug-all-breakoff t]
 	   ["Redisplay breakpoints"              ciao-debug-display-breakpt t]
     )
     ["Toggle debug mode (jump to bkp or spypt)" ciao-enable-debug t]
     ["Toggle trace mode"                        ciao-enable-trace t]
     "----"
     ["(Re)Load region (for debug)"              ciao-load-region  t]
     ["(Re)Load predicate (for debug)"           ciao-load-predicate t]
     "----"
     (list "Default query"
	   ["Set default query"                  ciao-set-query t]
	   ["Call default query"                 ciao-load-query t]            
	   ["Clear default query"                ciao-clear-query t]
	   )
     "----"
     ["Run tests in current module"              ciao-run-tests-in-buffer t]
     ["Run tests in current module and assertions of exports" ciao-run-tests-in-buffer-check-exp-assrts t]
     ["Run tests in current and all related modules" ciao-run-tests-in-buffer-and-related t]
     ["Show untested exported predicates"        ciao-show-untested-preds-buffer t]
   )
  "Ciao debugging menus.")

(defconst ciao-mode-menus-customize
  (list "CiaoOpts"
     ["Customize all Ciao settings"            ciao-customize-all t] 
     "----"
;;     ["Customize all Ciao system settings" (customize-group 'ciaocore) t]
     ["Set Ciao toplevel executable"           ciao-set-ciao-system t]
     ["Set Ciao toplevel args"                 ciao-set-ciao-system-args t]
     "----"
     ["Set Ciao library path"                  ciao-set-library-path t]
     "----"
;;     ["Customize all CiaoPP environment settings" (customize-group 'ciaopp) t]
     ["Set Ciao Preprocessor executable"       ciao-set-ciaopp-system t]
     ["Set Ciao Preprocessor executable args"  ciao-set-ciaopp-system-args t]
     "----"
;;     ["Customize all LPdoc environment settings" (customize-group 'lpdoc) t]
     ["Set LPdoc executable"                   ciao-set-lpdoc-system t]
     ["Set LPdoc executable args"              ciao-set-lpdoc-system-args t]
     ["Set LPdoc library path"                 ciao-set-lpdoc-libpath t]
     "----"
     ["Set root directory for scratchpad"      ciao-set-scratchpad-root t]
     "----"
     ["Customize all Ciao colors/faces"        (customize-group 
						'ciao-highlighting-faces) t]
  )
  "Customization menus for the Ciao mode.")

(defconst ciao-mode-menus-help
  (list "CiaoHelp" 
     ["Go to manual page for symbol under cursor" ciao-help-on-current-symbol t]
;; MH Not backwards compatible...
;;      :help "Go to manual page describing the symbol under the cursor" ]
;; Also, these had ( ) 
     ["Complete symbol under cursor"        ciao-complete-current-symbol t]
     ["Ciao manuals area in info index" ciao-goto-manuals t]
;;     ["Ciao system manual" ciao-goto-ciaocore-manual t]
;;     ["Ciao preprocessor manual" ciao-goto-ciaopp-manual t]
;;     ["LPdoc automatic documenter manual" ciao-goto-lpdoc-manual t]
     ["List all key bindings" ciao-describe-mode t]
     "----"
     ["Ciao environment (mode) version" ciao-report-mode-version t]
   )
  "Help menu for the Ciao mode.")

(defconst ciao-mode-menus-ciaopp
  (list "CiaoPP"
;;     "CIAO PREPROCESSOR (in development)"
     "Note: CiaoPP required (in development)"
     "----"
;;      ["Preprocess buffer (choosing options)"   ciao-preprocess-buffer-menu t]
;;      ["Preprocess buffer (w/previous options)" ciao-preprocess-buffer t]
;;      ["Check types and modes"  ciao-check-types-modes t]
     ["Analyze buffer"                         ciao-analyze-buffer t]
     ["Check buffer assertions"                ciao-check-assertions t]
     ["Optimize buffer"                        ciao-optimize-buffer t]
     "----"
     ["Browse analysis/checking/optimizing options"         
                                           ciao-browse-preprocessor-options t]
     ["Toggle graphical/textual menu" ciao-toggle-ciaopp-use-graphical-menu t]
     "----"
     ["Go to (next) preproc/compiler error msg" ciao-find-last-run-errors t]
     ["Remove error (and dbg) marks in buffers"  ciao-unmark-last-run-errors t]
     ["Show last preprocessor output file"     ciao-show-preprocessor-output t]
;;      ["Preprocess buffer (w/previous options) and show output"  
;;                                       ciao-preprocess-buffer-and-show-output t]
;;      ["Output only predicate-level analysis info" ciao-set-ciaopp-output-pred t]
;;      ["Output literal- and pred-level analysis info" ciao-set-ciaopp-output-full t]
;;      ["Do not output analysis info" ciao-set-ciaopp-output-none t]
     "----"
     ["Start Ciao preprocessor"                run-ciao-preprocessor t]
     )
  "Menus for CiaoPP mode.")

(defconst ciao-mode-menus-lpdoc
  (list "LPdoc"
;;      "----"
;;      "GENERATE/VIEW DOCUMENTATION"
     ["Generate documentation for buffer"        ciao-gen-buffer-doc t]
     ["View documentation in selected format"    ciao-start-viewer t]
     ["Change default doc format/visualizer"     ciao-set-lpdoc-docformat t]
     ["Goto (next) preproc/compiler error msg"   ciao-find-last-run-errors t]
     ["Remove error (and dbg) marks in buffers"  ciao-unmark-last-run-errors t]
     ["Visit(/create) SETTINGS.pl file"        ciao-visit-lpdoc-settings t]
     ["Generate documentation"                   ciao-gen-doc t]
     "----"
;;      "CHANGELOG / VERSION CONTROL"
     ["Set version control for file"  ciao-set-version-control-for-buffer t] 
     ["Insert changelog entry/increase patch #" ciao-add-comment-and-save t]
     ["Increase version number"              ciao-new-version t]
     ["Go to next changelog entry"           ciao-fetch-next-changelog-entry t]
     )
  "Menus for LPdoc mode.")

(defconst ciao-inferior-mode-menus
;;  (list "Ciao"
  (list "Ciao"
     ["Update syntax-based coloring"        ciao-fontify-buffer t]
     "----"
;;     "ERRORS"
     ["Locate (next) preproc/compiler error msg" ciao-find-last-run-errors t]
     ["Remove error marks in buffers"            ciao-unmark-last-run-errors t]
     "----"
;;     "COMPILER/TOP-LEVEL/DEBUGGER"
     ["Set query as default"               ciao-set-query t]
     ["Clear default query"                ciao-clear-query t]
     ["Load default query"                 ciao-load-query t]
     ["Start Ciao top level"               run-ciao-toplevel t]
     "----"
;;     "PREPROCESSOR (in development)"
     ["Show last preprocessor output file"     ciao-show-preprocessor-output t]
     ["Start Ciao preprocessor"                run-ciao-preprocessor t]
     )
  "Menus for the Ciao (inferior) mode.")

(defun ciao-setup-menu-bar ()
  "Define the menus for the Ciao major mode"

  ;; This weird ordering results in same layout in emacs and xemacs...
  (easy-menu-define ciao-menu-help ciao-mode-map 
    "Ciao Mode Help Menus" ciao-mode-menus-help)
  (easy-menu-define ciao-menu-customize ciao-mode-map 
    "Ciao Mode Customization Menus" 
    ciao-mode-menus-customize)
  (easy-menu-define ciao-menu-lpdoc ciao-mode-map 
    "LPdoc Mode Menus" ciao-mode-menus-lpdoc)
  (easy-menu-define ciao-menu-ciaopp ciao-mode-map 
    "CiaoPP Mode Menus" ciao-mode-menus-ciaopp)
  (easy-menu-define ciao-menu-debug ciao-mode-map 
    "Ciao Mode Debug Menus" ciao-mode-menus-debug)
  (easy-menu-define ciao-menu-sys ciao-mode-map 
    "Ciao Mode System Menus" ciao-mode-menus-sys)
  
  (easy-menu-add ciao-menu-sys)
  (easy-menu-add ciao-menu-debug)
  (easy-menu-add ciao-menu-ciaopp)
  (easy-menu-add ciao-menu-lpdoc)
  (easy-menu-add ciao-menu-customize)
  (easy-menu-add ciao-menu-help))

(defun ciao-setup-inferior-menu-bar ()
  "Define the menus for the Ciao inferior mode"

  (easy-menu-define ciao-inferior-menu-help ciao-inferior-mode-map 
    "Ciao Inferior Mode Help Menu" ciao-mode-menus-help) 
  (easy-menu-add ciao-inferior-menu-help)
  (easy-menu-define ciao-inferior-menu-customize ciao-inferior-mode-map 
    "Ciao Mode Customization Menus" ciao-mode-menus-customize)
  (easy-menu-add ciao-inferior-menu-customize)
  (easy-menu-define ciao-inferior-menu ciao-inferior-mode-map 
    "Ciao Mode Menu" ciao-inferior-mode-menus)
  (easy-menu-add ciao-inferior-menu))

;;---------------------------------------------------------------------------
;; Tool bars
;;---------------------------------------------------------------------------

(defcustom ciao-inhibit-toolbar nil
  "*Non-nil means don't use the specialized Ciao toolbar."
  :type 'boolean
  :group 'ciaoide)

;; MH Tool bar stuff (21.1 onwards)
;; Made a function, so that it is done when maps and menus are active.
;; - This one is for adding to the default toolbar (but modifies others :-( ).
;; (defun ciao-setup-tool-bar () 
;;   (set (make-local-variable 'tool-bar-map) 
;;        (if (display-graphic-p)
;; 	   (progn 
;; 	     (tool-bar-setup) ;; from tool-bar.el
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-help-on-current-symbol "left_arrow" ciao-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-make-exec "ciaoexe" ciao-mode-map)
;; 	     tool-bar-map))))

;; ;; - This one is for an independent tool bar (we add all stuff by hand):
;; (defun ciao-setup-tool-bar () 
;;   (if (display-graphic-p) 
;;       (progn
;; 	(make-local-variable 'tool-bar-map)
;; 	(setq tool-bar-map (make-sparse-keymap))
;; ;; General stuff (from standard tool bar)
;; 	(tool-bar-local-item-from-menu 'find-file "new" ;; "icons/ciaopl"
;;               tool-bar-map  global-map :help "Open or create a (Ciao) file") 
;; 	(tool-bar-local-item-from-menu 'dired "diropen" tool-bar-map)
;; 	(tool-bar-local-item-from-menu 'kill-this-buffer "close" tool-bar-map)
;; 	(tool-bar-local-item-from-menu 'save-buffer "save"
;;          tool-bar-map global-map  :visible '(or buffer-file-name
;; 						 (not (eq 'special
;; 							  (get major-mode
;; 							       'mode-class)))))
;; 	(tool-bar-local-item-from-menu 'write-file "saveas" 
;; 	 tool-bar-map global-map :visible '(or buffer-file-name
;; 						(not (eq 'special
;; 							 (get major-mode
;; 							      'mode-class)))))
;; 	(tool-bar-local-item-from-menu 'undo "undo" tool-bar-map  
;;          global-map :visible '(not (eq 'special (get major-mode 'mode-class))))
;; 	(tool-bar-local-item-from-menu 
;; 	 (lookup-key menu-bar-edit-menu [cut]) ;; 'kill-region 
;; 	 "cut" tool-bar-map global-map 
;; 	 :visible '(not (eq 'special (get major-mode 'mode-class))))
;; 	(tool-bar-local-item-from-menu 
;; 	 (lookup-key menu-bar-edit-menu [copy]) ;; 'menu-bar-kill-ring-save
;; 	 "copy" tool-bar-map)
;; 	(tool-bar-local-item-from-menu 
;; 	 (lookup-key menu-bar-edit-menu [paste]) ;; 'yank 
;; 	 "paste" tool-bar-map global-map 
;; 	 :visible '(not (eq 'special (get major-mode 'mode-class))))
;; 	(tool-bar-local-item-from-menu 
;; 	 'nonincremental-search-forward "search" tool-bar-map)
;; 	(tool-bar-local-item-from-menu 'print-buffer "print" tool-bar-map)
;; ;; Ciao-specific stuff
;; 	(tool-bar-local-item-from-menu  
;; 	 'run-ciao-toplevel "icons/ciao" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-fontify-buffer "icons/ciaorehighlight"
;; 	 tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-load-buffer "icons/ciaoload" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-find-last-run-errors "jump_to" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-unmark-last-run-errors "icons/clear" tool-bar-map ciao-mode-map) 
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-check-buffer-syntax "icons/ciaoasr" tool-bar-map ciao-mode-map) 
;; ;; 	     (tool-bar-local-item-from-menu  
;; ;; 	      'ciao-check-types-modes    "icons/checkassertions"
;; ;;            tool-bar-map ciao-mode-map)
;; ;; 	     (tool-bar-local-item-from-menu  
;; ;; 	      'ciao-preprocess-buffer-menu 
;; ;; 	      "icons/ciaopreprocask" tool-bar-map ciao-mode-map)
;; ;; 	     (tool-bar-local-item-from-menu  
;; ;; 	      'ciao-preprocess-buffer    "icons/ciaopreproc"
;; ;;            tool-bar-map ciao-mode-map) 
;; ;; 	     (tool-bar-local-item-from-menu  
;; ;; 	      'ciao-preprocess-buffer-and-show-output
;; ;; 	      "icons/ciaopreprocsee" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-analyze-buffer "icons/ciaoanalysis" tool-bar-map ciao-mode-map) 
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-check-assertions "icons/checkassertions"
;; 	 tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-optimize-buffer "icons/ciaopeval" tool-bar-map ciao-mode-map) 
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-browse-preprocessor-options
;; 	 "icons/ciaocustomize" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-debug-buffer "icons/ciaodebug" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-gen-buffer-doc "icons/lpdoc" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-start-viewer "icons/lpdocview" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-make-exec "icons/ciaoexeout" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-insert-script-header "icons/ciaoscrt" tool-bar-map ciao-mode-map)
;; ;; 	     (tool-bar-local-item-from-menu  
;; ;; 	      'ciao-make-po "icons/ciaopo" tool-bar-map ciao-mode-map)
;; ;; 	     (tool-bar-local-item-from-menu  
;; ;; 	      'ciao-make-exec "icons/ciaoitf" tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu
;; 	 'ciao-goto-manuals "icons/manuals"  ;; "ciaomanuals" 
;; 	 tool-bar-map ciao-mode-map
;; 	 :help "Go to area containing the Ciao system manuals")
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-help-on-current-symbol "icons/wordhelp"
;; 	 tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu  
;; 	 'ciao-complete-current-symbol "icons/complete"
;; 	 tool-bar-map ciao-mode-map)
;; 	(tool-bar-local-item-from-menu 
;; 	 'ciao-customize-all
;; 	 "preferences" tool-bar-map ciao-mode-map
;; 	 :help "Edit (customize) preferences for Ciao")
;; 	)))
;; 
;; 
;; - This part is for setting tool bars in both FSF and xemacs.
;; Menu bar accummulator for xemacs version
(defvar ciao-xemacs-tool-bar-tmp nil)

;; Portable tool-bar-add-item-from-menu function, adds to accumulators.
(defun ciao-tool-bar-local-item-from-menu 
       (def icon to-map &optional from-map &rest props)
  (if (boundp 'xemacs-logo)
      ;; xemacs
      ;; *** Still need to fish out help strings from menus and pass
      ;;     them to tooltip below
      (progn 
	(setq ciao-xemacs-tool-bar-tmp
	      (cons 
	       `[,(toolbar-make-button-list ;; icon
		   (ciao-find-icon (concat icon "-bg.xpm")))
		 ,def ;; the actual callback
		 t  ;; enabled
		 "" ;; tooltip
		 ] 
	       ciao-xemacs-tool-bar-tmp))
	ciao-xemacs-tool-bar-tmp)
    ;; FSF emacs
    (if (> emacs-major-version 21)
	(tool-bar-local-item-from-menu def icon to-map from-map props)
      (unless from-map
	(setq from-map global-map))
      (tool-bar-add-item-from-menu def icon from-map props))
    ))

;; Special case for xemacs
(defun ciao-tool-bar-local-item-from-menu-xemacs
  (def icon)
  (progn
    (set-specifier 
     default-toolbar 
     (cons 
      (current-buffer) 
      (append
       (specifier-specs default-toolbar 'global)
       ;; '([:style 2d :size 30])
       '(nil) ;; separator (flush right)
       `([,(toolbar-make-button-list ;; icon
	    (ciao-find-icon (concat icon "-bg.xpm")))
	  ,def ;; the actual callback
	  t  ;; enabled
	  "" ;; tooltip
	  ])
       )))))

(defun ciao-setup-tool-bar () 
  (if (and (not ciao-inhibit-toolbar) ;; ????
	   (or 
	    ;; xemacs case 
	    (and (boundp 'xemacs-logo)
		 (featurep 'toolbar)
		 (console-on-window-system-p))
	    ;; FSF emacs case 
	    (and (fboundp 'tool-bar-mode)
		 (display-graphic-p))
	   ))
      (ciao-do-setup-tool-bar)))

(defun ciao-do-setup-tool-bar () 
  (make-local-variable 'tool-bar-map) 
  (if (boundp 'xemacs-logo)
      (setq ciao-xemacs-tool-bar-tmp nil)
    (setq tool-bar-map (make-sparse-keymap)))
  ;; General stuff (from standard tool bar); added only in FSF emacs.
  (ciao-general-toolbar tool-bar-map)
  ;; Ciao-specific stuff - added in both FSF and xemacs
  ;; Ciao logo is a special case
  (if (boundp 'xemacs-logo)
      (ciao-tool-bar-local-item-from-menu-xemacs
       'run-ciao-toplevel "icons/ciao")
    (ciao-tool-bar-local-item-from-menu 
     'run-ciao-toplevel "icons/ciao" tool-bar-map ciao-mode-map))
  (ciao-tool-bar-local-item-from-menu  
   'ciao-fontify-buffer "icons/ciaorehighlight"
   tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-load-buffer "icons/ciaoload" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-find-last-run-errors "icons/jump_to" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-unmark-last-run-errors "icons/clear" tool-bar-map ciao-mode-map) 
;;   (ciao-tool-bar-local-item-from-menu  
;;    'ciao-check-buffer-syntax "icons/ciaoasr" tool-bar-map ciao-mode-map) 
  (ciao-tool-bar-local-item-from-menu  
   'ciao-run-tests-in-buffer-check-exp-assrts "icons/ciaoasr" tool-bar-map ciao-mode-map) 
;; 	     (ciao-tool-bar-local-item-from-menu  
;; 	      'ciao-check-types-modes    "icons/checkassertions"
;;            tool-bar-map ciao-mode-map)
;; 	     (ciao-tool-bar-local-item-from-menu  
;; 	      'ciao-preprocess-buffer-menu 
;; 	      "icons/ciaopreprocask" tool-bar-map ciao-mode-map)
;; 	     (ciao-tool-bar-local-item-from-menu  
;; 	      'ciao-preprocess-buffer    "icons/ciaopreproc"
;;            tool-bar-map ciao-mode-map) 
;; 	     (ciao-tool-bar-local-item-from-menu  
;; 	      'ciao-preprocess-buffer-and-show-output
;; 	      "icons/ciaopreprocsee" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-analyze-buffer "icons/ciaoanalysis" tool-bar-map ciao-mode-map) 
  (ciao-tool-bar-local-item-from-menu  
   'ciao-check-assertions "icons/checkassertions"
   tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-optimize-buffer "icons/ciaopeval" tool-bar-map ciao-mode-map) 
  (ciao-tool-bar-local-item-from-menu  
   'ciao-browse-preprocessor-options
   "icons/ciaocustomize" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-debug-buffer "icons/ciaodebug" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-gen-buffer-doc "icons/lpdoc" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-start-viewer "icons/lpdocview" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-make-exec "icons/ciaoexeout" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-insert-script-header "icons/ciaoscrt" tool-bar-map ciao-mode-map)
;; 	     (ciao-tool-bar-local-item-from-menu  
;; 	      'ciao-make-po "icons/ciaopo" tool-bar-map ciao-mode-map)
;; 	     (ciao-tool-bar-local-item-from-menu  
;; 	      'ciao-make-exec "icons/ciaoitf" tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu
   'ciao-goto-manuals "icons/manuals"  ;; "ciaomanuals" 
   tool-bar-map ciao-mode-map
   :help "Go to area containing the Ciao system manuals")
  (ciao-tool-bar-local-item-from-menu  
   'ciao-help-on-current-symbol "icons/wordhelp"
   tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-complete-current-symbol "icons/complete"
   tool-bar-map ciao-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'ciao-customize-all
   "icons/preferences" tool-bar-map ciao-mode-map
   :help "Edit (customize) preferences for Ciao")
  (ciao-xemacs-toolbar-postprocess ciao-xemacs-tool-bar-tmp))

(defun ciao-general-toolbar (tool-bar-map)
  (if (not (boundp 'xemacs-logo))
      (progn
	;; *** Comment does not show up...
	(ciao-tool-bar-local-item-from-menu 'find-file "new" ;; "icons/ciaopl"
              tool-bar-map  global-map :help "Open or create a (Ciao) file")
	(ciao-tool-bar-local-item-from-menu 
	 'dired (if (> emacs-major-version 21) "diropen" "open") 
	 tool-bar-map)
	(ciao-tool-bar-local-item-from-menu 'kill-this-buffer "close"
                                            tool-bar-map) 
	(ciao-tool-bar-local-item-from-menu 'save-buffer "save"
         tool-bar-map global-map  :visible '(or buffer-file-name
						 (not (eq 'special
							  (get major-mode
							       'mode-class)))))
	(ciao-tool-bar-local-item-from-menu 'write-file "saveas" 
	 tool-bar-map global-map :visible '(or buffer-file-name
						(not (eq 'special
							 (get major-mode
							      'mode-class)))))
	(ciao-tool-bar-local-item-from-menu 'undo "undo" tool-bar-map  
         global-map :visible '(not (eq 'special (get major-mode 'mode-class))))
	(ciao-tool-bar-local-item-from-menu 
	 (lookup-key menu-bar-edit-menu [cut]) ;; 'kill-region 
	 "cut" tool-bar-map global-map 
	 :visible '(not (eq 'special (get major-mode 'mode-class))))
	(ciao-tool-bar-local-item-from-menu 
	 (lookup-key menu-bar-edit-menu [copy]) ;; 'menu-bar-kill-ring-save
	 "copy" tool-bar-map)
	(ciao-tool-bar-local-item-from-menu 
	 (lookup-key menu-bar-edit-menu [paste]) ;; 'yank 
	 "paste" tool-bar-map global-map 
	 :visible '(not (eq 'special (get major-mode 'mode-class))))
	(ciao-tool-bar-local-item-from-menu 
	 'nonincremental-search-forward "search" tool-bar-map)
	(ciao-tool-bar-local-item-from-menu 'print-buffer "print" tool-bar-map)
	)))

(defun ciao-xemacs-toolbar-postprocess (ciao-xemacs-tool-bar-tmp)
  (if (boundp 'xemacs-logo)
      (progn
;; 	(set-default-toolbar-position 'left)
	(set-specifier right-toolbar-visible-p t)
	;; (set-specifier right-toolbar-width 60)
	(set-specifier right-toolbar-width 35)
	(set-specifier 
	 ;; default-toolbar 
	 ;; left-toolbar
	 right-toolbar
	 (cons 
	  (current-buffer) 
	  (append
	   ;; For adding the default stuff
	   ;; (specifier-specs default-toolbar 'global)
	   ;; Separator
	   ;; '([:style 3d :size 30])
	   (reverse ciao-xemacs-tool-bar-tmp)
	   ))))))

(defun ciao-setup-inferior-tool-bar () 
  (if (and (not ciao-inhibit-toolbar) ;; ????
	   (or 
	    ;; xemacs case 
	    (and (boundp 'xemacs-logo)
		 (featurep 'toolbar)
		 (console-on-window-system-p))
	    ;; FSF emacs case 
	    (and (fboundp 'tool-bar-mode)
		 (display-graphic-p))
	   ))
      (ciao-do-setup-inferior-tool-bar)))

(defun ciao-do-setup-inferior-tool-bar () 
  (make-local-variable 'tool-bar-map)
  (if (boundp 'xemacs-logo)
      (setq ciao-xemacs-tool-bar-tmp nil)
    (setq tool-bar-map (make-sparse-keymap)))
  ;; General stuff (from standard tool bar); added only in FSF emacs.
  (ciao-general-toolbar tool-bar-map)
  ;; Ciao-specific stuff - added in both FSF and xemacs
  ;; Ciao logo is a special case
  (if (boundp 'xemacs-logo)
      (ciao-tool-bar-local-item-from-menu-xemacs
       'run-ciao-toplevel "icons/ciao")
    (ciao-tool-bar-local-item-from-menu
     'run-ciao-toplevel "icons/ciao" tool-bar-map ciao-inferior-mode-map))
  (ciao-tool-bar-local-item-from-menu  
   'ciao-fontify-buffer "icons/ciaorehighlight" 
   tool-bar-map ciao-inferior-mode-map)
  (if (or (> emacs-major-version 21) (boundp 'xemacs-logo))
      (ciao-tool-bar-local-item-from-menu
       'comint-interrupt-subjob  
       "icons/stop" tool-bar-map comint-mode-map
       :help "Interrupt top level"))
  (ciao-tool-bar-local-item-from-menu  
   'ciao-find-last-run-errors "icons/jump_to" 
   tool-bar-map ciao-inferior-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-unmark-last-run-errors "icons/clear" 
   tool-bar-map ciao-inferior-mode-map) 
  (ciao-tool-bar-local-item-from-menu  
   'ciao-set-query "icons/ciaostorequery" 
   tool-bar-map ciao-inferior-mode-map) 
  (ciao-tool-bar-local-item-from-menu  
   'ciao-load-query "icons/ciaoprompt" 
   tool-bar-map ciao-inferior-mode-map) 
  (ciao-tool-bar-local-item-from-menu  
   'ciao-clear-query "icons/ciaoclearquery" 
   tool-bar-map ciao-inferior-mode-map) 
  (if (or (> emacs-major-version 21) (boundp 'xemacs-logo))
      (ciao-tool-bar-local-item-from-menu
       'comint-previous-input
       "icons/left-arrow" tool-bar-map comint-mode-map 
       :help "Insert previous inputs at prompt"))
  (if (or (> emacs-major-version 21) (boundp 'xemacs-logo))
      (ciao-tool-bar-local-item-from-menu
       'comint-next-input
       "icons/right-arrow" tool-bar-map comint-mode-map
       :help "Insert later inputs at prompt"))
  (ciao-tool-bar-local-item-from-menu
   'ciao-goto-manuals "icons/manuals"  ;; "ciaomanuals" 
   tool-bar-map ciao-inferior-mode-map
   :help "Go to area containing the Ciao system manuals")
  (ciao-tool-bar-local-item-from-menu  
   'ciao-help-on-current-symbol "icons/wordhelp"
   tool-bar-map ciao-inferior-mode-map)
  (ciao-tool-bar-local-item-from-menu  
   'ciao-complete-current-symbol "icons/complete"
   tool-bar-map ciao-inferior-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'ciao-customize-all
   "icons/preferences" tool-bar-map ciao-inferior-mode-map
   :help "Edit (customize) preferences for Ciao")
  (ciao-xemacs-toolbar-postprocess ciao-xemacs-tool-bar-tmp))

;;===========================================================================
;; Functions to set all bindings 
;;===========================================================================

(defun ciao-emacs-can-do-tool-bar-p ()
  "Tool bars are supported."
  (or (fboundp 'tool-bar-mode) (boundp 'xemacs-logo)))

;(public)
(defun ciao-setup-bindings ()
  "Setup the local mode variables for setting the menu bar and
the tool bar for the major Ciao mode."
  ;; Keybindings
  ;; MR added to avoid errors in xemacs
  (if (boundp 'xemacs-logo)
      (define-key ciao-mode-map 'backspace 'delete-backward-char))
  ;;
  (use-local-map ciao-mode-map)
  ;; TODO: correct? why? (it was previously added to ciao-mode-hook)
  (define-key ciao-mode-map "\C-x\C-s" 'ciao-save-buffer)
  ;; Menu bar
  (ciao-setup-menu-bar)
  ;; MH Tool bar stuff (21.1 onwards)
  (if (ciao-emacs-can-do-tool-bar-p)
      (ciao-setup-tool-bar)))

;(public)
(defun ciao-setup-inferior-bindings ()
  "Setup the local mode variables for setting the menu bar and
the tool bar for the inferior Ciao mode."
  ;; Keybindings
  (use-local-map ciao-inferior-mode-map)
  ;; Menu bar
  (ciao-setup-inferior-menu-bar)
  ;; MH Tool bar stuff (21.1 onwards)
  (if (ciao-emacs-can-do-tool-bar-p)
      (ciao-setup-inferior-tool-bar)))


;; Provide ourselves:

(provide 'ciao-bindings)

;;; ciao-bindings.el ends here

