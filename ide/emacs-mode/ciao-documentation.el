;; --------------------------------------------------------------------------
;; Mode documentation and acks (see also documentation in functions
;; and the CiaoMode.pl file included above)
;; --------------------------------------------------------------------------

(require 'ciao) ; ciao-mode-internal
(require 'ciao-bindings) ; ciao-documented-commands,
			 ; ciao-mode-emacs-version

(defun ciao-mode-documentation ()
  "This function generates documentation in lpdoc format for the
    Ciao mode commands and their bindings."
  (interactive)
  (switch-to-buffer "*ciao-tmp*")
  (ciao-mode-internal) ;; so that the bindings that we document are active!
;;  (ciao-inferior-mode) ;; so that the bindings that we document are active!
  
  (insert "@comment{** Do not edit--generated automatically **}

The Ciao @concept{emacs interface} (or @em{mode} @cindex{emacs
mode} in @apl{emacs} terms) provides a rich, integrated user
interface to the Ciao @index{program development environment}
components, including the @apl{ciaosh} interactive top level, the
@apl{lpdoc} documentation generator, the testing system, and the
@apl{ciaopp} preprocessor.  Most features of these Ciao
development environment components are available from the command
line of the top-level shell and the preprocessor and as
standalone tools. However, using Ciao from inside @apl{emacs} is
highly recommended. The facilities that this mode provides
include:

@begin{itemize}

@item @index{Syntax-based highlighting} (coloring), @cindex{coloring,
syntax} @index{auto-indentation}, @index{auto-fill}, etc. of code. This
includes the assertions used by the preprocessor and the documentation
strings used by the Ciao auto-documenter, @apl{lpdoc}.

@item Providing automatic access to @concept{on-line help} for all
predicates by accessing the Ciao system manuals in @apl{info} format.

@item Starting and communicating with the @index{Ciao top-level}, running
in its own @concept{sub-shell}. This facilitates loading programs, checking
the @em{syntax} of programs (and of @index{assertions} within programs),
marking and unmarking modules for interactive debugging, @index{tracing the
source code} @cindex{source-level debugging} @cindex{debugging,
source-level} during debugging, making standalone executables, compiling
modules to dynamically linkable Ciao objects, compiling modules to active
objects, etc.

@item Starting and communicating with @apl{lpdoc}, the @index{Ciao
auto-documenter}, running in its own @concept{sub-shell}. This allows
generating in a very convenient way manuals for any file(s) being
edited, in a variety of output formats, and is very useful for quickly
checking how the auto-generated documentation will look.

@item Running unit tests on files or applications.

@item Starting and communicating with @apl{ciaopp}, the @index{Ciao
preprocessor}, running in its own @concept{sub-shell}. This allows easily
performing certain kinds of @index{static checks} (useful for finding
errors in programs before running them), program analysis tasks, and
@index{program transformations} on source programs.

@item Syntax highlighting and coloring of the error and warning
messages produced by the top level, unit testing, preprocessor,
or any other tool using the same message format (such as the
@apl{lpdoc} auto-documenter), and @em{locating automatically the
points in the source files where such errors occur}.

@item This mode also includes a very simple automatic
@index{version control} system which allows keeping a
@index{changelog} for individual files or for whole
applications. This is done by automatically including changelog
entries in source files, which can then be processed by the
@apl{lpdoc} auto-documenter. This is useful for smaller projects
that are not stored in a repository and can also be used for
maintaining changelogs even for projects that are
repository-based.

@end{itemize}

This chapter explains how to use the Ciao @apl{emacs} interface (and
how to set up your @apl{emacs} environment for correct operation, even
though this is normally done automatically by the installation
process).  The Ciao @apl{emacs} interface can also be used to work
with traditional Prolog or CLP systems.

@section{Conventions for writing Ciao programs under Emacs}
@cindex{formatting conventions, for emacs} 

There are currently a number of syntactic conventions for Ciao
programs which greatly help operation of the Emacs development
environment.  These conventions are particularly important for the
@concept{source-level debugger} and the @concept{syntax-based
coloring} capabilities.  The need for such conventions comes from the
fact that it would be unrealistic to write a complete Ciao parser in
Emacs lisp. These conventions are the following, more or less in order
of importance:

@begin{itemize}

@item Clauses should begin on the first column (this is used to recognize
      the beginning of a clause). 

@item C style comments should not be used in a clause, but can be used
      outside any clause.

@end{itemize}

@noindent The following suggestions are not strictly necessary
but can improve operation. In particular, they allow much greater
precision in the location of program points during source-level
debugging (for line by line tracing, when marking breakpoints,
etc.):

@begin{itemize}

@item Body literals should be indented. 

@item There should be no more than one literal per line.

@end{itemize}

Other issues:

@begin{itemize}

@item Comments which start with @tt{%}s are indented to the right
if indentation is requested.

@item For syntax-based highlighting to be performed, font-lock
must be available and not disabled (the Ciao mode enables it but
it may be disabled elsewhere in, e.g., the @file{.emacs} file).

@end{itemize}

@section{Checking the installation}

Typically, a complete pre-installation of the Ciao @apl{emacs}
interface is performed during Ciao installation. To check that
installation was completed sucessfully, open a file with a @tt{.pl}
ending. You should see that @apl{emacs} enters Ciao mode: the
mode is identified in the @concept{status bar} below the
@concept{buffer} and, if the @concept{emacs menu bar} is enabled, you
should see the Ciao menus. You should be able from the
menu-bar, for example, to go to the Ciao manuals in the info or load
the @tt{.pl} file that you just opened into a Ciao top level.

If things don't work properly, see the section @ref{Installation of the
Ciao emacs interface} later in this chapter.

@section{Functionality and associated key sequences (bindings)}

The following sections summarize the capabilities of the Ciao emacs
interface and the (default) @index{key sequences} used to access those
capabilities.  Note however that most of these functions are also
accessible from the menu bar, so learning these key combinations
is not necessary: the list is provided mainly for illustration of the
capabilities available, as well as completeness and documentation.

")

  ;; This inserts the documentation strings for the bindings.
  (ciao-do-document-bindings (nreverse ciao-documented-commands))

  (insert (concat "

@section{Using Ciao mode capabilities in standard shells} 

The capabilities (commands, coloring, error location, ...) which are
active in the Ciao @em{inferior} mode can also be made
available in any standard command line shell which is being run within
emacs. This can be enabled by going to the buffer in which the shell
is running and typing ``@key{M-x} @tt{ciao-inferior-mode}''.  This is
very useful for example when running the standalone compiler, the
@apl{lpdoc} auto-documenter, or even certain user applications (those
that use the standard error message library) in an emacs
sub-shell. Turning the Ciao inferior mode on on that sub-shell
will highlight and color the error messages, and automatically find
and visit the locations in the files in which the errors are reported.

Finally, one the most useful applications of this is when using
the @concept{embedded debugger} (a version of the debugger which
can be embedded into executables so that an interactive debugging
session can be triggered at any time while running that
executable without needing the top-level shell). If an
application is run in a shell buffer which has been set with Ciao
inferior mode (@key{M-x} @tt{ciao-inferior-mode}) and this
application starts emitting output from the embedded
debugger (i.e., which contains the embedded debugger and is
debugging its code) then the Ciao emacs mode will be able to
follow these messages, for example tracking execution in the
source level code. This also works if the application is written
in a combination of languages, provided the parts written in Ciao
are compiled with the embedded debugger package and is thus a
covenient way of debugging multi-language applications. The only
thing needed is to make sure that the output messages appear in a
shell buffer that is in Ciao inferior mode.



@section{Customization}

This section explains all variables used in the Ciao emacs mode
which can be customized by users. Such customization can be
performed (in later versions of @apl{emacs}) from the @apl{emacs}
menus (@tt{Help -> Customize -> Top-level Customization Group}),
or also by adding a @tt{setq} expression in the @tt{.emacs}
file. Such @tt{setq} expression should be similar to:

@tt{(setq <variable> <new_value>)}

@noindent The following sections list the different variables which can be
customized for @apl{ciao}, @apl{ciaopp} and @apl{lpdoc}.\n"))

(ciao-document-variables)

(insert (concat "

@section{Installation of the Ciao emacs interface}

If opening a file ending with @tt{.pl} puts emacs in another
mode (such as @apl{perl} mode, which is the --arguably
incorrect-- default setting in some @apl{emacs} distributions),
then either the emacs mode was not installed or the installation
settings are being overwritten by other settings in your
@tt{.emacs} file or in some library.  In any case, you can set
things manually so that the Ciao mode is loaded by default in
your system. This can be done by including in your @file{.emacs}
file a line such as:

@tt{(load ""<INSTALL_STOREDIR>/ciao-mode-init"")}

@noindent This loads the above mentioned file from the Ciao
library. If you would like to configure things in a different
way, you can also copy the contents of this file to your
@file{.emacs} file and make the appropriate changes.  For
example, if you do not want @tt{.pl} files to be put
automatically in Ciao mode, then comment out (or remove) the
line:

@tt{(setq auto-mode-alist} ... @tt{)}

@noindent You will then need to switch manually to Ciao mode by
typing @tt{M-x ciao-mode} after opening a Ciao file.

If you are able to open the Ciao menu but the Ciao manuals are
not found or the @apl{ciao} command (the top-level) is not found
when loading @tt{.pl} files, the probable cause is that you do
not have the Ciao paths in the @tt{INFOPATH} and @tt{MANPATH}
@index{environment variables} (whether these variables are set
automatically or not for users depends on how the Ciao system was
installed). See the Ciao installation
instructions (@ref{Installing Ciao from the source distribution}
or @ref{Installing Ciao from a Win32 binary distribution}) for
details.

@section{Emacs version compatibility} "

ciao-mode-emacs-version "

@section{Acknowledgments (ciao.el)}

This code is derived from the 1993 version of the emacs interface for
@concept{&-Prolog} by @author{Manuel Hermenegildo}, itself derived
from the original @file{prolog.el} by @index{Masanobu Umeda} with
changes by @index{Johan Andersson}, @index{Peter Olin}, @index{Mats
Carlsson}, and @index{Johan Bevemyr} of @index{SICS}, Sweden. Other
changes also by @author{Daniel Cabeza}, @author{Manuel C. Rodriguez},
@author{David Trallero}, and @author{Jose Morales}. See the changelogs
for details. "

  ))
  (setq version-control 'never)
  (write-file "CiaoMode.lpdoc")
  )

;;------------------------------------------------------------
;; Auxiliary
;;------------------------------------------------------------

;; Functions for generating documentation for the ciao.el mode functions
;; in lpdoc format (!) M. Hermenegildo

(defun ciao-do-document-bindings (sec-commands)
  "Generate documentation for all the bindings in lpdoc format."
   (cond
    ((eq sec-commands nil) nil)
    ((equal (car (car sec-commands)) 'section)
     (insert "@section{")
     (insert (car (cdr (car sec-commands))))
     (insert "}\n\n")
     (insert (car (cdr (cdr (car sec-commands)))))
     (insert "\n")
     (ciao-do-document-bindings (cdr sec-commands)))
    ((equal (car (car sec-commands)) 'paragraph)
     (insert "\n\n")
     (insert (car (cdr (car sec-commands))))
     (insert "\n\n")
     (ciao-do-document-bindings (cdr sec-commands)))
    (t ;; else, list of bindings
     (insert "@begin{description}\n")
     (ciao-print-function-info (car sec-commands))
     (insert "@end{description} @p \n")
     (ciao-do-document-bindings (cdr sec-commands)))
    ))

(defun ciao-print-function-info (info)
  "Print the information on a function as an item in lpdoc format. If
function is a string it is taken to be the comment."
  (insert
   (concat 
    "\n@item{"
    (ciao-print-keys (car info))
    "} "
    (let ((function (car (cdr info))))
      (if (stringp function)
	  function
	(documentation function)))
    "\n"
    ))
  )

(defun ciao-print-keys (str) 
  "Format key binding sequences in lpdoc format."
  (cond 
   ((string= str "") "")
   ((eq (string-match "M-x" str 0) 0)
    (concat "@key{M-x} @tt{" (substring str 3) "}"))

   ((eq (string-match "M-" str 0) 0)
    (concat "@key{" (substring str 0 3) "} "
	    (ciao-print-keys (substring str 3))))

   ((eq (string-match "A-" str 0) 0)
    (concat "@key{" (substring str 0 3) "} "
	    (ciao-print-keys (substring str 3))))

   ((eq (string-match "C-" str 0) 0)
    (concat "@key{^" (upcase (substring str 2 3)) "} "
	    (ciao-print-keys (substring str 3))))

;;    ((eq (string-match " " str 0) 0)
;;     (concat "@key{SPC} " 
;; 	    (ciao-print-keys (substring str 1))))

;; Not correct, but tries to fix spurious spaces which are passed
   ((eq (string-match " " str 0) 0)
    (concat "" 
 	    (ciao-print-keys (substring str 1))))

   ((eq (string-match "SPC" str 0) 0)
    (concat "@key{SPC} " 
	    (ciao-print-keys (substring str 3))))

   ((eq (string-match "\t" str 0) 0)
    (concat "@key{TAB} " 
	    (ciao-print-keys (substring str 1))))

   ((eq (string-match "TAB" str 0) 0)
    (concat "@key{TAB} " 
	    (ciao-print-keys (substring str 3))))

   ((eq (string-match "\e" str 0) 0)
    (concat "@key{ESC} " 
	    (ciao-print-keys (substring str 1))))

   ((eq (string-match "ESC" str 0) 0)
    (concat "@key{ESC} " 
	    (ciao-print-keys (substring str 3))))

   ((eq (string-match "RET" str 0) 0)
    (concat "@key{RET} " 
	    (ciao-print-keys (substring str 3))))
   (t 
    (concat "@key{" 
;;	    (text-char-description (string-to-char (substring str 0 1) ))
	    (key-description (substring str 0 1) )
	    "} "
	    (ciao-print-keys 
	     (substring str 1))))))

(defun ciao-document-variables ()
  "Generate documentation for all user-defined variables in lpdoc format."
  (let ((sym-list)
	(ciao-vars nil)
	(ciaopp-vars nil)
	(lpdoc-vars nil)
	(ciao-faces nil))
    
    ;; Build a list of symbols that match pattern.
    (mapatoms (function
	       (lambda (sym)
		 (if (string-match "ciao" (symbol-name sym))
		     (setq sym-list (cons sym sym-list))))))
    
    ;; Classify variables
    ;; TODO: Obtain that from a list
    (mapcar (function (lambda (sym)
			(cond ;; Must be before others
			      ((string-match "face" (symbol-name sym))
			       (setq ciao-faces (cons sym ciao-faces)))
			      ((string-match "ciaopp" (symbol-name sym))
			       (setq ciaopp-vars (cons sym ciaopp-vars)))
			      ((string-match "lpdoc" (symbol-name sym))
			       (setq lpdoc-vars (cons sym lpdoc-vars)))
			      (t 
			       (setq ciao-vars (cons sym ciao-vars))))))
	    sym-list)

    ;; Generate the documentation
    (insert "\n@subsection{Ciao general variables}\n")
    (insert "@begin{description}\n")
    (mapcar 'ciao-describe-func (sort ciao-vars 'string<))
    (insert "@end{description}\n")
    (insert "\n@subsection{CiaoPP variables}\n")
    (insert "@begin{description}\n")
    (mapcar 'ciao-describe-func (sort ciaopp-vars 'string<))
    (insert "@end{description}\n")
    (insert "\n@subsection{LPdoc variables}\n")
    (insert "@begin{description}\n")
    (mapcar 'ciao-describe-func (sort lpdoc-vars 'string<))
    (insert "@end{description}\n")
    (insert 
     "\n@subsection{Faces used in syntax-based highlighting (coloring)}\n")
    (insert "@begin{description}\n")
    (mapcar 'ciao-describe-func (sort ciao-faces 'string<))
    (insert "@end{description}\n")))

(defun ciao-describe-func (s)
  "Format the description of a symbol."
  (cond
   ;; It is a customizable variable 
   ((and (boundp s) (get s 'custom-type)) 
    (insert 
     (concat "@item{@tt{" 
	     (symbol-name s)
	     "} (@em{"))
    (if (listp (get s 'custom-type))
	(insert
	 (symbol-name 
	  (type-of
	   (car (cdr (car (cdr 
			   (get 's 'custom-type))))))))
      (insert (symbol-name (get s
				       'custom-type))))
    (insert "})}\n")
    (insert 
     (concat 
      (documentation-property s 'variable-documentation)
      "\n")))
   ;; It is a face
   ((documentation-property s 'face-documentation)
    (insert 
     (concat "@item{@tt{" 
	     (symbol-name s)
	     "} (@em{face})}\n"
	     (documentation-property s 'face-documentation)
	     "\n")))
   ))


;; Provide ourselves:

(provide 'ciao-documentation)

;;; ciao-documentation.el ends here

