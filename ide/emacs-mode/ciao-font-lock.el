;;; ciao-font-lock.el --- Font locking module for Ciao Mode

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

(require 'font-lock)

(require 'ciao-faces) ; ciao-faces-use-variable-pitch-in-comments
(require 'ciao-syntax) ; ciao-any-prompt-pattern

;;----------------------------------------------------------------------------

(defun ciao-emacs-can-do-font-lock-p ()
  "Fontifying is supported (possible in windowing system,
modern emacses, and also in ascii mode with emacs>= 21.1)."
  (or window-system (fboundp 'tool-bar-mode)))

;;----------------------------------------------------------------------------

;; Just a bridge (for documentation and setting local binding)
;; but better than font-lock-fontify-buffer
; TODO: Just for java-ciaopp.el, is it correct?
(defun ciao-fontify-buffer ()
  "Undate (recompute) syntax-based highlighting (coloring)."
  (interactive)
  (save-excursion
    (font-lock-fontify-region (point-min) (point-max))))

;;----------------------------------------------------------------------------
;; Regular expressions and matching
;;----------------------------------------------------------------------------
;; TODO: move to ciao-syntax

(defvar ciao-predicate-directives
  '( "data" "dynamic" "multifile" "impl_defined" "meta_predicate"
     "discontiguous" "persistent"
     ;; moved as predicate directives (JFMC)
     "export" "redefining" 
     ;; indexer package (JFMC)
     "index"
     ;; new OO-module system (JFMC)
     "public" "constructor" "constant" "static" "attr" "mut" "fluid" "virtual"
     ;; O'Ciao
     "inheritable"
     )
  "Names of directives describing properties of predicates.")

(defvar ciao-module-block-directives
  '( "module" "package"
     ;; new OO-module system (JFMC)
     "class" "interface" "mixin" 
     )
  "Names of module directives that may contain curly braces.")

(defvar ciao-module-directives
  '( "use_module" "ensure_loaded" "use_active_module"
     "use_package" "include" "use_foreign_library" "use_foreign_source"
     "reexport" "import"
     "initialization" "on_abort"
     ;; new OO-module system (JFMC)
     "use_class"
     ;; ociao
     "implements" "inherit_class"
     )
  "Names of module directives.")
;; Module directives in prefix form (e.g. ":- extends F.")
;; TODO: should these be module directives too?
(defvar ciao-module-simple-directives
  '( ;; new OO-module system (JFMC)
     "extends"
     )
  "Names of simple module directives (in prefix form).")

(defvar ciao-builtin-directives
  '( "new_declaration" "op" 
     "load_compilation_module" "add_sentence_trans" "add_term_trans"
     "add_clause_trans" "add_goal_trans"
     "set_prolog_flag" "push_prolog_flag" "pop_prolog_flag" 
     ;; Resources and Granularity
     "compound_resource" "platform_constants" "platform"
     "load_resource_module" "resource" "head_cost" "literal_cost"
     "trust_default" "granularity_resources"
     ;; Unit-Testing
     "load_test_module"
     "load_test_package"
     ;; Foreign-interface
     "extra_compiler_opts" "extra_linker_opts"
     )
  "Names of other directives.")

(defvar ciao-condcode-directives
  '( ;; Conditional code (package(condcomp))
     "if" "else" "elif" "endif" "compilation_fact"
     )
  "Names of directives to define conditional code.")

(defvar ciao-library-directives
  '( 
;; functions
     "fun_eval" "fun_return" "lazy" "funct" 
     ;; backwards compatibility:
     "function"
;; argnames
     "argnames" 
;; make
     "make" 
     )
  "Names of additional directives defined in the libraries.")
 
(defcustom ciao-user-directives '( "mydirective" )
  "List of identifiers of any directives defined by users which you
would like highlighted (colored). Be careful, since wrong entries may
affect other syntax highlighting."
  :group 'ciaolang
  :type 'list)

;; Also, 'ciao-user-directives' now customizable; see above in file.

;; KNOWN FEATURES:
;;
;; atom with % => Decided to sacrify this. It is _very hard_ to
;; define a grammar in which comments and language grammar is included
;; (the way to do that is to "pre-propocess" the text.  In summary:
;; 'asdfsdf % asdfasdf' => is considered like: 'asdfsdf COMMENT, so no
;; right part is not highlighted.
;;
;; Atom that ends in 0:
;; 'antom0'.
;; 0'. is consider a character instead of an atom. This is important
;; in order not to break the highlighting syntax. Example:
;; X = 0'a,
;; Y = 0'b.
;; In this case:
;;      'a,
;; Y = 0'  would be considered an atom if 0' are not treated first.
;; But: X = 0'a,X = 0'a,X = 0'a (all text would be considered an atom).
;;
;; Comments in the middle of assertions that contain a period:
;; :- true pred l(X) %well...
;;    : list( X ) "bla bla bla".
;; A preprocessor would remove the comment and it would work, but
;; as we cannot "remove" it, dot is consider the end of the assertion :(
;;
;; Other things (those that are not written in good "Prolog programming
;; style"):
;; X = 
;; 'This is my atom'.
;; 
;; Order is forwards. Basically font lock works like this:
;; Once an expresion is matched, it is highligthed. This cannot be
;; "rehighlithed" unless rewrite or keep is specified (unlike what
;; happened with the hilit package, which has resulted in many changes).

(defun ciao-font-lock-defaults-create ()
  "Locally set `font-lock-defaults' for Ciao mode."
  ;; MR added to support font-lock
  (if (ciao-emacs-can-do-font-lock-p)
      (set (make-local-variable 'font-lock-defaults)
	   '(ciao-mode-font-lock-keywords 
	     t nil nil 
	     ;; Use all buffer refontifying...
	     beginning-of-buffer 
	     (font-lock-mark-block-function
	      . 
	      ;; Alternative: mark-paragraph
	      ;; Use whole buffer for refontifying...
	      (lambda () 
		(push-mark (point-max))
		(goto-char (point-min))))
	     ))))

(defun ciao-mode-font-lock-keywords ()
  `(
    ;; comments /* */
    ((lambda (limit) 
       (ciao-font-lock-match limit "/\\*" "\\*/"))
     . 
     ,(if ciao-faces-use-variable-pitch-in-comments
	  'ciao-face-comment-variable-pitch
	'ciao-face-comment))
    ;; comments
;    ("% TODO:" . ciao-face-lpdoc-bug-comment)
;    ("%.*$" . ciao-face-comment)
    ((lambda (limit) 
       (ciao-font-lock-match limit "% TODO:" ""))
     0 'ciao-face-lpdoc-bug-comment keep)
    ((lambda (limit) 
       (ciao-font-lock-match limit "%" ".*$"))
     0 'ciao-face-comment keep)
    ;; scripts
    ((lambda (limit) 
       (ciao-font-lock-match limit "^#!" "^[ \t]*$"))
     . ciao-face-script-header)

    ;; lpdoc comment strings in assertions
;    ("#[^\\\"]*\\(\"\\([^\\\"]\\|\\\\\\(.\\|\n\\)\\)*\"\\)" 
    ("#[ \n\t]*\\(\"\\([^\\\"]\\|\\\\\\(.\\|\n\\)\\)*\"\\)" 
     1 
     ,(if ciao-faces-use-variable-pitch-in-comments
	  'ciao-face-lpdoc-comment-variable-pitch
	'ciao-face-lpdoc-comment) keep)
    ;; lpdoc (bug) comments
    ((lambda (limit)
       (ciao-font-lock-match limit
		     "^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*bug\\>"
                     "[^\\\"]\"[ \t\n]*)[ \t\n]*\\.")) ; was [ \t]*$
     0 ciao-face-lpdoc-bug-comment keep)
    ;; lpdoc title comments
    ((lambda (limit)
       (ciao-font-lock-match-inside limit 1 2
		     "^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*title[ \t\n]*,[ \t\n]*\""
                     "[^\\\"]\\(\"\\)[ \t\n]*)[ \t\n]*\\.")) ; was [ \t]*$
     0 'ciao-face-sectioning-2-face keep)
    ;; lpdoc section comments
    ((lambda (limit)
       (ciao-font-lock-match-inside limit 1 2
		     "^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*section[ \t\n]*,[ \t\n]*\""
                     "[^\\\"]\\(\"\\)[ \t\n]*)[ \t\n]*\\.")) ; was [ \t]*$
     0 'ciao-face-sectioning-3-face keep)
    ;; lpdoc subsection comments
    ((lambda (limit)
       (ciao-font-lock-match-inside limit 1 2
		     "^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*subsection[ \t\n]*,[ \t\n]*\""
                     "[^\\\"]\\(\"\\)[ \t\n]*)[ \t\n]*\\.")) ; was [ \t]*$
     0 'ciao-face-sectioning-4-face keep)
    ;; lpdoc subsubsection comments
    ((lambda (limit)
       (ciao-font-lock-match-inside limit 1 2
		     "^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*subsubsection[ \t\n]*,[ \t\n]*\""
                     "[^\\\"]\\(\"\\)[ \t\n]*)[ \t\n]*\\.")) ; was [ \t]*$
     0 'ciao-face-sectioning-5-face keep)
    ;; other lpdoc comments
    ;; "^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*\\(version\\(_maintenance\\)?\\|doinclude\\|hide\\|filetype\\|nodoc\\)\\>" 
    ;; These ones have a string in the second argument
    ((lambda (limit)
       (ciao-font-lock-match 
	limit
	"^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)("
	"[^\\\"]\"[ \t\n]*)[ \t\n]*\\.")) ; was: [ \t]*$
     0
     ,(if ciao-faces-use-variable-pitch-in-comments
	  'ciao-face-lpdoc-comment-variable-pitch
	'ciao-face-lpdoc-comment) keep)
    ;; These ones do not have a string in the second argument
    ((lambda (limit)
       (ciao-font-lock-match 
	limit
	"^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*\\(version_maintenance\\|doinclude\\|hide\\|filetype\\|nodoc\\)\\>"
	"[ \t\n]*)[ \t\n]*\\.")) ; was: [ \t]*$
     0
     ,(if ciao-faces-use-variable-pitch-in-comments
	  'ciao-face-lpdoc-comment-variable-pitch
	'ciao-face-lpdoc-comment) keep)

    ;; Trick (ciao-font-lock-match-in-comment) to color LPdoc commands
    ;; only if they have already been colored as lpdoc comments (JFMC)
    ;;
    ;; The face is overridden by using the t parameter (override)
    ((lambda (limit) 
       (ciao-font-lock-match-in-comment limit "@begin{verbatim}" "@end{verbatim}"))
     0 ciao-face-lpdoc-verbatim t)

    ((lambda (limit) 
       (ciao-font-lock-match-in-comment limit "@include[^ {}@]*{" "[^}@]*}"))
     0 ciao-face-lpdoc-include t)

    ((lambda (limit)
       (ciao-font-lock-simple-match-in-comment
	limit
	"@\\([}{@]\\|\\([A-Za-z]+\\|[?!]\\)[ \t\n]\\)"))
     0 ciao-face-lpdoc-command t)

    ((lambda (limit)
       (ciao-font-lock-simple-match-in-comment
	limit
	"@[^ \t\n{}@=<>]*{[^{}@]*}"))
     0 ciao-face-lpdoc-command t)

    ;; quoted atoms
    ("\\('\\(\\\\\\\\\\|\\\\'\\|[^'\n]\\)*'\\)" 0 ciao-face-quoted-atom)
    ;; Strings
;;    ("^[^']*\\(\"\\([^\\\"]\\|\"\"\\|\\\\\\(.\\|\n\\)\\)*\"\\)" 1 ciao-face-string)
    ("\\(\"\\([^\\\"]\\|\"\"\\|\\\\\\(.\\|\n\\)\\)*\"\\)" 0 ciao-face-string)

    ;; funexp atoms (jfmc)
    ("\\(~[a-z][A-Za-z0-9_\\.^/]*[A-Za-z0-9_^/]\\|~[a-z]\\)" 0 ciao-face-funexp-atom)

    ;; Characters 0'...
    ("0'\\(\\\\.\\|.\\)" 0 ciao-face-string)
    
    ;; TODO: is it working? missing href
    ("@\\(cite\\|ref\\|section\\|subsection\\|subsubsection\\){[^{}@]*}"
     0 ciao-face-lpdoc-crossref)

    ;; Declaration of nested modules (new module system, JFMC)
    ((lambda (limit) 
       (ciao-font-lock-match-block-directive limit))
     0 ciao-face-module-directive keep)
    ;; Directives (keep => They can have a comment)
    ((lambda (limit) 
       (if (not (ciao-font-lock-match
		 limit    
		 (concat "^[ \t]*:-[ \t\n]*" 
			 (regexp-opt ciao-predicate-directives t) "\\>")
		 "[ \t]*\\({\\|\\.$\\|\\.[ \t\n]\\)"))
	   nil
	 (ciao-font-lock-avoid-curly) ;; Do not color trailing '{'
	 t
	 )) ; was: ^[ \t]*$\\|\\.$
     0 ciao-face-predicate-directive keep)
    ((lambda (limit)
       (ciao-font-lock-match
        limit
        (concat "^[ \t]*:-[ \t\n]*" 
		(regexp-opt ciao-builtin-directives t) "\\>")
        ")[ \t]*\\.")) ; was: "^[ \t]*$\\|\\."
     0 ciao-face-builtin-directive keep)
    ((lambda (limit) ;; (Not nested module)
       (ciao-font-lock-match
	limit
	(concat "^[ \t]*:-[ \t\n]*"
		(regexp-opt ciao-module-directives t) "\\>")
;	")[ \t]*\\.")) ; was: [ \t]*
	")[ \t]*\\."))
     0 ciao-face-module-directive keep)
    ((lambda (limit) 
       (ciao-font-lock-match
	limit
	(concat "^[ \t]*:-[ \t\n]*"
		(regexp-opt ciao-module-simple-directives t) "\\>")
;	")[ \t]*\\.")) ; was: [ \t]*
	"[ \t]*\\."))
     0 ciao-face-module-directive keep)
    ;; Conditional code (package(condcomp))
    ((lambda (limit) 
       (ciao-font-lock-match
	limit
	(concat "^[ \t]*:-[ \t\n]*"
		(regexp-opt ciao-condcode-directives t) "\\>")
	"\\."))
     0 ciao-face-condcode-directive keep)

    ((lambda (limit)
       (ciao-font-lock-match
        limit
        (concat "^[ \t]*:-[ \t\n]*" 
		(regexp-opt ciao-library-directives t) "\\>")
        "^[ \t]*$\\|\\.")) ; was: "^[ \t]*$\\|\\.$"
     0 ciao-face-library-directive keep)

    ((lambda (limit) 
       (ciao-font-lock-match
        limit
        (concat "^[ \t]*:-[ \t\n]*" (regexp-opt ciao-user-directives t) "\\>")
        "^[ \t]*$\\|\\.")) ; was: .$
     0 ciao-face-user-directive keep)
    ;; --- add whatever is like :- I dont know bla bla bla bla.

    ((lambda (limit) 
       (ciao-font-lock-match-until-matching-sexp limit "\\<debug_message("))
     0 ciao-face-debug-mess)

    ;; LPdoc comments
    ;; lpdoc version comments (and other related directives)
    ((lambda (limit) 
       (ciao-font-lock-match
	limit
	"^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*version(" 
	"[^\\\"]\"[ \t\n]*)[ \t\n]*\\.")) ; was [ \t]*$
     0 ciao-face-lpdoc-version-comment keep)

    ;; Assertions Variables
    ;; ("^[ \t]*:-[^(]*\\<\\([A-Z_][a-zA-Z0-9_]*\\)" 1 ciao-face-assrt-variable)

    ;; Assertions
    ((lambda (limit)
       (ciao-font-lock-match 
	limit (ciao-begin-assrt-regexp "checked") (ciao-end-assrt-regexp)))
     0 ciao-face-checked-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match 
	limit (ciao-begin-assrt-regexp "true") (ciao-end-assrt-regexp)))
     0 ciao-face-true-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match 
	limit (ciao-begin-assrt-regexp "false") (ciao-end-assrt-regexp)))
     0 ciao-face-false-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match 
	limit (ciao-begin-assrt-regexp "trust") (ciao-end-assrt-regexp)))
     0 ciao-face-trust-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "check") (ciao-end-assrt-regexp)))
     0 ciao-face-check-assrt keep)

    ((lambda (limit) 
       (ciao-font-lock-match-until-matching-sexp limit "^[ \t\n]*true("))
     0 ciao-face-true-assrt keep)
    ((lambda (limit) 
       (ciao-font-lock-match-until-matching-sexp limit "^[ \t\n]*false("))
     0 ciao-face-false-assrt keep)
    ((lambda (limit) 
       (ciao-font-lock-match-until-matching-sexp limit "^[ \t\n]*trust("))
     0 ciao-face-trust-assrt keep)
    ((lambda (limit) 
       (ciao-font-lock-match-until-matching-sexp limit "^[ \t\n]*check("))
     0 ciao-face-check-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match-until-matching-sexp limit "^[ \t\n]*checked("))
     0 ciao-face-checked-assrt keep)

    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp 
	       (regexp-opt '("decl" "pred" "comp" "calls" 
			     "success" "test" "texec") t)
	       )
	(ciao-end-assrt-regexp)))
     0 ciao-face-check-assrt keep)

    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "prop") (ciao-end-assrt-regexp)))
     0 ciao-face-prop-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "test") (ciao-end-assrt-regexp)))
     0 ciao-face-test-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "texec") (ciao-end-assrt-regexp)))
     0 ciao-face-texec-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "regtype") (ciao-end-assrt-regexp)))
     0 ciao-face-type-assrt keep)
    ;; mtype and mprop are experimental for mtypes package -- JFMC
    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "mtype") (ciao-end-assrt-regexp)))
     0 ciao-face-type-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "mprop") (ciao-end-assrt-regexp)))
     0 ciao-face-type-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "entry") (ciao-end-assrt-regexp)))
     0 ciao-face-entry-assrt keep)
    ((lambda (limit)
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "modedef") (ciao-end-assrt-regexp)))
     0 ciao-face-modedef-assrt keep)

    ;; Variables
    ("\\<\\([A-Z_][a-zA-Z0-9_]*\\)" 1 ciao-face-variable)
    ;; Concurrency ops
    ("\\([ \t]&&\\|[ \t]&>\\|[ \t]<&\\|[ \t]&\\|[ \t]@[ \t]\\)"
    ;; ("\\(&\\|&>\\|<&\\|@[^=<>]\\)"
     . ciao-face-concurrency-op) 
    ;; Cut
    ("!" . ciao-face-cut)
    ;; Declaration neck (somewhat of a warning --recognized ones
    ;; colored above)
    ("^[ \t]*:-" . ciao-face-lpdoc-bug-comment)
    ;; Necks.
    ("\\(:-\\|-->\\|:=\\)" . ciao-face-prompt)
    ;; Other major connectors, etc.
    ;; jfmc: disabled, coloring operators along with variables and
    ;;       funexps was distracting
;    ("\\(|\\|?\\|->\\|~\\)" . ciao-face-quoted-atom)
    ;; jfmc: all symbols
;    ("[^'a-zA-Z0-9,()_ \t\n]" . ciao-face-quoted-atom)
    ;; jfmc: all atoms
;    ("[a-zA-Z0-9_]+" . ciao-face-quoted-atom)

    ;; Clause heads
    ; this consider atoms _without_ spaces and normal names as a
    ; clause name
;    ("^\\([a-z'][^(\\.: ]*\\)\\([ \t\n]*\\.\\|[ \t\n]*:-\\|(\\)"
;      1 ciao-face-clauseheadname t)
;;    ("^[a-z][a-zA-Z0-9_]*" 0 ciao-face-clauseheadname)
;;    ("^\\('\\(\\\\'\\|[^']\\)*'\\)\\([ \t\n]*\\.\\|[ \t\n]*:-\\|(\\)" 
;;    0 ciao-face-clauseheadname)
;    ("^[a-z][a-zA-Z0-9_]*" 0 ciao-face-clauseheadname)
    ;; jfmc: tricky match fo clause head name
    ((lambda (limit)
       (ciao-font-lock-match-clauseheadname
	limit))
     0 ciao-face-clauseheadname keep)

    ;; quoted atoms larger than 1 line
    ("\\('\\(\\\\\\\\\\|\\\\'\\|[^']\\)*'\\)" 0 ciao-face-quoted-atom)

    ;; This is for debugging... if patata appears in green it means
    ;; all previous rules are understood by emacs (or XEmacs)
    ;; Emacs
    ;; ("patata" 0 patata_var t)
    ;; XEmacs

    ;; ("patata" 0 ciao-face-true-assrt t)
    )
  )

;; (defvar patata_var 'ciao-face-true-assrt)

(defun ciao-begin-assrt-regexp (identifier)
  (concat "^[ \t]*:-[ \t\n]*" identifier "[ \t\n]"))

;older
;"^[ \t]*:-[ \t]*\\(check\\)?[ \t]*\\(decl\\|pred\\|comp\\|calls\\|success\\) "

(defun ciao-end-assrt-regexp ()
  "[^#\\.]*\\(#\\|\\.\\)[ \t]*$")

;  "[^#\\.]*\\(#[ \t\n]*\\|\\.[ \t\n]*$\\)")
;  "[ \t]#[ \t\n]\\|^#[ \t\n]\\|\\.[ \t]*$")

(defun ciao-font-lock-match (limit beginexp endexp)
  (let ((begin 0) (end 0))
    (if (not (search-forward-regexp beginexp limit t))
	nil
      (setq begin (car (match-data)))
      (if (not (search-forward-regexp endexp limit t))
	  nil
	(setq end (cdr (match-data)))
	(set-match-data (cons begin end))
	t
      ))))

;; JFMC: like font-lock-match, but skips N elements from the match of
;; beginexp and M elements from the match of endexp
(defun ciao-font-lock-match-inside (limit n m beginexp endexp)
  (let ((begin 0) (end 0))
    (if (not (search-forward-regexp beginexp limit t))
	nil
      (setq begin (nth n (match-data)))
      (if (not (search-forward-regexp endexp limit t))
	  nil
	(setq end (nth m (match-data)))
	(set-match-data (cons begin (cons end nil)))
	t
      ))))

;; JFMC: match only if the text has been colored before as ciao-face-lpdoc-comment
(defun ciao-font-lock-simple-match-in-comment (limit beginexp)
  (if (not (search-forward-regexp beginexp limit t))
      nil
    (eq (get-text-property (car (match-data)) 'face) 'ciao-face-lpdoc-comment)))

(defun ciao-font-lock-match-in-comment (limit beginexp endexp)
  (let ((begin 0) (end 0))
    (if (not (search-forward-regexp beginexp limit t))
	nil
      (setq begin (car (match-data)))
      (if (not (search-forward-regexp endexp limit t))
	  nil
	(setq end (cdr (match-data)))
	(set-match-data (cons begin end))
	(eq (get-text-property (car (match-data)) 'face) 'ciao-face-lpdoc-comment)
      ))))

;;; jfmc: old definition
;; (defun ciao-font-lock-match-clauseheadname (limit)
;;  (search-forward-regexp "^[a-z][a-zA-Z0-9_]*" limit t))

;; Matches a clauseheadname
;; jfmc: version that works even if clauses are inner indented
(defun ciao-font-lock-match-clauseheadname (limit)
  (let ((found nil))
    (while (and (not found)
		(search-forward-regexp "[a-z][a-zA-Z0-9_]*" limit t))
      (let ((begin (match-beginning 0)))
	(if (save-match-data (save-excursion
			       (progn
				 (goto-char begin)
				 (ciao-scan-backward-clausehead))))
	    ;; Found!
	    (setq found t))))
    found))

(defun ciao-scan-backward-clausehead ()
  "Scan characters backwards to determine if we are at a clause head
   position"
  ;; Skip blanks just before clausehead (returns nil if there are no
  ;; blanks and the clausehead is not at column 0)
  (if (and (> (current-column) 0)
	   (= (skip-chars-backward " \t") 0))
      nil ;; no blank, this cannot be a clause head
    ;; Skip comments and empty lines
    (while (and (> (point) 1)
		(progn (forward-char -1)
		       (looking-at "\n")))
      ;; Find a possible comment and skip blanks
      ;; TODO: incorrectly treats quoted % as comments (jfmc)
      (beginning-of-line)
      (re-search-forward "[^%\n]*")
      (skip-chars-backward " \t"))
    ;; Detect if we are at a position that allows new clauses
    (cond ((= (point) 1) t) ;; beginning of buffer
	  ((looking-at "\\.") t) ;; clause end
	  ((looking-at "{") (ciao-scan-forward-curly-block)) ;; curly brace
	  (t nil)))) ;; no clause

(defun ciao-scan-forward-curly-block ()
  "Detect if the curly block contains sentences rather than terms"
  ;; TODO: imprecise, it should parse the tokens (i.e. {foo({bar}).} is not detected
  ;; TODO: {foo.} is not detected
  (re-search-forward "[^\\.}]*")
  (looking-at "\\.")
  )

(defun ciao-font-lock-match-block-directive (limit)
  "Match a directive that may contain a postfix block"
  (if (not (ciao-font-lock-match
	    limit
	    (concat "^[ \t]*:-[ \t\n]*"
		    "\\(public\\)?[ \t\n]*"
		    (regexp-opt ciao-module-block-directives t) "\\>")
	    "[ \t]*\\({\\|\\.$\\|\\.[ \t\n]\\)"))
      nil
    (ciao-font-lock-avoid-curly) ;; Do not color trailing '{'
    t
    )
  )

;; We do not want '{' to be colored, move end marker 1 character
;; backward if '{' is found.
(defun ciao-font-lock-avoid-curly ()
  (when (save-match-data (save-excursion (forward-char -1) (looking-at "{")))
    (let ((begin (car (match-data)))
	  (end (car (cdr (match-data)))))
      (setq end (copy-marker (- end 1)))
      (set-match-data (cons begin (cons end nil)))
      )
    )
  )

;; Matches corresponding closing delimiter
(defun ciao-font-lock-match-until-matching-sexp (limit beginexp)
  (let ((begin 0) (end 0))
    (if (not (search-forward-regexp beginexp limit t))
	nil
      (setq begin (car (match-data)))
      (goto-char (- (car (cdr (match-data))) 1))
      (forward-list)
      (setq end (cons (point) nil))
      (set-match-data (cons begin end))
      t
      )))

(defun ciao-inferior-font-lock-defaults-create ()
  "Locally set `font-lock-defaults' for inferior Ciao process."
  ;; MR added to support font-lock
  (if (ciao-emacs-can-do-font-lock-p)
      (set (make-local-variable 'font-lock-defaults)
	   '(ciao-inferior-font-lock-keywords 
	     t nil nil 
	     ;; Use all buffer refontifying...
	     beginning-of-buffer 
	     (font-lock-mark-block-function
	      . 
	      ;; Alternative: mark-paragraph
	      ;; Use whole buffer for refontifying...
	      (lambda () 
		(push-mark (point-max))
		(goto-char (point-min))))
	     ))))

(defun ciao-inferior-font-lock-keywords ()
      `(
	("^\\([A-Z][a-zA-Z0-9_]*\\) = \\(.*\\)\\(,\\| \\?.*\\)$"
         (1 ciao-face-answer-var)               ;; Answer variable
         (2 ciao-face-answer-val)               ;; Answer value
         (3 ciao-face-prompt)                   ;; Prompt after answer
         )
	("^\\([ \t]+[0-9]+[ \t]+[0-9]+\\)\\(Call:\\).*$"
         (1 ciao-face-debug-redo)               ;; 
         (2 ciao-face-debug-call)               ;; 
         )
	(
	 ,(ciao-any-prompt-pattern)
	 ;; "^\\(\\(\\|[0-9]+ \\|ciaopp \\|| \\)\\?-\\)"
         . ciao-face-prompt)                    ;; Prompts
	("^yes$" . ciao-face-yes-answer)        ;; Answer
	("^no$" . ciao-face-no-answer)          ;; Answer
;;	("^Select[^:]*:" . ciao-face-ciaopp-option) ;; Preproc prompt
	("\\([A-Z][a-zA-Z \\-]*:\\) *\\(\\[[a-zA-Z0-9, _\t\n]*\\]\\)[ \t\n]*\\(([^)]*)\\)[ \t\n]*\\(\\?\\)"
         (1 ciao-face-ciaopp-option)            ;; Option message
         (2 ciao-face-answer-val)               ;; Option values
         (3 ciao-face-answer-var)               ;; Default
         (4 ciao-face-prompt)                   ;; Prompt
         )
	("^{?ERROR.*$" . ciao-face-error-mess)  ;; Error messages
	("^{SYNTAX ERROR.*$" . ciao-face-error-mess)  ;; Error messages
	("^\\*\\* here \\*\\*[ \t]*$" . ciao-face-error-mess)  ;; Error mes
	("^{?WARNING.*$" . ciao-face-warning-mess)  ;; Error messages
	("^{DEBUG.*$" . ciao-face-debug-mess)       ;; Error messages
	("^{?Note:.*$" . ciao-face-note-mess)       ;; Error messages
	("^{NOTE.*$" . ciao-face-note-mess)         ;; Error messages
        ("^\\({.*\\|}\\)" . ciao-face-other-mess)   ;; Error messages
;;        ("^\\*\\*\\* ---------.*\n^\\*\\*\\* .*\n\\*\\*\\* ---------.*$" 
        ("^\\*\\*\\* \\(---------\\|=========\\).*$" 
	 . ciao-face-highlight-code)              ;; LPdoc (1.9) messages
        ("^\\*\\*\\* .*$" . ciao-face-debug-call) ;; LPdoc (1.9) messages
	; The different startup messages
;	("^Ciao\\>.*$" . ciao-face-startup-message);; Startup
	("^\\(Ciao [0-9]+\\.[0-9].*:\\|Ciao Preprocessor\\).*$" . ciao-face-startup-message);; Startup
        ; Recognizes a date at the end of the line (ciaopp still does it)
	("^(C) .* \\w\\w\\w \\w\\w\\w [1-3]?[0-9]\
 [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [A-Z][A-Z][A-Z] [1-2][0-9][0-9][0-9]$"
        . ciao-face-startup-message)              ;; Startup, second line
;	("\\(^\\?- *[^{ ]\\|^| \\?- *\\).*\\.[ \t]*\n"
;	 . ciao-face-prompt) ;; Query doesn't work(?)
))


;; Provide ourselves:

(provide 'ciao-font-lock)

;;; ciao-font-lock.el ends here
