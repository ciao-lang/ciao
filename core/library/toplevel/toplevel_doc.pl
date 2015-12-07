:- module(_,[], [assertions]).

:- doc(title,  "The interactive top-level shell").
:- doc(author, "Daniel Cabeza").
:- doc(author, "The CLIP Group").

:- doc(usage, "The following predicates can be used at the top-level
   shell natively (but see also the commands available in @ref{The
   interactive debugger} which are also available within the top-level
   shell).").

:- doc(copyright,"
Copyright @copyright{} 1996-2002 Daniel Cabeza/The CLIP Group.

@include{DocCopyright.lpdoc}
").

:- doc(module,"@apl{ciaosh} is the Ciao interactive top-level shell.
It provides the user with an interactive programming environment with tools
for incrementally building programs, debugging programs by following
their executions, and modifying parts of programs without having to
start again from scratch.  If available, it is strongly recommended to
use it with the @concept{emacs interface} provided, as it greatly
simplifies the operation.  This chapter documents general operation in
the shell itself.

@comment{Subsequent chapters document the relevant exported
predicates of the modules which provide functionalities to the shell.}

@section{Shell invocation and startup}

When invoked, the shell responds with a message of identification and
the prompt @tt{?- } as soon as it is ready to accept input.

When the shell is initialized it looks for a file @file{.ciaorc} in the
HOME directory and makes an @tt{include} of it, if it exists.  This file
is useful for including @decl{use_module/1} declarations for the modules
one wants to be loaded by default, changing @concept{prolog flag}s, etc.
(Note that the @file{.ciaorc} file can only contain directives, not
actual code; to load some code at startup put it in a separate file and
load it using e.g. a @decl{use_module/1} declaration.)  If the
initialization file does not exist, the default package
@lib{default_for_ciaosh} is included, to provide more or less what other
prologs define by default.  Thus, if you want to have available all
builtins you had before adding the initialization file, you have to
include @tt{:- use_package(default_for_ciaosh)} in it.  Two command-line
options control the loading of the initialization file:

@begin{description}

@item{@tt{-f}} Fast start, do not load any initialization file.

@item{@tt{-q}} Quiet, do not show banner.

@item{@tt{-i}} Force interactive even if @tt{stdin} is not a pseudo-terminal.

@item{@tt{-l} @var{File}} Look for initialization file @var{File}
  instead of @tt{~/.ciaorc}. If it does not exist, include the
  default package.

@item{@tt{-u} @var{File}} Use module @var{File} on startup.

@item{@tt{-p} @var{Prompt}} Set @var{Prompt} as prompt.

@item{@tt{-e} @var{Query}} Call @var{Query} on startup.

@end{description}

@section{Shell interaction}

After the shell outputs the prompt, it is expecting either an internal
command (see the following sections) or a @index{query} (a goal or
sequence of goals).  When typing in the input, which must be a valid
prolog term, if the term does not end in the first line, subsequent
lines are indented.  For example:

@begin{verbatim}
?- X =
   f(a,
   b).

X = f(a,b) ? 

yes
?- 
@end{verbatim}

The queries are executed by the shell as if they appeared in the
@concept{user module}.  Thus, in addition to builtin predicates, predicates
available to be executed directly are all predicates defined by loaded
user files (files with no module declaration), and imported predicates from
modules by the use of @tt{use_module}.

The possible answers of the shell, after executing an internal command or
query, are:

@begin{itemize}

@item If the execution failed (or produced an error), the answer is
      @tt{no}.

@item If the execution was successful and bindings where made (or
      constraints where imposed) on @concept{answer variable}s, then
      the shell outputs the values of answer variables, as a sequence
      of bindings (or constraints), and then prints a @tt{?} as a
      prompt.  At this point it is expecting an input line from the
      user.  By entering a carriage-return (@key{RET}) or any line
      starting with @tt{y}, the query terminates and the shell answer
      @tt{yes}.  Entering a `@tt{,}' the shell enters a
      @concept{recursive level} (see below).  Finally, any other
      answer forces the system to backtrack and look for the next
      solution (answering as with the first solution).

@item If the execution was successful, but no @concept{answer variable}
      was bound or constrained, the answer is simply @tt{yes}.  This
      behavior can be changed by setting the @concept{prolog flag}
      @tt{prompt_alternatives_no_bindings} to @tt{on}, so that if there
      are more solutions the user will be consulted as explained in the
      previous point (useful if the solutions produce side effects).

@end{itemize}

To allow using connection variables in queries without having to
report their results, variables whose name starts with @tt{_} are not
considered in answers, the rest being the @index{answer variable}s.
This example illustrates the previous points:

@begin{verbatim}
?- member(a, [b, c]).

no
?- member(a, [a, b]).

yes
?- member(X, [a|L]).

X = a ? ;

L = [X|_] ? 

yes
?- atom_codes(ciao, _C), member(L, _C).

L = 99 ? ;

L = 105 ? ;

L = 97 ? ;

L = 111 ? ;

no
?- 
@end{verbatim}

@section{Entering recursive (conjunctive) shell levels}

As stated before, when the user answers with `@tt{,}' after a solution
is presented, the shell enters a @index{recursive level}, changing its
prompt to @em{N} @tt{?- } (where @em{N} is the recursion level) and
keeping the bindings or constraints of the solution (this is inspired
by the @index{LogIn} language developed by @author{H. Ait-Kaci},
@author{P. Lincoln} and @author{Roger Nasr} @cite{E-overview}).  Thus,
the following queries will be executed within that context, and all
variables in the lower level solutions will be reported in subsequent
solutions at this level.  To exit a recursive level, input an
@key{EOF} character or the command @tt{up}.  The last solution after
entering the level is repeated, to allow asking for more solutions.
Use command @tt{top} to exit all recursive levels and return to the
top level.  Example interaction:

@begin{verbatim}
?- directory_files('.',_Fs), member(F,_Fs).

F = 'file_utils.po' ? ,

1 ?- file_property(F, mod_time(T)).

F = 'file_utils.po',
T = 923497679 ? 

yes
1 ?- up.

F = 'file_utils.po' ? ;

F = 'file_utils.pl' ? ;

F = 'file_utils.itf' ? ,

1 ?- file_property(F, mod_time(T)).

F = 'file_utils.itf',
T = 923497679 ? 

yes
1 ?- ^D
F = 'file_utils.itf' ? 

yes
?- 
@end{verbatim}
").

:- reexport(library(toplevel),
        [use_module/1, use_module/2, ensure_loaded/1,
	 make_exec/2,
         include/1, use_package/1, 
	 consult/1, compile/1, '.'/2
         ]).
:- use_module(library(libpaths), []).
:- reexport(library(compiler),
        [make_po/1, unload/1,
         set_debug_mode/1, set_nodebug_mode/1]).
:- reexport(library(compiler/exemaker),
        [make_actmod/2, force_lazy/1, undo_force_lazy/1,
         dynamic_search_path/1]).
:- reexport(library(compiler/c_itf),
        [multifile/1]).
:- use_module(library(debugger), []).

:- true pred use_module(Module) : sourcename
        # "Load into the top-level the module defined in
          @var{Module}, importing all the predicates it exports.".

:- true pred use_module(Module, Imports) : sourcename * list(predname)
        # "Load into the top-level the module defined in
          @var{Module}, importing the predicates in @var{Imports}.".

:- true pred ensure_loaded(File) : sourcenames
        # "Load into the top-level the code residing in file (or files)
           @var{File}, which is user (i.e. non-module) code.".

:- true pred include(File) : sourcename
        # "The contents of the file @var{File} are included in the
          top-level shell.  For the moment, it only works with some
          directives, which are interpreted by the shell, or with normal
          clauses (which are asserted), if library(dynamic) is loaded
          beforehand.".

:- true pred use_package(Package) : sourcenames
        # "Include the package or packages specified in @var{Package}.
          Most package contents can be handled in the top level, but
          there are currently still some limitations.".

:- true pred consult(File) : sourcenames
        # "Provided for backward compatibility.  Similar to
           @pred{ensure_loaded/1}, but ensuring each listed file is
           loaded in consult mode (see @ref{The interactive debugger}).".

:- true pred compile(File) : sourcenames
        # "Provided for backward compatibility.  Similar to
           @pred{ensure_loaded/1}, but ensuring each listed file is
           loaded in compile mode (see @ref{The interactive debugger}).".

:- true pred [File|Files] : sourcename * list(sourcename)
        # "Provided for backward compatibility, obsoleted by
          @pred{ensure_loaded/1}.".

:- true pred make_exec(File, ExecName)
%        :: atm(ExecName)
        : sourcenames(File) => atm(ExecName)
        # "Make a Ciao executable from file (or files) @var{File},
           giving it name @var{ExecName}.  If @var{ExecName} is a
           variable, the compiler will choose a default name for the
           executable and will bind the variable @var{ExecName} to that
           name.  The name is chosen as follows: if the main prolog file
           has no @tt{.pl} extension or we are in Windows, the
           executable will have extension @tt{.cpx}; else the executable
           will be named as the main prolog file without extension.".

:- true pred make_po(Files) : sourcenames
        # "Make object (@tt{.po}) files from @var{Files}. Equivalent to
          executing \"@tt{ciaoc -c}\" on the files.".

:- true pred unload(File) : sourcename
        # "Unloads dynamically loaded file @var{File}.".

:- true pred set_debug_mode(File) : sourcename
        # "Set the loading mode of @var{File} to @em{consult}. See
          @ref{The interactive debugger}.".

:- true pred set_nodebug_mode(File) : sourcename
        # "Set the loading mode of @var{File} to @em{compile}. See
          @ref{The interactive debugger}.".

:- true pred make_actmod(ModuleFile, PublishMod) : sourcename * atm
        # "Make an @concept{active module} executable from the module
          residing in @var{ModuleFile}, using address publish module of
          name @var{PublishMod} (which needs to be in the library paths).".

:- true pred force_lazy(Module) : atm
        # "Force module of name @var{Module} to be loaded lazily in the
          subsequent created executables.".

:- true pred undo_force_lazy(Module) :: atm
        # "Disable a previous @pred{force_lazy/1} on module @var{Module}
          (or, if it is uninstantiated, all previous @pred{force_lazy/1}).".

:- true pred dynamic_search_path(Name) : atm
        # "Asserting a fact to this data predicate, files using
          @concept{path alias} @var{Name} will be treated as dynamic in
          the subsequent created executables.".

:- true pred multifile(Pred) : predname
        # "Dynamically declare predicate @var{Pred} as multifile.  This
          is useful at the top-level shell to be able to call multifile
          predicates of loaded files.".

:- doc(doinclude, sourcenames/1).

:- doc(sourcenames/1, "Is defined as
           follows:@includedef{sourcenames/1} See @pred{sourcename/1}
           in @ref{Basic file/stream handling}").

:- true prop sourcenames(Files)
        # "@var{Files} is a source name or a list of source names.".

sourcenames(File) :- sourcename(File).
sourcenames(Files) :- list(Files, sourcename).
