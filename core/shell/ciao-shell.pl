:- module('ciao-shell', [], [assertions]).

:- doc(title,"The script interpreter").
:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Jose F. Morales (minor)").

:- doc(module,"@apl{ciao-shell} is the Ciao script interpreter. It can
   be used to write @index{Prolog shell scripts} (see
   @cite{sicstus-scripts-complangprolog-www}), that is, executable
   files containing source code, which are compiled on demand.

   Writing Prolog scripts can sometimes be advantageous with respect
   to creating binary executables for small- to medium-sized programs
   that are modified often and perform relatively simple tasks. The
   advantage is that no explicit compilation is necessary, and thus
   changes and updates to the program imply only editing the source
   file. The disadvantage is that startup of the script (the first
   time after it is modified) is slower than for an application that
   has been compiled previously.

   @section{How it works}

   Essentially, @tt{ciao-shell} is a smaller version of the Ciao
   top-level, which starts by loading the file given to it as the
   first argument and then starts execution at @pred{main/1} (the
   argument is instantiated to a list containing the command line
   options, in the usual way).  Note that the Prolog script cannot
   have a @tt{module} declaration for this to work.  While loading the
   file, @tt{ciao-shell} changes the @concept{prolog flag} @tt{quiet}
   so that no informational or warning messages are printed (error
   messages will be reported to @tt{user_error}, however).  The
   operation of @tt{ciao-shell} in Unix-like systems is based in a
   special compiler feature: when the first character of a file is
   '@tt{#}', the compiler skips the first lines until an empty line is
   found.  In Windows, its use is as easy as naming the file with a
   @tt{.pls} extension, which will launch @tt{ciao-shell}
   appropriately.

   For example, in a Linux/Unix system, assume a file called
   @file{hello} contains the following program:

@begin{verbatim}
#!/usr/bin/env ciao-shell
% -*- mode: ciao; -*-

main(_) :-
     write('Hello world'), nl.
@end{verbatim}

   Then, the file @file{hello} can be @em{run} by simply making it
   executable and invoking it from the command line:

@begin{verbatim}
$ chmod +x hello
$ ./hello
Hello world
@end{verbatim}
  
   The lines:
@begin{verbatim}
#!/usr/bin/env ciao-shell
% -*- mode: ciao; -*-
@end{verbatim}

   @noindent invokes @apl{ciao-shell} through @tt{/usr/bin/env}
   (POSIX.2 compliant), instructing it to read this same file, and
   passing it the rest of the arguments to @tt{hello} as arguments to
   the Prolog program. The second line @tt{% -*- mode: ciao; -*-} is
   simply a comment which is seen by @apl{emacs} and instructs it to
   edit this file in Ciao mode (this is needed because these script
   files typically do not have a @tt{.pl} ending). When
   @apl{ciao-shell} starts, if it is the first time, it compiles the
   program (skipping the first lines, as explained above), or else at
   successive runs loads the @tt{.po} object file, and then calls
   @pred{main/1}.

   Note that the process of creating Prolog scripts is made very
   simple by the Ciao @concept{emacs mode}, which automatically
   inserts the header and makes the file executable (See @ref{Using
   Ciao inside GNU emacs}).

   @section{Command line arguments in scripts}

   @noindent The following example illustrates the use of command-line
   arguments in scripts. Assume that a file called @tt{say} contains
   the following lines:

@begin{verbatim}
#!/usr/bin/env ciao-shell
% -*- mode: ciao; -*-

main(Argv) :-
     write_list(Argv), nl.

write_list([]).
write_list([Arg|Args]) :- 
     write(Arg),
     write(' '),
     write_list(Args).
@end{verbatim}

   @noindent An example of use is:

@begin{verbatim}
$ say hello dolly
hello dolly 
@end{verbatim}
").

:- use_module(engine(io_basic)).
:- use_module(library(libpaths), [get_alias_path/0]).
:- use_module(library(errhandle), [error_protect/2]).
:- use_module(engine(runtime_control), [current_prolog_flag/2, set_prolog_flag/2]).

:- if(defined(optim_comp)).
:- use_module(compiler(dynload), [ensure_loaded/1
%   ,set_debug_mode/1 % TODO: still not working...
    ]).
:- use_module(compiler(all_actions), []). % Include all compile capabilities
:- else.
:- use_module(library(compiler), [ensure_loaded/1, set_debug_mode/1]).
:- endif.

% TODO: do not force user:main/1 in executables, so that ciao-shell
%   can call (see internals:run_main_entry/0) without a "predicate
%   user:main/1 from this executable is being redefined" warning.
:- import(user, [main/1]).

:- export(main/0).
main :-
    get_alias_path, % TODO: needed? (check if it is working in optim_comp)
    current_prolog_flag(argv, Args),
    set_prolog_flag(quiet, warning),
    interpret_args(Args).

:- if(defined(optim_comp)).
interpret_args(['-i',File | Rest]) :- !,
    % set_debug_mode(File), % TODO: still not working...
    load_and_call(File,Rest).
:- else.
interpret_args(['-i',File | Rest]) :- !,
    set_debug_mode(File),
    load_and_call(File,Rest).
:- endif.
interpret_args([File | Rest]) :- !,
    load_and_call(File,Rest).
interpret_args(_) :-
    display('Usage: ciao-shell <source>'), nl,
    nl,
    display('Execute a Ciao script'), nl,
    halt(1).

load_and_call(File,Rest) :-
    ensure_loaded(File),
    set_prolog_flag(quiet, off),
    error_protect(main(Rest), fail). % TODO: fail or abort?
