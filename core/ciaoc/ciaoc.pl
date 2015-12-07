:- module(ciaoc, [main/1], [assertions, hiord, dcg, optparse, nortchecks]).

:- use_module(engine(internals),        ['$bootversion'/0]).
:- use_module(library(libpaths),        [get_alias_path/0]).
:- use_module(library(compiler),        [make_po/1, make_wam/1, use_module/3]).
:- use_module(library(compiler/c_itf), [opt_suffix/2]).
:- use_module(library(read_from_string), [read_from_atom/2]).
:- use_module(library(compiler/global_module_options)).

:- use_module(library(compiler/exemaker),
	    [make_exec/2, make_actmod/2, force_lazy/1, dynamic_search_path/1]).

:- doc(title,  "The standalone command-line compiler").
:- doc(author, "Daniel Cabeza").
:- doc(author, "Edison Mera").
:- doc(author, "The CLIP Group").

:- doc(copyright, "
Copyright @copyright{} 1996-2002 Daniel Cabeza/The CLIP Group.

@include{DocCopyright.lpdoc}
").

:- doc(module, "@cindex{compiling, from command line}
@cindex{compiler, standalone} @apl{ciaoc} @cite{ciaoc-entcs} is the
Ciao standalone command-line compiler.  @apl{ciaoc} can be used to
create executables or to compile individual files to object code (to
be later linked with other files).  @apl{ciaoc} is specially useful
when working from the command line. Also, it can be called to compile
Ciao programs from other tools such as, e.g., @concept{shell scripts},
@file{Makefile}s, or @concept{project files}. All the capabilities of
@apl{ciaoc} are also available from the interactive top-level shell,
which uses the @apl{ciaoc} modules as its components. 

@section{Introduction to building executables}

An @index{executable} can be built from a single file or from a
collection of inter-related files. In the case of only one file, this
file must define the predicate @pred{main/0} or @pred{main/1}. This
predicate is the one which will be called when the executable is
started. As an example, consider the following file, called
@tt{hello.pl}:

@begin{verbatim}
main :-
     write('Hello world'), 
     nl.
@end{verbatim}

@noindent To compile it from the command line using the @apl{ciaoc}
standalone compiler it suffices to type ``@tt{ciaoc hello}'' (in Win32
you may have to put the complete path to the @file{ciaoc} folder of
the Ciao distribution, where the installation process leaves a
@file{ciaoc.bat} file):

@begin{verbatim}
$ ciaoc hello
@end{verbatim}

@noindent This produces an executable called @tt{hello} in Unix
systems and @tt{hello.cpx} under Win32 systems.  This executable can
then be run in Win32 by double-clicking on it and on Unix systems by
simply typing its name (see for @ref{Running executables from the
command line} for how to run executables from the command line in
Win32):

@begin{verbatim}
$ ./hello
Hello world
@end{verbatim}

If the application is composed of several files the process is
identical. Assume @tt{hello.pl} is now:

@begin{verbatim}
:- use_module(aux, [p/1]).

main :-
     p(X),
     write(X), 
     nl.
@end{verbatim}

@noindent
where the file @tt{aux.pl} contains:

@begin{verbatim}
:- module(aux,[p/1]).

p('Hello world').
@end{verbatim}

@noindent This can again be compiled using the @apl{ciaoc} standalone
compiler as before:

@begin{verbatim}
$ ciaoc hello
$ ./hello
Hello world
@end{verbatim}

@noindent The invocation of @tt{ciaoc hello} compiles the file
@tt{hello.pl} and all connected files that may need recompilation --
in this case the file @tt{aux.pl}. Also, if any library files used had
not been compiled previously they would be compiled at this point (See
@ref{Intermediate files in the compilation process}). Also, if, say,
@tt{hello.pl} is changed and recompiled, the object code resulting
from the previous compilation of @tt{aux.pl} will be reused.  This is
all done without any need for @tt{Makefile}s, and considerably
accelerates the development process for large applications. This
process can be observed by selecting the @tt{-v} option when invoking
@tt{ciaoc} (which is equivalent to setting the
@tt{verbose_compilation} Prolog flag to @tt{on} in the top-level
interpreter).

If @pred{main/1} is defined instead of @pred{main/0} then when the
executable is started the argument of @pred{main/1} will be
instantiated to a list of atoms, each one of them corresponding to a
command line option. Consider the file @tt{say.pl}:

@begin{verbatim}
main(Argv) :-
     write_list(Argv), nl.

write_list([]).
write_list([Arg|Args]) :- 
     write(Arg),
     write(' '),
     write_list(Args).
@end{verbatim}

@noindent Compiling this program and running it results in the
following output:

@begin{verbatim}
$ ciaoc say
$ ./say hello dolly
hello dolly 
@end{verbatim}

The name of the generated executable can be controlled with the
@tt{-o} option (See @ref{Usage (ciaoc)}).

@section{Paths used by the compiler during compilation}

The compiler will look for files mentioned in commands such as
@decl{use_module/1} or @decl{ensure_loaded/1} in the current
directory.  Other paths can be added by including them in a file whose
name is given to @tt{ciaoc} using the @tt{-u} option. This file should
contain facts of the predicates @pred{file_search_path/2} and
@pred{library_directory/1} (see the documentation for these predicates
and also @ref{Customizing library paths and path aliases}
for details).

@section{Running executables from the command line}
@cindex{executables, how to run} 

As mentioned before, what the @tt{ciaoc} compiler generates and how it
is started varies somewhat from OS to OS.  In general, the product of
compiling an application with @tt{ciaoc} is a file that contains the
bytecode (the product of the compilation) and invokes the
@concept{Ciao engine} on it.

@begin{itemize} 

@item In Unix this is a @em{script} (see the first lines of the file)
      which invokes the ciao engine on this file. To run the generated
      executable from a Unix shell it suffices to type its name at the
      shell command line, as in the examples above.

@item In a Win32 system, the compiler produces a similar file with a
      @tt{.cpx} ending and an additional @tt{.bat} file.

      The Ciao installation process typically makes sure that the
      Windows registry contains the right entries so that @tt{.cpx}
      executables will run upon double-clicking and from a command
      shell (in NT systems).

      The @tt{.bat} files allow running the Ciao executable from any
      other processes (which typically does not use the Windows
      registry).

      Finally, in a system in which Cygwin is installed, executables
      can also be used directly from the @apl{bash} shell command
      line, without any associated @tt{.bat} files, by simply typing
      their name at the @apl{bash} shell command line, in the same way
      as in Unix.

@end{itemize}

Except for a couple of header lines, the contents of executables are
almost identical under different OSs (except for self-contained ones).
The bytecode they contain is architecture-independent.  In fact, it
is possible to create an executable under Unix and run it on Windows
or viceversa, by making only minor modifications (e.g., creating the 
@tt{.bat} file and/or setting environment variables or editing the
start of the file to point to the correct engine location).

@section{Types of executables generated}

@cindex{executables, types}

While the default options used by @apl{ciaoc} are sufficient for
normal use, by selecting other options @apl{ciaoc} can generate
several different types of executables, which offer interesting
tradeoffs among size of the generated executable, portability, and
startup time @cite{ciaoc-entcs}:

@begin{description}

@item{Dynamic executables:} @cindex{executables, dynamic}

  @apl{ciaoc} produces by default @em{dynamic} executables. In this
  case the executable produced is a @concept{platform-independent}
  file which includes in compiled form all the user defined files. On
  the other hand, any system libraries used by the application are
  loaded dynamically at startup. More precisely, any files that appear
  as @tt{library(...)} in @decl{use_module/1} and
  @decl{ensure_loaded/1} declarations will not be included explicitly
  in the executable and will instead be loaded dynamically.  Is is also
  possible to mark other @concept{path aliases} (see the documentation
  for @pred{file_search_path/2}) for dynamic loading by using the
  @tt{-d} option. Files accessed through such aliases will also be
  loaded dynamically.

  Dynamic loading allows making smaller executables. Such executables
  may be used directly in the same machine in which they were
  compiled, since suitable paths to the location of the libraries will
  be included as default in the executable by @apl{ciaoc} during
  compilation.

  The executable can also be used in another machine, even if the
  architecture and OS are different. The requirement is that the Ciao
  libraries (which will also include the appropriate @concept{Ciao
  engine} for that architecture and OS) be installed in the target
  machine, and that environment variables are set appropriately for
  the executable to be able to find them (see @ref{Environment
  variables used by Ciao executables}). How to do this differs
  slightly from OS to OS.

@item{Static executables:} @cindex{executables, static}

  Selecting the @tt{-s} option @tt{ciaoc} produces a @em{static}
  executable. In this case the executable produced (again a
  @concept{platform-independent} file) will include in it all the
  auxiliary files and any system libraries needed by the
  application. Thus, such an executable is almost complete, needing in
  order to run only the @concept{Ciao engine}, which is
  platform-specific.@footnote{Currently there is an exception to this
  related to libraries which are written in languages other than
  Prolog, as, e.g., C. C files are currently always compiled to
  dynamically loadable object files (@tt{.so} files), and they thus
  need to be included manually in a distribution of an
  application. This will be automated in upcoming versions of the Ciao
  system.} Again, if the executable is run in the same machine in
  which it was compiled then the engine is found automatically. If the
  executable is moved to another machine, the executable only needs
  access to a suitable engine (which can be done by setting the
  appropriate environment variables, see @ref{Environment variables
  used by Ciao executables}).

  This type of compilation produces larger executables, but has the
  advantage that these executables can be installed and run in a
  different machine, with different architecture and OS, even if Ciao
  is not installed on that machine. To install (or distribute) such an
  executable, one only needs to copy the executable file itself and
  the appropriate engine for the target platform (See @ref{Installing
  Ciao from the source distribution} or @ref{Installing Ciao from a
  Win32 binary distribution} and @ref{Multiarchitecture installation}), and
  to set things so that the executable can find the
  engine. @footnote{It is also possible to produce real standalone
  executables, i.e., executables that do not need to have an engine
  around. However, this is not automated yet, although it is planned
  for an upcoming version of the compiler. In particular, the compiler
  can generate a @tt{.c} file for each @tt{.pl} file. Then all the
  @tt{.c} files can be compiled together into a real executable (the
  engine is added one more element during link time) producing a
  complete executable for a given architecture. The downside of course
  is that such an executable will not be portable to other
  architectures without recompilation.}

@item{Dynamic executables, with lazy loading:} @cindex{executables, lazy load}

  Selecting the @tt{-l} option is very similar to the case of dynamic
  executables above, except that the code in the library modules is
  not loaded when the program is started but rather it is done during
  execution, the first time a predicate defined in that file is
  called. This is advantageous if a large application is composed of
  many parts but is such that typically only some of the parts are
  used in each invocation. The Ciao preprocessor, @apl{ciaopp}, is a
  good example of this: it has many capabilitites but typically only
  some of them are used in a given session. An executable with lazy
  load has the advantage that it starts fast, loading a minimal
  functionality on startup, and then loads the different modules
  automatically as needed.

@item{Self-contained executables:} @cindex{executables, self-contained} 

  @em{Self-contained} executables are static executables (i.e., this
  option also implies @em{static} compilation) which include a Ciao
  engine along with the bytecode, so they do not depend on an external
  one for their execution.  This is useful to create executables which
  run even if the machine where the program is to be executed does not
  have a Ciao engine installed and/or libraries. The disadvantage is
  that such execuatbles are @concept{platform-dependent} (as well as
  larger than those that simply use an external library).  This type
  of compilation is selected with the @tt{-S}
  option. Cross-compilation is also possible with the @tt{-SS} option,
  so you can specify the target OS and architecture.
  To be able to use the latter option, it is necessary to have
  installed a @tt{ciaoengine} for the target machine in the Ciao library
  (this requires compiling the engine in that OS/architecture and
  installing it, so that it is available in the library). 

@item{Compressed executables:} @cindex{executables, compressed}

  In @em{compressed} executables the bytecode is compressed. This
  allows producing smaller executables, at the cost of a slightly
  slower startup time. This is selected with the @tt{-z} option. You
  can also produce compressed libraries if you use @tt{-zl} along with
  the @tt{-c} option.  If you select @tt{-zl} while generating an
  executable, any library which is compiled to accomplish this will be
  also compressed.

@item{Active modules:} @cindex{modules, active} 

  The compiler can also compile (via the @tt{-a} option) a given file
  into an @index{active module} (see
  @ref{Active modules (high-level distributed execution)}
  for a description of this).  

  @comment{The way the ... using address publish module of
  name @var{PublishMod} (which needs to be in the library paths).}

@end{description}

@section{Intermediate files in the compilation process}

Compiling an individual source (i.e., @tt{.pl}) file produces a
@tt{.itf} file and a @tt{.po} file. The @tt{.itf} file contains
information of the @index{modular interface} of the file, such as
information on exported and imported predicates and on the other
modules used by this module. This information is used to know if a
given file should be recompiled at a given point in time and also to
be able to detect more errors statically including undefined
predicates, mismatches on predicate charaterictics across modules,
etc. The @tt{.po} file contains the platform-independent object code
for a file, ready for linking (statically or dynamically).

It is also possible to use @tt{ciaoc} to explicitly generate the
@tt{.po} file for one or more @tt{.pl} files by using the @tt{-c}
option.

If you want to view the wam instructions of one or more @tt{.pl} files
you can use the @tt{-w} option.  That will generate a @tt{.wam} file
with such instructions in a pretty format per each @tt{.pl} file.

@section{Usage (ciaoc)}

The following provides details on the different command line options
available when invoking @apl{ciaoc}:

@sp{2}

@begin{verbatim}
@includefact{usage_message/1}
@end{verbatim}
").


:- multifile library_directory/1.
:- dynamic library_directory/1.

main(Args) :-
	intercept((get_alias_path, parse_args(Args)),
	    compilation_error,
	    halt(1)).

% ---------------------------------------------------------------------------
% Specify here the default action (if no options are given)
% :- default_action(Action, Args).
% ---------------------------------------------------------------------------

:- default_action((verbose_version, make_exec(Args, _ExecName)), Args).

% ---------------------------------------------------------------------------
% Specify here simple options
% :- simple_option(Option, Action, Help, Terminates, Args0, Args).
% NOTE: simple_option/6 is expanded as exec_simple_option/4.
% ---------------------------------------------------------------------------

:- simple_option(['-h', '--help'],
    usage,
    "Show this help.",
    finished, Args, Args).
%
:- simple_option('-u',
    use_module(CFile, all, c_itf_internal),
    "Use <file> for compilation, often used to include LibDir paths, etc.",
    continue, [CFile|Args], Args).
%
:- simple_option('-op',
    opt_suffix(_, Suff),
    "Use <suffix> as the suffix for optimized (or otherwise tuned) code",
    continue, [Suff|Args], Args).
%
:- simple_option('-L',
    asserta_fact(library_directory(Dir)),
    "Look for libraries also in the <LibDir> directory",
    continue, [Dir|Args], Args).
%
:- simple_option('-c',
    ( verbose_version, make_po(Args) ),
    "Compile listed files (make .po objects)",
    finished, Args, []).
%
:- simple_option('-w',
    ( verbose_version, make_wam(Args) ),
    "Generate WAM code of listed files (in .wam files).",
    finished, Args, []).
%
:- simple_option('-S',
    ( set_prolog_flag(executables, static),
      get_platform(Target),
      set_prolog_flag(self_contained, Target) ),
    "Make standalone executable for the current OS and architecture, implies -s",
    continue, Args, Args).
%
:- simple_option('-SS',
    ( set_prolog_flag(executables, static),
      set_prolog_flag(self_contained, Target) ),
    "Make standalone executable for <target> OS and architecture \n\t" ||
    "(see ciao_sysconf for valid values for <target>), implies -s",
    continue, [Target|Args], Args).
%
:- simple_option('-ll',
    ( set_prolog_flag(executables, lazyload),
      force_lazy(Module) ),
    "Force <module> to be loaded lazily,  implies -l",
    continue, [Module|Args], Args).
%
:- simple_option('-ac',
    ( read_from_atom(P0, P),
      glbmod_add_package(_, P) ),
    "All the modules will be compiled using <Packages>",
    continue, [P0|Args], Args).
%
:- simple_option('-acm',
    ( read_from_atom(P0, P),
      glbmod_add_package(M, P) ),
    "<Modules> will be compiled using <Packages>",
    continue, [M, P0|Args], Args).
%
:- simple_option('-d',
    assertz_fact(dynamic_search_path(Path)),
    "Files using this path alias are dynamic (default: library)",
    continue, [Path|Args],  Args).
%
% TODO: Fix option parsing (do not use a difflist)
:- simple_option('-o',
    ( verbose_version, make_exec([File|Files], ExecName) ),
    "Make an executable from the listed files.",
    finished, [ExecName, File|Files], []).
%
:- simple_option('-a',
    ( verbose_version, make_actmod(Module, PublishMod) ),
    "Make an active module",
    finished, [PublishMod, Module], []).

% ---------------------------------------------------------------------------
% Specify here flag based options
% :- flag_based_option(FlagName, FlagValue, Option, Help).
% NOTE: flag_based_option/4 is expanded as flag_option/3.
% ---------------------------------------------------------------------------

% TODO: Flags domain duplicated here (see flag problem in */Manifest/*.config.pl)
% TODO: Some flags here are global options that cannot be safely selected on a module bases

:- flag_based_option(verbose_compilation, on, '-v',
    "Verbose mode").
%
:- flag_based_option(itf_format, r, '-ri',
    "Generate human readable .itf files").
%
:- flag_based_option(check_libraries, on, '-x',
    "Incremental compilation in the Ciao standard library (for developers)").
%
:- flag_based_option(executables, static, '-s',
    "Make a static executable (otherwise dynamic files are not included)").
%
:- flag_based_option(compress_exec, yes, '-z',
    "Generate executables with compressed bytecode").
%
:- flag_based_option(compress_lib, yes, '-zl',
    "Generate libraries with compressed bytecode - any library (re)compiled as \n\t"||
    "consequence of normal executable compilation will also be affected").
%
% NOTE: -e is redundant since it is already the default value -- EMM
% :- flag_based_option(executables, eagerload, '-e',
% "make executable with eager load of dynamic files at startup (default)").
%
:- flag_based_option(executables, lazyload, '-l',
    "Idem with lazy load of dynamic files (except insecure cases)").
%
% TODO: Is this necessary?
:- flag_based_option(use_global_module_options, no, '-np',
    "Ignore global module options").
%
:- flag_based_option(read_assertions, no, '-na',
    "Do not read the assertions in the code").
%
:- flag_based_option(runtime_checks, yes, '-rc',
    "Generate code with runtime checks, requires to read assertions").
%
:- flag_based_option(rtchecks_trust, no, [],
    "Disable rtchecks for trust assertions").
%
:- flag_based_option(rtchecks_entry, no, [],
    "Disable rtchecks for entry assertions").
%
:- flag_based_option(rtchecks_exit, no, [],
    "Disable rtchecks for exit assertions").
%
:- flag_based_option(rtchecks_test, yes, [],
    "Enable rtchecks for test assertions (for debugging \n\t"||
    "purposes only, unittest library is recommended)").
%
:- flag_based_option(rtchecks_level, exports, [],
    "Use rtchecks only for external calls of the exported predicates").
%
:- flag_based_option(rtchecks_inline, yes, [],
    "Expand library predicates inline as far as possible").
%
:- flag_based_option(rtchecks_asrloc, no, [],
    "Do not use assertion locators in the error messages").
%
:- flag_based_option(rtchecks_predloc, no, [],
    "Do not use predicate locators in the error messages").
%
:- flag_based_option(rtchecks_namefmt, short, [],
    "Show the name of predicates and properties in a reduced format").
%
:- flag_based_option(rtchecks_callloc, no, [],
    "Do not show the stack of predicates that caused the failure").
%
:- flag_based_option(rtchecks_callloc, literal, [],
    "Show the stack of predicates that caused the failure. Instrument it \n\t"||
    "in the literal. This mode provides more information, because reports \n\t"||
    "also the literal in the body of the predicate").
%
:- flag_based_option(unused_pred_warnings, yes, [],
    "Show warnings about unused predicates.  Note that a predicate is \n\t"||
    "being used if it is exported, it appears in clause body of a \n\t"||
    "predicate being used, in a multifile predicate, in a predicate \n\t"||
    "used in :- initialization(...) or :- on_abort(...) \n\t"||
    "declarations, or if it is the meta-argument of a metapredicate.").

:- doc(bug, " Also if appears in the body of an assertion referred
to a predicate being used, but that is not implemented, because the
assertion reader is not included in the compiler yet -- EMM.").

verbose_version :-
	current_prolog_flag(verbose_compilation, on), !,
	'$bootversion'.
verbose_version.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOTE:  base_message/2 is expanded as usage/0 and collects
% the help for each option
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- base_message('$bootversion', "\
ciaoc <MiscOpts> <ExecOpts> [-o <execname>] <file> ...

  Make an executable from the listed files.  If there is
  more than one file, they must be non-module, and the
  first one must include the main predicate.  The -o
  option allows generating an arbitrary executable name.

ciaoc <MiscOpts> <ExecOpts> -a <publishmod> <module>

  Make an active module executable from <module> with
  address publish module <publishmod>.

ciaoc <MiscOpts> -c  <file> ...

  Compile listed files (make .po objects).

ciaoc <MiscOpts> -w  <file> ...

  Generate WAM code of listed files (in .wam files).

<MiscOpts> can be: [-v] [-ri] [-u <file>]  [-rc] [-op <suffix>] [-L <LibDir>]

<ExecOpts> can be: [-s|-S|-SS <target>|-z|-zl|-e|-l|(-ll <module>)*]
                   (-d <alias>)* [-x]

default extension for files is '.pl'
").
