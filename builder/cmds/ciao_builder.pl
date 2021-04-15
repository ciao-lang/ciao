:- module(_, [], [fsyntax, assertions, isomodes, regtypes, dcg]).

:- doc(title, "The standalone command-line builder").
:- doc(author, "Jose F. Morales").
:- doc(author, "The Ciao Development Team").

:- doc(module, "
@cindex{builder, standalone} @apl{ciao_builder} is the Ciao standalone
command-line builder. @apl{ciao_builder} can be used to manage Ciao
source code organized as @concept{bundle}s. 

This command is available to the user in two forms: wrapped around the
@tt{ciao-boot.sh} (or @tt{ciao-boot.bat} in Windows) scripts (see
@ref{Bootstrapping the build system}), or as part of the general
@apl{ciao} command-line tool.

@section{Introduction to the build process}

This picture shows a detailed view all the elements of the build and
installation process of a @concept{bundle} or collection of bundles.

@begin{verbatim}
    Phase 0-
       Bootstrapping the build system
    Phase 1-
       SOURCE ---(configure)--> CONFIG
    Phase 2- (needs CONFIG)
       SOURCE ---(build)--> BUILD --(install)--> INSTALLED
@end{verbatim}

Above, arrows specify actions and nodes are collections of files:

@begin{description}
@item{SOURCE} Source code
@item{CONFIG} Configuration of the source
@item{BUILD} Binaries, compiled libraries, and generated
  documentation (.pdf, .html, etc.)
@item{INSTALLED} Copies of BUILD that exclude temporary files (mostly
  caches for separate and incremental compilation).
@end{description}

The source distribution only contains SOURCE elements. The actions
@tt{build} and @tt{install} create the BUILD and INSTALLED elements.

NOTE: Bootstrapping is a transparent process and its details are not
needed to understand and use the build system. See @ref{Bootstrapping
the build system} for more details on @em{phase 0}.

NOTE: Out-of-tree builds (where all the intermediate compiler output,
including @tt{.po}/@tt{.itf} files are stored in the @tt{build/cache} directory)
are enabled by default for bundles. This can be disabled setting the
environment variable @tt{CIAOCCACHE=0}. Note that bootstrapping the
system with @tt{CIAOCCACHE=0} is not recommended, as it will merge
@tt{.po}/@tt{.itf} files of (potentially) incompatible compiler iterations.

@section{Configuration and build}

Configuration and build can be reverted with @em{clean} commands.  The
meaning of @tt{clean} and @tt{distclean} is based on their standard
meaning (see the @tt{Makefile} example at
@href{http://www.gnu.org/software/make/manual/html_node/Complex-Makefile.html}),
except for @tt{realclean} that cleans both the system and the
bootstrap (equivalent to @tt{distclean} and @tt{boot-clean}).

The following table summarizes the actions that @em{undo} each build
or install operation:

@begin{verbatim}
    ('undo' reverts the effect of each marked 'action') 
                  ,--------+-----------+-----------.
   actions \\\\ undo | clean  | distclean | realclean |
 .----------------+--------+-----------+-----------+
 | boot-build     |        |           |    x      |
 +----------------+--------+-----------+-----------+
 | configure      |        |     x     |    x      |
 +----------------+--------+-----------+-----------+
 | build          |   x    |     x     |    x      |
 `----------------+--------+-----------+-----------+
@end{verbatim}

@section{Installation}

Different installation areas are supported. For personal
installations, the installation area can overlap with the build
staging area, such that no extra space is necessary.

@begin{verbatim}

      .............         install             ...........
      .           .---------------------------->.         .
      .           .        (generated           .         .
      .............         files such          ...........
       Build Staging Area       as binaries)      Installation Area
           .^.                                 _
            | build                            /|
            |                                 /
      .............          install         /
      .           .-------------------------'
      .           .         (source files
      .............          such as examples,
       Source Code           images, etc.)
@end{verbatim}

The @tt{install} command is undone with the @tt{uninstall} command.

@section{Bootstrapping the build system}

The @tt{ciao-boot.sh} (or @tt{ciao-boot.bat} in Windows) automatically
bootstraps and invokes the Ciao build system. The process is described
below.

Bootstrapping is the process that compiles the Ciao compiler and
builder in an environment where no existing Ciao binary exists. We
follow the same @em{phase 1} and @em{phase 2} steps above but the
whole process is driven by a very simplified version written in
(portable) shell-script code (since there is no running Ciao) and
using the @tt{ciaoc} @em{bootstrap compiler}.

The @tt{ciaoc} bootstrap compiler comes in a pre-compiled bytecode
form. This bytecode, together with the engine parts written in C, can
be executed in most systems with a C compiler. 

Once this bootstrap compiler is available, the system compiles the
bootstrap @apl{ciao_builder}, which drives the rest of the build and
installation process.

This step is transparent to the user and separated in a different
build directory (@tt{build-boot/}). However, there are options to
force the recompilation and cleaning of that part (see
@tt{ciao-boot.sh} help for more information).").

% TODO: document that opts and flags may end in '--'
% TODO: move cmd_fmt and ciaocl_help together
% TODO: allow --target=Tgt for some commands? (e.g., custom_run)
% TODO: obtain bundle origin from catalog (if possible)

% ===========================================================================

:- use_module(library(errhandle), [default_error_message/1]).
:- use_module(library(messages), [error_message/2]).

:- use_module(ciaobld(builder_cmds), [builder_run/2]).

:- use_module(ciaobld(ciaocl_help)).
:- use_module(ciaobld(ciaocl_parser)).

% ===========================================================================

% Invocation from the command-line interface
:- export(main/1).
main(Args) :-
    catch(main_(Args), E, builder_error(E)),
    !.

builder_error(E) :- builder_error_message(E), halt(1).
    
main_([Help0|Args]) :-
    norm_underscores(Help0, Help),
    help_mode(Help, Level, Prof), 
    !,
    ( Args = [] ->
        show_help(Level, Prof)
    ; Args = [Arg0] ->
        norm_underscores(Arg0, Cmd),
        show_help_cmd(Cmd, Prof)
    ; fail
    ).
main_(Args) :-
    parse_cmd(Args, Cmd, Opts),
    builder_run(Cmd, Opts),
    post_message(Cmd).

help_mode('help', summary, normal).
help_mode('help_all', all, normal).
help_mode('help_boot', summary, boot).
help_mode('help_all_boot', all, boot).

% ---------------------------------------------------------------------------

:- use_module(ciaobld(messages_aux), [normal_message/2]).
:- use_module(library(system_extra), [using_tty/0]).

% Show a help message after the command (only when run from a TTY)
post_message(Cmd) :-
    ( using_tty ->
        show_post_message(Cmd)
    ; true
    ).

show_post_message(cmd_on_set(configure(_), _)) :- !,
    normal_message(
"
Please check that all the configuration values above (if any) are
correct. If not, you can change or customize the configuration using
the command line or --interactive configure flag.

To continue the installation, execute 'build' and 'install' commands.", []).
show_post_message(_).

% ===========================================================================
:- doc(section, "Error messages").

builder_error_message(error_msg(Format, Args)) :- !,
    error_message(Format, Args).
builder_error_message(not_in_builder_boot(Cmd)) :- !,
    error_message("Command '~w' only available in 'ciao-boot.sh' or 'ciao-boot.bat'.~n", [Cmd]).
builder_error_message(unknown_target(Target)) :- !,
    error_message(
% ...........................................................................
"'~w' does not look like a valid builder target.~n"||
"~n"||
"It does not correspond to a known bundle name, a path to a bundle, or a path~n"||
"to a workspace containing bundles.~n"||
"~n"||
"Some possible reasons: the target is not reachable from CIAOROOT or CIAOPATH,~n"||
"or it does not contain the ACTIVATE mark (for catalogues), or the bundle~n"||
"sources are incomplete (no valid Manifest.pl?).", [Target]).
builder_error_message(unknown_bundle(Bundle)) :- !,
    error_message("'~w' is not a known bundle.~n", [Bundle]).
builder_error_message(unknown_cmd(Cmd)) :- !,
    error_message("Unknown command '~w'.~n", [Cmd]).
builder_error_message(builder_cmd_failed(Bundle, '', Target)) :- !,
    error_message("Command '~w' on bundle '~w' failed.~n", [Target, Bundle]).
builder_error_message(builder_cmd_failed(Bundle, Part, Target)) :- !,
    error_message("Command '~w' on bundle '~w' (part '~w') failed.~n", [Target, Bundle, Part]).
builder_error_message(E) :-
    default_error_message(E).


