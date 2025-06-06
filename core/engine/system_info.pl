:- module(system_info, [
    get_arch/1,
    get_os/1,
    get_platform/1,
    eng_debug_level/1,
    eng_is_sharedlib/0,
    get_ciao_ext/1,
    get_exec_ext/1,
    get_so_ext/1,
    get_a_ext/1,
    ciao_c_headers_dir/1,
    eng_supports/1
    ],
    [assertions, nortchecks, isomodes]).

:- doc(title, "Runtime system information").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Carro").
:- doc(author,"Jose F. Morales").

:- doc(module, "This module provides internal information about the
   current running runtime system (engine and enviroment). That
   information includes the architecture, platform, operating system,
   location of libraries, and C header files. That information is
   mainly used in parts of the Ciao dynamic compilation (location of
   source, generation of gluecode for the foreign interface, etc.).").

% TODO: GCC/clang compilation options should appear here too.

% ---------------------------------------------------------------------------

:- trust pred get_arch(?ArchDescriptor) => atm #
    "Unifies @var{ArchDescriptor} with a simple atom which describes
     the computer architecture currently executing the predicate.".

:- doc(get_arch/1,
    "This predicate will describe the computer architecture wich
     is currently executing the predicate.

     Computer architectures are identified by a simple atom.  This
     atom is implementation-defined, and may suffer any change
     from one Ciao version to another.

     For example, Ciao running on an 32-bit Intel-based machine 
     will retrieve:
@begin{verbatim}
?- get_arch(I).

I = i686 ? ;

no
?- 
@end{verbatim}
    ").

:- impl_defined(get_arch/1).

% ---------------------------------------------------------------------------

:- trust pred get_os(?OsDescriptor) => atm #
    "Unifies @var{OsDescriptor} with a simple atom which describes
     the running operating system when predicate was called.".
:- impl_defined(get_os/1).

% TODO: This would be the perfect place to enumerate all supported OS,
%       platforms, etc.

:- doc(get_os/1,
    "This predicate will describe the operating system which 
     is running on the machine currently executing the Prolog program.

     Operating systems are identified by a simple atom.  This atom
     is implementation-defined, and may suffer changes from one
     Ciao version to another.

     For example, Ciao running on Linux will retrieve:
@begin{verbatim}
?- get_os(I).

I = 'LINUX' ? ;

no
?- 
@end{verbatim}
    ").

:- trust pred get_platform(?Platform) => atm # "@var{Platform} is the
    atom describing the current operating system and computer
    architecture.".

get_platform(Platform) :-
    get_os(Os),
    get_arch(Arch),
    atom_concat(Os, Arch, Platform).

:- trust pred eng_debug_level(?Debug) => atm # "Unifies @var{Debug}
   with the value of @tt{core:debug_level} configuration flag used to
   build this engine.".

:- impl_defined(eng_debug_level/1).

% TODO: What is extension when there is no extension?
:- trust pred get_ciao_ext(?Ext) => atm # "@var{Ext} is the
    default extension for the executable Ciao programs.".
:- impl_defined(get_ciao_ext/1).

% TODO: What is extension when there is no extension?
:- trust pred get_exec_ext(?Ext) => atm # "@var{Ext} is the extension
    for executables.".
:- impl_defined(get_exec_ext/1).

:- trust pred get_so_ext(?Ext) :: atm # "@var{Ext} is the default
    extension for the shared libraries. For example, @tt{.dll} in
    Windows and @tt{.so} in most Unix systems.".
:- impl_defined(get_so_ext/1).

:- trust pred get_a_ext(?Ext) :: atm # "@var{Ext} is the default
    extension for the static libraries.".

get_a_ext('.a'). % TODO: '.a' is fine for MinGW but this is typically '.lib' in Windows

:- trust pred eng_is_sharedlib/0 # "This engine is linked as a shared
   library (instead of as an executable)".

:- impl_defined(eng_is_sharedlib/0).

% ---------------------------------------------------------------------------

:- trust pred ciao_c_headers_dir(Path) => atm(Path) #
    "@var{Path} is the path to the root of the installed Ciao
    header C files (.h), typically used for interfacing Ciao and
    C.".
:- impl_defined(ciao_c_headers_dir/1).

% ---------------------------------------------------------------------------

:- trust pred eng_supports(?Feature) => atm # "@var{Feature} is
   supported in the current engine and architecture.".

eng_supports(timeouts) :- 
    ( get_arch(Arch), arch_is_wasm(Arch) -> % TODO: obtain from build, some Emscripten options may enable them in the future
        fail
    ; true
    ).
eng_supports(processes) :-
    ( get_arch(Arch), arch_is_wasm(Arch) -> % TODO: obtain from build, some Emscripten options may enable them in the future
        fail
    ; true
    ).

arch_is_wasm(wasm32).
arch_is_wasm(wasm64).
arch_is_wasm(wasm32p64).
