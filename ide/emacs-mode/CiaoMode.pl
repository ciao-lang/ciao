% -----------------------------------------------------------
% Other comments, acknowledgments and changelog/version info 
% -----------------------------------------------------------

:- use_package([assertions]).

:- doc(filetype, documentation).

:- doc(title,"Using Ciao inside GNU emacs").
:- doc(subtitle,"An interactive program development environment for Ciao").

:- doc(subtitle_extra,"@bf{The Ciao System Documentation Series}").
:- doc(subtitle_extra,"Technical Report CLIP 4/00.5.81").
:- doc(subtitle_extra,"@em{Draft printed on:} @today{}").

:- doc(author,"Manuel Hermenegildo").
:- doc(author,"Manuel C. Rodriguez").
:- doc(author,"Daniel Cabeza").

:- include(ciao_docsrc(common/'ClipAddress')).
:- include(ciao_docsrc(common/'Copyright')).

:- doc(summary,"This documents the Ciao emacs interface (or @em{mode}
    in @apl{emacs} terms), which provides a rich, integrated user interface
    to the Ciao program development environment components, including the
    @apl{ciaosh} interactive top level and the @apl{ciaopp}
    preprocessor. While most features of the Ciao development environment
    are available from the command line of the preprocessor and the
    top-level shell, using Ciao inside @apl{emacs} is highly recommended,
    since it greatly facilitates the process of editing, compiling,
    running, and debugging Ciao programs. 

    In particular, source-level
    debugging and location of errors in source files, syntax highlighting,
    automatic access to online help, and automatic version control are only
    available within the Ciao emacs mode.").

:- doc(module,"@include{CiaoMode.lpdoc}").


