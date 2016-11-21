:- module(lpdoc_ref_man, [], [assertions]).

:- doc(filetype, application).

:- doc(title, "The lpdoc Documentation Generator").
:- doc(subtitle, "An Automatic Documentation Generator for (C)LP Systems").

:- doc(logo, 'lpdoc-logo-128').

:- doc(subtitle_extra, "REFERENCE MANUAL").
:- doc(subtitle_extra, "@bf{The Ciao Documentation Series}").
:- doc(subtitle_extra, "@href{http://ciao-lang.org/}").
:- doc(subtitle_extra, "@em{Generated/Printed on:} @today{}").
:- doc(subtitle_extra, "Technical Report CLIP 5/97.1-@version{}").

% TODO: Replace 'credits' by 'editor'? (JFMC)
% TODO: In this case, the people here are also the authors
:- doc(credits, "@bf{Edited by:}").
:- doc(credits, "Manuel Hermenegildo").
:- doc(credits, "Jos@'{e} Francisco Morales").

% :- include(ciao_docsrc(common/'ClipAddress')).

:- doc(copyright, "Copyright @copyright{} 1996-2015 Manuel
Hermenegildo and Jos@'{e} Francisco Morales.

@include{DocCopyright.lpdoc}
").

:- doc(summary, "@include{README_LPDOC.lpdoc}").

:- doc(module, "@include{Intro.lpdoc}

@section{lpdoc usage}
The following provides the different command line options available
when invoking @apl{lpdoc}. This description is intended only for
advanced users which might like to use @apl{lpdoc} in custom
applications. Note that the normal way to use @apl{lpdoc} is by
setting parameters in an @file{SETTINGS} file (see @ref{Generating a
manual}).

@begin{alert}
TODO: command line options not available here; need
cooperation with lpmake
@end{alert}
").

% ===========================================================================

:- doc(version_maintenance,dir('../../Manifest')).
:- include('../CHANGELOG').

