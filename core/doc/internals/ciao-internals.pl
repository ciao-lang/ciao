:- use_package(assertions).

:- doc(filetype, application). % TODO: or 'documentation'?

:- doc(title,"The Ciao Internals").
:- doc(subtitle,"The Ciao Compiler, Engine, and Runtime libraries").

:- doc(logo, 'ciao-shadow-64h').

:- doc(subtitle_extra,"REFERENCE MANUAL").
:- doc(subtitle_extra,"@bf{The Ciao Documentation Series}").
:- doc(subtitle_extra,"@href{https://ciao-lang.org/}").
:- doc(subtitle_extra,"@em{Generated/Printed on:} @today{}").
%:- doc(subtitle_extra,"Technical Report CLIP 3/97-@version{}").

:- include(ciao_docsrc(common/'ClipAddress')).
:- include(ciao_docsrc(common/'Copyright')).

%% :- doc(bug,"Although the documentation is acceptable at this
%%    point, we are still really in beta mode in this regard.").

:- doc(bug,"This is still just a first shot...").

:- doc(summary,"

   @include{Warning.lpdoc}

   @includefact{this_manual/1}

   ").

:- doc(module,"

   @include{Warning.lpdoc}

   @includefact{this_manual/1}
").

this_manual("This is the internal manual for the Ciao compiler,
  engine, and runtime libraries. It documents modules not included in
  the reference manual.

  Please consult the @em{alldocs} bundle for the Ciao reference
  manual.").


