:- use_package(assertions).

:- doc(filetype, application). % TODO: or 'documentation'?

:- doc(title, "Build Automation for Ciao").

:- doc(author, "Ciao Developer Team").
:- doc(author, "Jose F. Morales").

% :- doc(logo, 'ciao-shadow-64h').

:- doc(subtitle_extra,"REFERENCE MANUAL").
:- doc(subtitle_extra,"@bf{The Ciao Documentation Series}").
:- doc(subtitle_extra,"@href{http://ciao-lang.org/}").
:- doc(subtitle_extra,"@em{Generated/Printed on:} @today{}").
%:- doc(subtitle_extra,"Technical Report CLIP 3/97-@version{}").

% :- include(ciao_docsrc(common/'ClipAddress')).

% :- include(ciao_docsrc(common/'Copyright')).

:- doc(summary, "@apl{ciao_builder} is a
   @href{https://en.wikipedia.org/wiki/Build_automation}{build
   automation} system for building and packaging Ciao libraries and
   programs. @apl{ciao_builder} works on structured collections of
   code, dubbed as @concept{bundle}s.").

:- doc(module,"
   This is the main tool to manage Ciao source code, in order to
   automatize the configuration, build, (un)installation, packaging,
   and cleaning (removal of object files) of complex collections of
   code. 

   @section{Bundles}

   A @concept{bundle} is the term used in Ciao for a collection of
   related modules (@em{software package} or @em{component}, do not
   confuse with @concept{packages} as libraries). Bundles can be
   distributed and installed separatelly (and they may depend on other
   bundles).

   Modules allow programs to be separated and combined in a flexible
   way. However, modules alone are not enough to describe large
   libraries and applications. For example, many definitions
   concerning module alias paths, compilation options, documentation,
   licensing, authorship, etc. are usually global for a collection of
   modules. Moreover, applications often depend on external tools and
   data files whose dependencies cannot be easly specified.

   A @tt{bundle} (see @bf{note} below) is the equivalent in Ciao of
   both a @em{source project} and specification of a @em{software
   package}. It usually comprises:

   @begin{itemize}
   @item source code (as modules and packages)
   @item module alias paths
   @item dependencies to other bundles
   @item documentation (in LPdoc or other formats).
   @item custom code for build/installation
   @item at least one @tt{Manifest.pl} file that describes this
     @em{meta-information}
   @end{itemize}

   Declaring those definitions separately from the source code that is
   compiled is in general safer and easier than other lower-level
   solutions (like defining @pred{file_search_path/2} dynamically,
   which may lead to inconsistent incremental compilations if not done
   right).

   @bf{Note:} when talking about Ciao programs, we will avoid the term
   @em{package} as a collection of software, in order to avoid
   confusion with @em{packages} as language extensions for Ciao.

   @section{Packaged Bundles}

   A @index{pbundle}, or packaged bundle, is any of the possible
   packaged (ready to be distributed) versions of a bundle. You can
   think of them as the equivalent of @em{biological vectors} for
   bundles.

   A @concept{pbundle} may include source code, pre-compiled binaries
   for specific architectures, and installers for different operating
   systems. It may deliver a whole bundle, a combination of them, or
   parts (e.g., just manuals).
").


