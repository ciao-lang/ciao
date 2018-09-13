:- use_package(assertions).

:- doc(filetype, application). % TODO: or 'documentation'?

:- doc(title, "Build Automation for Ciao").

:- doc(author, "The Ciao Developer Team").
:- doc(author, "Jose F. Morales").

% :- doc(logo, 'ciao-shadow-64h').

:- doc(subtitle_extra,"REFERENCE MANUAL").
:- doc(subtitle_extra,"@bf{The Ciao Documentation Series}").
:- doc(subtitle_extra,"@href{https://ciao-lang.org/}").
:- doc(subtitle_extra,"@em{Generated/Printed on:} @today{}").
%:- doc(subtitle_extra,"Technical Report CLIP 3/97-@version{}").

:- doc(summary, "@apl{ciao_builder} is a
   @href{https://en.wikipedia.org/wiki/Build_automation}{build
   automation} system for managing Ciao libraries and
   programs. @apl{ciao_builder} works on structured collections of
   code, dubbed as @concept{bundle}s.").

:- doc(module,"
   The Ciao builder manages collections of source code, implementing
   the automatic configuration, build, (un)installation, packing, and
   cleaning of collections of Ciao modules. It offers a simplified
   interface with other Ciao (compiler, documentation generator, etc.)
   and external tools (e.g., installation of third-party software for
   library bindings).

   This tool also offers all the necessary machinery to bootstrap the
   Ciao system from a reduced set of dependencies.

   See the Ciao reference manual for a description of
   @concept{bundle}s, @concept{workspace}s, and the basic usage of the
   builder tool.
").

%   @section{Bundles vs. Packages}
%
%   Ciao defines @concept{package}s as libraries that implement a
%   language extension, which usually is comprises several modules and
%   included files. Bundles correspond with the traditional concept of
%   @em{software package}, @em{source project}, or @em{component}. In
%   order to avoid confusion, we will avoid the term @em{package} as a
%   collection of software.

%   @section{Usage}
%
%   The builder can be used in these forms:
%   @begin{itemize}
%   @item @tt{ciao}: the Ciao super-command (which manages bundles or
%     starts a Ciao toplevel)
%   @item @tt{ciao-boot.sh}: the bootstrap builder (from source)
%   @item @tt{https://ciao-lang.org/boot}: network installation (using
%   a @tt{curl} pipe to @tt{sh})
%   @end{itemize}

%   @section{Packing Bundles}
%
%   A @index{pbundle}, or packaged bundle, is any of the possible
%   packaged (ready to be distributed) versions of a bundle.
%
%   A @concept{pbundle} may include source code, pre-compiled binaries
%   for specific architectures, and installers for different operating
%   systems. It may deliver a whole bundle, a combination of them, or
%   parts (e.g., just manuals).

% ---------------------------------------------------------------------------

:- include('CHANGELOG').

