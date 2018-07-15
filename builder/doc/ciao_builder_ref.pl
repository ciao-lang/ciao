:- use_package(assertions).

:- doc(filetype, application). % TODO: or 'documentation'?

:- doc(title, "Build Automation for Ciao").

:- doc(author, "Ciao Developer Team").
:- doc(author, "Jose F. Morales").

% :- doc(logo, 'ciao-shadow-64h').

:- doc(subtitle_extra,"REFERENCE MANUAL").
:- doc(subtitle_extra,"@bf{The Ciao Documentation Series}").
:- doc(subtitle_extra,"@href{https://ciao-lang.org/}").
:- doc(subtitle_extra,"@em{Generated/Printed on:} @today{}").
%:- doc(subtitle_extra,"Technical Report CLIP 3/97-@version{}").

% :- include(ciao_docsrc(common/'ClipAddress')).

% :- include(ciao_docsrc(common/'Copyright')).

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

   @section{Bundles}

   Ciao @concept{bundle}s define structured collections of modules
   that are written with some common @em{global} assumptions in
   mind. Bundles include the necessary @em{meta-information} to
   specify their dependencies to other bundles, version information,
   @concept{path alias}es, configuration options, licensing, authorship,
   generated commands (executables), entry point for manuals,
   dependencies to third-party software, etc.

   Bundles consist on the source code and documentation (e.g., in
   LPdoc format), and a single @tt{Manifest.pl} file that describes
   all the necessary meta-information. Bundles can be distributed and
   installed separatelly, and they may depend and interact with other
   bundles.

   The support for bundles is implemented natively in Ciao and,
   compared with other ad-hoc solutions for other Prolog
   implementations (like defining @pred{file_search_path/2}
   dynamically), it is designed to ensure that analysis and
   incremental compilation produce consistent results.

   @section{Manifest files}

   A @tt{Manifest.pl} file includes the following meta-information:

   @begin{itemize}
   @item versions, authorship, etc.
   @item @concept{path alias}es
   @item dependencies to other bundles
   @item entry points for manuals
   @item custom rules for build, installation, clean
   @end{itemize}

   @section{Bundles vs. Packages}

   Ciao defines @concept{package}s as libraries that implement a
   language extension, which usually is comprises several modules and
   included files. Bundles correspond with the traditional concept of
   @em{software package}, @em{source project}, or @em{component}. In
   order to avoid confusion, we will avoid the term @em{package} as a
   collection of software.

   @section{Usage}

   The builder can be used in these forms:
   @begin{itemize}
   @item @tt{ciao}: the Ciao super-command (which manages bundles or
     starts a Ciao toplevel)
   @item @tt{ciao-boot.sh}: the bootstrap builder (from source)
   @item @tt{https://ciao-lang.org/boot}: network installation (using
   a @tt{curl} pipe to @tt{sh})
   @end{itemize}

   The network installater (for Unix) is delivered as an HTTPs
   redirection to
   @tt{https://raw.githubusercontent.com/ciao-lang/ciao/master/ciao-boot.sh}
   (which contains the latest version of @tt{ciao-boot.sh}
   script). The bootstrap script recognizes that it is being called
   without sources automatically. Examples:

@begin{verbatim}
curl https://ciao-lang.org/boot -sSfL | sh
curl https://ciao-lang.org/boot -sSfL | sh -s -- --prebuilt local-install
curl https://ciao-lang.org/boot -sSfL | sh -s -- --no-prebuilt get devenv
@end{verbatim}
").

% TODO: Network installation details probably do not belong here

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

