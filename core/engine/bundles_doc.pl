:- use_package(assertions).
:- doc(filetype, documentation).

:- doc(title, "Bundles and workspaces").

:- doc(author,"Jose F. Morales").
:- doc(author,"The Ciao Development Team").

:- doc(summary, "Bundles extend the modularity of Ciao to include an
   additional abstraction layer that divides large programs into sets
   of modules.").

:- doc(module, "In the same way that modules can be seen as collections 
of predicates, Ciao @concept{bundle}s define collections of
modules. The visibility of bundles is determined by
@concept{workspace}s. A workspace is a directory that may contain
bundles, each of them in a separate directory with the same name
declared on its manifest. See @ref{Bundle management} for details
about creating and managing bundles.

@section{Bundles}

Bundles are usually organized as follows:

@begin{verbatim}
Manifest/        Metadata
cmds/            Modules implementing main/{0,1} (for CLI tools)
{src,lib,...}/   Source code (modules, packages, included files, etc.)
@end{verbatim}

Bundles include some @em{metadata} describing versions, dependencies,
(relative) @concept{path alias}es, entry points for manuals and
commands, additional pre-build, build, and installation rules,
etc. This metadata is separated from the actual code into a single 
@tt{Manifest.pl} file at the root of the bundle, or inside the
@tt{Manifest/} directory. A typical @tt{Manifest.pl} file has the 
following form:

@begin{verbatim}
:- bundle(<Name>).
version(<Version>).
depends([Dep1, ..., DepN]).
alias_paths([
    <Alias1> = <RelPath1>,
    ...
    <AliasN> = <RelPathN>
]).
lib(<RelPath>).
cmd(<CmdName>, [main=<RelPathMainMod>, ...]).
manual(<ManName>, [main=<RelPathDocCfg>, ...]).
@end{verbatim}

where:
@begin{itemize}
@item @tt{version/1} specifies the bundle version.
@item @tt{depends/1} declares dependencies with other bundles.
@item @tt{alias_paths/1} declares @concept{path alias}es.
@item (multiple) @tt{lib/1} declares directories containing compilable modules.
@item (multiple) @tt{cmd/2} declares entry points for commands (executables).
@item (multiple) @tt{manual/2} declares entry points for manuals.
@end{itemize}

@bf{Versioning:} Versions are specified as atoms (see
@lib{version_strings}) of the form @tt{'X.Y.Z'}, where @tt{X}, @tt{Y},
and @tt{Z} are non-negative integers, and an optional @tt{-Prerelease}
suffix. Omitted version numbers are assumed to be 0. Dependencies may
include version constraints, e.g., @tt{core-[version>='1.15']} (see
@pred{version_compare/3} for the definition of the comparison).

@begin{alert}
Currently the system supports only one version of a bundle
at the same time. If two versions need to coexist at the same time,
the bundles must be renamed (e.g. @tt{bndv1}, @tt{bndv2}).
@end{alert}

@begin{alert}
@bf{Additional build rules:} Although the Ciao compiler is incremental
and performs the compilation on demand, some components may require
configuration steps and pre-building. For example, this is often the
case for bindings to foreign third-party libraries. Currently these
rules must be written in a @em{lower-level} module
@tt{Manifest/<bundle>.hooks.pl}.
@end{alert}

@section{Workspaces}

A workspace directory looks like:
@begin{verbatim}
build/           (Generated automatically)
<bundle1>/
...
<bundleN>/
@end{verbatim}

Each workspace directory contains a @tt{build/} directory with
intermediate and final results of compilation (@tt{build/bin/}) and
documentation generation (@tt{build/doc/}).

@bf{Bundle catalogs:} workspaces may contain directories holding
bundles. Such directories must be marked with an empty
@tt{BUNDLE_CATALOG} file. In that case, bundles are visible only if
they are marked with an empty @tt{ACTIVE} file. This mecanism is
useful to implement catalogs of bundles.
").

% TODO: Missing licensing and authorship metadata.

