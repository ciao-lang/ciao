:- module(doc_flags, [], [assertions, fsyntax]).

:- doc(title, "LPdoc config flags").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(module, "Options from LPdoc bundle (defined here since they are
   needed for the builder)").
% TODO: Alternatively, we can say that those are platform-dependent
%   flags that are abstracted by the builder (in the same way that we
%   abstract the C compiler, etc.). Thus they are not really part of
%   LPdoc.
	
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).

:- export(docformatdir/2).
docformatdir(html, Dir) :- !, get_bundle_flag(lpdoc:htmldir, Dir).
docformatdir(manl, Dir) :- !, get_bundle_flag(lpdoc:mandir, Dir).
docformatdir(info, Dir) :- !, get_bundle_flag(lpdoc:infodir, Dir).
docformatdir(_,    Dir) :- get_bundle_flag(lpdoc:docdir, Dir).

% TODO: Those are default formats for installation; use a bundle flag instead
:- export(docformat/1).
docformat := pdf|manl|info|html. % | ps.
