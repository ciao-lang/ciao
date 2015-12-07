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
	
:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).

% TODO: recover customization of lpdoc:bibfile?
%% .bib files now in a predefined location (JFMC)
%%bibfile := ~decompose(~get_bundle_flag(lpdoc:bibfiles), ',').
% Bibliography files for documentation (clip.bib and general.bib)
:- export(bibfile/1).
bibfile := ~fsR(bundle_src(ciao)/'doc'/'bibtex'/'clip')
	 | ~fsR(bundle_src(ciao)/'doc'/'bibtex'/'general').

% decompose(Text0, Separator, Element) :-
%	( atom_concat([Element0, ',', Text], Text0) ->
%	    ( Element = Element0
%	    ; decompose(Text, Separator, Element)
%	    )
%       ; Element = Text0
%       ).

:- export(docformatdir/2).
docformatdir(html, Dir) :- !, get_bundle_flag(lpdoc:htmldir, Dir).
docformatdir(manl, Dir) :- !, get_bundle_flag(lpdoc:mandir, Dir).
docformatdir(info, Dir) :- !, get_bundle_flag(lpdoc:infodir, Dir).
docformatdir(_,    Dir) :- get_bundle_flag(lpdoc:docdir, Dir).

:- export(docformat/1).
docformat := pdf|manl|info|html. % | ps.
