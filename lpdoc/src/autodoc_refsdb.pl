:- module(autodoc_refsdb, [], [dcg, assertions, regtypes]). 

:- doc(title,"Database of Documentation References").
:- doc(author,"Jose F. Morales").
% TODO: I am not happy with the name of this module

:- doc(module, "This module stores and manages all the documentation
   references (indices, sections, bibliography, etc.). It includes the
   generation of the table of contents.").

:- use_module(lpdoc(autodoc_state)).
:- use_module(lpdoc(autodoc_doctree)).
:- use_module(lpdoc(autodoc_structure)).
:- use_module(lpdoc(autodoc_filesystem)).

% ---------------------------------------------------------------------------

:- doc(section, "Resolving Bibliography").

:- use_module(lpdoc(autodoc_bibrefs), [resolve_bibliography/1]).

% (execute between scanning and final translation)
:- export(compute_refs_and_biblio/1).
compute_refs_and_biblio(DocSt) :-
	docst_gdata_restore(DocSt),
	( \+ docst_opt(no_biblio, DocSt) ->
	    % Keep the doctree for the bibliography in the state
	    % so that it can be used anywhere.
	    resolve_bibliography(DocSt)
	; true
	),
	( \+ docst_opt(no_biblio, DocSt) ->
	    % Save the bibliography
	    MVars = [biblio_pairs, biblio_doctree]
	; MVars = []
	),
	docst_gvar_save(DocSt, MVars),
	docst_gdata_clean(DocSt).

% ---------------------------------------------------------------------------

:- export(prepare_current_refs/1).
:- pred prepare_current_refs/1 :: docstate
   # "Prepare references for the translation of the current file.".
% Restore biblio if necessary and compute navigation and contents
prepare_current_refs(DocSt) :-
	docst_gdata_restore(DocSt),
	( % docst_currmod_is_main(DocSt),
	  % TODO: optimize, keep in memory for several passes...
	  % TODO: do not load all biblio (just biblio_pairs?)
          docst_backend(DocSt, Backend),
	  ( Backend = html ; Backend = texinfo ) -> % TODO: formats hardwired
	    % TODO: it should be more automatic
	    ( \+ docst_opt(no_biblio, DocSt) ->
	        % Load the bibliography
	        MVars = [biblio_pairs, biblio_doctree]
	    ; MVars = []
	    ),
	    docst_gvar_restore(DocSt, MVars),
	    ( \+ docst_opt(no_biblio, DocSt) ->
	        true
	    ; % Fill bibliography with empty data
	      docst_mvar_lookup(DocSt, biblio_pairs, []),
	      docst_mvar_lookup(DocSt, biblio_doctree, [])
	    ),
	    % Compute navigation links and table of contents for the current file
	    docst_currmod(DocSt, Name),
	    get_secttree(DocSt, SectTree),
	    get_nav(SectTree, Name, Nav, CurrTree),
	    docst_mvar_lookup(DocSt, full_toc_tree, SectTree),
	    docst_mvar_lookup(DocSt, curr_toc_tree, CurrTree),
	    docst_mvar_lookup(DocSt, nav, Nav)
	; true
	).

:- export(clean_current_refs/1).
:- pred clean_current_refs/1 :: docstate
   # "Clean the data stored by @pred{prepare_current_refs/1}.".
clean_current_refs(DocSt) :-
	docst_gdata_clean(DocSt).

% ---------------------------------------------------------------------------

:- doc(section, "Section Trees (Table of Contents)").

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(lists), [append/3]).

:- export(secttree/1).
:- regtype secttree/1 # "A tree of sections".
secttree(_). % TODO: Define

:- pred get_secttree/2 :: docstate * secttree # "Obtain the complete tree of sections".
get_secttree(DocSt, SectTree) :-
	findall(sect(Props, link_to(Base,SectLabel), T),
	        docst_gdata_query(DocSt, Base, sect(Props, SectLabel, T)),
		Sects),
	get_secttree_(Sects, _, -1, '__root__', SectTree).

% get_secttree_(+Xs, ?Ys, +BaseLevel, +BaseName, -Nodes):
%
%   Rebuild a tree from a flat representation @var{Xs} of sections.
%   Sections are either level or parent-annotated. @var{Ys} contain
%   the list of pending sections that are not descendants.
get_secttree_([], [], _BaseLevel, _BaseName, []).
get_secttree_([sect(Props,Link,T)|Ss], Ss0, BaseLevel, BaseName, Rs) :-
	Link = link_to(N, _),
	% Determine if it is a subsection based on level or parent relationship
	section_prop(level(L), Props),
	( \+ doclink_is_local(Link), docstr_node(N, _, Parent, Mode) -> Parent = BaseName, NextN = N
	; L > BaseLevel, NextN = '__local__', Mode = normal
	),
	!,
	% Get the subsections
%	display(user_error, foundsub(L, N, Parent, BaseName)), nl(user_error),
	get_secttree_(Ss, Ss1, L, NextN, SubRs),
	( Mode = normal -> Link2 = Link ; Link2 = no_link ),
	R = toc_node(Link2,T,Props,SubRs),
	Rs = [R|Rs1],
	% Continue with the sibling sections
	get_secttree_(Ss1, Ss0, BaseLevel, BaseName, Rs1).
get_secttree_(Ss, Ss, _BaseLevel, _BaseName, []).

:- use_module(lpdoc(autodoc_structure), [docstr_node/4]).

% TODO: CurrPath in nav(_,_,_,_,_) would not be needed if
%   we just traverse Up links; Top link may be global too.
:- export(get_nav/4).
:- pred get_nav/4 :: secttree * atm * term * secttree
   # "Obtain some navigation links and the tree for the current section from a global tree".
get_nav(Tree, Curr, Nav, CurrTree) :-
	( tree_df(Tree, [], Nodes, []),
	  % Find @var{Curr} (alongside previous and next nodes) in the
	  % list of nodes @var{Nodes}
	  doclink_at(CurrLink, Curr),
	  Curr0 = df_item(CurrLink, CurrTree0, RootPath),
	  member123(Prev0, Curr0, Next0, Nodes) ->
	    reverse(RootPath, CurrPath),
	    CurrTree = CurrTree0,
	    first_link(CurrPath, Top),
	    first_link(RootPath, Up),
	    df_link(Prev0, Prev),
	    df_link(Next0, Next)
	; % TODO: README*.lpdoc files fail here... find a better way to catch this
          fail %throw(error(arg(1,Curr), get_nav/1)) % not found!
	),
	!,
	Nav = nav(CurrPath, Top, Up, Prev, Next).
get_nav(_Tree, _Curr, Nav, CurrTree) :-
	Nav = nav([], no_link, no_link, no_link, no_link),
	CurrTree = [].

df_link(X, Link) :-
        ( X = [df_item(Link0, _, _)] -> Link = Link0 ; Link = no_link ).

first_link(Path, Link) :-
	( Path = [step(Link0,_)|_] -> Link = Link0 ; Link = no_link ).

:- use_module(library(lists), [reverse/2]).

% Obtain the list of df_item(Link,Sub,Path), using a depth-first traversal, where
% Path is the path from the tree root to Node, Link is the link in the node, and
% Sub is the subtree of the node.
% 
% Note: links to local sections are not included.
tree_df([], _, Xs, Xs).
tree_df([toc_node(Link,T,_Props,Sub)|Ts], RootPath0, Xs, Xs0) :-
	( doclink_is_local(Link) ->
	    Xs = Xs2
	; Xs = [df_item(Link, Sub, RootPath0)|Xs2]
	),
	RootPath = [step(Link,T)|RootPath0],
	tree_df(Sub, RootPath, Xs2, Xs1),
	tree_df(Ts, RootPath0, Xs1, Xs0).

% Prev+[Curr]+Next is a sublist of List.
%   where both Prev and Next are either singleton elements or the
%   empty list
% (nondet)
member123(Prev, Curr, Next, List) :-
	prefix01(Prev, Ns, Ns2), Ns2 = [Curr|Ns1], prefix01(Next, Ns1, _),
	append(_, Ns, List).

% (nondet)
prefix01(Sub, Ns, Ns0) :-
	( Ns = [N|Ns0], Sub = [N] % take one element
	; Ns = Ns0, Sub = [] % do not take anything
	).

% ---------------------------------------------------------------------------

:- export(secttree_resolve/3).
:- pred secttree_resolve(LabelName, Tree, Link) :: string * doctree * doclink
   # "Locate in the section tree @var{Tree} the section with label name
      @var{LabelName} and return the @var{Link} to the section.".

secttree_resolve(LabelName, Tree, Link) :-
	member(toc_node(Link0, _, _, Subs), Tree),
	( Link0 = link_to(_, Label),
	  ( Label = global_label(LabelName)
	  ; Label = local_label(LabelName)
	  ) ->
	    Link = Link0
	; secttree_resolve(LabelName, Subs, Link)
	).

