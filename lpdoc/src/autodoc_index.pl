:- module(autodoc_index, [], [dcg, assertions, regtypes]). 

:- doc(title,"Indexing Commands (Definition and Formatting)").
:- doc(author,"Jose F. Morales").

:- doc(module, "This module defines index commands and formatting.

@begin{alert}   
@bf{Note: This part needs better documentation. -- JFMC}
@end{alert}
   ").

:- use_module(library(dict)).
:- use_module(library(lists), [reverse/2, append/3]).
:- use_module(library(aggregates), [findall/3]).

:- use_module(lpdoc(autodoc_state)).
:- use_module(lpdoc(autodoc_filesystem)).
:- use_module(lpdoc(autodoc_doctree)).
:- use_module(lpdoc(autodoc_structure)).
% The database of references
:- use_module(lpdoc(autodoc_refsdb)).

%% ---------------------------------------------------------------------------

:- export(get_idxsub/2).
% Obtain the subfile name for a given index 
get_idxsub(IdxName, SubName) :-
	typeindex(IdxName, IndexId, _, _, _),
	atom_concat(IndexId, 'index', SubName).

:- export(get_idxbase/3).
% Obtain the base for a given index
get_idxbase(IdxName, _DocSt, IdxBase) :-
	get_mainmod(Base),
	get_idxsub(IdxName, SubName),
	get_subbase(Base, SubName, IdxBase).
	
%% ---------------------------------------------------------------------------

% TODO: Is it better to define @var{Comment} as a docstring or a doctree?
% TODO: are info Index names restricted to two characters?

:- export(typeindex/5).
:- pred typeindex(Type,Index,IType,Name,Comment) 

	=> atom * atm * string * string * doctree

        # "@var{Index} is the (info) index name in which objects of
           type @var{Type} go. @var{Name} is the title of the index in
           the documentation. @var{IType} is the type of index; an
           empty string means normal. code@var{Comment} is a comment
           to include before the index.".

%%%% Should not be empty...?
% TODO: make typeindex data so that the user can define new indices?
%:- data(typeindex/5).

typeindex(lib,    'li',"code","Library/Module Index",  string_esc("")).
typeindex(apl,    'ap',"code","Application Index",                string_esc("")).
typeindex(pred,   'pd',"code","Predicate/Method Index",string_esc("")).
%% typeindex(func,   'fu',"code","Function/Method Index", string_esc("")).
typeindex(prop,   'pr',"code","Property Index",        string_esc("")).
typeindex(regtype,'te',"code","Regular Type Index",    string_esc("")).
typeindex(decl,   'de',"code","Declaration Index",     string_esc("")).
typeindex(op,     'op',"code","Operator Index",        string_esc("")).
typeindex(modedef,'mo',"code","Mode Index",            string_esc("")).
typeindex(file,   'fi',"code","File/Directory Index",             string_esc("")).
typeindex(concept,'co',""    ,"Concept Index",         string_esc("")).
typeindex(author, 'au',"" ,"Author Index",                     string_esc("")).
%% Some versions of makeinfo get confused by this one (core dump!)
typeindex(global, 'gl',"code","Global Index",
[string_esc("This is a global index containing pointers to places where concepts, 
 predicates, modes, properties, types, applications, etc., are referred to
 in the text of the document.")
% ,string_esc("Note that due to limitations of the "),
% tt(string_esc("info")),
% string_esc(" format unfortunately only the first reference will appear in
% online versions of the document.")
]).

% TODO: compare with autodoc:assrt_type_text/4
def_text(lib,    "library").
def_text(apl,    "application").
%
def_text(pred,    "predicate").
def_text(compat,  "predicate").
def_text(calls,   "predicate").
def_text(success, "predicate").
def_text(comp,    "predicate").
%% def_text(func,    "FUNCTION").
def_text(prop,    "property").
def_text(regtype, "regular type").
def_text(decl,    "declaration").
def_text(modedef, "instantiation mode").
def_text(author,  "author").
def_text(entry,   "entry point").

% concepttype(index).
% concepttype(cindex).
% concepttype(concept).

:- export(idx_get_indices/3).
% Note: indices change depending on the mode of indexing (defining vs. using)
idx_get_indices(Mode, Cmd, Indices) :-
	( index_cmd(Cmd, Index, AlwaysDef, _) ->
	    ( AlwaysDef = yes -> % any ocurrence acts as a definition
	        Indices = [Index|Indices0]
	    ; Mode = def -> Indices = [Index|Indices0]
	    ; Mode = use -> Indices = Indices0
	    )
	; Indices = Indices0
	),
	Indices0 = [global].

% index_cmd(Cmd, Index, Autodef, Style)
%% Concept definition index entry
index_cmd(index, concept, yes, em) :- !.
%% Concept definition index entry (NOT including the body in-line)
index_cmd(cindex, concept, yes, none) :- !.
%% Reference to concept (NOT emphasized, goes only to global)
%% Concepts should appear only once in the concept index
index_cmd(concept, concept, no, normal) :- !.
%% Authors references
index_cmd(author, author, no, normal) :- !.
%% Predicate/Application/Property/Type/Operator/Library/File/etc. references
index_cmd(Type, Type, no, tt) :- codetype(Type), !.

:- export(is_index_cmd/1).
is_index_cmd(index) :- !.
is_index_cmd(cindex) :- !.
is_index_cmd(concept) :- !.
is_index_cmd(author) :- !.
is_index_cmd(X) :- codetype(X), !.

:- export(codetype/1).
% Indexing commands for several kinds of code
% TODO: Document
codetype(lib).
codetype(apl).
codetype(pred).
%% codetype(func).
codetype(prop).
codetype(regtype).
codetype(decl).
codetype(op).
codetype(modedef).
codetype(file).
codetype(global).
codetype(code).

% ---------------------------------------------------------------------------

:- export(normalize_index_cmd/3).
% Obtain a normalized index command for the given one
normalize_index_cmd(Cmd, Body, Token) :-
	Token = idx_env(use, Cmd, localnum_label(_), Body, Body).

% ---------------------------------------------------------------------------

:- export(fmt_idx_env/7).
fmt_idx_env(Mode, Type, IdxLabel, Ref, Body, DocSt, R) :-
	idx_get_indices(Mode, Type, Indices),
	index_cmd(Type, _, _, Style),
	( doctree_is_empty(Body) -> R0 = []
	; Style = none -> R0 = []
	; Style = normal -> R0 = [Body]
	; Style = tt -> R0 = [tt(Body)]
	; Style = em -> R0 = [em(Body)]
	),
	doctree_to_rawtext(Ref, DocSt, RefLab),
	( Indices = [I1|_] ->
	    get_idxbase(I1, DocSt, IdxBase)
	; throw(error(empty_indices, fmt_idx_env/7))
	),
	OutLink = link_to(IdxBase, local_label(RefLab)),
	%
	( docst_backend(DocSt, texinfo) ->
	    % TODO: I could use Ref, but accents break texinfo.tex
	    doctree_to_rawtext(Ref, DocSt, Ref1),
	    Ref2 = raw(Ref1)
	; Ref2 = Ref
	),
	%
	R = idx_anchor(Indices, IdxLabel, Ref2, OutLink, R0).

% ---------------------------------------------------------------------------

:- export(fmt_index/3).
fmt_index(IndexId, DocSt, R) :-
	sort_index_entries(IndexId, DocSt, DicDic),
	flatten_dic(DicDic, Groups, []),
	R = twocolumns(R0),
	fmt_index_groups(Groups, R0).

fmt_index_groups([], []).
fmt_index_groups([(G,Dic)|Gs], R) :-
	flatten_dic(Dic, KVs, []),
	index_links(KVs, Ls),
	( symbol_norm(G) -> Header = "Symbols" ; Header = [G] ),
	R = [subsection_title(string_esc(Header)), itemize_plain(Ls)|R0],
	fmt_index_groups(Gs, R0).

% Normalized initial characters for indices
norm_alpha(X, Y) :- X >= 0'a, X =< 0'z, !, Y is X + 0'A - 0'a.
norm_alpha(X, Y) :- X >= 0'A, X =< 0'Z, !, Y = X.

symbol_norm(0). % (so that it appears the first when sorted)

% Sort index entries in groups of groups
sort_index_entries(IndexId, DocSt, Dic) :-
	Dic0 = _, % empty dictionary
	findall(idx_e(ExtMode, Text, Base, IdxLabel), 
                query_index_entries(IndexId, DocSt, ExtMode, Text, Base, IdxLabel),
		Es),
	sort_index_entries_(Es, Dic0, Dic).

% Obtain a dictionary of dictionaries of pairs (UseBs, DefBs)
% TODO: Text is in rawtext; be careful since it may contain back-end specific escapes (except accents,
%   that are currently ignored)
sort_index_entries_([], D, D).
sort_index_entries_([idx_e(ExtMode,Text,Base,IdxLabel)|Es], D0, D) :-
	( Text = [G0|_], norm_alpha(G0, G) ->
	    true
	; symbol_norm(G)
	),
	dic_lookup(D0, G, Dic0),
	( dic_get(Dic0, Text, P0) ->
	    true
	; P0 = ([],[])
	),
	B = idx_b(ExtMode,Base,IdxLabel),
	P0 = (DefBs0, UseBs0),
	( ExtMode = def(_) ->
	    P = ([B|DefBs0], UseBs0)
	; P = (DefBs0, [B|UseBs0])
	),
	dic_replace(Dic0, Text, P, Dic1),
	dic_replace(D0, G, Dic1, D1),
	sort_index_entries_(Es, D1, D).

% Enumerates (on backtracking) all entries for index IndexId
% ((Text,Base) is the result)
query_index_entries(IndexId, DocSt, ExtMode, Text, Base, IdxLabel) :-
	( typeindex(IdxName, IndexId, _, _, _) ->
	    true
	; throw(error(wrong_index_id(IndexId), query_index_entries/6))
	),
	docst_gdata_query(DocSt, Base, idx(Mode, Type, IdxLabel, Text)),
	( Mode = def -> ExtMode = def(Type) % extend the mode with the type
	; ExtMode = Mode
	),
	idx_get_indices(Mode, Type, Ids),
	member(IdxName, Ids).

flatten_dic(D, Vs, Vs) :- var(D), !.
flatten_dic(dic(K,V,L,R), Vs, Vs0) :-
	flatten_dic(L, Vs, Vs2),
	Vs2 = [(K,V)|Vs1],
	flatten_dic(R, Vs1, Vs0).

index_links([], []).
index_links([(K,(DefBs,UseBs))|KVs], Rs) :-
	reverse(DefBs, DefBs2),
	reverse(UseBs, UseBs2),
	% show links to definitions before links to uses
	append(DefBs2, UseBs2, Bs),
	Rs = [R|Rs0],
	index_key(K, Bs, R),
	index_links(KVs, Rs0).

% TODO: precondition: Bs is not empty
index_key(K, [B|Bs], R) :-
	% format the first entry
	% TODO: K already escaped?, using raw(K) instead of string_esc(K)
	index_key_single([raw(K), string_esc(" ")], B, local_label(K), R, R0),
	% format other entries
	index_key2(Bs, Rbs),
	( Rbs = [] -> R0 = []
	; R0 = [itemize_none(Rbs)] % include as itemize
	).

index_key2([], []).
index_key2([B|Bs], [R|Rs]) :-
	index_key_single([], B, no_label, R, []),
	index_key2(Bs, Rs).

index_key_single(Pre, B, Label, R, R0) :-
	B = idx_b(ExtMode,Base,IdxLabel),
	Link = link_to(Base,IdxLabel),
	atom_codes(Base, BaseC),
	( ExtMode = def(Type) ->
	    ( def_text(Type, TypeC) -> true ; atom_codes(Type, TypeC) ),
	    Msg = [string_esc(BaseC), string_esc(" "), string_esc(TypeC)]
	; Msg = [string_esc("in "), string_esc(BaseC)]
	),
	Text3 = [Pre, string_esc("("), Msg, string_esc(")")],
	R = [item(""), simple_link(default, Label, Link, Text3)|R0].
