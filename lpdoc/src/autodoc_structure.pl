:- module(autodoc_structure, [], [dcg, assertions, regtypes, basicmodes, fsyntax]).

:- doc(title,"Handling the Document Structure").
:- doc(author,"Jose F. Morales").

%:- use_module(lpdoc(autodoc)).
:- use_module(library(pathnames), [path_splitext/3, path_basename/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms), [atom_concat/2]).

:- use_module(lpdoc(autodoc_settings)).

% ---------------------------------------------------------------------------
% Parse the whole document structure (a tree)

% TODO: Add a regular type for doc_structure/1 (used too for the SETTINGS files)

% docstr_node(Mod, Base, ParentComponent, Mode)
%
% E.g. docstr_node('toplevel_doc', 'toplevel/toplevel_doc', ..., ...)
:- export(docstr_node/4).
:- data docstr_node/4.

:- export(clean_docstr/0).
clean_docstr :- retractall_fact(docstr_node(_,_,_,_)).

:- export(parse_structure/0).
parse_structure :-
	clean_docstr, % TODO: fix clean: it should be done also when lpdoc ends
	( setting_value(doc_structure, S) ->
	    parse_structure_(S, '__root__')
	; true
	),
	check_deprecated_settings.

check_deprecated_settings :-
	% TODO: This could be part of the LPSETTING file itself...
	( ( setting_value(mainfile, _)
	  ; setting_value(component, _) ) ->
	      throw(make_error("Deprecated mainfile/1 or component/1 in SETTINGS file. "||
                               "Use doc_structure/1 instead.", []))
	; true
	),
	( ( setting_value(fileoption(_), _) ) ->
	      throw(make_error("Deprecated fileoption/1 in SETTINGS file. "||
                               "Use doc_mainopts/1 or doc_compopts/1 instead.", []))
	; true
	).

parse_structure_(S, Parent) :- is_list(S), !,
	parse_structure_list(S, Parent).
parse_structure_(S0, Parent) :-
	( S0 = S1-Ss ->
	    true
	; S1 = S0,
	  Ss = [] % no descendants
	),
	( S1 = phony(S) ->
	    Mode = phony
	; S = S1, Mode = normal
	),
	% Remove extension (if present)
	( path_splitext(S, Base, _) -> true ; Base = S ),
	% TODO: This could be simplified (I have repeated that code in some places)
	path_basename(Base, Mod), % just the name (i.e., ../<name>)
%	display(user_error, docstr_node(Mod, Base, Parent, Mode)), nl(user_error),
	add_docstr_node(Mod, Base, Parent, Mode),
	parse_structure_(Ss, Mod).

add_docstr_node(Mod, Base, Parent, Mode) :-
	( current_fact(docstr_node(Mod, Base, _, _)) ->
	    % TODO: This condition should change if a hierarchical module system is implemented
	    % TODO: Fix error reporting (this code is tried several times before exiting lpdoc)
	    throw(make_error("Duplicated ~w module in doc_structure/1 in SETTINGS file.", [Base]))
	; assertz_fact(docstr_node(Mod, Base, Parent, Mode))
	).

parse_structure_list([], _) :- !.
parse_structure_list([S|Ss], Parent) :- !,
	parse_structure_(S, Parent),
	parse_structure_list(Ss, Parent).

is_list([]).	
is_list([_|_]).	

% ---------------------------------------------------------------------------

% docstr for a standalone target (passed with '-c' option to LPdoc)
:- export(standalone_docstr/1).
standalone_docstr(Base) :-
	clean_docstr,
	parse_structure_(Base, '__root__').

% ---------------------------------------------------------------------------
% Query mainmod and components.

:- export(get_mainmod/1).
get_mainmod := R :-
	docstr_node(R0, _, '__root__', _), !,
	R = R0.

:- export(get_mainmod_spec/1).
get_mainmod_spec := R :-
	docstr_node(_, R0, '__root__', _), !,
	R = R0.

:- export(all_component_specs/1).
% TODO: Change name? secondary_bases?
all_component_specs := Cs :-
	findall(C, component_spec(C), Cs0),
	Cs = Cs0.

component_spec(C) :- docstr_node(_, C, P, _), \+ P = '__root__'.


