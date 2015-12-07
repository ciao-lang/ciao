:- module(_, [], [assertions, fsyntax]).

:- doc(title, "Template Support for the HTML Backend").
:- doc(author, "Jose F. Morales").

:- use_module(library(messages)).
:- use_module(library(aggregates)).
:- use_module(library(system)).
:- use_module(library(file_utils)).
:- use_module(library(system_extra)).

:- use_module(library(pathnames), [path_concat/3]).

:- use_module(library(lists)).
:- use_module(library(terms), [atom_concat/2]).

:- include(library(pillow/ops)).

:- use_module(library(make/make_rt)).

:- use_module(library(pillow/html), [html_template/3]).

:- use_module(lpdoc(autodoc_settings)).

% ---------------------------------------------------------------------------

% URL to an image
% TODO: Generalize, so that we can obtain the filesystem paths, relative URLs,
%       etc., for any file or resource.
:- export(img_url/2). % TODO: temporary?
:- pred img_url(Name, Url) :: atm * string
   # "Obtain the @var{URL} where image @var{Name} is or will be found.".
img_url(Name) := Url :-
	% TODO: Use relative URLs is htmlurl is '' (search uses of htmlurl)
	WebURL = ~setting_value(htmlurl),
	( WebURL = '' ->
	    P0 = 'images'
	; path_concat(WebURL, 'images', P0)
	),
	path_concat(P0, Name, P1),
	atom_codes(P1, Url).

% ---------------------------------------------------------------------------

:- export(fmt_html_template/3).
% TODO: (Macros with named arguments)
% Like html_template, but recognizes <v>Var</v> in strings too, and outputs
% a doctree
fmt_html_template(File, Args0) := R :-
	% TODO: Use relative URLs if htmlurl is '' (search uses of htmlurl)
	WebURL = ~setting_value(htmlurl),
	( WebURL = '' -> P0 = 'images'
	; path_concat(WebURL, 'images', P0)
	),
	path_concat(P0, '', DirImages), % (adds trailing '/')
	WS = [weburl = WebURL, dirImage = DirImages],
	%
	append(Args0, WS, Args),
	%
	( File2 = ~fixed_absolute_file_name(~locate_tmpl(File)),
	  file_exists(File2),
	  String = ~file_to_string(File2) ->
	    true
	; % TODO: this should be a normal user error, not a bug
          throw(error(html_template_not_found(File), fmt_html_template/3))
	),
	html_template(String, HtmlR0, FreeDict0),
	subst_params_in_strings(HtmlR0, HtmlR1, FreeDict0, FreeDict),
	html_to_doctree(HtmlR1, R),
	%
	connect_params(FreeDict, Args).

locate_tmpl(A, B) :-
	BR = ~setting_value(website_root_dir),
	B = ~path_concat(~path_concat(BR, 'tmpl'), A).

% Connect values in the first list with the mapping B
connect_params([], _Args).
connect_params([Var=Value|As], Args) :-
	( member(Var=V0, Args) -> true
	; % TODO: this should be a normal user error, not a bug
	  throw(error(tmpl_parameter_not_found(Var), connect_params/2))
	),
	( param_type(Var, Type) ->
	    ( Type = atom, atom(V0) -> atom_codes(V0, V1), V = raw(V1)
	    ; Type = string -> V = raw(V0) % TODO: raw or string_enc?
	    ; Type = doctree -> V = V0
	    ; throw(error(bad_param_type(Type, V0), connect_params/2))
	    )
	; V = V0 % assume doctree
	),
	Value = V,
	connect_params(As, Args).

% TODO: generalize, put in the template? or in the dictionary?
% Param type is used to tranform arbitrary encodings to valid doctree terms
param_type(weburl, atom). % TODO: check...
param_type(dirImage, atom). % TODO: check...

% Substitute parameter names by their values
subst_params_in_strings(A, A, D, D) :- var(A), !.
subst_params_in_strings([], [], D, D) :- !.
subst_params_in_strings([A|As], [A2|As2], D0, D) :- !,
	subst_params_in_strings(A, A2, D0, D1),
	subst_params_in_strings(As, As2, D1, D).
subst_params_in_strings(env(E, E1, E2), env(E, E1T, E2T), D0, D) :- !,
	subst_params_in_strings(E1, E1T, D0, D1),
	subst_params_in_strings(E2, E2T, D1, D).
subst_params_in_strings((ENV=STR), ENV=STRT, D0, D) :- !,
	subst_params_in_string(STR, STRT, D0, D).
subst_params_in_strings('$'(ENV, LIST), '$'(ENV, TLIST), D0, D) :- !,
	subst_params_in_strings(LIST, TLIST, D0, D).
subst_params_in_strings(E, E, D, D).

subst_params_in_string(T, TT, D0, D) :-
	nonvar(T),
	(VarDefStart = "<v>"||VarRest ; VarDefStart = "<V>"||VarRest),
	append(Before, VarDefStart, T),
	!,
	(VarDefEnd = "</v>"||R -> true ; VarDefEnd = "</V>"||R),
	append(VarName, VarDefEnd, VarRest),
	!,
	atom_codes(AtomVarName, VarName),
	( member(AtomVarName=Value, D0) ->
	    D1 = D0
	; D1 = [AtomVarName=Value|D0]
	),
	append(Before, [Value], NewBefore),
	append(NewBefore, VarRestT, TT),
	subst_params_in_string(R, VarRestT, D1, D).
subst_params_in_string(T, T, D, D).

% Translate from the extended Pillow HTML representation to a doctree
% (Note: the extended Pillow HTML comes from subst_params_in_string)
%html_to_doctree(A, A2) :- var(A), !, A2 = raw("UNBOUND").
html_to_doctree(A, A) :- var(A), !.
html_to_doctree(A, B) :- ( A = [] ; A = [_|_] ), !,
	html_list_to_doctree(A, B0),
	( B0 = [B1] -> B = B1 ; B = B0 ). % simplify
html_to_doctree(A, A2) :- atom(A), !, atom_codes(A, A1), A2 = raw(A1).
html_to_doctree(env(E, E1, E2), htmlenv(E, E1T, E2T)) :- !,
	attrs_to_doctree(E1, E1T),
	html_to_doctree(E2, E2T).
html_to_doctree('$'(E, Attrs0), htmlenv1(E, Attrs)) :- !,
	attrs_to_doctree(Attrs0, Attrs).
html_to_doctree(declare(X), htmldecl(X)) :- !.
html_to_doctree(comment(X), htmlcomment(X)) :- !.
html_to_doctree(E, _) :-
	throw(error(domain_error, html_to_doctree/2-env(['E'=E]))).

attrs_to_doctree([(V=Val0)|Ts0], [attr(V,Val)|Ts]) :- !,
	html_to_doctree(Val0, Val),
	attrs_to_doctree(Ts0, Ts).
attrs_to_doctree([], []) :- !.

html_list_to_doctree(A, _) :- var(A), !, throw(not_well_formed_html).
html_list_to_doctree(Xs, Ys) :- pick_string(Xs, Str, Xs0), \+ Str = [], !,
	Ys = [raw(Str)|Ys0],
	html_list_to_doctree(Xs0, Ys0).
html_list_to_doctree([E0|Es0], [E|Es]) :- !,
	html_to_doctree(E0, E),
	html_list_to_doctree(Es0, Es).
html_list_to_doctree([], []) :- !.

pick_string([X|Xs], [X|Ys], Zs) :- integer(X), !, pick_string(Xs, Ys, Zs).
pick_string(Xs, [], Xs).

% ---------------------------------------------------------------------------




