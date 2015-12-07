:- module(emugen_errors, [], [dcg, fsyntax, assertions]).

:- doc(title, "Error diagnosis for emugen").
:- doc(author, "Jose F. Morales").

:- doc(bug, "This module needs a major rewrite").

:- use_module(library(lists)).

:- export(handler_msg/2).

handler_msg(multiple_possible_translations(Bodies, _Store), Msg) :-
	diff_derivs(Bodies, Common, Diff),
	error_msg('Multiple possible translations:',
	          Common, diff(Diff), Msg, []).
handler_msg(no_translation_for(AllBodies, _Store), Msg) :- !,
	% TODO: customize errors?
%	    ( pred_prop(G, on_failure(Reason)) ->
%	        true
%	    ; Reason = store(Store)
%	    ),
	linearize_all(AllBodies, AllBodies2),
	as_trie(AllBodies2, AllBodies3),
	simp_trie(AllBodies3, AllBodies4),
	ck_trie(AllBodies4, AllBodies5),
	Msg = ['No translation found, deepest failed derivations:\n'|Msg0],
	msg_trie(AllBodies5, 0, Msg0, []).

% Eliminate closed rs, keep rs trace with failed 
ers([], []) --> !.
ers(['$push_rs', G|Xs], Zs) --> !,
	{ ers(Xs, Ys, Gs, []) },
	( { Ys = [] } ->
	    % Found diff, show context
	    [G],
	    emit(Gs),
	    { Zs = [] }
	; % Discard Gs
	  ers(Ys, Zs)
	).
ers(['$fail_lit'|_Xs], Zs) --> !,
	['$fail_lit'],
	{ Zs = [] }.
ers(['$pop_rs'|Xs], Zs) --> !,
	{ Zs = Xs }.
ers([_G|Xs], Zs) --> !,
	ers(Xs, Zs).

emit([]) --> [].
emit([X|Xs]) --> [X], emit(Xs).

linearize(B, [X|B2]) :-
	decomp_(B, X, Rest),
	( Rest = '$stop' ->
	    B2 = []
	; X = '$fail_lit' ->
	    B2 = []
	; linearize(Rest, B2)
	).

linearize_all([], []).
linearize_all([B|Bs], [B2|Bs2]) :-
	linearize(B, B2),
	linearize_all(Bs, Bs2).

:- use_module(library(sort), [sort/2]).

%:- export(as_trie/2).
% Create a trie from a list of lists
as_trie(Bs, Ts) :-
	sort(Bs, Bs2),
	as_trie_(Bs2, Ts).

as_trie_([], Ts) :- !, Ts = [].
as_trie_(Bs, Ts) :-
	common_prefix(Bs, Prefix, CommonBs, Bs2),
	( Prefix = [] ->
	    Ts = [leaf|Ts2]
	; Prefix = [Bh] ->
	    Ts = [Bh-BsTrie|Ts2],
	    as_trie_(CommonBs, BsTrie)
	),
	as_trie_(Bs2, Ts2).

common_prefix(Bs, Prefix, CommonBs, Rest) :-
	( Bs = [[Bh|_]|_] -> Prefix = [Bh]
	; Bs = [[]|_] -> Prefix = []
	),
	common_prefix_(Bs, Prefix, CommonBs, Rest).

common_prefix_([B1|Bs], Prefix, CommonBs, Rest) :-
	( Prefix = [Bh] ->
	    B1 = [B1h|B1t],
	    Bh == B1h,
	    CommonBs = [B1t|CommonBs0]
	; Prefix = [] ->
	    B1 == [],
	    CommonBs = CommonBs0
	),
	!,
	common_prefix_(Bs, Prefix, CommonBs0, Rest).
common_prefix_(Bs, _Prefix, CommonBs, Rest) :-
	CommonBs = [],
	Rest = Bs.

% Simplify the trie of derivations to catch the failed ones
% (innermost failure?)
simp_trie([], []).
simp_trie([leaf|KTss], [leaf|KTss]) :- !.
simp_trie([K-Ts|KTss], KTss2) :-
	remove_failed_ts(Ts, Ts2),
	!,
	simp_trie([K-Ts2|KTss], KTss2).
simp_trie([K-Ts|KTss], [K-Ts2|KTss2]) :- !,
	simp_trie(Ts, Ts2),
	simp_trie(KTss, KTss2).

remove_failed_ts(Ts, Ts2) :-
	length(Ts, N), N > 1,
	select(('$fail_lit'-_), Ts, Ts1),
	!,
	Ts2 = Ts1.

% Compact keys of trie
ck_trie([], []).
ck_trie([leaf|KTss], [leaf|KTss]) :- !.
ck_trie([K-[K1-Ts]|KTss], KTss2) :- !,
	% collapse keys
	ck_trie([(K, K1)-Ts|KTss], KTss2).
ck_trie([K-Ts|KTss], [K-Ts2|KTss2]) :- !,
	ck_trie(Ts, Ts2),
	ck_trie(KTss, KTss2).

% Message to display a trie
msg_trie([], _I) --> [].
msg_trie([KTs|KTss], I) -->
	nest_tabs(I),
	( { KTs = K-Ts } ->
	    { conj_to_list(K, K2, []) },
	    { ers(K2, _, K3, []) },
	    nest_path(K3, trie(Ts), I)
	; ['*leaf*\n']
	),
	msg_trie(KTss, I).

conj_to_list((A,B)) --> !, conj_to_list(A), conj_to_list(B).
conj_to_list(A) --> [A].

% Locate differences between two bodies
diff_derivs(Bs, Common, Diff) :-
	decomp(Bs, Xs, Bs2),
	( \+ all_same(['$stop'|Xs]),
	  all_same(Xs) ->
	    Xs = [X|_],
	    Common = [X|Common2],
	    diff_derivs(Bs2, Common2, Diff)
	; Common = [],
	  Diff = Bs
	).

all_same([]).
all_same([X|Xs]) :- all_same_(Xs, X).

all_same_([], _).
all_same_([X|Xs], Y) :- X == Y, all_same_(Xs, Y).

% Decompose a forest
decomp([], [], []).
decomp([B|Bs], [X|Xs], [B2|Bs2]) :-
	decomp_(B, X, B2),
	decomp(Bs, Xs, Bs2).

% Decompose a tree for linearization
decomp_('$rs'(G, X), '$push_rs', (G, X, '$pop_rs')) :- !.
decomp_((A, B), H, T) :- !,
	decomp_(A, Ah, At),
	H = Ah,
	( At = '$stop' -> T = B
	; T = (At, B)
	).
decomp_(X, X, '$stop').

% Format error message for multiple translations
error_msg(Msg0, Common, Error) -->
	[Msg0, '\n'],
	{ ers(Common, _, Common2, []) } ,
	nest_path(Common2, Error, 0).

% Nest path (for error reporting)
nest_path([], Error, I) --> !,
	nest_tabs(I),
	nest_error(Error, I).
nest_path([G|Xs], Error, I) --> !,
	( { I > 0 } ->
	    nest_tabs(I), ['|', '\n'],
	    nest_tabs(I), ['`- ']
	; ['   ']
	),
	[''(G), '\n'],
	{ I1 is I + 3 },
	nest_path(Xs, Error, I1).

nest_error(diff(Diff), I) -->
	['** Translates to: **', '\n'],
	nest_diff(Diff, I).
nest_error(trie(Ts), I) -->
	['** Cannot translate: **', '\n'],
	msg_trie(Ts, I).

nest_diff([], _I) --> [].
nest_diff([X|Xs], I) -->
	nest_tabs(I), nest_diff_(X), ['\n'],
	( { Xs = [] } -> []
	; nest_tabs(I), ['** or **', '\n']
	),
	nest_diff(Xs, I).

nest_diff_(X) -->
	{ decomp_(X, G, X0) },
	( { G = '$push_rs' } ->
	    nest_diff_(X0)
	; [''(G)],
	  ( { X0 = '$stop' } ->
	      []
	  ; [' ...']
	  )
	).

nest_tabs(I) -->
	{ nest_tabs_(I, Tabs, []), atom_codes(Tabs2, Tabs) },
        [Tabs2].

nest_tabs_(N) --> { N =< 0 }, !, "".
nest_tabs_(N) --> " ", { N1 is N - 1 }, nest_tabs_(N1).

