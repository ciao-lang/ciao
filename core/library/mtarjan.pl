:- module(mtarjan, [], [assertions, regtypes, isomodes, hiord, datafacts]).

:- doc(title, "Tarjan's strongly-connected components algorithm").
:- doc(author, "Jose F. Morales").

:- use_module(library(aggregates), [findall/3]).

:- export(find_sccs/3).
:- meta_predicate find_sccs(pred(1), pred(2), pred(1)).
:- pred find_sccs(PV, PE, SCCs)
   # "Given a graph (@var{PV},@var{PE}), obtain the list @var{SCCs} of
     all strongly connected components.".

find_sccs(PV, PE, SCCs) :-
	retractall_fact(scc_(_)),
	tarjan(PV, PE, add_scc),
	findall(SCC, retract_fact(scc_(SCC)), SCCs).

:- data scc_/1.
add_scc(Ws) :- assertz_fact(scc_(Ws)).

:- export(tarjan/3).
:- meta_predicate tarjan(pred(1), pred(2), pred(1)).
:- pred tarjan(PV, PE, OnSCC)
   # "Given a graph (@var{PV},@var{PE}), execute @var{OnSCC} for each
     strongly connected component.".

tarjan(PV, PE, OnSCC) :-
	retractall_fact(v_index(_,_)),
	retractall_fact(v_lowlink(_,_)),
	set_index(0),
	retractall_fact(s(_)),
	( % (failure-driven loop)
	  PV(V),
	    ( \+ v_index(V, _) -> strongconnect(V, PE, OnSCC)
	    ; true
	    ),
	    fail
	; true
	).

min(A,B,C) :- A < B, !, C = A.
min(_,B,B).

:- meta_predicate strongconnect(?, pred(2), pred(1)).
strongconnect(V, PE, OnSCC) :-
	% Set the depth index for v to the smallest unused index
	index(I),
	set_v_index(V, I),
	set_v_lowlink(V, I),
	inc_index,
	s_push(V),
	% Consider successors of v
	( % (failure-driven loop)
	  PE(V, W),
	    ( \+ v_index(W, _) ->
                % Successor w has not yet been visited; recurse on it
                strongconnect(W, PE, OnSCC),
		v_lowlink(W, WLowLink),
		%
		set_v_lowlink_min(V, WLowLink)
	    ; ( s(W) ->
		  % Successor w is in stack S and hence in the current SCC
		  v_index(W, WIndex),
		  %
		  set_v_lowlink_min(V, WIndex)
	      ; true
	      )
	    ),
	    fail
	; true
	),
	% If v is a root vertex, pop the stack and generate an SCC
	( v_lowlink(V, VLowLink),
	  v_index(V, VIndex),
	  VLowLink = VIndex ->
	    % start a new strongly connected component
	    s_pop_until(V, Ws),
	    OnSCC(Ws)
	; true
	).

:- data s/1.
:- data v_index/2.
:- data v_lowlink/2.

:- data index/1.

set_index(N) :-
	retractall_fact(index(_)),
	assertz_fact(index(N)).

inc_index :-
	retract_fact(index(N0)),
	N is N0 + 1,
	assertz_fact(index(N)).

set_v_index(V, N) :-
	retractall_fact(v_index(V, _)),
	assertz_fact(v_index(V, N)).

set_v_lowlink(V, N) :-
	retractall_fact(v_lowlink(V, _)),
	assertz_fact(v_lowlink(V, N)).

% Update lowlink of V, if Val is smaller
set_v_lowlink_min(V, Val) :-
	v_lowlink(V, VLowLink),
	min(VLowLink, Val, Min),
	set_v_lowlink(V, Min).

s_push(V) :-
	asserta_fact(s(V)).
s_pop(V) :-
	retract_fact(s(V0)), !, V = V0.

s_pop_until(V, Ws) :-
	findall(W, s_pop_until_(V, W), Ws).

s_pop_until_(V, W) :- repeat, s_pop(W), ( W = V -> ! ; true ).

% ---------------------------------------------------------------------------
% (A test for Tarjan)
% TODO: move test somewhere else

% :- use_module(library(sort)).
% 
% % :- test test_tarjan/0.
% :- export(test_tarjan/0).
% test_tarjan :-
% 	find_sccs(test_v, test_e, SCCs),
% 	SCCs = [[f,g],[h,d,c],[e,b,a]].
% 
% test_v(a).
% test_v(b).
% test_v(c).
% test_v(d).
% test_v(e).
% test_v(f).
% test_v(g).
% test_v(h).
% test_e(a,b).
% test_e(b,c).
% test_e(b,e).
% test_e(b,f).
% test_e(c,d).
% test_e(c,g).
% test_e(d,c).
% test_e(d,h).
% test_e(e,a).
% test_e(e,f).
% test_e(f,g).
% test_e(g,f).
% test_e(h,d).
% test_e(h,g).

% ---------------------------------------------------------------------------
