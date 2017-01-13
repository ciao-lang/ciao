:- module(pp_tr, [], []).

:- use_module(library(process)).
:- use_module(library(lists), [append/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(system), [current_executable/1]).
:- use_module(library(pathnames), [path_basename/2]).

% trace(X) :- display(X), nl.
trace(_).

:- data pp_load/1.
:- data pp_cmd/2.
:- data pp_opt/2.
:- data pp_post/2.

cleanup(M) :-
	retractall_fact(pp_load(M)),
	retractall_fact(pp_cmd(_, M)),
	retractall_fact(pp_opt(_, M)),
	retractall_fact(pp_post(_, M)).

% Called from CiaoPP?
% TODO: weak implementation...
:- data from_ciaopp_/1.
from_ciaopp(R) :-
	( from_ciaopp_(R0) ->
	    true
	; ( current_executable(Curr),
	    \+ path_basename(Curr, ciaopp) ->
              R0 = no
	  ; R0 = yes
	  ),
	  assertz_fact(from_ciaopp_(R0))
	),
	trace(from_ciaopp(R0)),
	R = R0.

:- export(pp_sent/3).
% TODO: perhaps pp_cmd/1 should be processed at a different priority level? (not a higher one)
pp_sent((:- pp_cmd(Cmd)), [], M) :- from_ciaopp(no), \+ pp_load(M), !,
	add_pp_cmd(Cmd, M).
pp_sent((:- pp_opt(Opt)), [], M) :- from_ciaopp(no), \+ pp_load(M), !,
	add_pp_opt(Opt, M).
pp_sent((:- pp_post(Cs)), [], M) :- from_ciaopp(no), \+ pp_load(M), !,
	add_pp_post(Cs, M).
pp_sent((:- pp_cmd(_)), [], _M) :- !.
pp_sent((:- pp_opt(_)), [], _M) :- !.
pp_sent((:- pp_post(_)), [], _M) :- !.
%
pp_sent((:- module(_A,_B,_C)), Cs, M) :- from_ciaopp(no), pp_load(M), !,
	% skip module/3, add prelude
	findall(C, get_pp_prelude(M, C), Cs).
pp_sent((H :- _:'$ctrt_nocall'(B)), Cs, M) :- from_ciaopp(no), pp_load(M), !,
	% remove '$ctrt_nocall' (added by transform(ctrt))
	Cs = [(H :- B)].
%
pp_sent(end_of_file, Cs, M) :- from_ciaopp(no), \+ pp_load(M), !,
	% Call CiaoPP
	% TODO: feed output!
	pp_module(M),
	assertz_fact(pp_load(M)),
	( pp_opt(output, M) ->
	    pp_output(M, M2)
	; M2 = M % load original file
	),
	append([(:- include(M2))], [end_of_file], Cs).
pp_sent(end_of_file, end_of_file, M) :- !,
	from_ciaopp(R),
	trace(doing_cleanup(M, R)),
	cleanup(M).
%
pp_sent(_, [], M) :- from_ciaopp(no), \+ pp_load(M), !,
	% Skip all sentences (we will feed from CiaoPP output)
	true.

add_pp_cmd(Cmd, M) :-
	assertz_fact(pp_cmd(Cmd, M)).

add_pp_opt(Opt, M) :-
	assertz_fact(pp_opt(Opt, M)).

add_pp_post(Cs, M) :-
	assertz_fact(pp_post(Cs, M)).

pp_module(M) :-
	trace(doing_pp(M)),
	findall(Cmd, get_pp_cmd(M, Cmd), Cmds),
	invoke_ciaopp_batch(Cmds),
	trace(done_pp(M)).

get_pp_cmd(M, Cmd) :-
	( pp_opt(auto_check_assert, M) ->
	    Cmd = auto_check_assert(M)
	; Cmd = module(M)
	).
get_pp_cmd(M, Cmd) :-
	pp_cmd(Cmd, M).
get_pp_cmd(M, Cmd) :-
	pp_opt(output, M),
	pp_output(M, M2),
	Cmd = output(M2).

%get_pp_prelude(M, C) :-
%	pp_opt(output, M),
%	C = (:- impl_defined(mshare/1)). % TODO: is this missing in nativeprops package?
get_pp_prelude(M, C) :-
	pp_opt(rtchecks, M),
	( C = (:- use_package(rtchecks))
	% ; C = (:- use_package(library(rtchecks/rtchecks_rt_inline))) % TODO: does not work?
	; C = (:- use_package(library(rtchecks/rtchecks_rt_library)))
	).
get_pp_prelude(M, C) :- % all clauses in pp_post
	pp_post(Cs, M),
	member(C, Cs).

% (do not use _co.pl, it will have problems with incremental
% compilation as the file is loaded implicitly by ciaoc)
pp_output(M, M2) :-
	% (e.g., foo.pl ==> foo.pp.pl)
	atom_concat(M, '.pp', M2).
 
% TODO: keep ciaopp alive for larger compilations?
% TODO: (cloned from builder code)
%:- export(invoke_ciaopp_batch/1).
% Batch execution of queries using ciaopp
invoke_ciaopp_batch(Cmds) :-
	process_call(path(ciaopp),
	       ['-T', '-q', '-f'],
	       [stdin(terms(Cmds))]).

