:- module(native_props_rtc,
        [
            rtc_succeeds/1,
% sharing/aliasing, groundness:
	    rtc_mshare/2,
            rtc_covered/2,
            rtc_linear/1,
% determinacy:
            rtc_is_det/1,
            rtc_non_det/1,
% non-failure:
            rtc_not_fails/1,
            rtc_fails/1,
% more general cardinality, choicepoints, and exact solutions:
            rtc_num_solutions/2,
            rtc_relations/2,
            rtc_solutions/2,
            rtc_no_choicepoints/1,
            rtc_leaves_choicepoints/1,
% exceptions
            rtc_exception/2,
            rtc_exception/1,
            rtc_no_exception/2,
            rtc_no_exception/1,
% signals
            rtc_signal/2,
            rtc_signal/1,
            rtc_no_signal/2,
            rtc_no_signal/1,
% polyhedral constraints
            rtc_constraint/1,
% other
            rtc_user_output/2
        ],[assertions, hiord, datafacts]).

% (rtcheck implementation for native_props.pl)

:- use_module(engine(hiord_rt), [call/1]).
:- use_module(engine(internals),   ['$setarg'/4]).

:- use_module(library(sort),       [sort/2]).
:- use_module(library(lists),      [member/2, length/2, append/3]).
:- use_module(library(stream_utils), [open_output/2, close_output/1]).
:- use_module(library(stream_utils), [file_to_string/2]).
:- use_module(library(system),     [mktemp_in_tmp/2, delete_file/1]).
:- use_module(library(terms_vars), [term_variables/2,varsbag/3,varset/2]).
:- use_module(library(rtchecks/rtchecks_send), [send_comp_rtcheck/3]).



% ----------------------------------------------------------------------

:- meta_predicate rtc_succeeds(goal).

% rtcheck version for native_props:succeeds/1
rtc_succeeds(Goal) :- \+ \+ call(Goal). % processed in rtchecks_basic

% ----------------------------------------------------------------------

% rtc_mshare(Vs,Sh): list(int-term) * list(list(int)).
% During intrumentation we have assigned a unique identificator for
% each variable in Vs, since on runtime they could be
% indistinguishable, and we have sorted Sh lexicographically with
% respect to Vs (e.g [a,b,c] ->
% [[a],[a,b],[a,b,c],[a,c],[b],[b,c],[c]]). Now Sh uses those
% identificators instead of the original variables, and Vs keeps the
% correspondency between the variables and the identificators. Also,
% we assume that Sh does not contain the empty set

rtc_mshare(Vs,Sh) :-
	varset(Vs,Us),
	maplist_varset(Vs,Uss),
	check_mshare(Us,Vs,Uss,Sh).

check_mshare([U|Us],Vs,Uss,Sh) :-
	check_sharing_set(U,Vs,Uss,[[]|Sh],Uss2),
	check_mshare(Us,Vs,Uss2,Sh).
check_mshare([],_,_,_).

% check(U,Vs,Uss,Sh,Uss2): checks that the sharing set of U according
% to Vs appears in Sh. Sh is sorted lexicographically with respect to
% Vs, Uss are the sets of free variables appearing in Vs, and U is the
% first variable of all of those. Uss2 is Uss without U
check_sharing_set(U,[N-_V|Vs],[[U1|Us]|Uss],Sh,[Us|Uss2]) :-
	U==U1, !,
	split_sh(Sh,N,NewSh,_), NewSh\=[],
	check_sharing_set(U,Vs,Uss,NewSh,Uss2).
check_sharing_set(U,[N-_V|Vs],[Us|Uss],Sh,[Us|Uss2]) :-
	split_sh(Sh,N,_,NewSh),
	check_sharing_set(U,Vs,Uss,NewSh,Uss2).
check_sharing_set(_,[],[],[[]],[]).


% split(Sh,V,Sh1,Sh2): splits Sh in those sharing sets which contain
% V, without V, and those which not. Sh is sorted, and V is the first
% variable of Sh. The empy set is included in Sh
split_sh([],_,[],[]).
split_sh([[]|Vss],W,Vss1,[[]|Vss2]) :- !,
	split_sh(Vss,W,Vss1,Vss2).
split_sh([[V|Vs]|Vss],V,[Vs|Vss1],Vss2) :- !,
	split_sh(Vss,V,Vss1,Vss2).
split_sh(Vss,_V,[],Vss).


% for some reason, maplist/n doesn't work when called from
% instrumentation. Maybe it has something to do with the module
% expansion and non_inst (see T263).
maplist_varset([],[]).
maplist_varset([V|Vs],[Us|Uss]) :-
	varset(V,Us),
	maplist_varset(Vs,Uss).

% ----------------------------------------------------------------------

rtc_covered(X,Y) :-
	\+ \+ (numbervars(Y,0,_), ground(X)).

:- use_module(library(write), [numbervars/3]).

% TODO: It can be made more efficient (it is not strictly necessary to
% fully traverse Y)

% ----------------------------------------------------------------------

% rtcheck version for native_props:linear/1
rtc_linear(T) :-
	varsbag(T, VarsBag, []),
	sort(VarsBag, VarsSet),
	length(VarsBag, N),
	length(VarsSet, N).

% ----------------------------------------------------------------------

:- meta_predicate rtc_is_det(goal).

% rtcheck version for native_props:is_det/1
rtc_is_det(Goal) :-
	Solved = solved(no),
	Goal,
	(
	    arg(1, Solved, no)
	->
	    true
	;
	    send_comp_rtcheck(Goal, is_det, non_det)
	    % more than one solution!
	),
	'$setarg'(1, Solved, yes, true).

% ----------------------------------------------------------------------

:- meta_predicate rtc_non_det(goal).

% rtcheck version for native_props:non_det/1
rtc_non_det(Goal) :-
	Solved = solved(no),
	(
	    true
	;
	    arg(1, Solved, one) ->
	    send_comp_rtcheck(Goal, non_det, is_det),
	    fail
	),
	'$metachoice'(C0),
	Goal,
	'$metachoice'(C1),
	(
	    arg(1, Solved, no) ->
	    (
		C1 == C0 ->
		!,
		send_comp_rtcheck(Goal, non_det, no_choicepoints)
	    ;
		'$setarg'(1, Solved, one, true)
	    )
	;
	    '$setarg'(1, Solved, yes, true)
	).

% ----------------------------------------------------------------------

% OLD CODE
% :- meta_predicate not_fails( goal ).
%
% not_fails( X ) :-
% 	if( X , true , throw( rtcheck( nf , fail , X  ) ) ).

:- meta_predicate rtc_not_fails(goal).

% rtcheck version for native_props:not_fails/1
rtc_not_fails(Goal) :-
	Solved = solved(no),
	(
	    true
	;
	    arg(1, Solved, no) ->
	    send_comp_rtcheck(Goal, not_fails, fails),
	    fail
	),
	'$metachoice'(C0),
	no_exception_2(Goal, not_fails, _),
	'$metachoice'(C1),
	( C0 == C1 -> !
	; '$setarg'(1, Solved, yes, true) ).

% ----------------------------------------------------------------------

:- meta_predicate rtc_fails(goal).

% rtcheck version for native_props:fails/1
rtc_fails(Goal) :-
	Solved = solved(no),
	no_exception_2(Goal, fails, _),
	(
	    arg(1, Solved, no) ->
	    send_comp_rtcheck(Goal, fails, not_fails),
	    '$setarg'(1, Solved, yes, true)
	;
	    true
	).

% ----------------------------------------------------------------------

:- meta_predicate rtc_num_solutions(goal, addterm(pred(1))).

% rtcheck version for native_props:num_solutions/2
rtc_num_solutions(Goal, _, N) :-
	int(N),
	!,
	num_solutions_eq(Goal, N).
rtc_num_solutions(Goal, Check, Term) :-
	Sols = num_solutions(0),
	(
	    true
	;
	    arg(1, Sols, N0),
	    (
		Check(N0) -> fail
	    ;
		send_comp_rtcheck(Goal, num_solutions(Term),
		    num_solutions(N0)),
		fail
	    )
	),
	'$metachoice'(C0),
	call(Goal),
	'$metachoice'(C1),
	arg(1, Sols, N0),
	N1 is N0 + 1,
	(
	    C1 == C0 ->
	    !,
	    (
		Check(N1) -> true
	    ;
		send_comp_rtcheck(Goal, num_solutions(Term), num_solutions(N0))
	    )
	;
	    '$setarg'(1, Sols, N1, true)
	).

:- meta_predicate num_solutions_eq(goal, ?).

num_solutions_eq(Goal, N) :-
	Sols = solutions(0),
	(
	    true
	;
	    arg(1, Sols, A),
	    (
		(A == done ; A == N) -> fail
	    ;
		send_comp_rtcheck(Goal, num_solutions(N), Sols),
		fail
	    )
	),
	'$metachoice'(C0),
	call(Goal),
	'$metachoice'(C1),
	arg(1, Sols, A),
	(
	    A == done -> true
	;
	    N1 is A + 1,
	    (
		C1 == C0 ->
		!,
		(
		    N1 == N -> true
		;
		    send_comp_rtcheck(Goal, num_solutions(N),
			num_solutions(N1))
		)
	    ;
		(
		    N1 > N ->
		    send_comp_rtcheck(Goal, num_solutions(N),
			num_solutions('>'(N))),
		    '$setarg'(1, Sols, done, true)
		;
		    '$setarg'(1, Sols, N1, true)
		)
	    )
	).

% ----------------------------------------------------------------------

:- meta_predicate rtc_relations(goal, ?).

% rtcheck version for native_props:relations/2

rtc_relations(Goal, Term) :- rtc_num_solutions(Goal, Term).

% ----------------------------------------------------------------------

:- meta_predicate rtc_solutions(addterm(goal), ?).

% rtcheck version for native_props:solutions/2

rtc_solutions(Goal, Sol, Sols) :-
	Remaining = solutions(Sols),
	(
	    true
	;
	    arg(1, Remaining, Sols0),
	    (
		(Sols == done ; Sols0 == []) -> fail
	    ;
		append(Sols2, Sols0, Sols),
		send_comp_rtcheck(Goal, solutions(Sols), solutions(Sols2)),
		fail
	    )
	),
	'$metachoice'(C0),
	call(Goal),
	'$metachoice'(C1),
	arg(1, Remaining, Sols0),
	(
	    Sols0 == done -> true
	;
	    [Elem|Sols1] = Sols0,
	    (
		C1 == C0 ->
		!,
		(
		    Elem \= Sol ->
		    append(Curr, Sols0, Sols),
		    append(Curr, [Sol], Sols2),
		    send_comp_rtcheck(Goal, solutions(Sols), solutions(Sols2))
		;
		    true
		)
	    ;
		(
		    Elem \= Sol ->
		    append(Curr, Sols0,   Sols),
		    append(Curr, [Sol|_], Sols2),
		    send_comp_rtcheck(Goal, solutions(Sols), solutions(Sols2)),
		    '$setarg'(1, Remaining, done, true)
		;
		    '$setarg'(1, Remaining, Sols1, true)
		)
	    )
	).

% ----------------------------------------------------------------------

:- meta_predicate rtc_no_choicepoints(goal).

% rtcheck version for native_props:no_choicepoints/1
rtc_no_choicepoints(Goal) :-
	'$metachoice'(C0),
	Goal,
	'$metachoice'(C1),
	( C1 == C0 -> true
	; send_comp_rtcheck(Goal, no_choicepoints, leaves_choicepoints)
	).

% ----------------------------------------------------------------------

:- meta_predicate rtc_leaves_choicepoints(goal).

% rtcheck version for native_props:leaves_choicepoints/1
rtc_leaves_choicepoints(Goal) :-
	'$metachoice'(C0),
	Goal,
	'$metachoice'(C1),
	( C1 == C0 ->
	    send_comp_rtcheck(Goal, leaves_choicepoints, no_choicepoints)
	; true ).

% ----------------------------------------------------------------------

:- meta_predicate rtc_exception(goal, ?).

% rtcheck version for native_props:exception/2
rtc_exception(Goal, E) :-
	catch(Goal, F,
	    (
		( E \= F ->
		    send_comp_rtcheck(Goal, exception(E), exception(F))
		;
		    true
		),
		throw(F)
	    )),
	send_comp_rtcheck(Goal, exception(E), no_exception).

% ----------------------------------------------------------------------

:- meta_predicate rtc_exception(goal).

% rtcheck version for native_props:exception/1
rtc_exception(Goal) :-
	Goal,
	send_comp_rtcheck(Goal, exception, no_exception).

% ----------------------------------------------------------------------

:- meta_predicate rtc_no_exception(goal).

% rtcheck version for native_props:no_exception/1
rtc_no_exception(Goal) :- no_exception_2(Goal, no_exception, _).

:- meta_predicate rtc_no_exception(goal, ?).

% rtcheck version for native_props:no_exception/2
rtc_no_exception(Goal, E) :- no_exception_2(Goal, no_exception(E), E).

:- meta_predicate no_exception_2(goal, ?, ?).
no_exception_2(Goal, Prop, E) :-
	catch(Goal, E,
	    (
		send_comp_rtcheck(Goal, Prop, exception(E)),
		throw(E)
	    )).

% ----------------------------------------------------------------------

:- meta_predicate rtc_signal(goal).

% rtcheck version for native_props:signal/1
rtc_signal(Goal) :- rtc_signal(Goal, _).

:- meta_predicate rtc_signal(goal, ?).

% rtcheck version for native_props:signal/2
rtc_signal(Goal, E) :-
	'$metachoice'(Choice),
	asserta_signal_check(Choice, Goal, E, yes),
	'$metachoice'(C0),
	intercept(Goal, E, (emit_signal(Choice, E), send_signal(E))),
	'$metachoice'(C1),
	retract_signal_check(Choice, Goal, E, yes),
	(C0 == C1 -> ! ; true).

% ----------------------------------------

:- meta_predicate rtc_no_signal(goal).

% rtcheck version for native_props:no_signal/1
rtc_no_signal(Goal) :- rtc_no_signal(Goal, _).

:- meta_predicate rtc_no_signal(goal, ?).

% rtcheck version for native_props:no_signal/2
rtc_no_signal(Goal, E) :-
	'$metachoice'(Choice),
	asserta_signal_check(Choice, Goal, E, no),
	'$metachoice'(C0),
	intercept(Goal, E, (emit_signal(Choice, E), send_signal(E))),
	'$metachoice'(C1),
	retract_signal_check(Choice, Goal, E, no),
	(C0 == C1 -> ! ; true).

% ----------------------------------------

:- data signal_db/3.

asserta_signal_check(Choice, _, E, _) :-
	asserta_fact(signal_db(Choice, no, E)).
asserta_signal_check(Choice, Goal, _, CheckThrown) :-
	end_signal_check(Choice, Goal, CheckThrown), fail.

retract_signal_check(Choice, Goal, _, CheckThrown) :-
	end_signal_check(Choice, Goal, CheckThrown).
retract_signal_check(Choice, _, E, _) :-
	asserta_fact(signal_db(Choice, no, E)),
	fail.

signal_prop(yes, E, signal(yes, E), signal(no,  E)).
signal_prop(no,  E, signal(no,  E), signal(yes, E)).

end_signal_check(Choice, Goal, CheckThrown) :-
	retract_fact_nb(signal_db(Choice, Thrown, E)),
	signal_prop(CheckThrown, E, EP, EV),
	( Thrown = CheckThrown -> true
	; send_comp_rtcheck(Goal, EP, EV)
	).

emit_signal(Choice, E) :-
	retract_fact_nb(signal_db(Choice, _, _)),
	assertz_fact(signal_db(Choice, yes, E)).

% ----------------------------------------------------------------------
:- meta_predicate rtc_contraint(list(goal)).

rtc_constraint([]).
rtc_constraint([C|Cs]) :-
        check_if_valid(C),
        rtc_constraint(Cs).

% copied from native_props.pl --NS
check_if_valid(=(Lin_Expr1, Lin_Expr2)) :-
	lin_expr(Lin_Expr1),
	lin_expr(Lin_Expr2),
        Lin_Expr1 =:= Lin_Expr2.
check_if_valid(=<(Lin_Expr1, Lin_Expr2)) :-
	lin_expr(Lin_Expr1),
	lin_expr(Lin_Expr2),
        Lin_Expr1 =< Lin_Expr2.
check_if_valid(>=(Lin_Expr1, Lin_Expr2)) :-
	lin_expr(Lin_Expr1),
	lin_expr(Lin_Expr2),
        Lin_Expr1 >= Lin_Expr2.
check_if_valid(<(Lin_Expr1, Lin_Expr2)) :-
	lin_expr(Lin_Expr1),
	lin_expr(Lin_Expr2),
        Lin_Expr1 < Lin_Expr2.
check_if_valid(>(Lin_Expr1, Lin_Expr2)) :-
	lin_expr(Lin_Expr1),
	lin_expr(Lin_Expr2),
        Lin_Expr1 > Lin_Expr2.

lin_expr(PPL_Var) :-
	ppl_var(PPL_Var), !.
lin_expr(Coeff) :-
	coefficient(Coeff).
lin_expr(+(Lin_Expr)) :-
	lin_expr(Lin_Expr).
lin_expr(-(Lin_Expr)) :-
	lin_expr(Lin_Expr).
lin_expr(+(Lin_Expr1, Lin_Expr2)) :-
	lin_expr(Lin_Expr1),
	lin_expr(Lin_Expr2).
lin_expr(-(Lin_Expr1, Lin_Expr2)) :-
	lin_expr(Lin_Expr1),
	lin_expr(Lin_Expr2).
lin_expr(*(Coeff, Lin_Expr)) :-
	coefficient(Coeff),
	lin_expr(Lin_Expr).
lin_expr(*(Lin_Expr, Coeff)) :-
	coefficient(Coeff),
	lin_expr(Lin_Expr).

ppl_var(Var) :-
	var(Var).

coefficient(Coeff) :-
	ground(Coeff),
	int(Coeff). % TODO: shouldn't it be a num/1?

% ----------------------------------------------------------------------

:- use_module(library(stream_utils), [write_string/1]).

:- meta_predicate rtc_user_output(goal, ?).

% rtcheck version for native_props:user_output/1
rtc_user_output(Goal, S) :-
	'$metachoice'(Choice),
	mktemp_in_tmp('tmpciaoXXXXXX', FileName),
	asserta_user_output_check(Choice, FileName, Goal, S),
	'$metachoice'(C0),
	catch(Goal, E,
	    (end_output_check(Choice, FileName, Goal, S), throw(E))),
	'$metachoice'(C1),
	retract_user_output_check(Choice, FileName, Goal, S),
	(C0 == C1 -> ! ; true).

:- data output_db/2.

asserta_user_output_check(Choice, FileName, _, _) :-
	ini_output_check(Choice, FileName).
asserta_user_output_check(Choice, FileName, Goal, S) :-
	end_output_check(Choice, FileName, Goal, S), fail.

retract_user_output_check(Choice, FileName, Goal, S) :-
	end_output_check(Choice, FileName, Goal, S).
retract_user_output_check(Choice, FileName, _, _) :-
	ini_output_check(Choice, FileName), fail.

ini_output_check(Choice, FileName) :-
	open_output(FileName, SO),
	assertz_fact(output_db(Choice, SO)),
	!.

end_output_check(Choice, FileName, Goal, S) :-
	retract_fact(output_db(Choice, SO)),
	close_output(SO),
	file_to_string(FileName, S1),
	delete_file(FileName),
	write_string(S1),
	(
	    S \== S1 ->
	    send_comp_rtcheck(Goal, user_output(S), user_output(S1))
	;
	    true
	),
	!.

%%%%%%%%%%%%%%
%%%% This one is in the testing library (unittest)
%% :- prop user_error(Goal, S) #
%% 	"Calls of the form @var{Goal} write @var{S} to standard error.".
%% 
%% :- meta_predicate user_error(goal, ?).
%% user_error(Goal, S) :-
%% 	mktemp_in_tmp('tmpciaoXXXXXX', FileName),
%% 	open_error(FileName, SO),
%% 	call(Goal),
%% 	close_error(SO),
%% 	file_to_string(FileName, S1),
%% 	write_string(S1),
%% 	(
%% 	    S \== S1 ->
%% 	    send_comp_rtcheck(Goal, user_error(S), user_error(S1))
%% 	;
%% 	    true
%% 	).
%% 
%%%%%%%%%%%%%%



