:- module(argnames_trans, [argnames_def/3, argnames_use/3, argnames_goal/3], [assertions, dcg]).

:- use_module(library(terms), [arg/2]).
:- use_module(library(lists), [sequence_to_list/2, select/3]).
:- use_module(library(messages)).
:- use_module(library(compiler/c_itf_internal), [location/3]).
:- use_module(library(aggregates)).

% argnames(Functor, Arity, Str, Module)
:- data argnames/4.

% runtime argnames: emit runtime predicate
% rt_argnames(Functor, Pred, Module)
:- data rt_argnames/3. 

argnames_def(0, _, M) :- % initialization
        retractall_fact(argnames(_,_,_,M)),
        retractall_fact(rt_argnames(_,_,M)).
argnames_def((:- argnames(R)), _, M) :-
        functor(R, F, N),
        add_argnames_def(R, F, N, M).
argnames_def((:- data(R)), (:- data(F/N)), M) :-
        functor(R, F, N),
        (F,N) \== ('/',2),
        (F,N) \== (',',2),
        add_argnames_def(R, F, N, M).
% catch the end of file, introduce runtime data
argnames_def(end_of_file, Ys, M) :- !,
	findall((Str, ArgGetName), rt_argnames(Str, ArgGetName, M), Xs),
	emit_runtime_info(Xs, Ys, [end_of_file]).
argnames_def((:- _), _, _) :- !, fail.
argnames_def((?- _), _, _) :- !, fail.
argnames_def((Head :- Body), (NewHead :- NewBody), Mod) :- !,
        argnames_head(Head, Mod, NewHead, NewBody0, []),
	( NewBody0 = [] ->
	    NewBody = Body
	; list_to_conjs(NewBody0, NewBody1),
	  NewBody = (Body, NewBody1)
	),
	Head \== NewHead.
argnames_def(Head, (NewHead :- NewBody), Mod) :- !,
        argnames_head(Head, Mod, NewHead, NewBody0, []),
	list_to_conjs(NewBody0, NewBody),
	Head \== NewHead.

list_to_conjs([], true) :- !.
list_to_conjs([X], X) :- !.
list_to_conjs([X|Xs], (X, Ys)) :-
	list_to_conjs(Xs, Ys).

argnames_head(Head, Mod, NewHead2, Xs, Ys) :-
	argnames_exp(Head, Mod, NewHead, Xs0, Ys),
	( var(NewHead) ->
	    Head = $(Str, _),
	    argnames(Str, Arity, _, _),
	    functor(Head2, Str, Arity),
	    Xs = [Head2 = NewHead|Xs0],
	    NewHead2 = Head2
	; Xs = Xs0,
	  NewHead2 = NewHead
	).

current_location(Loc) :-
	location(S, L0, L1),
	Loc = loc(S, L0, L1),
	!.
current_location(_).

add_argnames_def(R, F, N, M) :-
        ( argnames(F, _, R0, M)  ->
            ( R0 == R -> true
            ; current_location(Loc),
	      error_message(Loc, "incompatible argnames declaration ~w",[R])
            )
        ; arg(R, A), \+ atomic(A) ->
          current_location(Loc),
	  error_message(Loc, "invalid argnames declaration ~w",[R])
        ; assertz_fact(argnames(F,N,R,M))
        ).

argnames_use($(F,TheArgs), T, M) :-
        atom(F),
        argnames_the_args(TheArgs, Args),
	!,
        argnames_trans(Args, F, M, T).
argnames_use($~(V, $(F,TheArgs), NV), T, _M) :- % A goal
        atom(F), !,
        T = $~(V, F, TheArgs, NV).
argnames_use($~(V, $(F,TheArgs)), T, _M) :- % A function call
        atom(F), !,     
        T = $~(V, F, TheArgs).

argnames_trans(argnames, F, M, T) :-
        argnames(F, _, Def, M), !,
	Def =.. [_|T].
argnames_trans((/), F, M, T) :-
        argnames(F, A, _, M),
        T = F/A, !.
argnames_trans(Args, F, M, T) :-
	ground_argnames(Args),
        argnames(F, A, R, M),
        functor(T, F, A),
        insert_args(Args, R, A, T), !.
% BUG: rt_argnames are asserted AFTER end_of_file or 0 (executed by goal expansion)
% So we need this null term expansion
% todo: improve!
argnames_trans(Args, F, M, _T) :-
	\+ ground_argnames(Args),
	!,
	register_rt_info(M, F, _, _, _, _),
	fail.
argnames_trans(Args, F, _, _) :-
        argnames_the_args(TheArgs, Args), !,
	current_location(Loc),
	warning_message(Loc, "Invalid argnames `~w$~w', - not translated", [F, TheArgs]),
        fail.

insert_args([], _, _, _).
insert_args('=>'(F,A), R, N, T) :-
        insert_arg(N, F, A, R, T).
insert_args(('=>'(F,A), As), R, N, T) :-
        insert_arg(N, F, A, R, T),
        insert_args(As, R, N, T).

insert_arg(N, F, A, R, T) :-
        N > 0, !,
        (   arg(N, R, F) ->
                arg(N, T, A)
        ;   N1 is N-1,
            insert_arg(N1, F, A, R, T)
        ).
insert_arg(0, F,_A, R,_T) :-
	current_location(Loc),
        error_message(Loc,
	    "Argname ~w invalid for defined argnames ~w", [F, R]),
        fail.

argnames_the_args({}, []).
argnames_the_args({Args}, Args).

% todo: cannot appear runtime arguments of $~
argnames_goal($~(V, F, TheArgs, NV), NewGoal, M) :- !,
	NewGoal = (V = VT, NV = NVT),
        argnames_list(TheArgs, Args),
        argnames(F, A, R, M),
        functor(VT, F, A),
        functor(NVT, F, A),
        set_or_copy_argnames(A, R, Args, VT, NVT).
argnames_goal(Goal, NewGoal, Mod) :- !,
        argnames_exp(Goal, Mod, Goal1, NewGoal0, [Goal1]),
	list_to_conjs(NewGoal0, NewGoal),
	NewGoal \== Goal.

set_or_copy_argnames(0, R, Args,_VT,_NVT) :- !,
        ( Args \== [] ->
            get_names(Args, AN),
	    current_location(Loc),
            error_message(Loc, "Invalid argname(s) `~w' for defined argnames `~w'", [AN, R])
        ;
            true
        ).
set_or_copy_argnames(A, R, Args, VT, NVT) :-
        arg(A, R, ArgName),
        ( select((ArgName => Value), Args, RArgs), !
        ; RArgs = Args,
          arg(A, VT, Value)
        ),
        arg(A, NVT, Value),
        A1 is A-1,
        set_or_copy_argnames(A1, R, RArgs, VT, NVT).

argnames_list({}, []).
argnames_list({Args}, List) :-
        sequence_to_list(Args, List).

get_names([], []).
get_names([N => _|As], [N|Ns]) :-
        get_names(As, Ns).

% emits runtime info predicates for each (Str, ArgGetName) pair in the list
emit_runtime_info([]) --> [].
emit_runtime_info([X|Xs]) --> emit_runtime_info__2(X), emit_runtime_info(Xs).

emit_runtime_info__2((Str, ArgGetName)) -->
	{ argnames(Str, _, Def, _), functor(Def, _, Arity) },
	emit_runtime_info__3(1, Arity, Def, ArgGetName, Str).

% emits the relation argname/argnumber
emit_runtime_info__3(I, Arity, _, _, _) -->
	{ I > Arity }, !.
emit_runtime_info__3(I, Arity, Def, ArgGetName, Str) -->
	{ arg(I, Def, ArgName) },
	{ functor(T, Str, Arity) }, 
	{ arg(I, T, A) },
	{ arg_get_head(ArgGetName, ArgName, T, A, ArgGet) },
	[ArgGet], % ArgGetName(ArgName, T, A).
	{ I1 is I + 1 },
	emit_runtime_info__3(I1, Arity, Def, ArgGetName, Str).

% get the runtime info goal and remember to emit the predicate definition
register_rt_info(M, Str, ArgName, T, A, ArgGet) :-
	( current_fact(rt_argnames(Str, ArgGetName, M)) ->
	    true
	; atom_concat('$argnames_runtime_info_', Str, ArgGetName),
	  assertz_fact(rt_argnames(Str, ArgGetName, M))
	),
	arg_get_head(ArgGetName, ArgName, T, A, ArgGet).

% the head of a argget predicate
arg_get_head(ArgGetName, ArgName, Term, Arg, ArgGet) :-
	functor(ArgGet, ArgGetName, 3),
	arg(1, ArgGet, ArgName),
	arg(2, ArgGet, Term),
	arg(3, ArgGet, Arg).

% expansion of goal or structure arguments
argnames_args(0, _, _, _) --> !.
argnames_args(N, T0, Mod, T1) --> !,
        { arg(N, T0, A0) },
        { arg(N, T1, A1) },
        { N1 is N-1 },
        argnames_exp(A0, Mod, A1),
        argnames_args(N1, T0, Mod, T1).

argnames_exp(V,_Mod, V) --> { var(V) }, !.
argnames_exp($(Str,TheArgs), Mod, T) -->
	{ atom(Str), nonvar(TheArgs) },
	{ argnames_the_args(TheArgs, Args) },
	argnames_exp_2(Str, Args, T, Mod), !.
argnames_exp(T0, Mod, T1) -->
        { functor(T0, F, A) },
        { functor(T1, F, A) },
        argnames_args(A, T0, Mod, T1).

argnames_exp_2(Str, Args, T, M) -->
        { 
	  argnames(Str, A, Def, M),
	  functor(T2, Str, A)
	},
	( { ground_argnames(Args) } -> % simply replace the term
	    { T = T2 }
	; [T = T2]
	),
	assign_ground_argnames(Args, Def, A, T2, M),
        % insert a unification and a runtime arg for each nonvar arg
	assign_var_argnames(Args, A, T, Str, M),
	!.
argnames_exp_2(Str, Args, _, _) -->
        { argnames_the_args(TheArgs, Args) }, !,
	{ current_location(Loc) },
	{ warning_message(Loc, "Invalid argnames `~w$~w', - not translated", [Str, TheArgs]) },
	{ fail }.

ground_argnames([]).
ground_argnames('=>'(ArgName, _)) :- ground(ArgName).
ground_argnames((A, B)) :- ground_argnames(A), ground_argnames(B).

assign_ground_argnames([], _, _, _, _) --> [].
assign_ground_argnames('=>'(ArgName,A0), Def, N, T, M) -->
        argnames_exp(A0, M, A),
        assign_ground_argname(N, ArgName, A, Def, T, M).
assign_ground_argnames((A, B), Def, N, T, M) -->
        assign_ground_argnames(A, Def, N, T, M),
        assign_ground_argnames(B, Def, N, T, M).

% search the argname in def
% todo: Check how p${a => 1, a => 2} behave
assign_ground_argname(N, ArgName, A, Def, T, M) --> { ground(ArgName) }, !,
        { N > 0 }, 
        ( { arg(N, Def, ArgName) } ->
            { arg(N, T, A) }
        ; { N1 is N-1 },
          assign_ground_argname(N1, ArgName, A, Def, T, M)
        ).
assign_ground_argname(_, _, _, _, _, _) --> [].

assign_var_argnames([], _, _, _, _) --> [].
assign_var_argnames('=>'(ArgName,A0), N, T, Str, M) --> 
        argnames_exp(A0, M, A),
        assign_var_argname(N, ArgName, A, T, Str, M).
assign_var_argnames((A, B), N, T, Str, M) -->
        assign_var_argnames(A, N, T, Str, M),
        assign_var_argnames(B, N, T, Str, M).

% insert a runtime argname search
assign_var_argname(_N, ArgName, A, T, Str, M) --> { var(ArgName) }, !,
	{ register_rt_info(M, Str, ArgName, T, A, ArgGet) },
	[ArgGet].
assign_var_argname(_, _, _, _, _, _) --> [].

/********************************
  Example translations :

:- argnames person(name, age, profession).

p(person${}).
q(person${age=> 25}).
r(person${name=> D, profession=>prof(D),age=>age(D)}).
s(person${age=>t(25), name=> daniel}).

% argnames(person, 3, person(name,age,profession)).
% 
% p(person(_,_,_)).
% q(person(_,25,_)).
% r(person(A,age(A),prof(A))).
% s(person(daniel,t(25),_)).

********************************/
