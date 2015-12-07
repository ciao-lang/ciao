:- module(pl2wam, [
	reset_counter/1, % jf-new
	set_compiler_mode_out/2, % EFM
        set_compiler_mode/1,
        set_compiler_out/1,
        compile_clause/2,
        compile_clause_in_mode/3,
        proc_declaration/4,
        proc_declaration_in_mode/5,
        cleanup_compilation_data/0
                 ], [assertions,nortchecks,dcg]).

:- use_module(library(iso_misc), [compound/1]).
:- use_module(library(sort)).
:- use_module(library(dict)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(write), [portray_clause/2]). % for wam output
:- use_module(library(lists), 
        [append/3, length/2, dlist/3, list_lookup/3,
         contains1/2, nocontainsx/2, contains_ro/2, nonsingle/1,
         intset_insert/3, intset_delete/3, intset_in/2, intset_sequence/3]).
:- use_module(engine(internals), 
        [poversion/1, '$make_bytecode_object'/4, '$compiled_clause'/4,
         '$interpreted_clause'/2, '$set_property'/2, '$define_predicate'/2]).

:- use_module(library(compiler/pl2wam_tables)).

% TODO: temporarily imported (so that incore_mode_of/2 can be updated
%   for subdefs -- incore_mode_of/2 should be in internals)
:- use_module(library(compiler/c_itf_internal), [incore_mode_of/2]).

:- set_prolog_flag(multi_arity_warnings, off).

/*** B_GAUGE
:- include(gauge_aux).
E_GAUGE ***/

% CIAO low-level compiler
%


% The overall structure of the compiler is a repeat-fail loop that compiles
% a clause at a time, emits the object code, and records indexing information.
% At EOF, indexing code is emitted.  



% The following instructions are emitted by the compiler.
% Some of the file formats use a different syntax.

%		choice;
%		switch_on_term((Clause)*, (Clause)*, (Key-Clause)*, (Clause)*);
%			       %Var       %List      %Other         %Default
%
%		true(Integer);
%		call(PredLabel,Integer);
%		execute(PredLabel);
%		proceed;
%		fail;
%		builtin_1(Name,Arg1);
%		builtin_2(Name,Arg1,Arg2);
%		builtin_3(Name,Arg1,Arg2,Arg3);
%		function_1(Name,Value,Arg1,N,EffAr);
%		function_2(Name,Value,Arg1,Arg2,N,EffAr);
%		put_x_variable(Arg,Arg);
%		put_y_variable(Arg,Arg);	% not after first call
%	/******	put_y_first_value(Arg,Arg);	% after first call ******/
%		put_x_value(Arg,Arg);
%		put_y_value(Arg,Arg);
%		put_x_unsafe_value(Arg,Arg);
%		put_y_unsafe_value(Arg,Arg);
%		put_constant(Atomic,Arg);
%		put_structure(Functor,Arg);
%		put_nil(Arg);
%		put_list(Arg);
%
%		get_x_variable(Arg,Arg);
%		get_y_variable(Arg,Arg);	% not after first call
%		get_y_first_value(Arg,Arg);	% after first call
%		get_x_value(Arg,Arg);
%		get_y_value(Arg,Arg);
%		get_constant(Atomic,Arg);
%		get_structure(Functor,Arg);
%		get_nil(Arg);
%		get_list(Arg);
%		get_constant_x0(Atomic);
%		get_structure_x0(Functor);
%		get_nil_x0;
%		get_list_x0;
%
%		unify_void;
%		unify_x_variable(Arg);
%		unify_y_variable(Arg);		% not after first call
%		unify_y_first_value(Arg);	% after first call
%		unify_x_value(Arg);
%		unify_y_value(Arg);
%		unify_x_local_value(Arg);
%		unify_y_local_value(Arg);
%		unify_constant(Atomic);
%		unify_structure(Functor);
%		unify_nil;
%		unify_list;
%
%		allocate;
%		deallocate;
%		init(L);
%		neck(N);			% create/update choicepoint
%		choice_x(Arg);
%		choice_y(Arg);
%		cutb;				% before allocate
%		cute;				% after allocate before init
%		cutf;				% after init
%		cutb_x(Arg);			% before allocate
%		cute_x(Arg);			% after allocate before init
%		cutf_x(Arg);			% after init
%		cut_y(Arg);
%
%		heapmargin_call(N, EffAr)

% HOW TO ADD A NEW KERNEL PREDICATE AND ITS INSTRUCTION:
%
% 1. Add a unit clause to inline_codable/1 and open_code/5, 
%    saying how to compile goal.
% 2. Add a clause to x_def_use_heap/3 to tell which temporaries the instruction
%    defines and uses and how much heap it requires.

% EMULATOR INTERFACE
% A bit mask is computed for each clause.  The bits are:
%	2'0000001	X0 may be var
%	2'0000010	X0 may be number
%	2'0000100	X0 may be atom
%	2'0001000	X0 may be list
%	2'0010000	X0 may be structure
%	2'0100000	this clause does an implicit cut
%	2'1000000	this clause contains an explicit cut

%-----------------------------------------------------------------------------

:- data compiler_out/1,   % Stream to which is written the po
        compiler_mode/1,  % Compiling in MODE, one of:
                          % {wam, ql(Profiling), incore(Profiling),
                          %  incoreql(Profiling)} with Profiling one of
                          % {profiled, unprofiled}
	'$compiled'/3,    % HEAD has a clause CLAUSE_NAME with DATA
	'$predicate'/3,   % HEAD which is F/A has the list of properties L
	'$qlval'/2.

% these are defined only when changing engine data structures
:- data new_x_offset/1, new_y_offset/1.

%new_x_offset(37).

% jf-new
reset_counter(Module0) :-
	retractall_fact(counter(_)),
	retractall_fact(counter_module(_)),
	asserta_fact(counter(0)),
	( Module0 = user(Base) ->
	    atom_concat('user$$$', Base, Module)
	; Module0 = Module
	),
	asserta_fact(counter_module(Module)).

set_compiler_mode(Mode) :- asserta_fact(compiler_mode(Mode)).

set_compiler_out(Out) :-
        asserta_fact(compiler_out(Out)),
        ql_emit_version(Out).

set_compiler_mode_out(Mode, Out) :-
	set_compiler_mode(Mode),
	asserta_fact(compiler_out(Out)),
	ql_emit_mode_version(Mode, Out).

compile_clause(H, B) :-
        functor(H, F, A),
        current_fact('$predicate'(H,F/A,Props)),
        (contains1(Props, (dynamic)) ; contains1(Props, (concurrent))), !,
        compile_file_emit(clause(F/A, (H:-B))).
compile_clause(H, B) :-
        define_predicate_mode(Profiling),
        compile_all(H, B, Profiling).

compile_clause_in_mode(Mode, H, B) :-
        asserta_fact(compiler_mode(Mode), Ref),
        compile_clause(H, B),
        erase(Ref).

define_predicate_mode(unprofiled) :-
	compiler_mode(wam),
	!.
define_predicate_mode(M) :-
	compiler_mode(Mode),
	!,
        arg(1, Mode, M).


proc_declaration(Decl, P, F, A) :-
        \+ (Decl = multifile, current_fact('$predicate'(P,F/A,_))),
        \+ current_fact('$compiled'(P,_,_)), !,
        compile_file_emit(set_property(P,Decl)).
proc_declaration(Decl, _P, F, A) :-
        message(warning, [Decl,' ',~~(F/A),' - declaration ignored']).

proc_declaration_in_mode(Mode, Decl, P, F, A) :-
        asserta_fact(compiler_mode(Mode), Ref),
        proc_declaration(Decl, P, F, A),
        erase(Ref).

cleanup_compilation_data :-
	retractall_fact(counter(_)), % jf-new
	retractall_fact(counter_module(_)), % jf-new
	retractall_fact(compiler_mode(_)),
	retractall_fact(compiler_out(_)),
	retractall_fact('$compiled'(_,_,_)),
	retractall_fact('$predicate'(_,_,_)),
	retractall_fact('$qlval'(_,_)).

:- include(wamql).
:- include(emulator_data).

compile_file_emit(Item) :-
	compiler_mode(Mode), !,
	compile_file_emit(Mode, Item).

compile_file_emit(ql(_), Item) :-
        compiler_out(S),
	ql_compile_file_emit(Item, S).
compile_file_emit(incore(_), Item) :-
	incore_compile_file_emit(Item).
compile_file_emit(incoreql(_), Item) :-
        compiler_out(S),
	incore_ql_compile_file_emit(Item, S).
compile_file_emit(wam, Item) :-
        compiler_out(S),
	wam_compile_file_emit(Item, S).

compile_all(H, B, Profiled) :-
	parse_clause(H, B, Parsed, Profiled),
	compile_internals(Parsed, Profiled, [ClName0-Data]),
	functor(H, Name, Ar),
	functor(F, Name, Ar),
	wam_clause_name(ClName0, ClName),
	asserta_fact('$compiled'(F,ClName,Data)), !.
compile_all(H, B, _) :-
        message(warning, [H,' :- ',B,' - clause failed to compile}']).

wam_clause_name(Name/Ar/0, Name/Ar/I) :-
	functor(F, Name, Ar),
	current_fact('$compiled'(F,Name/Ar/I0,_)), !,
	I is I0+1.
wam_clause_name(Name/Ar/0, Name/Ar/1) :- !.
% The next commented code is obsolete:
% wam_clause_name((Name0-Sub)/Ar/No, (Name-Sub)/Ar/No) :-
% 	wam_clause_name(Name0, Name).
wam_clause_name(SubName/Ar/No, SubName/Ar/No) :- !.
wam_clause_name(Name0, _Name) :-
	message(warning, [Name0, ' failed to get name}']),
	fail.

% get_key(N/Ar, Key) :- get_key(N, Ar, Key).

get_key(N/Ar/_-_, _, Key) :- !, get_key(N, Ar, Key).
get_key(N, Ar, Key) :- functor(Key, N, Ar).

get_clause_id(structure(Name,Args), No, Name/Ar/No) :-
	length(Args, Ar),
	compile_file_emit(declare_clause(Name/Ar,No)).

% This predicate unifies MangledName with an atom that represent the
% clause name.  Note that the Module parameter is not redundant (for
% example in multifile clauses or user prolog files.

mangle_clause(Module, Name/Ar/No, MangledName) :-
	atom_codes(Name, NameC),
	append(_ModuleC,":"||PredC,NameC),
	!,
	atom_codes(Pred, PredC),
	atom_number(ArA, Ar),
	atom_number(NoA, No),
	atom_concat([Module,':', Pred, '/', ArA, '/', NoA, '$$'], MangledName).

compile_internals([], _, []).
compile_internals([parsed_clause(Head,Body,Choice,No)|Cs], Profiled, [ClName-D|Ds]) :-
	get_clause_id(Head, No, ClName),
	compile_one_proc(Profiled, Head, Body, Choice, ClName, D),
	compile_internals(Cs, Profiled, Ds).

compile_one_proc(Profiled, Head, Body, Choice, ClName, Data) :-
	transform_clause(Head, Body, Choice, TypeKey0, Body1, ClName, Internals, []),
	compile_internals(Internals, Profiled, InternalData),
	(   doing_wam ->
	    index_internals(InternalData)
	;   true
	),
	profile_struct(Profiled, ProfileData, _, _, _),
	trans_clause(ProfileData, Body1, FinalCode, TypeKey0, TypeKey),
	compile_file_emit(clause(ClName,FinalCode,ProfileData,TypeKey,Data)),
	emit_models(ProfileData, ClName).

doing_wam :-
        compiler_mode(Mode), !, Mode = wam.

/*** B_GAUGE
%  Create a data structure to hold the information gathered by the compiler
%  for profiling.

profile_struct(profiled, profiled(ClauseModel,InsnModel,Counters),
               ClauseModel, InsnModel, Counters) :- !.
E_GAUGE ***/
profile_struct(unprofiled, unprofiled, [try(var)-1,try(indexed)-2], [], 2).

emit_models(unprofiled, _).
/*** B_GAUGE
emit_models(profiled(CntrModel,InsnModel,_Counters), ClName) :-
	compile_file_emit(clause_model(ClName, CntrModel)),
	compile_file_emit(instruction_model(ClName, InsnModel)).
E_GAUGE ***/


%-----------------------------------------------------------------------------
% Make second-level structures, open up disjunctions, take care of cut.

% safe_structure(structure(F,Args), S) :- atom(F), S=..[F|Args].

structure(structure(F,Args), S) :- S=..[F|Args].

parse_clause(H, B, [parsed_clause(structure(Fu,Args),Pbody,Ch1,0)], Prof) :-
	functor(H, Fu, _),
	parse_body(B, outer, B1),
	(   Prof=unprofiled -> B1 = B2
/*** B_GAUGE
	;   insert_counters(B1, B2)
E_GAUGE ***/
	),
	trans_args(1, H, Args, Dic),
	trans_term(Ch, Ch1, Dic),
	trans_goal(B2, Pbody, 'basiccontrol:CUT IDIOM'(Ch), Dic).

parse_body(P, _, 'hiord_rt:call'(P)) :- var(P), !.
%parse_body('aggregates:^'(V,P0), _, 'aggregates:^'(V,P)) :- !,
%	parse_body(P0, outer, P).
parse_body('basiccontrol:false', _, 'basiccontrol:fail') :- !.
parse_body('basiccontrol:otherwise', _, 'basiccontrol:true') :- !.
parse_body('basiccontrol:;'(P0,Q0), _, 'basiccontrol:;'(P,Q)) :- !,
	parse_body(P0, inner, P),
	parse_body(Q0, inner, Q).
parse_body('basiccontrol:->'(P0, Q0), outer, 'basiccontrol:;'('basiccontrol:->'(P, Q), 'basiccontrol:fail')) :- !,
	parse_body(P0, outer, P),
	parse_body(Q0, outer, Q).
parse_body('basiccontrol:->'(P0, Q0), inner, 'basiccontrol:->'(P, Q)) :- !,
	parse_body(P0, outer, P),
	parse_body(Q0, outer, Q).
parse_body('basiccontrol:\\+'(P0), _, P) :- !,
	parse_body(P0, outer, P1),
	parse_negate_goal(P1, P).
parse_body('basiccontrol:,'(P0,Q0), _, 'basiccontrol:,'(P,Q)) :- !,
	parse_body(P0, outer, P),
	parse_body(Q0, outer, Q).
parse_body('basiccontrol:if'(P0,Q0,R0), _,
	   'basiccontrol:,'('term_basic:='(Flag, [no]), 'basiccontrol:;'('basiccontrol:,'(P, 'basiccontrol:,'('basiccontrol:IF BUILTIN'(Flag), Q)), 'basiccontrol:,'('term_basic:='(Flag, [no]), R)))) :- !,
	parse_body(P0, outer, P),
	parse_body(Q0, outer, Q),
	parse_body(R0, outer, R).
parse_body(P, _, P).

parse_negate_goal('term_typing:var'(X), 'term_typing:nonvar'(X)) :- !.
parse_negate_goal('term_typing:nonvar'(X), 'term_typing:var'(X)) :- !.
parse_negate_goal('term_compare:=='(X,Y), 'term_compare:\\=='(X,Y)) :- !.
parse_negate_goal('term_compare:\\=='(X,Y), 'term_compare:=='(X,Y)) :- !.
parse_negate_goal('term_compare:@<'(X,Y), 'term_compare:@>='(X,Y)) :- !.
parse_negate_goal('term_compare:@>='(X,Y), 'term_compare:@<'(X,Y)) :- !.
parse_negate_goal('term_compare:@>'(X,Y), 'term_compare:@=<'(X,Y)) :- !.
parse_negate_goal('term_compare:@=<'(X,Y), 'term_compare:@>'(X,Y)) :- !.
parse_negate_goal(P, 'basiccontrol:;'('basiccontrol:->'(P, 'basiccontrol:fail'), 'basiccontrol:true')).

%trans_goal('aggregates:^'(_,Goal), S, Cut, Dic) :- !,
%	trans_goal(Goal, S, Cut, Dic).
trans_goal('basiccontrol:!', Tran, Cut, Dic) :- !,
	trans_term(Cut, Tran, Dic).
trans_goal('basiccontrol:;'(X,Y), S, Cut, Dic) :- !,
	structure(S, 'basiccontrol:;'(X1,Y1)),
	trans_goal(X, X1, Cut, Dic),
	trans_goal(Y, Y1, Cut, Dic).
trans_goal('basiccontrol:->'(X,Y), S, Cut, Dic) :- !,
	structure(S, 'basiccontrol:->'(X1,Y1)),
	trans_goal(X, X1, Cut, Dic),
	trans_goal(Y, Y1, Cut, Dic).
trans_goal('basiccontrol:,'(X,Y), S, Cut, Dic) :- !,
	structure(S, 'basiccontrol:,'(X1,Y1)),
	trans_goal(X, X1, Cut, Dic),
	trans_goal(Y, Y1, Cut, Dic).
trans_goal(X, structure(Fu,Args), _, Dic) :-
	functor(X, Fu, _),
	trans_args(1, X, Args, Dic).

trans_args(I, Term, [X1|Args], Dic) :-
        compound(Term),
	arg(I, Term, X), !,
	trans_term(X, X1, Dic),
	I1 is I+1, trans_args(I1, Term, Args, Dic).
trans_args(_, _, [], _).

trans_term(V, Tran, Dic) :-
	var(V), !,
	dic_lookup(Dic, V, Tran),
	Tran=var(_,_).
trans_term([X|Y], list(X1,Y1), Dic) :- !,
	trans_term(Y, Y1, Dic),
	trans_term(X, X1, Dic).
trans_term([], nil, _) :- !.
trans_term(C, Tran, _) :- 
	atomic(C), !,
	Tran=constant(C).
trans_term(X, structure(Fu,Args), Dic) :- 
	functor(X, Fu, _),
	trans_args(1, X, Args, Dic).

%-----------------------------------------------------------------------------
% Given a clause, break off separate predicates for special forms such as
% disjunctions, negations, implications.

transform_clause(Head, Body0, Choice, TypeKey, Body, ClName) -->
	{straight_goal(Body0, _, Head, type_key(2'11111,nohash),
	               TypeKey, Body1, [])},
	transform_clause_1(Head, Body1, Choice, Body, ClName).

transform_clause_1(Head, Body1, Choice, body(Choice,Head,Body2), _) -->
	{simple_body(Body1, Body2), !,
	 allocate_temps(Choice),
	 allocate_temps(Head),
	 allocate_temps_body(Body2)}.
transform_clause_1(Head, Body1, Choice, body(Choice,Head,Body2), ClName) -->
	{mk_occurrences_list([inline(list(Choice,Head))|Body1], 0, 0, List)},
	straight_body(Body1, List, Body2, ClName, 0, 1, 1),
	{allocate_vas(List, 0)}.
	    

simple_body([], []) :- !.
simple_body([Goal|Body0], [Goal|Body]) :-
	Goal = inline(_), !,
	simple_body(Body0, Body).
simple_body(Body0, Body) :-
    	Body0 = [pcall(S,_)],
	structure(S, Fu),
	(   Fu = 'basiccontrol:true' -> Body = []
	;   Fu = 'basiccontrol:;'(_, _) -> fail
	;   Body = Body0
	).

straight_goal('basiccontrol:,'(X1, X2), _, Flag, Head, TK0, TK) --> !,
        straight_goal(X1, Flag, Head, TK0, TK1),
	straight_goal(X2, Flag, Head, TK1, TK).
straight_goal('basiccontrol:CUT IDIOM'(X), S, Flag, structure(_, Args), TK0, TK) -->
        {var(Flag),
	 trivial_head(Args, []),
	 TK0 = type_key(Type0, Key),
	 Type is Type0\/2'0100000,
         Type =\= Type0,
	 TK = type_key(Type, Key)},
	(   {nocontainsx(Args, X)}
	;   straight_goal(Flag, inline(S))
	), !.
/*** B_GAUGE
straight_goal('PROFILE POINT'(_), S, _, _, TK, TK) -->
	!,
	straight_goal(_, inline(S)).
E_GAUGE ***/
straight_goal(Goal, S, Flag, structure(_, Args), TK0, TK) -->
        {var(Flag),
	 type_ck(Goal, Args, Type1, Auto),
	 TK0 = type_key(Type0, Key),
	 TK = type_key(Type, Key),
         Type0 =\= Type0\/2'0100000},
	(   {trivial_head(Args, []),
             Auto=1,
	     Type is Type0/\Type1}
	;   {inline_codable(Goal)},
	    {Type is Type0/\(Type1\/1)},
            straight_goal(Flag, inline(S))
	), !.
straight_goal(Goal, S, Flag, _, TK, TK) -->
	(   {inline_codable(Goal)},
	    straight_goal(Flag, inline(S))
	;   straight_goal(Flag, pcall(S,_))
        ), !.

straight_goal(S, Flag, Head, TK0, TK) -->
	{structure(S, Goal)},
	straight_goal(Goal, S, Flag, Head, TK0, TK).

straight_goal(flag, Item) --> [Item].

type_ck('term_typing:var'(X), [Y|_], 2'1, 1) :- X==Y.
type_ck('attributes:get_attribute'(X,_), [Y|_], 2'1, 0) :- X==Y.     % DMCAI -- ATTRVARS
type_ck('term_typing:nonvar'(X), [Y|_], 2'11110, 1) :- X==Y.
type_ck('term_typing:atom'(X), [Y|_], 2'100, 0) :- X==Y.
type_ck('term_typing:atomic'(X), [Y|_], 2'110, 0) :- X==Y.
type_ck('term_typing:number'(X), [Y|_], 2'10, 0) :- X==Y.
type_ck('basiccontrol:,'(S0,S1), Args, Code, Auto) :-
	structure(S0, F0), structure(S1, F1),
	type_ck(F0, Args, Code0, Auto0),
	type_ck(F1, Args, Code1, Auto1),
	Code is Code0/\Code1,
	Auto is Auto0/\Auto1.
/*** I_GAUGE
S_GAUGE ***/
type_ck('basiccontrol:;'(S0, S1), Args, Code, Auto) :-
	structure(S0, F0), structure(S1, F1),
	type_ck(F0, Args, Code0, Auto0),
	type_ck(F1, Args, Code1, Auto1),
	Code is Code0\/Code1,
	Auto is Auto0/\Auto1.
/*** F_GAUGE ***/

trivial_head([], _).
trivial_head([X|Xs], Seen) :-
	X=var(_,_),
	nocontainsx(Seen, X),
	trivial_head(Xs, [X|Seen]).

straight_body([], _, [], _, _, _, _) --> [].
straight_body([inline(Fu)|Gs], List,
	       [inline(Fu)|Gs1], ClName, Chn, Gn, SubNo) --> !,
	{Gn1 is Gn+1},
	straight_body(Gs, List, Gs1, ClName, Chn, Gn1, SubNo).
straight_body([pcall(S, Size)|Gs], List,
	      [pcall(S1, Size)|Gs1], ClName, Chn, Gn, SubNo) -->
	{structure(S, 'basiccontrol:;'(G1,G2)), !,
	 internal_name(ClName, SubName), % jf-new solve multifile bug
	 internal_predicate(List, Chn-Gn, SubName, S1), % jf-new solve multifile bug
%jf-old	 internal_predicate(List, Chn-Gn, ClName-SubNo, S1),
	 structure(G1, F1),
	 structure(G2, F2)},
	straight_body(F1, left, S1, 0, No),
	straight_body(F2, right, S1, No, _),
        {Chn1 is Chn+1},
	{SubNo1 is SubNo+1},
	straight_body(Gs, List, Gs1, ClName, Chn1, 0, SubNo1).
straight_body([Goal|Gs], List, [Goal|Gs1], ClName, Chn, _, SubNo) -->
	{Chn1 is Chn+1},
	straight_body(Gs, List, Gs1, ClName, Chn1, 0, SubNo).

% jf-new solve multifile bug (hmm, that's temporary code)
:- data counter/1. 
:- data counter_module/1.
internal_name(ClName,SubName) :- !,
	current_fact(counter_module(Module)),
	current_fact(counter(Counter)),
	Counter1 is Counter + 1,
	set_fact(counter(Counter1)),
	number_codes(Counter, CounterCodes),
	atom_codes(CounterAtom, CounterCodes),
	mangle_clause(Module, ClName, MangledName),
	atom_concat(MangledName, CounterAtom, SubName).

internal_predicate(List, Gn, SubName, structure(SubName, Shared)) :-
	linking_vars(List, Gn, Shared, []).

linking_vars(List, _) --> {var(List)}, !.
linking_vars([V-Occs|List], Gn) -->
	{nonsingle(Occs), contains_ro(Occs, Gn)}, !,
	[V],
	linking_vars(List, Gn).
linking_vars([_|List], Gn) -->
	linking_vars(List, Gn).

straight_body('basiccontrol:fail', right, _, No, No) --> !.
straight_body('basiccontrol:;'(G1,G2), right, Head, No0, No) --> !,
	{structure(G1, F1),
	 structure(G2, F2)},
	straight_body(F1, left, Head, No0, No1),
	straight_body(F2, right, Head, No1, No).
straight_body('basiccontrol:->'(If, Then), _, Head, No0, No) --> !,
	{No is No0+1},
	{copy_goal_and_head('basiccontrol:->'(If, Then), Head,
	 'basiccontrol:->'(If1, Then1), Head1),
	 structure(B1, 'basiccontrol:CUT IDIOM'(var(C1,C2))),
	 structure(B2, 'basiccontrol:,'(B1,Then1)),
	 structure(S2, 'basiccontrol:,'(If1,B2))},
	[parsed_clause(Head1, S2, var(C1,C2), No)].
straight_body(Goal, _, Head, No0, No) -->
	{No is No0+1},
	{structure(S, Goal)},
	{copy_goal_and_head(S, Head, S1, Head1)},
	[parsed_clause(Head1, S1, var(_,_), No)].

copy_goal_and_head(S, structure(F,L), S1, structure(F,L1)) :-
	copy_term(S-L, S1-L1).

%-----------------------------------------------------------------------------
% Compute environment sizes, allocate perm. variables.
mk_occurrences_list([], _, _, _).
mk_occurrences_list([inline(G)|Gs], Chn, Gn, List) :- !,
	Gn1 is Gn+1,
	mk_occurrences_list(Gs, Chn, Gn1, List),
	record_occurrences(G, Chn-Gn, List).
mk_occurrences_list([pcall(G,U)|Gs], Chn, Gn, List) :-
	Chn1 is Chn+1,
	mk_occurrences_list(Gs, Chn1, 0, List),
	add_last(List, size(U)),
	record_occurrences(G, Chn-Gn, List).

add_last(L, X) :- var(L), !, L = [X|_].
add_last([_|L], X) :- add_last(L, X).

record_occurrences(Var, Gn, D) :-
	Var=var(_,_),
	list_lookup(D, Var, Occs),
	contains1(Occs, Gn).
record_occurrences(list(X,Y), Gn, D) :-
	record_occurrences(X, Gn, D),
	record_occurrences(Y, Gn, D).
record_occurrences(structure(_,Args), Gn, D) :-
	record_occurrences_args(Args, Gn, D).
record_occurrences(nil, _, _).
record_occurrences(constant(_), _, _).

record_occurrences_args([], _, _).
record_occurrences_args([X|Xs], Gn, D) :-
	record_occurrences(X, Gn, D),
	record_occurrences_args(Xs, Gn, D).


allocate_temps_body([]).
allocate_temps_body([inline(S)|Goals]) :- !,
	allocate_temps(S),
	allocate_temps_body(Goals).
allocate_temps_body([pcall(S,_)|Goals]) :-
	allocate_temps(S),
	allocate_temps_body(Goals).

allocate_temps(nil).
allocate_temps(constant(_)).
allocate_temps(var(_,'x'(_))).
allocate_temps(list(X,Y)) :-
	allocate_temps(X),
	allocate_temps(Y).
allocate_temps(structure(_,Args)) :-
	allocate_temps_args(Args).

allocate_temps_args([]).
allocate_temps_args([X|Xs]) :-
	allocate_temps(X),
	allocate_temps_args(Xs).

allocate_vas([], _) :- !.
allocate_vas([size(N)|List], N) :- !,
	allocate_vas(List, N).
allocate_vas([var(_,Name)-Occs|List], N0) :-
	allocate_vas(Name, Occs, N0, N),
	allocate_vas(List, N).

allocate_vas('x'(_), Occs, N, N) :- single_chunk(Occs, _), !.
allocate_vas('y'(N0), _, N0, N) :- N is N0+1.

single_chunk([], _) :- !.
single_chunk([Chn-_|Occs], Chn) :-
	single_chunk(Occs, Chn).

%-----------------------------------------------------------------------------
% wam pretty printing: Compile all clauses of a pred and link them together.
/*
emit_all_predicates :-
	current_fact('$predicate'(_,Pred,Props), Ref),
	erase(Ref),
	nocontainsx(Props,(dynamic)),
        nocontainsx(Props,(concurrent)), 
	link_unit(Pred),
	fail.
emit_all_predicates.

link_unit(Pred) :-
	index_predicate(Pred, Clauses, Code, []),
	compile_file_emit(predicate(Pred,Clauses,Code)),
	!.
*/
index_internals([]) :- !.
index_internals(InternalData) :-
	index_internals(InternalData, Pred, Clauses, Residue, Code, []),
	compile_file_emit(predicate(Pred,Clauses,Code)),
	index_internals(Residue).
/*
index_predicate(Pred, Clauses) -->
	{get_key(Pred, F),
	 collect_clauses(F, Pred, no, Use, 1, Clauses)},
	index_collected_clauses(Use, Clauses).
*/
index_internals(Items, Pred, Clauses, Residue) -->
	{internal_clauses(Items, no, Use, Clauses, Pred, Residue)},
	index_collected_clauses(Use, Clauses).

index_collected_clauses(Use, Clauses) -->
	(   {Use=yes} -> [choice]
	;   []
	),
	[switch_on_term(Lvar, Llist, Lothers, Ldefault)],
	{appropriate_items(2'1, Clauses, VarItems),
	 appropriate_items(2'1000, Clauses, ListItems),
	 appropriate_items(2'10110, Clauses, OtherItems), 
	 items_clauses(VarItems, Lvar),
	 items_clauses(ListItems, Llist),
	 items_keys_defaults(OtherItems, Keys0, Ldefault),
	 sort(Keys0, Keys),
	 distribute_default_items(Keys, OtherItems, Lothers, [])}.

distribute_default_items([], _) --> [].
distribute_default_items([Key|Keys], Items) -->
	distribute_default(Items, Key),
	distribute_default_items(Keys, Items).

distribute_default([], _) --> [].
distribute_default([Clause-Data|Items], Key) -->
	{index_data(key, Data, Hash)},
	(   {Hash=nohash} -> [Key-Clause]
	;   {Hash=hash(Key)} -> [Key-Clause]
	;   []
	),
	distribute_default(Items, Key).

index_data(type, Data, Type) :-
	arg(1, Data, Type).
index_data(key, Data, X) :-
	arg(2, Data, Key),
	var(Key), !,
	X=nohash.
index_data(key, Data, X) :-
	arg(2, Data, Key),
	atomic(Key), !,
	X=hash(Key).
index_data(key, Data, hash(N/A)) :-
	arg(2, Data, Key),
	functor(Key, N, A).

items_keys_defaults([], [], []).
items_keys_defaults([C-Data|Items], Keys, [C|Cs]) :-
	index_data(key, Data, nohash), !,
	items_keys_defaults(Items, Keys, Cs).
items_keys_defaults([_-Data|Items], [Key|Keys], Cs) :-
	index_data(key, Data, hash(Key)),
	items_keys_defaults(Items, Keys, Cs).

internal_clauses([ClName-Data|Items], Use0, Use, [ClName-Data|Clauses], Pred, Residue) :-
	ClName = Pred/_, !,
	index_data(type, Data, Type),
	(   Type/\2'1000000 =:= 2'1000000 -> Use1=yes
        ;   Use1=Use0
        ),
	internal_clauses(Items, Use1, Use, Clauses, Pred, Residue).
internal_clauses(Items, Use, Use, [], _, Items).
/*
collect_clauses(F, Pred, Use0, Use, No, [Pred/No-Data|Cs]) :-
	compiled_clause(F, Pred, No, Data), !,
	index_data(type, Data, Type),
	(   Type/\2'1000000 =:= 2'1000000 -> Use1=yes
        ;   Use1=Use0
        ),
	No1 is No+1,
	collect_clauses(F, Pred, Use1, Use, No1, Cs).
collect_clauses(_, _, Use, Use, _, []).

compiled_clause(Func, Pred, No, Data) :-
	current_fact('$compiled'(Func,Pred/No,Data), Ref),
	erase(Ref).
*/
items_clauses([], []).
items_clauses([Clause-_|Items], [Clause|Clauses]) :-
	items_clauses(Items, Clauses).

% (I did't find any use of this - DCG)
% items_keyclauses_clauses([], [], []).
% items_keyclauses_clauses([Clause-Data|Items],
% 			 [Key-Clause|KeyClauses], Clauses) :-
%         index_data(key, Data, hash(Key)), !,
% 	items_keyclauses_clauses(Items, KeyClauses, Clauses).
% items_keyclauses_clauses([Clause-Data|Items],
% 			 KeyClauses, [Clause|Clauses]) :-
%         index_data(key, Data, nohash),
% 	items_keyclauses_clauses(Items, KeyClauses, Clauses).

appropriate_items(_, [], []) :- !.
appropriate_items(Type, [C|Cs], [C|Out]) :-
	appropriate_clause_item(Type, _, C), !,
	appropriate_items(Type, Cs, Out, C).
appropriate_items(Type, [_|Cs], Out) :-
	appropriate_items(Type, Cs, Out).

appropriate_items(Type, _, [], C) :- last_clause_for_key(C, Type), !.
appropriate_items(Type, Cs, Out, _) :- appropriate_items(Type, Cs, Out).

appropriate_clause_item(Type, Key, _-Data) :-
	index_data(type, Data, Type1),
	Type/\Type1>0,
	index_data(key, Data, Key).

last_clause_for_key(_-Data, _) :-
	index_data(type, Data, T),
	T =:= T\/2'0100000,
	index_data(key, Data, nohash).

% Translating a clause: Emit naive code, then extract  info. and delay
% 'get_variables', then allocate temps for deep and shallow cases,
% finally pho emitted code.

trans_clause(Profiled, Body, Code, TypeKey0, TypeKey) :-
	trans_clause_1(Profiled, Body, Kind, InArity, OutArity, Code0, []),
	extract_index(Code0, Code1, TypeKey0, TypeKey1, Gets, Gets, OutArity),
	clause_lifetime(Profiled, Kind, InArity, OutArity,
	                [ensure_space(Kind, InArity, _)|Code1], Live, Code2),
	trans_clause(Live, TypeKey1, TypeKey),
	peep_clause(Kind, Code2, Code3, []),
        peep_counters(Profiled, Code3, Code).

trans_clause([-1|_], type_key(Type0,Key), type_key(Type,Key)) :- !,
	Type is Type0\/2'1000000.
trans_clause(_, TypeKey, TypeKey).

/*** I_GAUGE
% peep_counters/3 defined in another file
S_GAUGE ***/
peep_counters(unprofiled,Code,Code).
/*** F_GAUGE ***/

/*** B_GAUGE
clause_lifetime(profiled(_,_,_), Kind, InArity, OutArity,
	        [Ensure, counted_neck(N,T,R)|Code], Live,
	        [ifshallow, profile_point(entry(shallow)), bump_counter, counted_neck(N,T,R), 
		 profile_point(head_sucess(shallow)),bump_counter,else, 
		 profile_point(entry(deep)),profile_point(head_sucess(deep)), bump_counter, endif, Ensure|Code]) :- !,
	guard_lifetime(Kind, InArity, OutArity, Live, [Ensure|Code], []).
E_GAUGE ***/
clause_lifetime(unprofiled, Kind, InArity, OutArity,
	        [Ensure, neck(N)|Code], Live,
	        [ifshallow, neck(N), else, endif, Ensure|Code]) :- !,
	guard_lifetime(Kind, InArity, OutArity, Live, [Ensure|Code], []).
/*** B_GAUGE
clause_lifetime(profiled(_,_,_), Kind, InArity, OutArity, [X,C|Code0], Live, Code) :-
	guard_and_body(C, 1, Guard1, G2, Body, Code0),
	copy_term(G2, Guard2),
	guard_lifetime(Kind, InArity, OutArity, Live, [X|Guard1], []),
	guard_lifetime(Kind, InArity, OutArity, _, [X|Guard2], []),
	body_lifetime(0, _, Body, []),
	compare_streams(Guard1, Guard2, Head1, [R1|Out1h], [R1|R1s], 
					Head2, Out2h, Rest2),
	merge_insn_tails(R1s, Rest2, 
			 Out1h, Out1t, Out2h, Out2t, Out3h, Body, _),
	merge_insn_streams(Out3h, Code, [X|Head1], Out1t, Head2, Out2t).
E_GAUGE ***/
clause_lifetime(unprofiled, Kind, InArity, OutArity, [X,C|Code0], Live, Code) :-
	Kind \== unit,
	guard_and_body(C, 0, Guard1, G2, Body, Code0), !,
	copy_term(G2, Guard2),
	guard_lifetime(Kind, InArity, OutArity, Live, [X|Guard1], []),
	guard_lifetime(Kind, InArity, OutArity, _, [X|Guard2], []),
	body_lifetime(0, _, Body, []),
	compare_streams(Guard1, Guard2, Head1, [R1|Out1h], [R1|R1s], 
					Head2, Out2h, Rest2),
	(   R1=neck(_) ->
	    dlist([X|Guard1], Code, Body)
	;   merge_insn_tails(R1s, Rest2, 
			     Out1h, Out1t, Out2h, Out2t, Out3h, Body, _),
	    merge_insn_streams(Out3h, Code, [X|Head1], Out1t, Head2, Out2t)
	).
clause_lifetime(unprofiled, Kind, InArity, OutArity, Code, Live, Code) :-
	guard_lifetime(Kind, InArity, OutArity, Live, Code, []).

compare_streams([X|Xs], [X|Ys], [X|P0], P1, P, [X|Q0], Q1, Q) :- !,
	compare_streams(Xs, Ys, P0, P1, P, Q0, Q1, Q).
compare_streams(Xs, Ys, P, P, Xs, Q, Q, Ys).


merge_insn_streams([I|Out3h], Out, Out1h, [I|Out1t], Out2h, [I|Out2t]) :-
	duplicatable_insn(I), !,
	merge_insn_streams(Out3h, Out, Out1h, Out1t, Out2h, Out2t).
/*** B_GAUGE
merge_insn_streams(Out3h,
   [ifshallow|Out1h], Out1h, 
   [profile_point(exit(shallow)),bump_counter,else|Out2h], Out2h, 
   [profile_point(exit(deep)),bump_counter,endif,profile_point(entry(body))|Out3h]) :- 
	Out1h = [_,profile_point(_)|_],
        !.
E_GAUGE ***/
merge_insn_streams(Out3h,
	           [ifshallow|Out1h], Out1h, [else|Out2h], Out2h, [endif|Out3h]).

duplicatable_insn(unify_x_variable(_)).
duplicatable_insn(unify_y_variable(_)).
duplicatable_insn(get_x_variable(_, _)).
duplicatable_insn(get_y_variable(_, _)).
duplicatable_insn(put_x_value(_, _)).
duplicatable_insn(put_y_value(_, _)).
duplicatable_insn(execute(_)).
/*** B_GAUGE
duplicatable_insn(profile_point(_)).
duplicatable_insn(bump_counter).
E_GAUGE ***/

% merge_insn_tails(In1, In2, Out1h, Out1t, Out2h, Out2t, Out3h, Out3t, X) :
% Split the two lists In1 and In2 into distinct fronts (Out1, Out2)
% and a shared tail (Out3).  length(In1)=length(In2).
merge_insn_tails([], [], O1, O1, O2, O2, O3, O3, +).
merge_insn_tails([I|Is], [J|Js], O1h, O1t, O2h, O2t, O3h, O3t, X) :-
	merge_insn_tails(Is, Js, O1a, O1t, O2a, O2t, O3a, O3t, X0),
	merge_insn_tails(X0, X, I, J, O1h, O1a, O2h, O2a, O3h, O3a).

merge_insn_tails(+, +, I, I, T1, T1, T2, T2, [I|T3], T3) :- !.
merge_insn_tails(_, -, I, J, [I|T1], T1, [J|T2], T2, T3, T3).

/*** B_GAUGE
copy_profile(
   profile_point(entry(_)),
   profile_point(entry(shallow)),
   profile_point(entry(deep))) :- !.
copy_profile(
   profile_point(head_sucess(_)),
   profile_point(head_sucess(shallow)),
   profile_point(head_sucess(deep))) :- !.
copy_profile(
   profile_point(body_goal(N,_)),
   profile_point(body_goal(N,shallow)),
   profile_point(body_goal(N,deep))) :- !.

guard_and_body([counted_neck(N,T,R)|Xs], [counted_neck(N,T,R)|Ys], Zs, Body) :-
	!, guard_and_body(Xs, Ys, Zs, Body).
guard_and_body([P|Xs], [PS|Ys], [PD|Zs], Body) :-
	copy_profile(P,PS,PD),
	!, 
	guard_and_body(Xs, Ys, Zs, Body).
E_GAUGE ***/
/*
guard_and_body([neck(N)|Xs], [neck(N)|Ys], Zs, Body) :- !,
	guard_and_body(Xs, Ys, Zs, Body).
guard_and_body([X|Xs], [X], [X], Xs) :-
	xfer_insn(X, _), !.
guard_and_body([X|Xs], [X|Ys], [X|Zs], Body) :-
	guard_and_body(Xs, Ys, Zs, Body).
*/
/*** B_GAUGE
guard_and_body(counted_neck(N,T,R), Seen, [counted_neck(N,T,R)|Ys], Zs, Body, [X|Xs]) :- !,
	Seen=1,
	guard_and_body(X, Seen, Ys, Zs, Body, Xs).
guard_and_body(I, Seen, [IS|Ys], [ID|Zs], Body, [X|Xs]) :-
	copy_profile(I, IS, ID), !,
	guard_and_body(X, Seen, Ys, Zs, Body, Xs).
E_GAUGE ***/
guard_and_body(neck(N), Seen, [neck(N)|Ys], Zs, Body, [X|Xs]) :- !,
	Seen=1,
	guard_and_body(X, Seen, Ys, Zs, Body, Xs).
guard_and_body(I, _, [I|Ys], [I|Zs], Body, [X|Xs]) :-
	I=unify_x_variable(_), !,
	guard_and_body(X, 1, Ys, Zs, Body, Xs).
guard_and_body(I, _, [I], [I], Body, Xs) :-
	xfer_insn(I, _), !,
	Body=Xs.
guard_and_body(I, Seen, [I|Ys], [I|Zs], Body, [X|Xs]) :-
	guard_and_body(X, Seen, Ys, Zs, Body, Xs).

% Extract  information from code i.e. from matching first argument.

index_insn(get_constant(K,0), get_constant_x0(K), Type, Key) :- 
	type_of_constant(K, Type),
	key_of_constant(K, Key).
index_insn(get_large(K,0), get_large_x0(K), Type, Key) :- 
	type_of_constant(K, Type),
	key_of_constant(K, Key).
index_insn(get_structure(S,0), get_structure_x0(S), 2'10001, S).
index_insn(get_nil(0), get_nil_x0, 2'101, []).
index_insn(get_list(0), get_list_x0, 2'1001, '.'/2).


/*  extract_index(C0, C1, TK0, TK, Y0, Y) :-
	C0 = sequence of WAM insns,
	C1 = C0, with neck(_) moved and
    with all elements in Y0 moved to the end,
	TK = type and key info (TK0), updated by indexing insn,
	Y0 = Y = all 'get_variable' up to first call.
*/
/*** B_GAUGE
extract_index([I|Code0], [I|Code], TypeKey0, TypeKey, Queue, Head, A) :-
	(I = bump_counter ; I = profile_point(_)), 
	!,
	extract_index(Code0, Code, TypeKey0, TypeKey, Queue, Head, A).
E_GAUGE ***/
extract_index(Code0, Code, TypeKey0, TypeKey, Queue, Head, A) :-
	get_variable(V, X, _, Code0, Code1), !,
	filter_get(V, X, A, Queue, Queue1),
	extract_index(Code1, Code, TypeKey0, TypeKey, Queue1, Head, A).
extract_index([put_x_value(X,X)|Code0], Code, TypeKey0, TypeKey, Queue, Head, A) :- !,
	extract_index(Code0, Code, TypeKey0, TypeKey, Queue, Head, A).
extract_index([I|Code0], Code, TypeKey0, TypeKey, Queue, Head, _) :-
	index_insn(I, I1, Type1, Key1),
	TypeKey0=type_key(Type0,nohash),
	Type0 =\= Type0\/2'0100000, !,
	Type is Type0/\Type1,
	TypeKey=type_key(Type,hash(Key1)),
	extract_index_2(Code0, Code1, Queue, Head),
	ins_first_unless_neck(Code1, I1, Code).
extract_index(Code0, Code, TypeKey, TypeKey, Queue, Head, _) :-
	extract_index_2(Code0, Code, Queue, Head).

extract_index_2(Code0, Code, Queue, Head) :-
	cut(C, Code0, Code1), !,
	cut(C, Code, [Neck|Head]),
	butneck(Neck, Queue, Code3, Code1, Code2),
	(Neck = neck(N); Neck = counted_neck(N,_,_)),
	rest_of_chunk(Code2, Code3, N).
extract_index_2(Code0, [Neck|Head], Queue, Head) :-
	safe_insns(Neck, Queue, Code3, Code0, Code2), !,
	(Neck = neck(N); Neck = counted_neck(N,_,_)),
	rest_of_chunk(Code2, Code3, N).
extract_index_2([I|Code0], [I|Code], Queue, Head) :-
	extract_index_collapse(I),
	extract_index_2(Code0, Code, Queue, Head).

% Pre register allocation:
% If 'choice' is in x(-1) it will not be used.
% Arg regs Ai where i >= arity of first goal cannot cause conflicts.
% Collapse right away for these cases.
filter_get(x(-1), -1, _, Q, Q) :- !.
filter_get(x(X), X, A, Q, Q) :- X >= A, !.
filter_get(V, X, _, Q0, Q) :- get_variable(V, X, _, Q0, Q).

% Pre register allocation.  The following can always be
% collapsed before 'neck', because they do not involve output arg regs.
extract_index_collapse(put_x_value(X,X)) :- !.
extract_index_collapse(put_x_variable(X,X)) :- !.
extract_index_collapse(_).

/*** B_GAUGE
ins_first_unless_neck([counted_neck(N,T,R)|Code], I, [counted_neck(N,T,R), I|Code]) :- !.
E_GAUGE ***/
ins_first_unless_neck([neck(N)|Code], I, [neck(N), I|Code]) :- !.
ins_first_unless_neck(Code, I, [I|Code]).

/*** B_GAUGE
butneck(counted_neck(N,T,R), S, S) --> [counted_neck(N,T,R)], !.
E_GAUGE ***/
butneck(neck(N), S, S) --> [neck(N)], !.
butneck(N, [I|S1], S) --> [I],
	{extract_index_collapse(I)},
	butneck(N, S1, S).

/*** B_GAUGE
safe_insns(counted_neck(N,T,R), S, S) --> [counted_neck(N,T,R)], !.
E_GAUGE ***/
safe_insns(neck(N), S, S) --> [neck(N)], !.
safe_insns(N, [I|S1], S) --> [I],
	{safe_insn(I)},
	safe_insns(N, S1, S).

safe_insn(get_x_variable(_,_)).
safe_insn(get_y_variable(_,_)).
safe_insn(put_x_value(X,X)).
safe_insn(put_x_variable(X,X)).
safe_insn(unify_x_variable(_)).
safe_insn(unify_y_variable(_)).
safe_insn(function_1(_,_,_,_,_)).
safe_insn(function_2(_,_,_,_,_,_)).
/*** B_GAUGE
safe_insn(bump_counter).
safe_insn(profile_point(_)).
E_GAUGE ***/

% Pre register allocation: put_x_value(X,A) can be collapsed after neck
% if A is a temporary or is >= arity of head.
rest_of_chunk([I|Code], [I|Code], _) :-
	xfer_insn(I, _), !.
rest_of_chunk([put_x_value(M,M)|In], Code, N) :-
	var(M), !,
	rest_of_chunk(In, Code, N).
rest_of_chunk([put_x_value(M,M)|In], Code, N) :-
	M>=N, !,
	rest_of_chunk(In, Code, N).
rest_of_chunk([I|In], [I|Code], N) :-
	rest_of_chunk(In, Code, N).

type_of_constant(K,    2'00011) :- number(K), !.
type_of_constant(K,    2'00101) :- atom(K), !.
type_of_constant([_|_], 2'01001) :- !.
type_of_constant(_,    2'10001).

key_of_constant(K, K) :- atomic(K), !.
key_of_constant(K, F/L) :- functor(K, F, L).


% Translate a clause: first head, then body.  Note whether will need env.
/*** B_GAUGE
trans_clause_1(profiled(_,_,_), body(var(D,Arg),Head,Body), Kind, InArity, OutArity) -->
	{kind_of_clause(Body, Kind)},
	get_variable(Arg, -1, D),
	c_profiled_head_args(Head, Cache, InArity),
	{cache_add(Cache, -1, var(D,Arg))},
	c_profiled_guards(Kind, Body, InArity, OutArity, Cache).
E_GAUGE ***/
trans_clause_1(unprofiled, body(var(D,Arg),Head,Body), Kind, InArity, OutArity) -->
	{kind_of_clause(Body, Kind)},
	get_variable(Arg, -1, D),
	c_head_args(Head, Cache, InArity),
	{cache_add(Cache, -1, var(D,Arg))},
	c_guards(Kind, Body, InArity, OutArity, Cache).

kind_of_clause([], unit).
kind_of_clause([inline(_)|Gs], Kind) :- !,
	kind_of_clause(Gs, Kind).
kind_of_clause([_], iterative) :- !.
kind_of_clause([pcall(_,Size)|_], recursive(Size)).

/*** B_GAUGE
open_code('PROFILE POINT'(P), _) -->
	{structure(P,body_goal(constant(N),constant(W)))},
	[profile_point(body_goal(N,W)),bump_counter].
E_GAUGE ***/
open_code('basiccontrol:CHOICE IDIOM'(var(D,Arg)), _) --> !,
	get_variable(Arg, -1, D).
open_code('basiccontrol:CUT IDIOM'(var(D,Arg)), Dic) --> !,
	{cached_ref(Arg, D, Arg1, _, Dic)},
	cut(Arg1).
open_code('term_basic:='(X,Y), Dic) --> !,
	c_equal(X, Y, Dic).
open_code('term_basic:C'(X,Y,Z), Dic) --> !,
	c_equal(X, list(Y,Z), Dic).
open_code('arithmetic:is'(Value,Expr), Dic) --> !,
	c_expr_top(Expr, Dest, Dic),
	c_equal(Dest, Value, Dic).
open_code(Builtin, Dic) --> 				% DMCAI --- ATTRVARS
  	{
            Builtin='term_typing:type'(_,_)
        ;
            Builtin='attributes:get_attribute'(_,_)
        }, !,
  	{name_of_builtin(Builtin, Name, X, Value)},
  	c_put_arg(X, Xreg, 1000, Dic, _),
  	[function_1( Name, Vreg, Xreg, _, _)],
  	c_equal(var(g,'x'(Vreg)), Value, Dic).
open_code(Builtin, Dic) -->
	{Builtin='term_basic:arg'(_,_,_); Builtin='term_compare:compare'(_,_,_)},
	{name_of_builtin(Builtin, Name, X, Y, Value)}, !,
	c_put_arg(X, Xreg, 1000, Dic, _),
	c_put_arg(Y, Yreg, 1000, Dic, _),
	[function_2(Name, Vreg, Xreg, Yreg, _, _)],
	c_equal(var(g,'x'(Vreg)), Value, Dic).
open_code(Builtin, Dic) -->
	{name_of_builtin(Builtin, Name, X)}, !,
	c_put_arg(X, Xreg, 1000, Dic, _),
	[builtin_1(Name, Xreg)].
open_code(Builtin, Dic) -->
	{name_of_builtin(Builtin, Name, X, Y)}, !,
	(   {eval_builtin(Builtin)} ->
	     c_expr(Y, Y1, Dic),
	     c_expr(X, X1, Dic),
	     c_put_arg(X1, Xreg, 1000, Dic, _),
	     c_put_arg(Y1, Yreg, 1000, Dic, _)
	;    /* true -> */
	     c_put_arg(X, Xreg, 1000, Dic, _),
	     c_put_arg(Y, Yreg, 1000, Dic, _)
	),
	[builtin_2(Name, Xreg, Yreg)].
open_code(Builtin, Dic) -->
	{name_of_builtin(Builtin, Name, X, Y, Z)},
	c_put_arg(X, Xreg, 1000, Dic, _),
	c_put_arg(Y, Yreg, 1000, Dic, _),
	c_put_arg(Z, Zreg, 1000, Dic, _),
	[builtin_3(Name, Xreg, Yreg, Zreg)].

c_expr_top(Expr0, Expr, Dic) -->
	{c_expr(Expr0, Expr, Dic, S0, S),
	 S0\==S}, !,
	c(S0, S).
c_expr_top(Expr0, Expr, Dic) -->
	c_expr(structure(+,[Expr0]), Expr, Dic).

c_expr(list(Expr0,nil), Expr, Dic) --> !,
	c_expr(Expr0, Expr, Dic).
c_expr(E, var(g,'x'(Vreg)), Dic) -->
	{structure(E, E1)},
	{   E1=X-constant(1) -> E2= --(X)  %% shorthand for 'SUB1 FUNCTION'
	;   E1=X+constant(1) -> E2= ++(X)  %% shorthand for 'ADD1 FUNCTION'
	;   E2=E1
	},
	{name_of_function(E2, N, X)}, !,
	c_expr(X, X1, Dic),
	c_put_arg(X1, Xreg, 1000, Dic, _),
	[function_1(N, Vreg, Xreg, _, _)].
c_expr(E, var(g,'x'(Vreg)), Dic) -->
	{structure(E, E1),
	 name_of_function(E1, N, X, Y)}, !,
	c_expr(Y, Y1, Dic),
	c_expr(X, X1, Dic),
	c_put_arg(X1, Xreg, 1000, Dic, _),
	c_put_arg(Y1, Yreg, 1000, Dic, _),
	[function_2(N, Vreg, Xreg, Yreg, _, _)].
c_expr(E, E, _) --> [].

c_equal(U, U1, _) -->
	{U==U1}, !.
c_equal(var(Du,U), var(Dv,V), Dic) --> !,
	c_eq_vars(U, V, Du, Dv, Dic).
c_equal(U, var(Dv,V), Dic) -->
	{var(Dv), Dv=g}, !,
	top_put(V, U, Dic).
c_equal(var(Du,U), V, Dic) -->
	{var(Du), Du=g}, !,
	top_put(U, V, Dic).
c_equal(U, var(D,N), Dic) --> !,
	{cached_ref(N, D, N1, _, Dic)},
	top_get(N1, U, Dic).
c_equal(var(D,N), U, Dic) --> !,
	{cached_ref(N, D, N1, _, Dic)},
	top_get(N1, U, Dic).
c_equal(list(L0,L1), list(L2,L3), Dic) --> !,
	c_equal(L0, L2, Dic),
	c_equal(L1, L3, Dic).
c_equal(structure(S,S0), structure(S,S1), Dic) --> !,
	c_equal_args(S0, S1, Dic).
c_equal(_, _, _) -->
	[fail].					% An impossible match.

c_equal_args([], [], _) --> !.
c_equal_args([X|Xs], [Y|Ys], Dic) --> !,
	c_equal(X, Y, Dic),
	c_equal_args(Xs, Ys, Dic).
c_equal_args(_, _, _) -->
	[fail].					% An impossible match.

% Explicit unif. of variable and non-variable.
% Introduce 'T1' here to avoid a cache clash.
top_get(V, X, Dic) -->
	put_value(V, T1),
	c_get_arg(X, T1, Dic).

top_put(V, X, Dic) -->
	put_variable(V, T1, _),
	c_get_arg(X, T1, Dic).

% Treat explicit unif. of two variables as a special case, because it's
% faster and because of the dangling pointer problem: 'get_variable Yn, Xm'
% may place a dangling pointer in 'Yn'.

c_eq_vars(U, V, Du, Dv, Dic) -->
    (   {var(Du), U=V, Du=Dv} -> =          % avoid DCG bug
    ;   {var(Dv), U=V, Du=Dv} -> =

 %% %%% BEGIN OF FIXED CODE --- ERRONEOUS PATCH!!!
 %%         (   {var(Du)}, {cached_ref(V, Dv, V1, Dv1, Dic), U=V1, Du=Dv1} -> =
 %%         ;   {var(Du), U=V, Du=Dv} -> =          % avoid DCG bug
 %%         ;   {var(Dv)}, {cached_ref(U, Du, U1, Du1, Dic), U1=V, Du1=Dv} -> =
 %%         ;   {var(Dv), U=V, Du=Dv} -> =
 %% %%% END OF FIXED CODE

        ;   {var(Du), var(Dv)} ->
            c_eq_var_var(U, V, Du, Dv)
        ;   {var(Du)}, {cached_ref(V, Dv, V1, Dv1, Dic)} ->
            c_eq_var_value(U, V1, Du, Dv1)
        ;   {var(Dv)}, {cached_ref(U, Du, U1, Du1, Dic)} ->
            c_eq_var_value(V, U1, Dv, Du1)
        ;   {cached_ref(U, Du, U1, Du1, Dic),
             cached_ref(V, Dv, V1, Dv1, Dic)},
            c_eq_value_value(U1, V1)
        ).

c_eq_var_var('x'(I), 'y'(J), J, J) --> !,
	[put_y_variable(J,I)].
c_eq_var_var('y'(I), 'y'(J), I, I) -->
	{I < J}, !,
	[put_y_variable(I,T1)],
	[get_y_variable(J,T1)].
c_eq_var_var(Var1, Var2, D1, D2) -->
	c_eq_var_var(Var2, Var1, D2, D1).

c_eq_var_value('x'(I), 'x'(J), D, D) --> !,
	[put_x_variable(I,T1)],
	get_value('x'(J), T1).
c_eq_var_value('x'(I), 'y'(J), D, D) --> 
	[put_y_value(J,I)].
c_eq_var_value('y'(I), 'x'(J), g, g) --> !,
	[get_y_variable(I,J)].
c_eq_var_value('y'(I), Val, I, _) -->
	[put_y_variable(I,T1)],
	get_value(Val, T1).

c_eq_value_value('x'(I), Val) -->
	get_value(Val, I).
c_eq_value_value('y'(I), 'x'(J)) --> !,
	[get_y_value(I,J)].
c_eq_value_value('y'(I), 'y'(J)) -->
	[put_y_value(I,T1)],
	[get_y_value(J,T1)].


goal_args(pcall(structure(F, Args), Size), F/A, Size, Args) :-
	length(Args, A).

% Compile the body up to first general call.
c_guards(recursive(Size), [inline(G)|Gs], N0, N, Dic) --> 
	{structure(G, Fu), builtin_uses_heap(Fu)}, !,
	[neck(N0), true(Size)],
	open_code(Fu, Dic),
	c_goals(Gs, N, Dic).
c_guards(Kind, [inline(G)|Gs], N0, N, Dic) --> !,
	{structure(G, Fu)},
	open_code(Fu, Dic),
	c_guards(Kind, Gs, N0, N, Dic).
c_guards(_, Gs, N0, N, Dic) --> 
	[neck(N0)],
	c_goals(Gs, N, Dic).

/*** B_GAUGE
c_profiled_guards(Kind, [inline(G)|Gs], N0, N, Dic) --> !,
	{structure(G, Fu)},
	c_profiled_guards(Fu, Kind, Gs, N0, N, Dic).
c_profiled_guards(_, Gs, N0, N, Dic) --> 
	[counted_neck(N0,_,_)],
	c_goals(Gs, N, Dic).

c_profiled_guards('PROFILE POINT'(X), _, Gs, N0, N, Dic) -->
	{Gs = [pcall(_, _)|_]}, !,
	[counted_neck(N0,_,_)],
	open_code('PROFILE POINT'(X), Dic),
	c_goals(Gs, N, Dic).
c_profiled_guards(Fu, recursive(Size), Gs, N0, N, Dic) -->
	{builtin_uses_heap(Fu)}, !,
	[counted_neck(N0,_,_), true(Size)],
	open_code(Fu, Dic),
	c_goals(Gs, N, Dic).
c_profiled_guards(Fu, Kind, Gs, N0, N, Dic) -->
	open_code(Fu, Dic),
	c_profiled_guards(Kind, Gs, N0, N, Dic).
E_GAUGE ***/

builtin_uses_heap('arithmetic:is'(_,_)).
builtin_uses_heap(Fu) :-
	eval_builtin(Fu),
	arg(1, Fu, structure(_,_)).
builtin_uses_heap(Fu) :-
	eval_builtin(Fu),
	arg(2, Fu, structure(_,_)).

% Compile a tail of the body.
c_goals([], 0, _) --> 
	[execute('basiccontrol:true'/0)].
c_goals([inline(G)|Gs], N, Dic) --> !,
	{structure(G, Fu)},
	open_code(Fu, Dic),
	c_goals(Gs, N, Dic).
c_goals([G], N, Dic) --> !,
	c_last_goal(G, N, Dic).
c_goals([G|Gs], N, Dic) --> 
	c_goal(G, N, Dic),
	c_goals(Gs, _, _).

% Compile a body goal.
c_goal(G, N, Dic) -->
	{goal_args(G, Fu/N, Size, Args)},
	c_goal_args(Args, Size, Dic, _),
	[call(Fu/N,Size), ensure_space(cont,0,_)].

% Compile last body goal.
c_last_goal(G, N, Dic) -->
	{goal_args(G, Fu/N, Size, Args)},
	c_goal_args(Args, Size, Dic, _),
	[execute(Fu/N)].

% Head arguments unification.  Match args that are vas first.
% This seems to need fewer temporaries than the source sequence.
c_head_args(structure(_,Args), Dic, N) -->
	head_arguments(Args, 0, N, Arga, [], Dic),
	c_head_args_c(Arga, Dic).

/*** B_GAUGE
c_profiled_head_args(S, Dic, N) -->
	[profile_point(entry(fact)),bump_counter],
	c_head_args(S, Dic, N),
	[profile_point(head_sucess(fact)),bump_counter].
E_GAUGE ***/

c_head_args_c([], _) --> [].
c_head_args_c(['term_basic:='(X,Y)|Xs], Dic) -->
	c_get_arg(X, Y, Dic),
	c_head_args_c(Xs, Dic).

head_arguments([], N, N, S, S, _) --> [].
head_arguments([var(D,V)|As], I, N, S0, S, Dic0) -->
	{var(D)}, !,
	{Dic0 = dic(I,var(D,V),_,Dic)},
	{get_variable(V, I, D, G0, G)},
	{I1 is I+1},
	head_arguments(As, I1, N, S0, S, Dic),
	c(G0, G).
head_arguments([A|As], I, N, ['term_basic:='(A,I)|S0], S, Dic) -->
	{I1 is I+1},
	head_arguments(As, I1, N, S0, S, Dic).

% Goal arguments unification.  Put args that are vas last.
% This seems to need fewer temporaries than the source sequence.
c_goal_args(Xs, Size, Dic0, Dic) -->
	goal_arguments_nonvar(Xs, 0, Size, Dic0, Dic1),
	goal_arguments_var(Xs, 0, Size, Dic1, Dic).

goal_arguments_nonvar([], _, _, Dic, Dic) --> [].
goal_arguments_nonvar([var(_,_)|Args], I, Size, Dic0, Dic) --> !,
	{I1 is I+1},
	goal_arguments_nonvar(Args, I1, Size, Dic0, Dic).
goal_arguments_nonvar([Arg|Args], I, Size, Dic0, Dic) -->
	c_put_arg(Arg, I, Size, Dic0, Dic1),
	{I1 is I+1},
	goal_arguments_nonvar(Args, I1, Size, Dic1, Dic).

goal_arguments_var([], _, _, Dic, Dic) --> [].
goal_arguments_var([var(D,V)|Args], I, Size, Dic0, Dic) --> !,
	c_put(var(D,V), I, Size, Dic0, Dic1),
	{I1 is I+1},
	goal_arguments_var(Args, I1, Size, Dic1, Dic).
goal_arguments_var([_|Args], I, Size, Dic0, Dic) -->
	{I1 is I+1},
	goal_arguments_var(Args, I1, Size, Dic0, Dic).

% Linearize term before emitting 'get' + 'put' + 'unify'.

flat(S, V, GP) -->
	flat(S, V, GP, 1, 0, _).

flat(var(D,V), var(D,V), _, _, C, C) --> !.
flat(constant(K), S1, GP, 0, _, 1) -->
	{large_heap_usage(K, _), !,
	 (GP=put -> G=g; true),
	 S1 = var(G,'x'(T))},
	['term_basic:='(T,constant(K))].	
flat(constant(K), constant(K), _, _, C, C) --> !.
flat(nil, nil, _, _, C, C) --> !.
flat(S, S1, get, 0, _, 1) --> !,
	{S1 = var(_,'x'(T))},
	['term_basic:='(T,S2)],
	flat(S, S2, get).
flat(S, S1, put, 0, _, 1) --> !,
	{S1 = var(g,'x'(T))},
	flat(S, S2, put),
	['term_basic:='(T,S2)].
flat(list(A,B), list(A1,B1), GP, N, C0, C) -->
	{N1 is N-1},
	flat(A, A1, GP, N1, 0, C1),
	{N2 is N-C1,
	 C2 is C0\/C1},
	flat(B, B1, GP, N2, C2, C).
flat(structure(S,L), structure(S,L1), GP, N, C0, C) -->
	flat_args(L, L1, GP, N, 0, C0, C).

flat_args([X], [X1], GP, N, C0, C1, C) --> !,
	{N1 is N-C0,
	 C2 is C0\/C1},
	flat(X, X1, GP, N1, C2, C).
flat_args([X|Xs], [X1|Xs1], GP, N, C0, C1, C) -->
	{N1 is N-1},
	flat(X, X1, GP, N1, C0, C2),
	flat_args(Xs, Xs1, GP, N, C2, C1, C).


% Compile matching a head argument.
% First linearize goal argument.
c_get_arg(list(A,B), V, Dic) --> !,
	{flat(list(A,B), S1, get, S, [])},
	c_get(['term_basic:='(V,S1)|S], Dic).
c_get_arg(structure(A,L), V, Dic) --> !,
	{flat(structure(A,L), S1, get, S, [])},
	c_get(['term_basic:='(V,S1)|S], Dic).
c_get_arg(X, V, Dic) -->
	c_get(X, V, Dic).

% Using X registers as a cache for Y values and constants.
c_get([], _) --> [].
c_get(['term_basic:='(V,X)|R], Dic) -->
	c_get(X, V, Dic),
	c_get(R, Dic).

c_get(var(D,N), V, Dic) -->
	{var(D)}, !,
	get_variable(N, V, D),
	{cache_add(Dic, V, var(D,N))}.
c_get(var(D,N), V, Dic) -->
	{cached_ref(N, D, N1, _, Dic)},
	get_value(N1, V),
	{cache_add(Dic, V, var(D,N))}.
c_get(constant(K), V, Dic) -->
	(   {large_heap_usage(K, _)} -> [get_large(K, V)]
	;   [get_constant(K, V)]
	),
	{cache_add(Dic, V, constant(K))}.
c_get(nil, V, Dic) -->
	[get_nil(V)],
	{cache_add(Dic, V, nil)}.
c_get(list(X,Y), V, Dic) -->
	[get_list(V)],
	c_unify(X, Dic),
	c_unify(Y, Dic).
c_get(structure(F,Args), V, Dic) -->
	[get_structure(F/A, V)],
	{length(Args, A)},
	c_unify_args(Args, Dic).

% Compile loading a goal argument.
% First linearize goal argument.
c_put_arg(list(A,B), V, Size, Dic0, Dic) --> !,
	{flat(list(A,B), S1, put, S, ['term_basic:='(V,S1)])},
	c_put(S, Size, Dic0, Dic).
c_put_arg(structure(A,L), V, Size, Dic0, Dic) --> !,
	{flat(structure(A,L), S1, put, S, ['term_basic:='(V,S1)])},
	c_put(S, Size, Dic0, Dic).
c_put_arg(X, V, Size, Dic0, Dic) -->
	c_put(X, V, Size, Dic0, Dic).

c_put([], _, Dic, Dic) --> [].
c_put(['term_basic:='(V,X)|R], Size, Dic0, Dic) -->
	c_put(X, V, Size, Dic0, Dic1),
	c_put(R, Size, Dic1, Dic).

c_put(var(D,N), V, _, Dic, Dic1) -->
	{var(D)}, !,
	put_variable(N, V, D),
	{cache_store(Dic, V, var(D,N), Dic1)}.
c_put(var(D,N), V, Size, Dic, Dic1) -->
	{cached_ref(N, D, N1, D1, Dic)},
	emit_put_value(D1, Size, N1, V),
	{cache_store(Dic, V, var(D1,N), Dic1)}.
c_put(constant(K), V, _, Dic, Dic1) -->
	(   {cache_search(Dic, N1, constant(K))} -> [put_x_value(N1, V)]
	;   {large_heap_usage(K, _)} -> [put_large(K, V)]
	;   [put_constant(K, V)]
	),
	{cache_store(Dic, V, constant(K), Dic1)}. 
c_put(nil, V, _, Dic, Dic1) -->
	(   {cache_search(Dic, N1, nil)} -> [put_x_value(N1, V)]
	;   [put_nil(V)]
	),
	{cache_store(Dic, V, nil, Dic1)}. 
c_put(list(X,Y), V, _, Dic, Dic1) -->
	[put_list(V)],
	{cache_clear(Dic, V, Dic1)},
	c_unify(X, Dic1), c_unify(Y, Dic1).
c_put(structure(F,Args), V, _, Dic, Dic1) -->
	[put_structure(F/A, V)],
	{length(Args, A)},
	{cache_clear(Dic, V, Dic1)},
	c_unify_args(Args, Dic1).

c_unify_args([], _) --> [].
c_unify_args([X|Xs], Dic) -->
	c_unify(X, Dic), c_unify_args(Xs, Dic).

c_unify(var(D,N), _) -->
	{var(D)}, !,
	unify_variable(N, D).
c_unify(var(D,N), Dic) -->
	{cached_ref(N, D, N1, D1, Dic)},
	emit_unify_value(D1, N1).
c_unify(constant(K), _) -->
	(   {large_heap_usage(K, _)} -> [unify_large(K)]
	;   [unify_constant(K)]
	).
c_unify(nil, _) --> [unify_nil].
c_unify(list(X,Y), Dic) -->
	[unify_list],
	c_unify(X, Dic),
	c_unify(Y, Dic).
c_unify(structure(F,Args), Dic) -->
	[unify_structure(F/A)],
	{length(Args, A)},
	c_unify_args(Args, Dic).

% Use argument registers as a cache for constants and variables.
% A cache is a binary tree.

cache_add(Dic, V, X1) :-
	integer(V), !,
	dic_lookup(Dic, V, X1).
cache_add(_, _, _).

cache_search(D, K, Val) :- dic_node(D,dic(K,Val,_,_)).

cache_store(Cache0, K, V, Cache) :-
	integer(K), !,
	dic_replace(Cache0, K, V, Cache).
cache_store(Cache, _, _, Cache).

cache_clear(Cache0, K, Cache) :-
	integer(K), !,
	dic_replace(Cache0, K, void, Cache).
cache_clear(Cache, _, Cache).

cached_ref(Ref, _, 'x'(K), D1, Cache) :-
	cache_search(Cache, K, var(D1,Ref0)),
	Ref==Ref0, !.
cached_ref(Ref, D, Ref, D, _).

emit_put_value(I, Size, N, V) -->
	{integer(I), I>=Size}, !,
	put_unsafe_value(N, V).
emit_put_value(_, _, N, V) -->
	put_value(N, V).

emit_unify_value(g, N) --> !,
	unify_value(N).
emit_unify_value(_, N) -->
	unify_local_value(N).


%-----------------------------------------------------------------------------
% Lifetime analysis and allocation of X registers.  The first chunk does
% steps 1-4.  Subsequent chunks do step 4 only.

% 1. Make a dictionary of all variables involved in moves (Map).
% 2. Fill in Def-LastUse for temporaries and LastInargUse-OutargDef
%    for argument registers, by scanning the chunk backwards.
% 3. Consider one move at a time.  Collapse it if the temporary and
%    arg reg have disjoint lifetimes.  If they have, update the DU info
%    of the arg reg.
% 4. Traverse chunk backward and assign free arg regs for yet unallocated
%    temporaries.  Maintain a list of live arg regs during traversal.
guard_lifetime(unit, InArity, _, Live) --> !,
	body_lifetime(InArity, Live).
guard_lifetime(_, InArity, OutArity, Live) -->
	[ensure_space(_,_,Heap), Insn],
	guard_def_use_list(Insn, DUreverse, [], Moves, Heap, Map),
	{lifetime_map(DUreverse, Map),
	 guard_collapse_moves(Moves),
	 intset_sequence(OutArity, [], Live0),
	 guard_allocate(DUreverse, InArity, Live0, Live)},
	body_lifetime(0, _).

guard_def_use_list(I, DUlist0, DUlist, Moves, Heap, _) -->
	{xfer_insn(I, _), !,
	 DUlist0=DUlist,
	 Moves=[],
	 Heap=0}.
guard_def_use_list(I, DUlist0, DUlist, Moves0, Heap, Map) -->
	[I1],
	guard_def_use_list(I1, DUlist0, DUlist1, Moves, Heap0, Map),
	{x_def_use_heap(I, DU, Heap1),
	 insn_heap_usage(I, Heap0),
	 Heap is Heap0+Heap1,
	 guard_def_use(DU, DUlist1, DUlist, Moves0, Moves, Map)}.  

guard_def_use(x_d0u0, DUs, DUs, Moves, Moves, _) :- !.
guard_def_use(x_d1u1p(X1,X2), DUs0, DUs, Moves0, Moves, Map) :- !,
	compare(C, X1, X2),
	guard_def_use(C, X1, X2, DUs0, DUs, Moves0, Moves, Map).
guard_def_use(DU, [DU|DUs], DUs, Moves, Moves, _).

guard_def_use(<, T, R, DUs0, DUs, Moves0, Moves, Map) :-
	var(T), !,
	DUs0 = [x_d1u1p(T,R)|DUs],
	Moves0 = [alias(T,R,Titem,Ritem)|Moves],
	dic_lookup(Map, T, Titem),
	dic_lookup(Map, R, Ritem).
guard_def_use(>, R, T, DUs0, DUs, Moves0, Moves, Map) :-
	var(T), !,
	DUs0 = [x_d1u1p(R,T)|DUs],
	Moves0 = [alias(T,R,Titem,Ritem)|Moves],
	dic_lookup(Map, T, Titem),
	dic_lookup(Map, R, Ritem).
guard_def_use(=, _, _, DUs, DUs, Moves, Moves, _) :- !.
guard_def_use(_, X1, X2, [x_d1u1p(X1,X2)|DUs], DUs, Moves, Moves, _).


guard_allocate([], _, Set, Set).
guard_allocate([DU|DUlist], Max, Set0, Set) :-
	simple_x_alloc(DU, Set0, Set1, Max),
	guard_allocate(DUlist, Max, Set1, Set).

guard_collapse_moves([]).
guard_collapse_moves([alias(T,R,Titem,Ritem)|Moves]) :-
	(   var(T), check_eq(Titem, Ritem) -> T=R
        ;   true
	),
	guard_collapse_moves(Moves).

check_eq(T, _) :- var(T), !.
% check_eq(T1-T1, _) :- !.
check_eq(T1-T2, [Rhead|Rtail]) :-
	check_eq_last(Rtail, Rhead, R1-R2, [S1-S2|_]),
	check_eq(T1, T2, R1, R2, S1, S2).

check_eq(A, B, void, B, _, A) :- !.
check_eq(A, B, A, C, B, C) :- B =< C, !.
check_eq(B, C, A, C, A, B) :- A =< B.

check_eq_last(Tail0, Head0, Head, Tail) :- var(Tail0), !,
	Head0=Head,
	Tail0=Tail.
check_eq_last([Head0|Tail0], _, Head, Tail) :-
	check_eq_last(Tail0, Head0, Head, Tail).

lifetime_map(_, Map) :- var(Map), !.
lifetime_map(DUs, Map) :- lifetime_map(DUs, 0, Map).

lifetime_map([], _, _).
lifetime_map([DU|DUs], I, Map) :-
	lifetime_map(DU, I, I1, Map),
	lifetime_map(DUs, I1, Map).

lifetime_map(x_d0u0, I, I, _).
lifetime_map(x_d0u1(A), I, I1, Map) :-
	I1 is I-1,
	lifetime_map_u(A, I, Map).
lifetime_map(x_d0u2(A,B), I, I1, Map) :-
	I1 is I-1,
	lifetime_map_u(A, I, Map),
	lifetime_map_u(B, I, Map).
lifetime_map(x_d0u3(A,B,C), I, I1, Map) :-
	I1 is I-1,
	lifetime_map_u(A, I, Map),
	lifetime_map_u(B, I, Map),
	lifetime_map_u(C, I, Map).
lifetime_map(x_d0un(A), I, I1, Map) :-
	I1 is I-1,
	lifetime_map_un(0, A, I, Map).
lifetime_map(x_d1u0p(A), I, I1, Map) :-
	I1 is I-1,
	lifetime_map_d(A, I, Map).
lifetime_map(x_d1u0(A), I, I1, Map) :-
	I1 is I-1,
	lifetime_map_d(A, I, Map).
lifetime_map(x_d1u1p(A,B), I, I1, Map) :-
	I1 is I-1,
	lifetime_map_move(A, B, I, Map).
lifetime_map(x_d1u1live(A,B,_), I, I1, Map) :-
	I1 is I-1,
	lifetime_map_d(A, I, Map),
	lifetime_map_u(B, I, Map).
lifetime_map(x_d1u2live(A,B,C,_), I, I1, Map) :-
	I1 is I-1,
	lifetime_map_d(A, I, Map),
	lifetime_map_u(B, I, Map),
	lifetime_map_u(C, I, Map).
lifetime_map(x_d2u0p(A,B), I, I1, Map) :-
	I1 is I-1,
	lifetime_map_d(A, I, Map),
	lifetime_map_d(B, I, Map).

lifetime_map_move(Def, _, _, Map) :-
	var(Def),
	dic_get(Map, Def, Dint),
	var(Dint), !.
lifetime_map_move(Def, Use, I, Map) :-
	lifetime_map_d(Def, I, Map),
	lifetime_map_u(Use, I, Map).

lifetime_map_d(A, I, Map) :-
	dic_get(Map, A, Int), !,
	lifetime_map_d1(A, I, Int).
lifetime_map_d(_, _, _).

lifetime_map_d1(A, I, I-_) :- var(A), !.
lifetime_map_d1(_, I, [_-I|_]).

lifetime_map_u(A, I, Map) :-
	dic_get(Map, A, Int), !,
	lifetime_map_u1(A, I, Int).
lifetime_map_u(_, _, _).

lifetime_map_u1(A, I, _-I) :- var(A), !.
lifetime_map_u1(A, I, [I-J|_]) :- nonvar(A), nonvar(J), !.
lifetime_map_u1(_, _, _).

lifetime_map_un(A, A, _, _) :- !.
lifetime_map_un(A0, A, I, Map) :-
	dic_get(Map, A0, Int), !,
	lifetime_map_u1(A0, I, Int),
	A1 is A0+1,
	lifetime_map_un(A1, A, I, Map).
lifetime_map_un(A0, A, I, Map) :-
	A1 is A0+1,
	lifetime_map_un(A1, A, I, Map).


body_lifetime(InArity, Live) -->
	[ensure_space(Kind, _, Heap), I], !,
	body_lifetime(I, InArity, Live, Kind, Heap),
	body_lifetime(0, _).
body_lifetime(_, []) --> [].

body_lifetime(put_x_value(U,U), InArity, Live, Kind, Heap) --> !,
	[I1],
	body_lifetime(I1, InArity, Live, Kind, Heap).
body_lifetime(execute('basiccontrol:true'/0), _, [], unit, 128) --> !.
body_lifetime(I, _, Live, _, 0) -->
	{xfer_insn(I, A), !,
	 intset_sequence(A, [], Live)}.
body_lifetime(I, InArity, Live, Kind, Heap) -->
	[I1],
	body_lifetime(I1, InArity, Live1, Kind, Heap0),
	{x_def_use_heap(I, DU, Heap1),
	 insn_heap_usage(I, Heap0),
	 Heap is Heap0+Heap1,
	 simple_x_alloc(DU, Live1, Live, InArity)}.

simple_x_alloc(x_d0un(0), Set, Set, _) :- !.
simple_x_alloc(x_d0un(N), Set0, Set, Max) :-
	N1 is N-1,
	x_alloc(Set0, N1, Set1, Max),
	simple_x_alloc(x_d0un(N1), Set1, Set, Max).
simple_x_alloc(x_d0u0, Set, Set, _).
simple_x_alloc(x_d0u1(A), Set0, Set, Max) :-
	x_alloc(Set0, A, Set, Max).
simple_x_alloc(x_d0u2(A,B), Set0, Set, Max) :-
	x_alloc(Set0, A, Set1, Max),
	x_alloc(Set1, B, Set, Max).
simple_x_alloc(x_d0u3(A,B,C), Set0, Set, Max) :-
	x_alloc(Set0, A, Set1, Max),
	x_alloc(Set1, B, Set2, Max),
	x_alloc(Set2, C, Set, Max).
simple_x_alloc(x_d1u0(X), Set0, Set, Max) :-
	delete_check(Set0, X, Set, Max).
simple_x_alloc(x_d1u0p(X), Set0, Set, _) :-
	delete_check_p(Set0, X, Set).
simple_x_alloc(x_d1u1live(X,A,Set), Set0, Set, Max) :-
	delete_check(Set0, X, Set1, Max),
	x_alloc(Set1, A, Set, Max).
simple_x_alloc(x_d1u1p(X,A), Set0, Set, Max) :-
	x_alloc_move(Set0, X, A, Set, Max).
simple_x_alloc(x_d1u2live(X,A,B,Set), Set0, Set, Max) :-
	delete_check(Set0, X, Set1, Max),
	x_alloc(Set1, A, Set2, Max),
	x_alloc(Set2, B, Set, Max).
simple_x_alloc(x_d2u0p(X,Y), Set0, Set, _) :-
	delete_check_p(Set0, X, Set1),
	delete_check_p(Set1, Y, Set).

delete_check(Set0, X, Set, _) :-
	nonvar(X),
	intset_delete(Set0, X, Set), !.
delete_check(Set, X, Set, Max) :-
	x_alloc(Set, X, _, Max).

delete_check_p(Set0, X, Set) :-
	nonvar(X),
	intset_delete(Set0, X, Set), !.
delete_check_p(Set, _, Set).

x_alloc_move(Set0, X, A, Set, Max) :-
	nonvar(X),
	intset_delete(Set0, X, Set1), !,
	x_alloc(Set1, A, Set, Max).
x_alloc_move(Set, _, _, Set, _).

x_alloc(Set0, X, Set, Free) :- 
	var(X), !, x_free_arg(Set0, Free, X, Set).
x_alloc(Set0, X, Set, _) :-
	nonvar(X), intset_insert(Set0, X, Set).

% x_free_arg(Set0, I0, I, Set)
%   I is the smallest integer such that
%   I0=<I, I not in Set0, Set=union(Set,I).

x_free_arg([], I0, I, Set) :-
	!,
	I=I0,
	Set=[I0].
x_free_arg(Set0, I0, I, Set) :-
	Set0=[X|_],
	X>I0, !,
	I=I0,
	Set=[I0|Set0].
x_free_arg([X|Xs], I0, I, Set) :-
	X<I0, !,
	Set=[X|Set1],
	x_free_arg(Xs, I0, I, Set1).
x_free_arg(Set0, I0, I, Set) :-
	I1 is I0+1,
	x_free_arg(Set0, I1, I, Set).


% Convenient generic instructions.

cut('x'(I)) --> [cut_x(I)].
cut('y'(I)) --> [cut_y(I)].

put_variable('x'(I), J, g) --> [put_x_variable(I,J)].
put_variable('y'(I), J, I) --> [put_y_variable(I,J)].

put_value('x'(I), J) --> [put_x_value(I,J)].
put_value('y'(I), J) --> [put_y_value(I,J)].

put_unsafe_value('x'(I), J) --> [put_x_unsafe_value(I,J)].
put_unsafe_value('y'(I), J) --> [put_y_unsafe_value(I,J)].

get_variable('x'(I), J, r) --> [get_x_variable(I,J)].
get_variable('y'(I), J, r) --> [get_y_variable(I,J)].

get_value('x'(I), J) --> [get_x_value(I,J)].
get_value('y'(I), J) --> [get_y_value(I,J)].

unify_variable('x'(I), g) --> [unify_x_variable(I)].
unify_variable('y'(I), g) --> [unify_y_variable(I)].

unify_value('x'(I)) --> [unify_x_value(I)].
unify_value('y'(I)) --> [unify_y_value(I)].

unify_local_value('x'(I)) --> [unify_x_local_value(I)].
unify_local_value('y'(I)) --> [unify_y_local_value(I)].

% What X registers does an instruction use or ?
/*** B_GAUGE
x_def_use_heap(counted_neck(N,_,_), x_d0un(N), 0) :- !.
E_GAUGE ***/
x_def_use_heap(neck(N), x_d0un(N), 0) :- !.
x_def_use_heap(cut_x(A), x_d0u1(A), 0) :- !.
x_def_use_heap(function_1(_,V,X,_,L), x_d1u1live(V,X,L), 0) :- !.
x_def_use_heap(function_2(_,V,X,Y,_,L), x_d1u2live(V,X,Y,L), 0) :- !.
x_def_use_heap(builtin_1(N,X), x_d0u1(X), H) :- !,
	builtin_heap_usage(N, H).
x_def_use_heap(builtin_2(N,X,Y), x_d0u2(X,Y), H) :- !,
	builtin_heap_usage(N, H).
x_def_use_heap(builtin_3(N,X,Y,Z), x_d0u3(X,Y,Z), H) :- !,
	builtin_heap_usage(N, H).
x_def_use_heap(get_x_variable(V,A), x_d1u1p(V,A), 0) :- !.
x_def_use_heap(get_y_variable(_,A), x_d0u1(A), 0) :- !.
x_def_use_heap(get_x_value(V,A), x_d0u2(V,A), 0) :- !.
x_def_use_heap(get_y_value(_,A), x_d0u1(A), 0) :- !.
x_def_use_heap(get_constant(_,A), x_d0u1(A), 0) :- !.
x_def_use_heap(get_large(C,A), x_d0u1(A), H) :- !,
	large_heap_usage(C, H).
x_def_use_heap(get_nil(A), x_d0u1(A), 0) :- !.
x_def_use_heap(get_structure(_/Ar,A), x_d0u1(A), H) :- !, H is Ar+1.
x_def_use_heap(get_list(A), x_d0u1(A), 2) :- !.
x_def_use_heap(get_constant_x0(_), x_d0u1(0), 0) :- !.
x_def_use_heap(get_large_x0(C), x_d0u1(0), H) :- !,
	large_heap_usage(C, H).
x_def_use_heap(get_nil_x0, x_d0u1(0), 0) :- !.
x_def_use_heap(get_structure_x0(_/Ar), x_d0u1(0), H) :- !, H is Ar+1.
x_def_use_heap(get_list_x0, x_d0u1(0), 2) :- !.
x_def_use_heap(put_x_variable(V,A), x_d2u0p(V,A), 1) :- !.
x_def_use_heap(put_y_variable(_,A), x_d1u0(A), 0) :- !.
x_def_use_heap(put_x_value(V,A), x_d1u1p(A,V), 0) :- !.
x_def_use_heap(put_y_value(_,A), x_d1u0(A), 0) :- !.
x_def_use_heap(put_x_unsafe_value(V,A), x_d1u1live(A,V,_), 1) :- !.
x_def_use_heap(put_y_unsafe_value(_,A), x_d1u0(A), 1) :- !.
x_def_use_heap(put_constant(_,A), x_d1u0(A), 0) :- !.
x_def_use_heap(put_large(C,A), x_d1u0(A), H) :- !,
	large_heap_usage(C, H).
x_def_use_heap(put_nil(A), x_d1u0(A), 0) :- !.
x_def_use_heap(put_structure(_/Ar,A), x_d1u0(A), H) :- !, H is Ar+1.
x_def_use_heap(put_list(A), x_d1u0(A), 2) :- !.
x_def_use_heap(unify_x_variable(V), x_d1u0p(V), 0) :- !.
x_def_use_heap(unify_x_value(V), x_d0u1(V), 0) :- !.
x_def_use_heap(unify_x_local_value(V), x_d0u1(V), 0) :- !.
x_def_use_heap(unify_structure(_/Ar), x_d0u0, H) :- !, H is Ar+1.
x_def_use_heap(unify_large(C), x_d0u0, H) :- !,
	large_heap_usage(C, H).
x_def_use_heap(unify_list, x_d0u0, 2) :- !.
x_def_use_heap(_, x_d0u0, 0).

% Which instructions have operands encoding heap requirements?
insn_heap_usage(function_1(_,_,_,H,_), H) :- !.
insn_heap_usage(function_2(_,_,_,_,H,_), H) :- !.
insn_heap_usage(_, _).

large_heap_usage(F, 4) :- float(F), !.
large_heap_usage(I, H) :-
	integer(I),
	I>>25 =\= I>>26,
	J is I>>31,
	int_heap_usage(J, 3, H).

int_heap_usage(I, H0, H) :-
	(   I =:= I>>1 -> H=H0
	;   J is I>>16,
	    K is J>>16,
	    H1 is H0+1,
	    int_heap_usage(K, H1, H)
	).

% What instructions jump?
xfer_insn(call(_/A,_), A).
xfer_insn(execute(_/A), A).


%-----------------------------------------------------------------------------
% Final pass over entire compiled code to emit 'allocate', 'deallocate',
% and cut related instructions.

peep_clause(recursive(_), Code) --> !, peep(Code, noenv, _Dic).
peep_clause(_, Code) --> peep(Code).

% simple case: no environments.
peep([]) --> [].
peep([Insn|Insns]) --> peep_1(Insn), peep(Insns).

peep_1(execute('basiccontrol:true'/0)) --> !, [proceed].
peep_1(execute('basiccontrol:fail'/0)) --> !, [fail].
peep_1(cut_x(-1)) --> !, [cutb].
peep_1(cut_x(X)) --> !, [cutb_x(X)].
peep_1(get_x_variable(U,U)) --> !, [].
peep_1(put_x_value(U,U)) --> !, [].
peep_1(put_x_value(-1,U)) --> !, [choice_x(U)].
peep_1(put_x_variable(void,void)) --> !, [].
peep_1(put_x_variable(A,A)) --> !, [put_x_void(A)].
peep_1(unify_x_variable(void)) --> !, [unify_void].
peep_1(ensure_space(Kind,EffAr,H)) --> !, emit_ensure_space(Kind, EffAr, H).
peep_1(Insn) --> peep_heap_usage(Insn).

peep_heap_usage(function_1(A,B,C,H,Set)) --> !,
	[function_1(A,B,C,H,Arity)],
	{live_arity(Set, 0, Arity)}.
peep_heap_usage(function_2(A,B,C,D,H,Set)) --> !,
	[function_2(A,B,C,D,H,Arity)],
	{live_arity(Set, 0, Arity)}.
peep_heap_usage(Insn) --> [Insn].

live_arity([], X, X) :- !.
live_arity([X|Xs], _, Y) :- X1 is X+1, live_arity(Xs, X1, Y).


% general case: treat environments.
peep([], _, _Dic) -->
	[].
peep([Insn|Insns], Env, Dic) -->
	peep_1(Insn, Env, Env1, Dic),
	peep(Insns, Env1, Dic).

peep_1(else, _, noenv, _) --> !, [else].
peep_1(cut_x(-1), noenv, noenv, _) --> !, [cutb].
peep_1(cut_x(X), noenv, noenv, _) --> !, [cutb_x(X)].
peep_1(cut_x(-1), env(A,B), env(A,B), _) --> !, [cute].
peep_1(cut_x(X), env(A,B), env(A,B), _) --> !, [cute_x(X)].
peep_1(cut_x(-1), env(A), env(A), _) --> !, [cutf].
peep_1(cut_x(X), env(A), env(A), _) --> !, [cutf_x(X)].
peep_1(call(U,A), Env, Env1, Dic) --> !,
	ensure_call(Env, Env1, A, Dic),
	[call(U,A)].
peep_1(true(A), Env, Env1, Dic) --> !,
	ensure_call(Env, Env1, A, Dic),
	[true(A)].
peep_1(execute('basiccontrol:true'/0), noenv, noenv, _) --> !, 
	[proceed].
peep_1(execute('basiccontrol:fail'/0), noenv, noenv, _) --> !, 
	[fail].
peep_1(execute(U), Env, Env1, _) --> !,
	ensure_no_env(Env, Env1),
	[execute(U)].
peep_1(get_x_variable(U,U), Env, Env, _) --> !.
peep_1(get_y_variable(U,-1), Env, Env1, _) --> !,
	ensure_env(Env, Env1, U),
	[choice_y(U)].
peep_1(put_x_value(U,U), Env, Env, _) --> !.
peep_1(put_x_value(-1,U), Env, Env, _) --> !, [choice_x(U)].
peep_1(put_x_variable(void,void), Env, Env, _) --> !.
peep_1(put_x_variable(A,A), Env, Env, _) --> !, [put_x_void(A)].
peep_1(get_y_variable(Y,A), env(E), env(E), _) --> !, [get_y_first_value(Y,A)].
peep_1(put_y_variable(Y,A), env(E), env(E), _) --> !, [put_y_value(Y,A)].
peep_1(unify_y_variable(Y), env(E), env(E), _) --> !, [unify_y_first_value(Y)].
peep_1(unify_x_variable(void), Env, Env, _) --> !, [unify_void].
peep_1(ensure_space(Kind,EffAr,H), Env, Env, _) --> !,
	emit_ensure_space(Kind, EffAr, H).
peep_1(get_y_variable(Y,A), Env, Env1, _) --> !,
	ensure_env(Env, Env1, Y),
	[get_y_variable(Y,A)].
peep_1(unify_y_variable(Y), Env, Env1, _) --> !,
	ensure_env(Env, Env1, Y),
	[unify_y_variable(Y)].
peep_1(put_y_variable(Y,A), Env, Env1, _) --> !,
	ensure_env(Env, Env1, Y),
	[put_y_variable(Y,A)].
peep_1(Insn, Env, Env, _) --> peep_heap_usage(Insn).

emit_ensure_space(Kind, EffAr, H) -->
	(   {Kind=cont, H>128} -> [heapmargin_call(H, 0)]
	;   {H>1152} -> [heapmargin_call(H, EffAr)]
	;   []
	).

ensure_env(noenv, env(_,Ys), Y) -->
	[allocate],
	{Set0=[]},
	{intset_insert(Set0, Y, Ys)}.
ensure_env(env(Size), env(Size), _) --> [].
ensure_env(env(Size,Ys), env(Size,Ys1), Y) -->
	{intset_insert(Ys, Y, Ys1)}.

ensure_call(noenv, env(_), Size, Dic) -->
	[allocate, init(L)],
	{Set0=[]},
	{init_list(0, Size, Set0, L, Dic)}.
ensure_call(env(Size), env(_), Size, _) --> [].
ensure_call(env(Size,Ys), env(_), Size, Dic) -->
	[init(L)],
	{init_list(0, Size, Ys, L, Dic)}.

ensure_no_env(noenv, noenv) --> [].
ensure_no_env(env(-1), noenv) --> [deallocate].

init_list(Max, Max, _, [], _) :- !.
init_list(Y, Max, Perms, Ys, Dic) :-
	intset_in(Y, Perms), !,
	Y1 is Y+1,
	init_list(Y1, Max, Perms, Ys, Dic).
init_list(Y, Max, Perms, [Y|Ys], Dic) :-
	Y1 is Y+1,
	init_list(Y1, Max, Perms, Ys, Dic).


c(S0, S, S0, S).
