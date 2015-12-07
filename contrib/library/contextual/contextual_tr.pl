:- module( contextual_tr, [ contextual_sentence_tr/3], [assertions, regtypes, isomodes]).

:- op(1200, xfx,[(-->)]).
% ======================================================================

:- include( contextual_tr_doc).

%:- doc( context/5, "The contextual


:- use_module(library(contextual/contextual_rt)).
:- use_module(library(lists), [append/3]).
:- use_module(library(aggregates)).
:- use_module(library(format), [format_to_string/3]).

:- data '$_context_expansion'/3.
%:- export( '$_context_expansion'/3).
:- data current_signature/1.
:- data required_module/1.
:- data module_location/2.
:- data method_declaration/4.
:- data method_implementation/3.
:- data abstract_context/2.
:- data interface_def/2.
:- data default_module/1.
:- data included_types/1.

%:- export( method_declaration/4).
%:- export( method_implementation/3).

% ======================================================================

contextual_sentence_tr( 0, [], _Mod):- !,
	retractall_fact( '$_context_expansion'( _, _, _)),
	retractall_fact( required_module( _)),
	retractall_fact( module_location( _, _)),
	retractall_fact( method_declaration( _, _, _, _)),
	retractall_fact( method_implementation( _, _, _)),
	retractall_fact( abstract_context( _, _)),
	retractall_fact( interface_def( _, _)),
	retractall_fact( default_module( _)),
	retractall_fact( included_types( _)),
	asserta_fact( default_module( _Mod)).

contextual_sentence_tr( (:- def_context( Def)), CheckerClauses, Mod):- !,
	parse_macro_def( Mod, Def, MMod, Macro, Expansion),
	(  '$_context_expansion'( Macro, _, _)
	-> error( [$$("Attempt to redefine context "), ''(Macro),
	           $$(" in module "), Mod]),
	   fail
	;  register_macro( MMod, Macro, Expansion, Mod, CheckerClauses)
	).

contextual_sentence_tr( (:- undef_context( Left)), [], _):- !,
	nonvar( Left), callable( Left),
	retractall_fact( '$_context_expansion'( Left, _, _)).

contextual_sentence_tr( (H --> B), [(H1:- B1)], _):- !,
	transform_clause( H/'$source', ('$source' --> B), H1, B1).

contextual_sentence_tr( (:- def_interface( IDef)), [], Mod):- !,
	nonvar( IDef), IDef= (Iface=Def),
	define_interface( Iface, Def, Mod).

contextual_sentence_tr( (:- apply_interface( Iface)), Clauses, Mod):- !,
	apply_interface( Iface, Clauses, Mod).

contextual_sentence_tr( (:- default_module( MMod)), [], _):-
	atom( MMod), !,
	retractall_fact( default_module( _)),
	asserta_fact( default_module( MMod)),
	(  required_module( MMod)
	-> true
	;  assertz_fact( required_module( MMod))
	).

contextual_sentence_tr( (:- default_module), [], Mod):-
	retractall_fact( default_module( _)),
	(  ( Mod = user( _); Mod = user)
	-> true
	;  asserta_fact( default_module( Mod))
	).

contextual_sentence_tr( end_of_file, Clauses, Mod):- !,
	check_interface( Mod, Clauses0),
	use_required_modules( Mod, UseModules),
	append( Clauses0, UseModules, Clauses1),
	append( Clauses1, [end_of_file], Clauses). 

contextual_sentence_tr( (H :- B), [(H1 :- B1)], _ ):-
	transform_clause( H, B, H1, B1).

contextual_sentence_tr( (:- module_location( LocDef)), [], _Mod):-
	nonvar( LocDef), ground( LocDef),
	(  proper_list( LocDef)
	-> register_location( LocDef)
	;  register_location( [LocDef])
	).

contextual_sentence_tr( (:- types), Clauses, Mod):- !,
	(  Mod=user( X), atom( X)
	-> Stem= X
	;  Stem= Mod
	),
	contextual_sentence_tr( (:- types( Stem)), Clauses, Mod).

contextual_sentence_tr( (:- types( Mod)), Clauses, _):-
	ground( Mod), callable( Mod), !,
	(  included_types( Mod)
	-> Clauses= []
	;  assertz_fact( included_types( Mod)),
	   add_types_ext( Mod, File),
	   Clauses= [(:- include( File))]
	).
	
% ----------------------------------------------------------------------

add_types_ext( X, Y):-
	X=.. [XF|XR],
	(  XR=[]
	-> format_to_string( "~w", [XF], S),
	   (  append( P, ".pl", S)
	   -> append( P, "_types.pl", S1)
	   ;  append( S, "_types", S1)
	   ),
	   atom_codes( Y, S1)
	;  XR= [A1|AR],
	   add_types_ext( A1, A1T),
	   Y=.. [XF, A1T | AR]
	).
% ......................................................................

register_macro( MMod, Macro, Expansion, Module, CheckerClauses):-
	retractall_fact( '$_context_expansion'( Macro, MMod, _)),
	assertz_fact( '$_context_expansion'( Macro, Expansion, MMod)),
	(  uniform_macro( Macro, Sig)
	-> build_checker( Macro, Cl1),
	   build_maker( Macro, Cl2),
	   append( Cl1, Cl2, CheckerClauses),
	   (  MMod\=Module, MMod\=abstract, MMod\=user, 
	      \+ required_module( MMod)
	   -> assertz_fact( required_module( MMod))
	   ;  MMod=abstract
	   -> assertz_fact( abstract_context( Macro, Sig))
	   ;  true
	   )
	;  true
	).

uniform_macro( M, Sig):-
	context( i, M, [Ti], _, _),
	context( o, M, [To], _, _),
	signature( Ti, Sig), signature( To, Sig).

register_location( []).
register_location( [D|Rest]):-
	nonvar( D), D=(Mod=Loc), atom( Mod), nonvar( Loc),
	callable( Loc),
	asserta_fact( module_location( Mod, Loc)),
	register_location( Rest).

% ======================================================================

transform_clause( Head, Body, NewHead, NewBody):-
	retractall_fact( current_signature( _)),
	mapping( h, Head, NewHead, Cin, Gin, Cout, Gout),

	connect( Cin, Gin, Body, C1, _G1, Body1),
	commons( C1, Cout),
	preconditions( Cin, Gin, Body1, Body2),

	(  final_goal( Body1, FG),
	   \+ FG=NewHead
	-> gamma_join( Gin, Gout, Gprim),
	   gamma_diff( Gprim, Cin, C1, Gpost),
	   postconditions( C1, Gpost, Body2, NewBody)
	;  NewBody= Body2
	).

% ......................................................................

final_goal( (_, B), C):- !,
	final_goal( B, C).
final_goal( (_ -> B), C):- !,
	final_goal( B, C).
final_goal( A, A).

elim_ident( [], _, []).
elim_ident( [A|L], B, R):-
	member( X, B),
	X==A, !,
	elim_ident( L, B, R).
elim_ident( [A|L], B, [A|R]):-
	elim_ident( L, B, R).

% ======================================================================

:- export( context/5).

context( IO, Spec, T, Cout, Gout):-
	context1( IO, [], [], [], [], Spec, T, Cout, _, Gout-[]).

% ----------------------------------------------------------------------

% Logical variable in context

context1( _, C0, L0, _, _, V, [V], C0, L0, G-G):- 
	var( V), !.

% Empty context

context1( _, C0, L0, _, _, (*), [], C0, L0, G-G):- !.

% Empty list is an atom, but we want to treat it as a literal, instead
% of as a context variable

context1( _, C0, L0, _, _, [], [[]], C0, L0, G-G):- !.

% Anything - always produces a fresh variable

context1( _, C0, L0, _, _, (?), [_], C0, L0, G-G):- !.

% Functor preservation with {}/1

context1( _, C0, L0, _, _, {V}, [V], C0, L0, G-G):-
	var( V),  !.
context1( IO, C0, L0, Expanded, Pfx, {X}, [X1], C1, L0, GA-GZ):- 
	nonvar( X), !,
	X=.. [F|Args],
	context2( IO, C0, L0, Expanded, Pfx, Args, Args1-[], C1, GA-GZ),
	X1=.. [F|Args1].

% Name prefix

context1( IO, C0, L0, Expanded, Pfx, F^X, T, C1, L1, GA-GZ):-
	atom( F), !,
	context1( IO, C0, L0, Expanded, [F|Pfx], X, T, C1, L1, GA-GZ).

% Typing

context1( IO, C0, L0, Expanded, Pfx, '::'(X, Y), T, C1, L0, GA-GZ):-
	nonvar( X), 
	context1( IO, C0, L0, Expanded, Pfx, X, T, C0a, L0a, GA-GB),
	add_typing_all( T, Y, C0a, L0a, Expanded, Pfx, C1, GB-GZ),
	!.

% Input only

context1( IO, C0, L0, Expanded, Pfx, +X, T, C1, L1, GA-GZ):- !,
	(  IO = i
	-> context1( i, C0, L0, Expanded, Pfx, X, T, C1, L1, GA-GZ)
	;  T=[], C1=C0, L1=L0, GA=GZ
	).

% Output only

context1( IO, C0, L0, Expanded, Pfx, -X, T, C1, L1, GA-GZ):- !,
	(  IO = o
	-> context1( o, C0, L0, Expanded, Pfx, X, T, C1, L1, GA-GZ)
	;  T=[], C1=C0, L1=L0, GA=GZ
	).

% Difference notation

context1( IO, C0, L0, Expanded, Pfx, X-Y, T, C1, L1, GA-GZ):- !,
	(  IO= i
	-> context1( i, C0, L0, Expanded, Pfx, X, T, C1, L1, GA-GZ)
	;  context1( o, C0, L0, Expanded, Pfx, Y, T, C1, L1, GA-GZ)
	).

% Joining with comma

context1( IO, C0, L0, Expanded, Pfx, (X,Y), T, C1, L0, GA-GZ):- !,
	context1( IO, C0, L0, Expanded, Pfx, X, T1, C0a, L0a, GA-GB),
	context1( IO, C0a, L0a, Expanded, Pfx, Y, T2, C1, _, GB-GZ),
	append( T1, T2, T).

% Contracting list

context1( IO, C0, L0, Expanded, Pfx, [X|Y], T, C1, L0, GA-GZ):- !,
	context1( IO, C0, L0, Expanded, Pfx, X, T1, C0a, L0a, GA-GB),
	context1( IO, C0a, L0a, Expanded, Pfx, Y, T2, C1, _, GB-GZ),
	to_list( T1, T2, T).

% Extension atom for structure

context1( IO, C0, L0, Expanded, Pfx, (\X), T, C1, L1, GA-GZ):-
	extname( X, A), !,
	context1( IO, C0, L0, Expanded, Pfx, A, T, C1, L1, GA-GZ).

% Local macro definition

context1( _, C0, L0, _, _, (M=E), [], C0, [N=E|L0], G-G):- 
	nonvar( M), 
	(  M= \X, nonvar( X), callable( X)
	-> extname( X, N)
	;  atom( M), N= M
	), !.

% Transformation

context1( IO, C0, L0, Expanded, Pfx, ( From -> To ), T, C1, L1, GA-GZ):-
	nonvar( From), callable( From),
	nonvar( To), callable( To),
	context1( IO, C0, L0, Expanded, Pfx, ( +From, -([](To))), 
	T, C1, L1, GA-GZ).

% Closed form

context1( IO, C0, L0, Exp, Pfx, [](X), T, C1, L1, GA-GZ):-
	cl_form( X, Y), !,
	context1( IO, C0, L0, Exp, Pfx, Y, T, C1, L1, GA-GZ).

% Context macro expansion

context1( IO, C0, L0, Expanded, Pfx, A, T, C1, L0, GA-GZ):-
	nonvar( A), callable( A), 
	(  member( A=Expansion, L0)
	;  '$_context_expansion'( A, Expansion, _)
	), !,
	(  \+ member( A, Expanded)
	-> context1( IO, C0, L0,[A|Expanded], Pfx, Expansion, T, C1, _, GA-GZ)
	;  throw( invalid_context( circular_reference( A)))
	).

% Context variable

context1( _, C0, L0, _, Pfx, A, [V], C1, L0, G-G):-
	atom( A), !, comb_prefix( Pfx, A, QA), 
	(  member( QA=V, C0)
	-> C1= C0 
	;  C1= [QA=V|C0] 
	).

% Other atomic stuff taken literally

context1( _, C0, L0, _, _, X, [X], C0, L0, G-G):-
	atomic( X), !.

% Throw an error on other stuff

context1( _, _, _, _, _, X, _, _, _, _):-
	throw( invalid_context( unknown_macro( X))).

% Generally treat all other terms

% context1( IO, C0, L0, Expanded, Pfx, X, [X1], C1, L0, GA-GZ):-
% 	X=.. [F|Args],
% 	context2( IO, C0, L0, Expanded, Pfx, Args, Args1-[], C1, GA-GZ),
% 	X1=.. [F|Args1].

% ......................................................................

cl_form( X, X):- var( X), !.
cl_form( (X,Y), ([](X), [](Y))):- !.
cl_form( (X->Y), (([](X)->Y))):- !.
cl_form( [X|Y], [[](X)|[](Y)]):- !.
cl_form( {X}, {X}):- var( X), !.
cl_form( {X}, {Y}):- !,
	X=.. [F | A],
	cl_arg( A, CA),
	Y=.. [F | CA].
cl_form( +X, +([](X))):- !.
cl_form( -X, -([](X))):- !.
cl_form( X::Y, [](X)::Y):- !.
cl_form( M=E, M=E):- !.
cl_form( \X, \X):- !.
cl_form( X, (\X=[], X)):-
	callable( X), !.
cl_form( X, X).
	
cl_arg( [], []).
cl_arg( [A|L], [CA|R]):-
	cl_form( A, CA),
	cl_arg( L, R).

% ......................................................................

good_macro( X):- 
	nonvar( X), callable( X), functor( X, F, A), 
	\+ member( F/A, ['*'/0, '?'/0, ','/2, '->'/2, '::'/2, []/0,
	'\\'/1, '.'/2, '/'/2, '//'/2, '{}'/1, '+'/1, '-'/1, '='/2, 
	':'/2, '-'/2]).

% ......................................................................

context2( _, C0, _, _, _, [], A-A, C0, G-G):- !.
context2( IO, C0, L0, Expanded, Pfx, [H|T], A-Z, Cout, GA-GZ):-
	context1( IO, C0, L0, Expanded, Pfx, H, H1, C1, L1, GA-GB),
	set_difflist( H1, A-B),
	context2( IO, C1, L1, Expanded, Pfx, T, B-Z, Cout, GB-GZ).

% ======================================================================

extname( X, A):-
	nonvar( X), 
	functor( X, XF, XA),
	format_to_string( "$_~w/~w_ext", [XF, XA], S),
	atom_codes( A, S).

% ======================================================================

% context_terms( Ctx, Bindings, T):- 
% 	proper_list( Bindings),
% 	context( o, Ctx, T, C, G),
% 	construct_

% :- export( complement/3).

complement( [], C, C).
complement( [A=V|B], C0, C1):-
	overridden( A=V, C0), !,
	complement( B, C0, C1).
complement( [A=V|B], C0, C1):-
	complement( B, [A=V|C0], C1).

% :- export( overridden/2).

overridden( A=_, [A=_|_]):-
	!.
overridden( X, [_|L]):-
	overridden( X, L).


comb_prefix( [], V, V).
comb_prefix( [P|A], V, R):-
	comb_prefix( A, P^V, R).

set_difflist( [], Z-Z).
set_difflist( [X|L], [X|A]-Z):-
	set_difflist( L, A-Z).

%:- export( set_difflist/2).
/*
comb_pair( [], []).
comb_pair( [X], [X]).
comb_pair( [X, Y], [(X, Y)]).
*/
to_list( X, [], [X]):- !.
to_list( [], [A|_], [A]):- !.
to_list( [X|L], [A|_], [R]):- 
	to_list2( [X|L], A, R).

to_list2( [X], A, [X|A]):- !.
to_list2( [X,Y|L], A, [X|R]):-
	to_list2( [Y|L], A, R).
/*
rectail( [A], A).
rectail( [A,B|L], [A|R]):-
	rectail( [B|L], R).

entail( [X], A, [X|A]).
entail( [X,Y|L], A, [X|R]):-
	entail( [Y|L], A, R).
*/
%:- export( to_list/3).

% ======================================================================

%:- export( mapping/7).

mapping( h, Goal0, Goal1, C0, G0, C1, G1):-
	nonvar( Goal0), 
	Goal0 = SubGoal/Context, !,
	nonvar( SubGoal), callable( SubGoal),

        context( i, Context, T0, C0, G0),
	context( o, Context, T1, C1, G1),
	SubGoal =.. [ F | L ],
	append( L, T0, L1),
	append( L1, T1, L2),
	Goal1 =.. [ F | L2 ].

mapping( b, \+(Call0), \+(Call1), C0, G0, C1, G1):- !,
	mapping( b, Call0, Call1, C0, G0, C1, G1).

mapping( b, Call0, T0=[H|Tprim], C0, G0, C1, G1):-
	nonvar( Call0), 
	Call0 = SubGoal/Context,
	nonvar( SubGoal), SubGoal=[H|T],
	proper_list( T), !,

	context( i, Context, [T0], C0, G0),
	context( o, Context, [T1], C1, G1), !,
	add_tail( T, T1, Tprim).

mapping( b, Call0, Call1, C0, G0, C1, G1):-
	nonvar( Call0), 
	Call0 = SubGoal/Context, 
	nonvar( SubGoal), SubGoal= get( X), !,
	mapping( b, =(X) / +Context, Call1, C0, G0, C1, G1).

mapping( b, Call0, Call1, C0, G0, C1, G1):-
	nonvar( Call0), 
	Call0 = SubGoal/Context, 
	nonvar( SubGoal), SubGoal= set( X), !,
	mapping( b, =(X) / -Context, Call1, C0, G0, C1, G1).

mapping( b, Call0, Call1, C0, G0, C1, G1):-
	nonvar( Call0), 
	module_call( Call0, Module, Goal0), 
	Goal0 = SubGoal/Context, !,
	nonvar( SubGoal), callable( SubGoal),

        context( i, Context, T0, C0, G0),
	context( o, Context, T1, C1, G1),
	SubGoal =.. [ F | L ],
	append( L, T0, L1),
	append( L1, T1, L2),
	Goal1 =.. [ F | L2 ],
	module_call( Call1, Module, Goal1).

mapping( h, Goal0, Goal1, C0, G0, C1, G1):-
	nonvar( Goal0),
	Goal0 = SubGoal//Context,
	nonvar( SubGoal), callable( SubGoal),
	SubGoal \= _^_, !,

	context( i, Context, T0, C0, G0),
	context( o, Context, T1, C1, G1),
	append( T0, T1, [LT0|_]),
	signature( LT0, Sig), 
	functor( SubGoal, F, A),
	assertz_fact( current_signature( Sig)),
	assertz_fact( method_implementation( F/A, Sig, Context)),

	SubGoal =.. [F|L],

	Goal1 =.. ['$_context_method', F, Sig, L, T0, T1].

mapping( b, Goal0, Goal1, C0, G0, C1, G1):-
	nonvar( Goal0),
	Goal0 = SuperGoal//Context,
	nonvar( SuperGoal), SuperGoal= super^SubGoal, !,
	nonvar( SubGoal), callable( SubGoal),
	nonvar( Context),
	
	current_signature( Sig),
	supersignature( Sig, SuperSig),

	check_meth_call( SubGoal, Context),

	context( i, Context, T0, C0, G0),
	context( o, Context, T1, C1, G1),
	SubGoal =.. [ F | L ],
	Goal1 =.. ['$_context_method', F, SuperSig, L, T0, T1].

mapping( b, Goal0, Goal1, C0, G0, C1, G1):-
	nonvar( Goal0),
	Goal0 = SubGoal//Context, !,
	nonvar( SubGoal), callable( SubGoal),
	nonvar( Context),

	check_meth_call( SubGoal, Context),

	context( i, Context, T0, C0, G0),
	context( o, Context, T1, C1, G1),
	SubGoal =.. [ F | L ],
	append( T0, T1, TW),
	TW= [Sig|_],
	Goal1 =.. ['$_context_method', F, Sig, L, T0, T1].

% ----------------------------------------------------------------------

%check_meth_call( _, _).

check_meth_call( Subgoal, Ctx):-
	functor( Subgoal, F, A),
	(  method_declaration( F/A, _, DC, _),
	   compatible_contexts( Ctx, DC)
	-> true
	;  warning( [$$("Call to undeclared method "), ''( Subgoal//Ctx)])
	).

merge( [], C1, C2, [], [], L):-
	!, commons( C1, C2),
	complement( C1, C2, L).

merge( [A=V0|R], C1, C2, X1, X2, [A=NV|NR]):-
	append( PC1, [A=V1|SC1], C1), 
	append( PC2, [A=V2|SC2], C2),
	append( PC1, SC1, C1x),
	append( PC2, SC2, C2x),
	(  V0==V1
	-> (  V0==V2
	   -> NV= V0, X1=X1a, X2=X2a
	   ;  X1= [V1=V2|X1a], X2=X2a, NV= V2
	   )
	;  (  V0==V2
	   -> X2=[V1=V2|X2a], X1=X1a, NV= V1
	   ;  V1=V2, NV=V1, X2= X2a, X1=X1a
	   )
	),
	merge( R, C1x, C2x, X1a, X2a, NR).

conc_goals( ( P, Q), R, (P, U)):- !,
	conc_goals( Q, R, U).
conc_goals( ( P -> Q), R, (P -> U)):-
	conc_goals( Q, R, U).
conc_goals( P, (Q,R), (P, Q, R)):- !.
conc_goals( P, (Q -> R), (P, Q -> R)):- !.
conc_goals( P, Q, (P, Q)).

add_eqns( [], G, G):- !.
add_eqns( L, G, G2):-
	split_eqns( L, L1, L2),
	C1=.. [context | L1],
	C2=.. [context | L2],
	conc_goals( C1=C2, G, G2).

split_eqns( [], [], []).
split_eqns( [V1=V2|A], [V1|B], [V2|C]):-
	split_eqns( A, B, C).

/*
union( [], C2, C2):- !.
union( [A=V|B], C2, [A=V|L]):-
	(  append( Prefix, [A=V|Suffix], C2) 
	-> append( Prefix, Suffix, C2a)
	;  C2a= C2
	),
	union( B, C2a, L).
*/
% ----------------------------------------------------------------------

connect( Cin, Gin, Goal, Cout, Gout, ExpGoal):-
	transform_goal( Goal, GoalPrim), !,
	connect( Cin, Gin, GoalPrim, Cout, Gout, ExpGoal).

connect( Cin, Gin, (G1 , G2), Cout, Gout, ExpGoal):- !,
	connect( Cin, Gin, G1, Cprim, Gprim, XG1),
	connect( Cprim, Gprim, G2, Cout, Gout, XG2),
	conc_goals( XG1, XG2, ExpGoal).

connect( Cin, Gin, (G1 -> G2), Cout, Gout, (XG1 -> XG2)):- !,
	connect( Cin, Gin, G1, Cprim, Gprim, XG1),
	connect( Cprim, Gprim, G2, Cout, Gout, XG2).

connect( Cin, Gin, (G1 ; G2), Cout, Gout, (XEG1; XEG2)):- !,
	connect( Cin, Gin, G1, C10, G10, EG1),
	connect( Cin, Gin, G2, C20, G20, EG2),
	merge( Cin, C10, C20, X1, X2, Cout),
	add_eqns( X1, EG1, XEG1),
	add_eqns( X2, EG2, XEG2),
	gamma_isect( G10, G20, Gout).


connect( Cin, Gin, Goal, Cout, Gout, ExpGoal):-
	mapping( b, Goal, ExpGoal, C0, _G0, C1, G1), !,
	commons( Cin, C0),
	theta_diff( C0, Cin, Cuninit),
	warn_uninit( Cuninit, Goal),
	theta_diff( Cin, C1, Cprim),
	append( Cprim, C1, Cout),
	gamma_join( Gin, G1, Gout).

connect( Cin, Gin, Goal, Cin, Gin, Goal).

% ......................................................................

transform_goal( Goal, call( Goal)):-
	var( Goal), !.

transform_goal( V / Context, call( V) / Context):- 
        var( V), !.

transform_goal( (Axis --> Body), Body/Axis):- !.

transform_goal( [C1|C2], (get( X)/C1, set( X)/C2)):-
	nonvar( C1), callable( C1), nonvar( C2), callable( C2), !.

transform_goal( ! / _, !):- !.

transform_goal( [] / _, true):- !.

transform_goal( {G} / _, G):- !.

transform_goal( (A,B) / C, (A/C, B/C)):- !.

transform_goal( (A->B) / C, (A/C -> B/C)):- !.

transform_goal( (A;B) / C, (A/C;B/C)):- !.

% ----------------------------------------------------------------------

theta_diff( [], _, []).
theta_diff( [X=_|A], B, C):-
	member( X=_, B), !,
	theta_diff( A, B, C).
theta_diff( [E|A], B, [E|C]):-
	theta_diff( A, B, C).

gamma_join( [], L, L).
gamma_join( [X/P|A], L, B):-
	member( X/R, L),
	P==R, !,
	gamma_join( A, L, B).
gamma_join( [E|A], L, [E|B]):-
	gamma_join( A, L, B).

gamma_diff( [], _, _, []).
gamma_diff( [X/_|A], C0, C1, B):-
	member( X=V0, C0),
	member( X=V1, C1),
	V0==V1, !,
	gamma_diff( A, C0, C1, B).
gamma_diff( [E|A], C0, C1, [E|B]):-
	gamma_diff( A, C0, C1, B).

gamma_isect( [], _, []).
gamma_isect( [X/P|A], B, [X/P|C]):-
	member( X/R, B),
	R==P, !,
	gamma_isect( A, B, C).
gamma_isect( [_|A], B, C):-
	gamma_isect( A, B, C).
/*
gamma_new( [], _, []).
gamma_new( [X/_|A], C, B):-
	member( X=_, C), !,
	gamma_new( A, C, B).
gamma_new( [E|A], C, [E|B]):-
	gamma_new( A, C, B).
*/

commons( [], _).
commons( [X|S1], S2):-
	(  member( X, S2) -> true
	;  true
	),
	commons( S1, S2).	

proper_list( E):- var( E), !, fail.
proper_list( []).
proper_list( [_|T]):- proper_list( T).

add_tail( [], T, T).
add_tail( [A|B], Y, [A|C]):-
	add_tail( B, Y, C).

/*
norm_seq( (A,B), D):- !,
	norm_seq( A, C),
	norm_seq( B, E),
	norm_seq2( C, E, D).
norm_seq( D, D).

norm_seq2( (A,B), C, D):- !,
	norm_seq2( A, (B,C), D).
norm_seq2( A, C, (A,C)).
*/

module_call( Module:Call, Module, Call):-
	nonvar( Module), !.
module_call( Call, _, Call).

build_checker( Macro, [(Head :- Body), (SHead:-true)]):-
	context( i, Macro, T, C, G),
	Macro=.. [ F | L],
	append( T, L, L1),
	Head=.. [F | L1],
	filter_guards( G, G1),
	elim_dup_guards( G1, G2),
	(  guards_to_seq( C, G2, Body)
	-> true
	;  Body= true
	),
	format_to_string( "struct__~a", [F], SFS),
	atom_codes( SF, SFS),
	SHead=.. [SF | T].

build_maker( Macro, [(Head1 :- Body1), (Head2 :- Body2)]):-
	context( o, (\Macro=[], Macro), T, C, G),
	Macro=.. [F | L],
	append( L, T, L1),
	format_to_string("make__~a", [F], F1S),
	atom_codes( F1, F1S),
	Head1=.. [F1, [] | L1],
	filter_guards( G, G1),
	elim_dup_guards( G1, G2),
	(  guards_to_seq( C, G2, Body1)
	-> true
	;  Body1= true
	),
	Head2=.. [F1, [A=V|B] | L1],
	gen_setters( C, A, V, Setters),
	TailCall=.. [F1, B | L1],
	conc_goals( Setters, TailCall, Body2).

gen_setters( [], A, _, throw( context_maker( invalid_argument( A)))).
gen_setters( [T=W|L], A, V, ((A==T -> V=W);Rest)):-
	gen_setters( L, A, V, Rest).
	

elim_dup_guards( [], []).
elim_dup_guards( [X/Guard|A], B):-
	\+( \+( (member( X/Duplicate, A), Duplicate==Guard ) )), !,
	elim_dup_guards( A, B).
elim_dup_guards( [P|A], [P|B]):-
	elim_dup_guards( A, B).

filter_guards( [], []).
filter_guards( [_/Q|A], B):- nonvar( Q), Q=term, !,
	filter_guards( A, B).
filter_guards( [G|A], [G|B]):-
	filter_guards( A, B).

%:- export( build_checker/2).

%:- export( guards_to_seq/3).

guards_to_seq( C, [X], Y) :- guard_to_goal( C, X, Y).
guards_to_seq( C, [X,Y|R], (U, W)):-
	guard_to_goal( C, X, U),
	guards_to_seq( C, [Y|R], W).

guard_to_goal( C, X/P, call( P, V)):- 
	var( P), !,
	member( X=V, C).
guard_to_goal( C, X/P, Goal):-
	callable( P),
	member( X=V, C),
	P =.. [ PF | PA ],
	Goal =.. [ PF, V | PA ]. 

add_typing_all( [], _, C0, _, _, _, C0, G-G).
add_typing_all( [V|R], Y, C0, L0, Expanded, Pfx, C1, GA-GZ):-
%	[C0]=[C0],
	var( V), member( B=W, C0), W==V, !,
	add_typing( B, Y, C0, L0, Expanded, Pfx, C0a, GA-GB),
	add_typing_all( R, Y, C0a, L0, Expanded, Pfx, C1, GB-GZ).

add_typing( QA, Y, C0, _, _, _, C0, [ QA/Y |GZ]-GZ):-
	var( Y), !.
add_typing( QA, (Y1,Y2), C0, L0, Expanded, Pfx, C1, GA-GZ):- !,
	add_typing( QA, Y1, C0, L0, Expanded, Pfx, C0a, GA-GB),
	add_typing( QA, Y2, C0a, L0, Expanded, Pfx, C1,  GB-GZ).
add_typing( QA, [Y|Args], C0, _L0, _Expanded, _Pfx, C0, [QA/YI|GZ]-GZ):-
	atom( Y), proper_list( Args), !, 
	YS=.. [Y, _|Args],
	YI= supports_interface( YS).
add_typing( QA, Y, C0, _L0, _Expanded, _Pfx, C0, [QA/Y|GZ]-GZ):-
	callable( Y).
% add_typing( QA, Y, C0, L0, Expanded, Pfx, C1, GA-GZ):-
%	callable( Y),
% 	Y =.. [ YF | YA ],
% 	context2( i, C0, L0, Expanded, Pfx, YA, YAC-[], C1, GA-GY),
% 	S =.. [ YF | YAC ],
% 	GY= [ QA/S| GZ].
	

preconditions( C, G, Body, BodyWithPreconds):-
	filter_guards( G, G1),
	elim_dup_guards( G1, G2),
	(  guards_to_seq( C, G2, S)
	-> conc_goals( S, Body, BodyWithPreconds)
	;  BodyWithPreconds= Body
	).

postconditions( C, G, Body, BodyWithPostconds):-
	filter_guards( G, G1),
	elim_dup_guards( G1, G2),
	(  guards_to_seq( C, G2, S)
	-> conc_goals( Body, S, BodyWithPostconds)
	;  BodyWithPostconds= Body
	).

% ======================================================================

warn_uninit( [], _).
warn_uninit( [A|B], Goal):-
	extract_cvars( [A|B], L),
	warning( [ $$("Uninitialized context variables "), L, 
	           $$(" in call to "), ''( Goal)]).

extract_cvars( [], []).
extract_cvars( [X=_|L], [X|R]):-
	extract_cvars( L, R).

% ======================================================================

declare_methods( MDef, Mod):-
	(  proper_list( MDef)
	-> declare_methods1( MDef, Mod)
	;  declare_methods1( [MDef], Mod)
	).

declare_methods1( [], _).
declare_methods1( [Meth|Rest], Mod):-
	declare_method( Meth, Mod),
	declare_methods1( Rest, Mod).

method_descr( virtual:(F/A // Context), F, A, Context, true):-
	atom( F), integer( A), A>=0.
method_descr( F/A // Context, F, A, Context, false):-
	atom( F), integer( A), A>=0.

declare_method( Meth, Mod):-
	nonvar( Meth),
	method_descr( Meth, F, A, Context, Virtual),
	context( i, Context, T1, _, _),
	context( o, Context, T2, _, _),
	append( T1, T2, [LM|_]),
	signature( LM, Sig),
	(  method_declaration( F/A, Sig, Context0, Virtual)
	-> (  compatible_contexts( Context, Context0)
	   -> true
	   ;  warning( [ $$("Attempt to redefine method "), 
	                 ''( F/A // Context0), $$(" with new context "), 
		         ''( Context), $$(" in module "), Mod])
	   )
	;  assertz_fact( method_declaration( F/A, Sig, Context, Virtual))
	).

% ======================================================================

use_required_modules( Mod, L):-
	findall( M, required_module( M), LM),
	use_rm1( Mod, LM, L).

use_rm1( _, [], []).
use_rm1( Mod, [M|L], Uses):-
	(  M\=Mod
	-> Uses= [(:- use_module( MLoc))|R],
 	   (  module_location( M, MLoc)
	   -> true
	   ;  MLoc= M
	   )
	;  Uses= R
	),
	use_rm1( Mod, L, R).

% ======================================================================

check_interface( abstract, _):- !.
check_interface( Mod, Clauses):- 
	findall( Macro/SigList, 
	( '$_context_expansion'( Macro, _, Mod),
	  uniform_macro( Macro, Sig), Sig\=[],
	  siglist( Sig, SigList)
	),
	Macros),
	findall( meth( F/A, MSig, Context, Virtual),
	method_declaration( F/A, MSig, Context, Virtual),
	DeclaredMethods),
	check_itf1( Mod, Macros, DeclaredMethods, Clauses-[]),
	findall( meth( F/A, MSig, Context),
	method_implementation( F/A, MSig, Context),
	ImplementedMethods),
	check_itf2( Mod, ImplementedMethods, DeclaredMethods).

% ----------------------------------------------------------------------

check_itf1(  _, [], _, Cl-Cl):- !.
check_itf1( Mod, [Macro/SigList|Rest], Methods, ClA-ClZ):- 
	check_macro( Mod, Macro/SigList, Methods, ClA-ClB),
	check_itf1( Mod, Rest, Methods, ClB-ClZ).

check_macro( _, _, [], Cl-Cl):- !.
check_macro( Mod, Macro/SigList, [meth( F/A, Sig, Context, Virtual)|L], 
ClA-ClZ):-
	(  member( Sig, SigList)
	-> (  SigList= [Sig0|_],
	      method_implementation( F/A, Sig0, Ctx2),
	      compatible_contexts( Context, Ctx2)
	   -> ClB= ClA
	   ;  Virtual=true, SigList= [Sig0, SuperSig|_]
	   -> H= '$_context_method'( F, Sig0, Args, Tin, Tout),
	      B= '$_context_method'( F, SuperSig, Args, Tin, Tout),
	      ClA= [( H :- B)|ClB]
	   ;  warning(  [$$("Module "), Mod, $$(" does not implement method "),
	      ''(F/A//Context), $$(" for context "), ''(Macro)]),
	      ClB= ClA
	   )
	;  ClB= ClA
	),
	check_macro( Mod, Macro/SigList, L, ClB-ClZ).

:- export( compatible_contexts/2).

compatible_contexts( C1, C2):-
	compatible1( i, C1, C2),
	compatible1( o, C1, C2).

compatible1( Dir, C1, C2):-
	context( Dir, C1, T1, _, _),
	context( Dir, C2, T2, _, _),
	\+( \+ T1=T2).

check_itf2( _, [], _).
check_itf2( Mod, [M|L], DeclaredMethods):-
	check_impl_method( Mod, M, DeclaredMethods),
	check_itf2( Mod, L, DeclaredMethods).

check_impl_method( Mod, meth( F/A, Sig, Context), DeclaredMethods):-
	siglist( Sig, SigList),
	(  check_im2( F/A, SigList, Context, DeclaredMethods)
	-> true
	;  warning( [$$("Module "), Mod, $$(" implements undeclared method "),
	   ''(F/A//Context)])
	).

check_im2( F/A, SigList, Context, [meth(DF/DA, DSig, DContext, _)|L]):-
	(  F/A=DF/DA,
	   member( DSig, SigList),
	   compatible_contexts( Context, DContext)
	-> true
	;  check_im2( F/A, SigList, Context, L)
	).

% ======================================================================

define_interface( Iface, Def, Mod):-
	good_interface( Iface),
	(  interface_def( Iface, _)
	-> warning( [ $$("Attempt to redefine interface "),
	              ''(Iface), $$(" in module "), ''( Mod)]),
	   fail
	;  get_method_list( Def, MethodList),
	   assertz_fact( interface_def( Iface, MethodList))
	).

good_interface( Iface):-
	nonvar( Iface), callable( Iface),
	Iface=.. [_, Class | B],
	var( Class), all_vars( B).

all_vars( []).
all_vars( [V|R]):-
	var( V), 
	all_vars( R).

get_method_list( Left+Right, List):- !,
	nonvar( Left), callable( Left),
	(  interface_def( Left, List1)
	-> get_method_list( Right, List2),
	   append( List1, List2, List)
	;  warning( [$$("Invalid interface "), ''(Left)]),
	   fail
	).
get_method_list( [], []):- !.
get_method_list( M, [M]):- 
	nonvar( M),
	method_descr( M, _, _, _, _), !.
get_method_list( [M|L], [M|R]):-
	nonvar( M),
	method_descr( M, _, _, _, _),
	get_method_list( L, R).

% ======================================================================

apply_interface( Interface, Clauses, Mod):-
	(  nonvar( Interface), callable( Interface), 
	   Interface=.. [_, Macro | _], nonvar( Macro), 
	   callable( Macro)
	-> true
	;  warning( [$$("Invalid interface "), ''(Interface)]),
	   fail
	),
	(  '$_context_expansion'( Macro, _, MMod),
	   uniform_macro( Macro, Sig), Sig\=[]
	-> true
	;  warning( [$$("Macro "), ''(Macro), $$(" is not a proper class")]),
	   fail
	),
	(  interface_def( Interface, PredList)
	-> true
	;  warning( [$$("Undefined interface "), ''(Interface)]),
	   fail
	),
	declare_methods( PredList, Mod),
	context( i, Macro, [X], C, G),
	filter_guards( G, G1),
	elim_dup_guards( G1, G2),
	(  guards_to_seq( C, G2, Body)
	-> true
	;  Body= true
	),	
	(  ( MMod=Mod ; MMod=abstract, default_module( Mod))
	-> Clauses= [ (supports_interface( X, Interface):-Body) ]
	;  Clauses= []
	).

% ======================================================================

parse_macro_def( Mod, Def, MacroMod, Macro, Expansion):-
	nonvar( Def),
	mdef_module( Mod:Def,  MacroMod:Def1),
	nonvar( Def1),
	def_lr( Def1, Macro, Expansion).

mdef_module( Mod:(L=R), Mod1:(L1=R)):- !,
	mdef_module( Mod:L, Mod1:L1).
mdef_module( _:(Mod1:Def1), Mod1:Def1):- !,
	atom( Mod1).
mdef_module( _:Def, Mod:Def):- 
	default_module( Mod), !.
mdef_module( Mod:Def, Mod:Def).


def_lr( Left=Right, Left, Right):-
	good_macro( Left).
def_lr( '::'(Derived, Base), Macro, Expansion):-
	good_macro( Derived), nonvar( Base), 
	def_lr1( Derived, Base, (*), Macro, Expansion).
def_lr( '::'(Derived, Base) + Extension, Macro, Expansion):-
	good_macro( Derived), nonvar( Base), 
	def_lr1( Derived, Base, Extension, Macro, Expansion).

def_lr1( Derived, *, Extension, Derived, {FullExtension}):- !,
	Derived=.. [DF | DA],
	append( DA, [Extension], DAX),
	FullExtension=.. [ DF, \Derived | DAX].

def_lr1( Derived, Base, Extension, Derived, (\Base={FullExtension}, Base)):-
	good_macro( Base),
	Derived=.. [DF | DA],
	Base=.. [_ | BA],
	elim_ident( DA, BA, DANI),
	append( DANI, [Extension], DAX),
	FullExtension=.. [DF, \Derived | DAX].


	

