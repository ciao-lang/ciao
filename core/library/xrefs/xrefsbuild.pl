
:- module(xrefsbuild,
	[ xref/1,
	  xrefs/2,
	  set_flag/1
	],
	[ assertions, regtypes
	]).

:- use_module(library(sort), [sort/2]).  
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(xrefs/xrefsread), [meta_call/3, xrefs_files/1]).

%-----------------------------------------------------------------------------
% entry points

:- pred xrefs(Xrefs,Graph) : xref * var
	# "Obtains a graph of xrefs of type @var{Xrefs} based on the
           crossed-checking of the source code of the current files read.".

xrefs(Flag,HGraph):-
	xrefs_files(CList),
	do(defined,CList,Defs),
	do(used,CList,Uses),
	cross(Flag,Defs,Uses,HGraph).

:- regtype xref(Xref)
	# "@var{Xref} is either @tt{whodefs} or @tt{whouses}.".

xref(whodefs).
xref(whouses).

:- doc(set_flag(Flag),"@var{Flag} encodes in binary the references
	wanted from a file to:
        1st bit- other files, 2nd bit- same file, 3rd bit- no file.
	Default flag is: only other files.").
:- pred set_flag(Flag) : number
	# "Sets the current flag to @var{Flag}.".
:- pred set_flag(Flag) : var
	# "Binds @var{Flag} to the current flag.".

set_flag(Flag):-
	var(Flag), !,
	flag(Flag).
set_flag(Flag):-
	number(Flag),
	retractall_fact(flag(_)),
	asserta_fact(flag(Flag)).

:- data flag/1.

flag(1).

%-----------------------------------------------------------------------------

do(Xref,CList,List):-
	functor(Call,Xref,4),
	arg(1,Call,In),
	arg(2,Call,F),
	arg(3,Call,Out),
	arg(4,Call,Tail),
	collect(CList,c(Call,In,F,Out,Tail),List).

collect([],_Spec,[]).
collect([(F,Cls)|Fs],Spec,[(F,L)|Ls]):-
	see_file(Cls,F,Spec,L1),
	sort(L1,L),
	collect(Fs,Spec,Ls).

see_file([],_F,_Spec,[]).
see_file([Cl|Cls],F,Spec,L):-
	copy_term(Spec,c(Call,Cl,F,L,L0)),
	call(Call),
	see_file(Cls,F,Spec,L0).

defined((H:-_),_F,List,T):-
	( number(H)
	-> List = T
	 ; List = [Name|T],
	   getname(H,Name)
	).

used((H:-B),F,Subgoals,Tail):- !,
	( number(H)
	-> Subgoals = Tail
	 ; getname(H,Pred),
	   body_calls(B,Pred,F,Subgoals,Tail)
	).

body_calls(X,_,_F,Out,Tail):-
	var(X), !,
	Out = [metacall|Tail].
body_calls(_:X,F,P,Xones,Tail):- !,
	body_calls(X,F,P,Xones,Tail).
body_calls(!,_,_F,Out,Tail):- !,
	Out = Tail.
body_calls((X,Y),F,P,Xones,Tail):- !,
	body_calls(X,F,P,Xones,Yones),
	body_calls(Y,F,P,Yones,Tail).
body_calls((X;Y),F,P,Xones,Tail):- !,
	body_calls(X,F,P,Xones,Yones),
	body_calls(Y,F,P,Yones,Tail).
body_calls((X->Y),F,P,Xones,Tail):- !,
	body_calls(X,F,P,Xones,Yones),
	body_calls(Y,F,P,Yones,Tail).
body_calls(X,F,P,Xones,Tail):-
	meta_call(X,F,Y), !,
	( Y == 0
	-> Yones=[metacall|Tail]
	 ; body_calls(Y,F,P,Yones,Tail)
	),
	getname(X,Name),
	get_rid_of(P,Name,Xones,Yones).
body_calls(X,_F,P,Xname,Tail):-
	getname(X,Name),
	get_rid_of(P,Name,Xname,Tail).

get_rid_of(P,P,Out,Tail):- !,         % Do not consider recursive calls
	Out = Tail.
get_rid_of(_,X,[X|Tail],Tail).

getname(H,Name):-
	functor(H,F,A),
        name(A,N),
        atom_codes(At,N),
	atom_concat([F,'/',At],Name).

%-----------------------------------------------------------------------------

cross(whouses,Defs,Uses,HGraph):-
	cross_(Defs,Uses,HGraph).
cross(whodefs,Defs,Uses,HGraph):-
	cross_(Uses,Defs,HGraph).

cross_([],_,[]).
cross_([(F,Ls)|FLs],ToCross,[(F,Crossed)|More]):-
	cross_each(Ls,F,ToCross,Crossed),
	cross_(FLs,ToCross,More).

cross_each([],_,_,[]).
cross_each([L|Ls],F,ToCross,LabelPairs):-
	cross_targets(ToCross,L,Targets1),
	cross_autoreference(Targets1,F,L,LabelPairs,Tail),
	cross_each(Ls,F,ToCross,Tail).

cross_targets([],_,[]).
cross_targets([(F,Ls)|More],L,Targets):-
	member(L,Ls), !,
	Targets = [F|Fs],
	cross_targets(More,L,Fs).
cross_targets([_|More],L,Fs):-
	cross_targets(More,L,Fs).

cross_autoreference(Targets,F,L,LabelPairs,Tail):-
	flag(Flag),
	Flag1 is Flag/\3, Flag2 is Flag/\4,
	cross_autoreference_(Flag2,Flag1,Targets,F,L,LabelPairs,Tail).

cross_autoreference_(4,Flag1,Targets,F,L,LabelPairs,Tail):- !,
	empty_in(Targets,Flag1,F,L,LabelPairs,Tail).
cross_autoreference_(_Flag2,Flag1,Targets,F,L,LabelPairs,Tail):-
	simplify_if(Flag1,Targets,F,L,LabelPairs,Tail).

empty_in([],_Flag1,_F,L,LabelPairs,Tail):- !,
	LabelPairs = [(L,[])|Tail].
empty_in(Targets,Flag1,F,L,LabelPairs,Tail):-
	simplify_if(Flag1,Targets,F,L,LabelPairs,Tail).

simplify_if(3,Targets,_F,L,LabelPairs,Tail):-
	empty_out(Targets,L,LabelPairs,Tail).
simplify_if(1,Targets1,F,L,LabelPairs,Tail):-
	select(Targets1,F,_,Targets),
	empty_out(Targets,L,LabelPairs,Tail).
simplify_if(2,Targets1,F,L,LabelPairs,Tail):-
	select(Targets1,F,Targets,_),
	empty_out(Targets,L,LabelPairs,Tail).
simplify_if(0,_Targets1,_F,L,LabelPairs,Tail):-
	empty_out([],L,LabelPairs,Tail).

empty_out([],_L,LabelPairs,Tail):- !,
	LabelPairs = Tail.
empty_out(Targets,L,[(L,Targets)|Tail],Tail).

select([],_,[],[]).
select([Element|Rest],Element,[Element],Rest):- !.
select([Head|Tail],Element,More,[Head|Rest]) :-
	select(Tail,Element,More,Rest).
