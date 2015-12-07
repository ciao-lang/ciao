:- module(arrays_rt,_,[functional,hiord,assertions,regtypes,isomodes]).

:- include(arrays_ops).

:- doc(title,"Some simple array operations with syntactic support").
:- doc(author,"Pro Grammer").

:- doc(module,"This library implements a very simple set of
   operations on arrays. The idea is to illustrate the use of
   functional syntax (operators) by providing syntactic support for
   invoking array operations such as element access, array (vector)
   addition, etc.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Regtypes

%% :- doc(doinclude,array/1).
%% :- doc(doinclude,vector/1).
%% :- doc(doinclude,dim/1).

:- regtype array(A) #"@var{A} is a multi-dimensional array.".
% Should obviously be defined in more detail...
array(A) :- struct(A).

:- regtype dim(D) # "@var{D} represents the dimensions of an array.".
dim(D) :- list(D,int).

:- regtype vector(V) # "@var{V} is a one-dimensional fixed-size array.".
vector(V) :- fixed_array([N],V), int(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred fixed_array(Dim,Array) :: dim * array
# "@var{Array} is an array of fixed dimensions @var{Dim}.".

fixed_array([N|Ms],A):-
	functor(A,a,N),
	rows(N,Ms,A).
fixed_array([N],A):-
	functor(A,a,N).

rows(0,_Ms,_A).
rows(N,Ms,A):-
	N > 0,
	arg(N,A,Arg),
	fixed_array(Ms,Arg),
	rows(N-1,Ms,A).

:- pred @(Array,Index,Elem):: array * dim * int
# "@var{Elem} is the @var{Index}-th element of @var{Array}.".

V@[I]    := ~arg(I,V).
V@[I|Js] := ~arg(I,V)@Js.

:- pred <+>(V1,V2,V3) :: vector * vector * vector
# "@var{V3} is @var{V1} + @var{V2}.".

V1 <+> V2 := V3 :-
	V1 = ~fixed_array([N]),
	V2 = ~fixed_array([N]),
	V3 = ~fixed_array([N]),
	V3 = ~vecplus_(N,V1,V2).

vecplus_(0,_,_,_).
vecplus_(N,V1,V2,V3) :- 
	N > 0,
	V3@[N] = V1@[N] + V2@[N],
	vecplus_(N-1,V1,V2,V3).

:- pred <*>(V1,V2,V3) :: vector * vector * vector
# "@var{V3} is @var{V1} * @var{V2} (inner product).".

V1 <*> V2 := ~vecmul_(N,V1,V2,0) :- 
    V1 = ~fixed_array([N]),
    V2 = ~fixed_array([N]).
    
vecmul_(0,  _,  _, Acc, Acc).
vecmul_(N, V1, V2, Acc, IP) :-
    N > 0,
    vecmul_( N-1, V1, V2, Acc + ( V1@[N] * V2@[N] ), IP).
