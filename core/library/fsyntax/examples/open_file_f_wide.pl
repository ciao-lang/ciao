:- module(_,[create_streams/2],[fsyntax,assertions,regtypes]).

:- entry create_streams(A,B) : list(A,num).

create_streams([])     := [].
create_streams([N|NL]) := [ ~open_file(Fname,write) | ~create_streams(NL) ] 
	:-
 	app("/tmp/../",~number_codes(N),Fname).
%	app("/tmp/",~number_codes(N),Fname).

app([],L) := L.
app([X|Xs],L) := [X|~app(Xs,L)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% open_file library:

open_file(Fname,Mode) := ~open(File,Mode) :- atom_codes(File,Fname).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Safety policy:

:- check calls open_file(Fname,_,_) : safe_name(Fname).

:- regtype  safe_name/1.    safe_name("/tmp/" || L) :- list(L,alphnum_code).
  
:- regtype  alphnum_code/1. alphnum_code := ~alph_code | ~num_code.

:- regtype  alph_code/1.    alph_code := 0'a | 0'b | 0'c | 0'd | 0'e | 0'f .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
