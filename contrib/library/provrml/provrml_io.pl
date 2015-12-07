:- module(provrml_io,[
	     out/1,
	     out/3,
	     convert_atoms_to_string/2,
	     read_terms_file/2,
	     write_terms_file/2,
	     read_vrml_file/2,
	     write_vrml_file/2
	     ],[assertions,
	        isomodes,
		dcg,
		iso,
		regtypes]).

:- doc(author, "G@..{o}ran Smedb@..{a}ck").

:- use_module(library(lists), [append/3]).

:- set_prolog_flag(write_strings,on).
:- set_prolog_flag(multi_arity_warnings, off).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- doc(module,"This file implements I/O predicates of different types.

   Implemented by G@..{o}ran Smedb@..{a}ck

").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred out(+ListOfOutput)
   :: list(atm)
   # "The predicate used is out/3 (DCG) where we will 'save' the 
      output in the second argument. The tird argument is the rest, nil.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
out(Out) :-
	out(Out,_,[]).

out([]) -->
	[].

out([Out|Rest]) -->
	{
	atomic(Out),
	name(Out,[35|_More])
	},
	out0(Out),
	out0('\n'),
	out(Rest).

out([List|Rest]) -->
	{
	list(List)
	},
	out(List),
	out(Rest).

out([Out|Rest]) -->
	out0(Out),
	out(Rest).

out0(Atom) -->
	[Atom].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred convert_atoms_to_string(+Atoms,-String) 
        :: list(atm) * list(num)
        # "The predicate transforms a list of atoms to a string.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_atoms_to_string(Atoms,String) :-
	convert_atoms_to_code(Atoms,String).

convert_atoms_to_code([],[]).
convert_atoms_to_code([A|Rest],Answer) :-
	( number(A)
	->number_codes(A,String)
	; atom_codes(A,String)
	),
	convert_atoms_to_code(Rest,Rest_ans),
	append(String,Rest_ans,Answer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred read_terms_file(+Filename, -Term) 
   :: atm * atm
   # "Given a filename to a file with terms, the predicate reads the terms
  and are returned in the second argument. @var{Filename} is an 
  atom  and @var{Term} is the read prolog terms. ".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_terms_file(FileName, Data) :-
    open(FileName, read, Stream),
    read_data_term(Stream, Data),
    close(Stream).

read_data_term(Stream, Data) :-
    read(Stream, T),
    read_data_term3(Stream, T, [Data]).

read_data_term3(_, end_of_file, []).

read_data_term3(Stream, T, [T|Data]) :-
    read(Stream, T1),
    read_data_term3(Stream, T1, Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred read_vrml_file(+FileName, -Data)
   :: atm * string
   #
   "Given a filename, the predicate returns the substance.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_vrml_file(FileName, Data) :-
    open(FileName, read, Stream),
    read_data(Stream, Data),
    close(Stream),
    !.

read_data(Stream, Data) :-
	get0(Stream, C),
	read_data3(Stream, C, Data).

read_data3(_, -1, []).

read_data3(Stream, C, [C|Acc]) :-
	get0(Stream, C1),
	read_data3(Stream, C1, Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred write_vrml_file(+FileName, +Data)
   :: atm * string
   # 
   "Given a filename and data in form of a string, the predicate will write the
    data  to the named file.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_vrml_file(FileName, Data) :-
	open(FileName, write, Stream),
	write_data(Stream, Data),
	close(Stream).

write_data(_,[]).

write_data(Stream, [C|Rest]) :-
	put(Stream, C),
	write_data(Stream, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred write_terms_file(+FileName, +List)
 ::  atm * list(atm)
 #
 "Given a filename and a list of terms  the predicate will write 
  them down to the file.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_terms_file(FileName, Data) :-
	open(FileName, write, Stream),
	write(Stream, '[' ),
	write_data_term(Stream, Data),
	close(Stream).

write_data_term(Stream,[]) :-
	write(Stream, '].\n' ).
	

write_data_term(Stream, [C|Rest]) :-
	writeq(Stream, C),
	write_data_term_mid(Stream, Rest).

write_data_term_mid(Stream,[]) :-
	write(Stream, '].\n' ).
	

write_data_term_mid(Stream, Rest) :-
	put(Stream, 0',),
	write_data_term(Stream, Rest).

put(Stream, Char) :-
	put_code(Stream, Char).

get0(Stream, Char) :-
	get_code(Stream, Char).
