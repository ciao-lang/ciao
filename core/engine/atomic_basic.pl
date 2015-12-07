:- module(atomic_basic, [ name/2, atom_codes/2, number_codes/2, number_codes/3,
	atom_number/2, atom_number/3, atom_length/2, atom_concat/3,
	sub_atom/4, valid_base/1], [assertions, nortchecks, isomodes,
	nativeprops, unittestdecls]).

%:- use_module(engine(internals), ['$prolog_radix'/2]).

:- doc(title, "Basic predicates handling names of constants").

:- doc(author, "The CLIP Group").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module, "The Ciao system provides builtin predicates which
   allow dealing with names of constants (atoms or numbers).  Note that
   sometimes strings (character code lists) are more suitable to handle
   sequences of characters.").

:- doc(name(Const,String), "@var{String} is the list of the ASCII
   codes of the characters comprising the name of @var{Const}.  Note
   that if @var{Const} is an atom whose name can be interpreted as a
   number (e.g. '96'), the predicate is not reversible, as that atom
   will not be constructed when @var{Const} is uninstantiated.  Thus it
   is recommended that new programs use the ISO-compliant predicates
   @pred{atom_codes/2} or @pred{number_codes/2}, as these predicates do
   not have this inconsistency.").


:- trust pred name(+constant,?string) + eval.
:- trust pred name(-constant,+string) + eval
   # "If @var{String} can be interpreted as a number, @var{Const} is unified
      with that number, otherwise with the atom whose name is @var{String}.".
:- true comp name/2 + ( sideff(free), native ).
:- impl_defined(name/2).


:- doc(atom_codes(Atom,String), "@var{String} is the list of the ASCII
   codes of the characters comprising the name of @var{Atom}.").

:- test atom_codes(A,B): (A='año') => (B = "año") + (not_fails, is_det).

:- trust pred atom_codes(+atm,?string) + eval.
:- trust pred atom_codes(?atm,+string) + eval.
:- true comp atom_codes/2 + ( sideff(free), native, iso, is_det ).
:- impl_defined(atom_codes/2).

:- prop valid_base/1 + regtype # "Valid numeric base to convert
   numbers to strings or atoms".

valid_base(2).  valid_base(3).  valid_base(4).  valid_base(5).  valid_base(6).
valid_base(7).  valid_base(8).  valid_base(9).  valid_base(10). valid_base(11).
valid_base(12). valid_base(13). valid_base(14). valid_base(15). valid_base(16).
valid_base(17). valid_base(18). valid_base(19). valid_base(20). valid_base(21).
valid_base(22). valid_base(23). valid_base(24). valid_base(25). valid_base(26).
valid_base(27). valid_base(28). valid_base(29). valid_base(30). valid_base(31).
valid_base(32). valid_base(33). valid_base(34). valid_base(35). valid_base(36).

:- doc(number_codes(Number,String), "@var{String} is the list of the
   ASCII codes of the characters comprising a representation of
   @var{Number}.").


:- trust pred number_codes(+num,?string) + eval.
:- trust pred number_codes(-num,+string) + eval.
:- true comp number_codes/2 + ( sideff(free), native, iso ).

:- true success number_codes(A,B) : int(A) => list(B,num_code).

:- impl_defined(number_codes/2).

:- doc(number_codes(Number,Base,String), "@var{String} is the list
   of the ASCII codes of the characters comprising a representation of
   @var{Number} in base @var{Base}.").


:- test number_codes(A, B, C) : ( A = 0.0, valid_base(B) ) => C = "0.0".
:- test number_codes(A, B, C) : ( valid_base(B), C="0.0" ) => A = 0.0.
:- test number_codes(A, B, C) : ( A = 1.0, valid_base(B) ) => C = "1.0".
:- test number_codes(A, B, C) : ( valid_base(B), C="1.0" ) => A = 1.0.
:- test number_codes(A, B, C) : ( A = 0.Inf, valid_base(B) ) => C = "0.Inf".
:- test number_codes(A, B, C) : ( valid_base(B), C="0.Inf" ) => A = 0.Inf.
:- test number_codes(A, B, C) : ( A = -1.0, valid_base(B) ) => C = "-1.0".
:- test number_codes(A, B, C) : ( valid_base(B), C="-1.0" ) => A = -1.0.
:- test number_codes(A, B, C) : ( A = -1.0, valid_base(B) ) => C = "-1.0".
:- test number_codes(A, B, C) : ( valid_base(B), C = "-1.0" ) => A = -1.0.
:- test number_codes(A, B, C) : ( A = -0.Inf, valid_base(B) ) => C = "-0.Inf".
:- test number_codes(A, B, C) : ( valid_base(B), C = "-0.Inf" ) => A = -0.Inf.
:- test number_codes(A, B, C) : ( A = 0.Nan, valid_base(B) ) => C = "0.Nan".
:- test number_codes(A, B, C) : ( valid_base(B), C = "0.Nan" ) => A = 0.Nan.

% TODO: number_codes/3 should not be a prop. Fix rtchecks.
:- prop number_codes/3 # "We defined number_codes/3 as a property to
        use it in the tests.".

:- impl_defined(number_codes/3).

:- test number_codes(A, B, C) :
	(
	    float_random(A),
	    valid_base(B)
	) =>
	call((
	    number_codes(A1, B, C),
	    near(A1, A, 0.0000000001)
	)) + times(50) # "Reversibility test 1".

:- test number_codes(A, B, C) :
	(
	    float_random(A0),
	    valid_base(B),
	    number_codes(A0, B, C)
	) =>
	(
	    near(A, A0, 0.0000000001)
	) + times(50) # "Reversibility test 2".

:- test number_codes(A, B, C) : ( A = 19.26, B = 10 ) => C = "19.26".
:- test number_codes(A, B, C) : ( B = 10, C = "19.26" ) => A = 19.26.


:- trust pred number_codes(+num,+int,?string) + eval.
:- trust pred number_codes(-num,+int,+string) + eval.
:- true comp number_codes/3 + ( sideff(free), native ).


:- doc(atom_number(Atom,Number), "@var{Atom} can be read as a
   representation of @var{Number}.").

:- test atom_number(A, B) : ( B = 0.0 ) => ( A = '0.0' ).
:- test atom_number(A, B) : ( A = '0.0' ) => ( B = 0.0 ).
:- test atom_number(A, B) : ( B = -0.0 ) => ( A = '-0.0' ).
:- test atom_number(A, B) : ( A = '-0.0' ) => ( B0 = -0.0, B = B0 ).
:- test atom_number(A, B) : ( B = 1.0 ) => ( A = '1.0' ).
:- test atom_number(A, B) : ( A = '1.0' ) => ( B = 1.0 ).
:- test atom_number(A, B) : ( B = 0.Inf ) => ( A = '0.Inf' ).
:- test atom_number(A, B) : ( A = '0.Inf' ) => ( B = 0.Inf ).
:- test atom_number(A, B) : ( B =  -1.0 ) => ( A = '-1.0' ).
:- test atom_number(A, B) : ( A = '-1.0' ) => ( B = -1.0 ).
:- test atom_number(A, B) : ( B =  -1.0 ) => ( A = '-1.0' ).
:- test atom_number(A, B) : ( A = '-1.0' ) => ( B = -1.0 ).
:- test atom_number(A, B) : ( B =  -0.Inf ) => ( A = '-0.Inf' ).
:- test atom_number(A, B) : ( A = '-0.Inf' ) => ( B = -0.Inf ).
:- test atom_number(A, B) : ( B =  -0.Inf ) => ( A = '-0.Inf' ).
:- test atom_number(A, B) : ( B = 0.Nan ) => ( A = '0.Nan' ).
:- test atom_number(A, B) : ( A = '0.Nan' ) => ( B = 0.Nan ).

% Comment from summer 2005:
% Cannot prove the success part (exceptions), this is not ISO
:- pred atom_number(+atm,?num) => atm * num  + eval.
:- pred atom_number(-atm,+num) => atm * num + eval.

:- true comp atom_number/2 + ( sideff(free), native, is_det ).

:- doc(atom_number(Atom,Base, Number), "@var{Atom} can be read as a
   representation of @var{Number} in base @var{Base}.").

:- true comp atom_number/3 + ( sideff(free), native, is_det ).

:- pred atom_number(+atm,?num,+num).
:- pred atom_number(-atm,+num,+num).

atom_number(A, N) :-
	atom_number(A, 10, N).

atom_number(A, B, N) :-
        atom(A), integer(B), number(N), !,
        atom_codes(A, S),
        number_codes(N0, B, S),
        N = N0.               % So that atom_number('2.3e1',23.0) succeeds
atom_number(A, B, N) :-
        atom(A), integer(B), var(N), !,
        atom_codes(A, S),
        number_codes(N, B, S).
atom_number(A, B, N) :-
        var(A), integer(B), number(N), !,
        number_codes(N, B, S),
        atom_codes(A, S).
atom_number(A, B, N) :-
        ( var(A) ->
	  ( var(B) ->
	    throw(error(instantiation_error, atom_number/3-2))
	  ; \+ int(B) -> throw(error(type_error(integer, B), atom_number/3-2))
	  ; ( var(N) ->
	      throw(error(instantiation_error, atom_number/3-1))
	    ; throw(error(type_error(number, N), atom_number/3-3))
	    )
	  )
        ; atom(A) ->
	  ( var(B) -> throw(error(instantiation_error, atom_number/3-2))
	  ; \+ int(B) -> throw(error(type_error(integer, B), atom_number/3-2))
	  ; throw(error(type_error(number, N), atom_number/3-3))
	  )
        ; ( var(B) -> throw(error(instantiation_error, atom_number/3-2))
	  ; \+ nnegint(B) -> throw(error(type_error(integer, B), atom_number/3-2))
	  ; throw(error(type_error(atom, A), atom_number/3-1))
	  )
        ).

:- doc(atom_length(Atom,Length), "@var{Length} is the number of
   characters forming the name of @var{Atom}.").

:- trust pred atom_length(+atm,?int) + eval.
:- true comp atom_length/2 + ( sideff(free), native, iso, is_det ).
:- impl_defined(atom_length/2).

:- doc(atom_concat(Atom_1,Atom_2,Atom_12), "@var{Atom_12} is the
   result of concatenating @var{Atom_1} followed by @var{Atom_2}.").

:- trust pred atom_concat(+atm,+atm,?atm) => atm * atm * atm  + eval
   # "Concatenate two atoms.".
:- trust pred atom_concat(-atm,-atm,+atm) => atm * atm * atm + eval
   # "Non-deterministically split an atom.".
:- trust pred atom_concat(-atm,+atm,+atm) => atm * atm * atm + eval
   # "Take out of an atom a certain suffix (or fail if it cannot be done).".
:- trust pred atom_concat(+atm,-atm,+atm) => atm * atm * atm + eval
   # "Take out of an atom a certain prefix (or fail if it cannot be done).".
:- true comp atom_concat/3 + ( sideff(free), native, iso, is_det ).

:- impl_defined(atom_concat/3).

:- doc(sub_atom(Atom,Before,Length,Sub_atom), "@var{Sub_atom} is
   formed with @var{Length} consecutive characters of @var{Atom}
   after the @var{Before} character.  For example, the goal
   @tt{sub_atom(summer,1,4,umme)} succeeds.").

:- trust pred sub_atom(+atm,+int,+int,?atm) + eval.
:- true comp sub_atom/4 + ( sideff(free), native ).

:- impl_defined(sub_atom/4).
