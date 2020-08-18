:- module(atomic_basic, [], [noprelude, assertions, nortchecks, isomodes, nativeprops]).

:- doc(title, "Conversion between constants and strings").
:- doc(author, "The Ciao Development Team").

:- doc(usage, "@include{InPrelude.lpdoc}").

:- doc(module, "The Ciao system provides builtin predicates which
   allow conversions between constants (atomic terms, i.e., atoms or
   numbers) and their string representation (character code
   lists). Note that sometimes strings are more suitable to handle
   sequences of characters.").

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_typing)).
:- use_module(engine(term_basic)).
:- use_module(engine(exceptions)).

:- if(defined(optim_comp)).
:- '$native_include_c_source'(.(atomic_basic)).
:- endif.

% ---------------------------------------------------------------------------
:- export(name/2).
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
:- trust comp name/2 + ( sideff(free), native ).
:- if(defined(optim_comp)).
:- '$props'(name/2, [impnat=cbool(prolog_name)]).
:- else.
:- impl_defined(name/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(atom_codes/2).
:- doc(atom_codes(Atom,String), "@var{String} is the list of the ASCII
   codes of the characters comprising the name of @var{Atom}.").

:- trust pred atom_codes(+atm,?string) + eval.
:- trust pred atom_codes(?atm,+string) + eval.
:- trust comp atom_codes/2 + ( sideff(free), native, iso, is_det ).
:- if(defined(optim_comp)).
:- '$props'(atom_codes/2, [impnat=cbool(prolog_atom_codes)]).
:- else.
:- impl_defined(atom_codes/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(number_codes/2).
:- doc(number_codes(Number,String), "@var{String} is the list of the
   ASCII codes of the characters comprising a representation of
   @var{Number}.").

:- trust pred number_codes(+num,?string) + eval.
:- trust pred number_codes(-num,+string) + eval.
:- trust comp number_codes/2 + ( sideff(free), native, iso ).

:- trust success number_codes(A,B) : int(A) => string(B).

:- if(defined(optim_comp)).
:- '$props'(number_codes/2, [impnat=cbool(prolog_number_codes_2)]).
:- else.
:- impl_defined(number_codes/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(number_codes/3).
:- doc(number_codes(Number,Base,String), "@var{String} is the list
   of the ASCII codes of the characters comprising a representation of
   @var{Number} in base @var{Base}.").

:- trust pred number_codes(+num,+int,?string) + eval.
:- trust pred number_codes(-num,+int,+string) + eval.
:- trust comp number_codes/3 + ( sideff(free), native ).

:- if(defined(optim_comp)).
:- '$props'(number_codes/3, [impnat=cbool(prolog_number_codes_3)]).
:- else.
:- impl_defined(number_codes/3).
:- endif.

% ---------------------------------------------------------------------------
:- export(atom_number/2).
:- doc(atom_number(Atom,Number), "@var{Atom} can be read as a
   representation of @var{Number}.").

% Comment from summer 2005:
% Cannot prove the success part (exceptions), this is not ISO
:- pred atom_number(+atm,?num) => atm * num + eval.
:- pred atom_number(-atm,+num) => atm * num + eval.

:- trust comp atom_number/2 + ( sideff(free), native, is_det ).

atom_number(A, N) :-
    atom_number(A, 10, N).

% ---------------------------------------------------------------------------
:- export(atom_number/3).
:- doc(atom_number(Atom,Base, Number), "@var{Atom} can be read as a
   representation of @var{Number} in base @var{Base}.").

:- trust comp atom_number/3 + ( sideff(free), native, is_det ).

:- pred atom_number(+atm,?num,+num).
:- pred atom_number(-atm,+num,+num).

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
      ; \+ integer(B) -> throw(error(type_error(integer, B), atom_number/3-2))
      ; ( var(N) ->
          throw(error(instantiation_error, atom_number/3-1))
        ; throw(error(type_error(number, N), atom_number/3-3))
        )
      )
    ; atom(A) ->
      ( var(B) -> throw(error(instantiation_error, atom_number/3-2))
      ; \+ integer(B) -> throw(error(type_error(integer, B), atom_number/3-2))
      ; throw(error(type_error(number, N), atom_number/3-3))
      )
    ; ( var(B) -> throw(error(instantiation_error, atom_number/3-2))
      ; \+ integer(B) -> throw(error(type_error(integer, B), atom_number/3-2))
      ; throw(error(type_error(atom, A), atom_number/3-1))
      )
    ).

% ---------------------------------------------------------------------------
:- export(atom_length/2).
:- doc(atom_length(Atom,Length), "@var{Length} is the number of
   characters forming the name of @var{Atom}.").

:- trust pred atom_length(+atm,?int) + eval.
:- trust comp atom_length/2 + ( sideff(free), native, iso, is_det ).
:- if(defined(optim_comp)).
:- '$props'(atom_length/2, [impnat=cbool(prolog_atom_length)]).
:- else.
:- impl_defined(atom_length/2).
:- endif.

% ---------------------------------------------------------------------------
:- export(atom_concat/3).
:- doc(atom_concat(Atom_1,Atom_2,Atom_12), "@var{Atom_12} is the
   result of concatenating @var{Atom_1} followed by @var{Atom_2}.").

:- trust pred atom_concat(+atm,+atm,?atm) => atm * atm * atm + eval
   # "Concatenate two atoms.".
:- trust pred atom_concat(-atm,-atm,+atm) => atm * atm * atm + eval
   # "Non-deterministically split an atom.".
:- trust pred atom_concat(-atm,+atm,+atm) => atm * atm * atm + eval
   # "Take out of an atom a certain suffix (or fail if it cannot be done).".
:- trust pred atom_concat(+atm,-atm,+atm) => atm * atm * atm + eval
   # "Take out of an atom a certain prefix (or fail if it cannot be done).".
:- trust comp atom_concat/3 + ( sideff(free), native, iso, is_det ).

:- if(defined(optim_comp)).
:- '$props'(atom_concat/3, [impnat=cbool(prolog_atom_concat)]).
:- else.
:- impl_defined(atom_concat/3).
:- endif.

% ---------------------------------------------------------------------------
:- export(sub_atom/4).
:- doc(sub_atom(Atom,Before,Length,Sub_atom), "@var{Sub_atom} is
   formed with @var{Length} consecutive characters of @var{Atom}
   after the @var{Before} character.  For example, the goal
   @tt{sub_atom(summer,1,4,umme)} succeeds.").

:- trust pred sub_atom(+atm,+int,+int,?atm) + eval.
:- trust comp sub_atom/4 + ( sideff(free), native ).

:- if(defined(optim_comp)).
:- '$props'(sub_atom/4, [impnat=cbool(prolog_sub_atom)]).
:- else.
:- impl_defined(sub_atom/4).
:- endif.
