:- module(term_ser, []).

% ==================================================================
% == Term serialization ============================================
% ==================================================================

% ------------------------------------------------------------------
:- export(serialize_term / 2).

serialize_term( Term, Bytes) :-
        serialize_term( Term, _, Bytes, []).

% ..................................................................
:- export( serialize_term / 3).

serialize_term( Term, Bytes, Rest) :-
        serialize_term( Term, _, Bytes, Rest).

% ..................................................................
:- export( serialize_term / 4).

serialize_term( Term, Dict, Bytes, Rest) :-
        tdb_( Term, Dict, Bytes, Rest).

% ------------------------------------------------------------------

tdb_( V, D) -->
        { var( V) },
        !,
        { var_index( D, V, Index) },
        [ 0'_ ],
        { number_codes( Index, S) },
        put_string( S).

tdb_( 0.Inf, _) -->
        !,
        [ 0'+ ].

tdb_( -0.Inf, _) -->
        !,
        [ 0'- ].

tdb_( 0.Nan, _) -->
        !,
        [ 0'n ].

tdb_( N, _) -->
        { number( N) },
        !,
        [ 0'# ],
        { number_codes( N, S) },
        put_string( S).

tdb_( S, _) -->
        { is_string( S) },
        !,
        [ 34 ],
        put_string( S).

tdb_( [H|T], D) -->
        !,
        [ 0'[ ],
        tdb_( H, D),
        tdb_list( T, D).

tdb_( A, _) -->
        { atom( A) },
        !,
        [ 0'' ],
        { atom_codes( A, S) },
        put_string( S).

tdb_( Struct, D) -->
        [ 0'( ],
        { Struct =.. [ F | Args ],
          atom_codes( F, FS) 
        },
        put_string( FS),
        tdb_args( Args, D).

% ..................................................................

tdb_args( [], _) -->
        [ 0') ].
tdb_args( [ Arg | Args ], D) -->
        tdb_( Arg, D),
        tdb_args( Args, D).

% ..................................................................

tdb_list( L, D) -->
        (  { nonvar( L), L = [] }
        -> [ 0'] ]
        ;  { nonvar( L), L = [H|T] }
        -> [ 0', ],
           tdb_( H, D),
           tdb_list( T, D)
        ;  [ 0'| ],
           tdb_( L, D)
        ).

% ..................................................................

put_string( S) -->
        put_chars( S).
% ..................................................................

put_chars( []) -->
        !,
        [ 0'| ].
put_chars( [ 0'% | Chars ]) -->
        !,
        [ 0'%, 0'2, 0'5 ],
        put_chars( Chars).
put_chars( [ 0'| | Chars ]) -->
        !,
        [ 0'%, 0'7, 0'c ],
        put_chars( Chars).
put_chars( [ C | Chars ]) -->
        (  { C >= 32, C =< 127 }
        -> [ C ]
        ;  put_hex_byte( C)
        ),
        put_chars( Chars).

% ..................................................................

put_hex_byte( C) -->
        { Nlo is C /\ 15,
          Nhi is (C >> 4) /\ 15
        },
        [ 37 ],
        put_hex_digit( Nhi),
        put_hex_digit( Nlo).

put_hex_digit( C) -->
        {  C < 10 
        -> D is 48 + C
        ;  D is 87 + C
        },
        [ D ].
% ..................................................................

var_index( D, V, Index) :-
        var_index_( D, V, 0, Index).

var_index_( D, V, Z, Index) :-
        (  var( D)
        -> Index = Z,
           D = [ V = Index | _ ]
        ;  D = [ W = Index | _ ],
	   V == W
        -> true
        ;  D = [ _ | D1 ]
        -> Z1 is Z + 1,
           var_index_( D1, V, Z1, Index)
        ).

index_var( D, Index, V) :-
	(  var( D)
	-> D = [ V = Index | _ ]
	;  D = [ V = Index | _ ]
	-> true
	;  D = [ _ | D1 ]
	-> index_var( D1, Index, V)
	).

% ==================================================================
% == Deserializing terms ===========================================
% ==================================================================

% ..................................................................
:- export( deserialize_term / 2).

deserialize_term( Term, Bytes) :-
        bdt_( Term, _, Bytes, []).

% ..................................................................
:- export( deserialize_term / 3).

deserialize_term( Term, Bytes, Rest) :-
        bdt_( Term, _, Bytes, Rest).

% ..................................................................
:- export( deserialize_term / 4).

deserialize_term( Term, Dict, Bytes, Rest) :-
        bdt_( Term, Dict, Bytes, Rest).

% ..................................................................

bdt_( V, D) -->
        [ 0'_ ],
        !,
        get_string( S),
        { number_codes( Index, S),
          index_var( D, Index, V) 
        }.

bdt_( 0.Inf, _) -->
        [ 0'+ ],
        !.

bdt_( -0.Inf, _) -->
        [ 0'- ],
        !.

bdt_( 0.Nan, _) -->
        [ 0'n ],
        !.

bdt_( N, _) -->
        [ 0'# ],
        !,
        get_string( S),
        { number_codes( N, S) }.

bdt_( S, _) -->
        [ 34 ],
        !,
        get_string( S).

bdt_( A, _) -->
        [ 0'' ],
        !,
        get_string( S),
        { atom_codes( A, S) }.

bdt_( [H|T], D) -->
        [ 0'[ ],
        !,
        bdt_( H, D),
        bdt_list( T, D).

bdt_( Struct, D) -->
        [ 0'( ],
        get_string( FS),
        { atom_codes( F, FS) },
        bdt_args( Args, D),
        { Struct =.. [ F | Args ] }.

% ..................................................................

bdt_args( [], _) -->
        [ 0') ],
        !.
bdt_args( [ Arg | Args ], D) -->
        bdt_( Arg, D),
        bdt_args( Args, D).

% ..................................................................

bdt_list( [], _) -->
        [ 0'] ],
        !.
bdt_list( [H|T], D) -->
        [ 0', ],
        !,
        bdt_( H, D),
        bdt_list( T, D).
bdt_list( T, D) -->
        [ 0'| ],
        bdt_( T, D).

% ..................................................................

get_string( Chars) -->
        get_chars( Chars).

get_chars( []) -->
        [ 0'| ],
        !.
get_chars( [ C | Chars ]) -->
        [ 0'% ],
        !,
        get_hex_digit( Dhi),
        get_hex_digit( Dlo),
        { C is (Dhi << 4) \/ Dlo },
        get_chars( Chars).
get_chars( [ C | Chars ]) -->
        [ C ],
        get_chars( Chars).


get_hex_digit( D) -->
        [ C0 ],        
        { C is C0 /\ 255 },
        {  C >= 65, C =< 70
        -> D is C - 55
        ;  C >= 97, C =< 102
        -> D is C - 87
        ;  C >= 48, C =< 57
        -> D is C - 48
        }.

% ..................................................................

is_string( L) :-
	var( L),
	!,
	fail.
is_string( []) :- 
	!.
is_string( [ C | S ]) :-
	integer( C),
	C >= 0,
	C =< 255,
	is_string( S).
