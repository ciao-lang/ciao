:- module(json, [], [assertions, regtypes, basicmodes, dcg, fsyntax]).

:- doc(title, "JSON encoder and decoder").
:- doc(author, "Jose F. Morales").

:- doc(stability, devel).

:- doc(module, "This module defines a term representation for
   @href{http://json.org/}{JSON} (JavaScript Object Notation), as well
   as encoders and decoders.
").

:- doc(bug, "The grammar probably incomplete. See
   @href{http://json.org} for a complete reference.").

:- doc(bug, "Missing tests. See @href{http://json.org/example.html}
   for many examples.").

:- doc(bug, "For a parser, any JSON value may be acceptable. However
  only object or array are valid as top-level for JSON text").

:- use_module(library(strings), [string/3]). % (for DCGs)
:- use_module(library(lists), [member/2]).

% ===========================================================================

:- export(json/1).
:- regtype json/1 # "A JSON object. @includedef{json/1}".
json(json(Attrs)) :- json_attrs(Attrs).

:- export(json_attrs/1).
:- regtype json_attrs/1 # "Attributes (pairs of key/value) of a JSON object. @includedef{json_attrs/1}".
json_attrs([]).
json_attrs([X|Xs]) :- json_attr(X), json_attrs(Xs).

:- export(json_attr/1).
:- regtype json_attr/1.
json_attr(Id=Val) :- atm(Id), json_val(Val).

:- export(json_val/1).
:- regtype json_val/1.
json_val(string(X)) :- string(X). % TODO: Use native string?
json_val(X) :- number(X).
json_val(X) :- json(X).
json_val(X) :- json_list(X).
json_val(true).
json_val(false).
json_val(null).

:- export(json_list/1).
:- regtype json_list/1 # "A list of JSON elements".
json_list([]).
json_list([X|Xs]) :- json_val(X), json_list(Xs).

% ===========================================================================

:- export(json_to_string/2).
:- pred json_to_string(+Term, ?String) :: json_val * string # "Encode
   a JSON value @var{Term} as a character list.".
json_to_string(X, String) :- val_to_str(X, String, []).

obj_to_str(json(Attrs)) --> "{", attrs_to_str(Attrs), "}".

attrs_to_str([]) --> [].
attrs_to_str([X]) --> !, attr_to_str(X).
attrs_to_str([X|Xs]) --> attr_to_str(X), ",", attrs_to_str(Xs).

attr_to_str(Id=Val) --> "\"", key_to_str(Id), "\":", val_to_str(Val).

key_to_str(X) --> { atom(X) }, !, { atom_codes(X, Cs) }, stringq(Cs).
key_to_str(X) --> { number(X) }, !, { number_codes(X, Cs) }, string(Cs).
key_to_str(X) --> { is_list(X) }, !, stringq(X).

val_to_str(string(X)) --> !, "\"", stringq(X), "\"".
val_to_str(X) --> { number(X), number_codes(X, Cs) }, string(Cs).
val_to_str(X) --> { X = json(_) }, !, obj_to_str(X).
val_to_str(X) --> { is_list(X) }, !, list_to_str(X).
val_to_str(true) --> !, "true".
val_to_str(false) --> !, "false".
val_to_str(null) --> !, "null".

list_to_str(Xs) --> "[", list_to_str_(Xs), "]".

list_to_str_([]) --> [].
list_to_str_([X]) --> !, val_to_str(X).
list_to_str_([X|Xs]) --> val_to_str(X), ",", list_to_str_(Xs).

stringq([]) --> [].
stringq([C|Cs]) --> { needs_quote(C, C2) }, !, "\\", [C2], stringq(Cs).
% (quote other C<32)
stringq([C|Cs]) --> { C < 32 }, !,
    { H is (C >> 4) + 0'0 },
    { L is (C /\ 15) + 0'0 },
    "\\u00", [H, L],
    stringq(Cs).
stringq([C|Cs]) --> [C], stringq(Cs).

:- export(string_to_json/2).
:- pred string_to_json(+String, ?Term) :: string * json_val # "Decode
   a character list as a JSON value @var{Term}.".
string_to_json(String, X) :-
    string_to_json_(X, String, []).

string_to_json_(X) --> blanks, str_to_val(X), blanks.

% (no leading blanks)
str_to_val(string(Xs)) --> "\"", !, quoted_string_(Xs).
str_to_val(json(Attrs)) --> "{", !, blanks, str_to_attrs_(Attrs).
str_to_val(X) --> "[", !, blanks, str_to_list_(X).
str_to_val(true) --> "true", !.
str_to_val(false) --> "false", !.
str_to_val(null) --> "null", !.
str_to_val(X) --> number_string(Cs), !, { number_codes(X, Cs) }.

% (no leading blanks)
str_to_attrs_([]) --> "}", !.
str_to_attrs_([X|Xs]) -->
    str_to_attr(X),
    blanks, str_to_attrs__(Xs).

% (no leading blanks)
str_to_attrs__([]) --> "}", !.
str_to_attrs__([X|Xs]) -->
    ",",
    blanks, str_to_attr(X),
    blanks, str_to_attrs__(Xs).

% (no leading blanks)
str_to_attr(Id=Val) -->
    quoted_string(Id0),
    { atom_codes(Id, Id0) },
    blanks, ":",
    blanks, str_to_val(Val).

% (no leading blanks)
quoted_string(Cs) --> "\"", quoted_string_(Cs).

quoted_string_([]) --> "\"", !.
quoted_string_([C|Cs]) --> "\\", [C0], { quoted_as(C0, C1) }, !, { C = C1 }, quoted_string_(Cs).
% quoted_string_("\\"||Cs) --> "\\\\", !, quoted_string_(Cs).
% quoted_string_("\b"||Cs) --> "\\b", !, quoted_string_(Cs).
% quoted_string_("\f"||Cs) --> "\\f", !, quoted_string_(Cs).
% quoted_string_("\n"||Cs) --> "\\n", !, quoted_string_(Cs).
% quoted_string_("\r"||Cs) --> "\\r", !, quoted_string_(Cs).
% quoted_string_("\t"||Cs) --> "\\t", !, quoted_string_(Cs).
quoted_string_([C|Cs]) --> [C], quoted_string_(Cs).

% (no leading blanks)
str_to_list_([]) --> "]", !.
str_to_list_([X|Xs]) -->
    str_to_val(X),
    blanks, str_to_list__(Xs).

% (no leading blanks)
str_to_list__([]) --> "]", !.
str_to_list__([X|Xs]) --> ",",
    blanks, str_to_val(X),
    blanks, str_to_list__(Xs).

blanks --> blank, !, blanks.
blanks --> [].

blank --> [C], { is_blank(C) }.

is_blank(0' ).
is_blank(0'\n).
is_blank(0'\t).

number_string("-"||Cs) --> "-", !, number_string_(Cs).
number_string(Cs) --> number_string_(Cs).

number_string_(Cs) -->
    int_string(Cs, Cs2),
    maybe_frac(Cs2, Cs1, Frac),
    maybe_exp(Cs1, [], Frac).

maybe_frac("."||Cs, Cs0, yes) --> ".", !, digits(Cs, Cs0).
maybe_frac(Cs, Cs, no) --> !.

maybe_exp(Cs, Cs0, Frac) -->
    { Frac = yes -> Cs2 = Cs
    ; % We have exponent but not fractional part, add it so that we
      % can used our number_codes/2 as usual.
      Cs = ".0"||Cs2
    },
    exp(Cs2, Cs1), !, digits(Cs1, Cs0).
maybe_exp(Cs, Cs, _) --> !. 

exp("e+"||Cs, Cs) --> "e+", !.
exp("e-"||Cs, Cs) --> "e-", !.
exp("e"||Cs, Cs) --> "e", !.
exp("E+"||Cs, Cs) --> "E+", !.
exp("E-"||Cs, Cs) --> "E-", !.
exp("E"||Cs, Cs) --> "E", !.

int_string("-"||Cs, Cs0) --> "-", !, int_string_(Cs, Cs0).
int_string(Cs, Cs0) --> int_string_(Cs, Cs0).

int_string_("0"||Cs, Cs) --> "0", !.
int_string_(Cs, Cs0) --> digits(Cs, Cs0).

digits([C|Cs], Cs0) --> digit(C), !, digits(Cs, Cs0).
digits(Cs, Cs) --> !.

digit(C) --> [C], { is_digit(C) }.

is_digit(C) :- C >= 0'0, C =< 0'9.

% TODO: Missing unicode
needs_quote(0'\", 0'\").
needs_quote(0'\\, 0'\\).
needs_quote(0'\b, 0'b).
needs_quote(0'\f, 0'f).
needs_quote(0'\n, 0'n).
needs_quote(0'\r, 0'r).
needs_quote(0'\t, 0't).

% TODO: Missing unicode
% (reverse of needs_quote)
quoted_as(0'\", 0'\").
quoted_as(0'\\, 0'\\).
quoted_as(0'b, 0'\b).
quoted_as(0'f, 0'\f).
quoted_as(0'n, 0'\n).
quoted_as(0'r, 0'\r).
quoted_as(0't, 0'\t).

% ===========================================================================

% (Shallow test for lists)
is_list([]).
is_list([_|_]).

% ===========================================================================
% Other predicates for accessing term representation of JSON objects 

:- export(json_get/3).
% Lookup Key in JSON dictionary
json_get(json(Xs), Key, Value) :-
    member(Key0=Value0, Xs), Key == Key0, !,
    Value = Value0.

:- export(json_get_atm/3).
% Lookup Key in JSON dictionary and translate to atm
json_get_atm(Json, Key, Value) :-
    json_get(Json, Key, Value0),
    json_as_atm(Value0, Value).

:- export(atomiclst_to_json_strlist/2).
% From list of constants to JSON value
atomiclst_to_json_strlist([]) := [].
atomiclst_to_json_strlist([A|As]) := [~atomic_to_json_str(A)|Bs] :-
    Bs = ~atomiclst_to_json_strlist(As).

:- export(atomic_to_json_str/2).
% From constant (atomic) to JSON value
atomic_to_json_str(X) := string(R) :- number(X), !, number_codes(X, R).
atomic_to_json_str(X) := string(R) :- atom(X), !, atom_codes(X, R).

:- export(json_as_atm/2).
% From json string to atom
json_as_atm(Value, Atm) :-
    Value = string(Xs),
    atom_codes(Atm, Xs).

:- export(json_as_num/2).
% From json string to number
json_as_num(Value, Num) :-
    Value = string(Xs),
    number_codes(Num, Xs).

