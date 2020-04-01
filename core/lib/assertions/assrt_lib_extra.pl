:- module(assrt_lib_extra, [], [assertions, isomodes, hiord]).

:- doc(title, "Additional syntactic sugar for assertions").

:- doc(authors, "Jose F. Morales").
:- doc(authors, "Manuel Hermenegildo").

:- doc(module, "This module implements additional operations on
assertions and experimetal syntactic extensions:

@begin{itemize}
@item @tt{Head(..., X :: Type, ...)} notation (types in arguments)
@item @tt{_ is _} for comp properties
@end{itemize}
").

:- doc(bug, "Merge into the different versions of assrt_lib").
%   (try to unify them before):
%        core/lib/assertions/assrt_lib.pl
%        ciaopp/p_unit/assrt_norm.pl
%        core_OC/compiler/assertions__syntactic.pl

:- use_module(library(assertions/assrt_lib), [assertion_body/7]).

:- export(assrt_set_comment/3).
% Replace the comment of `Body0` with by `Co` in `Body`.
assrt_set_comment(ABody0, CO, ABody) :-
    assertion_body(PD,DP,CP,AP,GP,_CO0,ABody0),
    assertion_body(PD,DP,CP,AP,GP,CO,ABody).

:- export(maybe_assrt_extra/2).
% Normalize an assertion declaration, or leave unchanged.
maybe_assrt_extra(Decl0, Decl) :-
    assrt_extra(Decl0, Decl1), !, Decl = Decl1.
maybe_assrt_extra(Decl, Decl).

:- export(assrt_extra/2).
% (fail if this is not extra syntax for assertions)
assrt_extra((:- pred ABody), (:- pred ABody2)) :-
    % TODO: Use normalize_status_and_type
    norm_body_extra(ABody, _, ABody1),
    !, % if fail, assume that it was not an assertion
    norm_args(ABody1, ABody2).

norm_args(ABody0, ABody) :-
    assertion_body(PD0,true,CP,AP,GP,CO,ABody0),
%       display(aa(assertion_body(PD0,true,CP,AP,GP,CO,ABody0))), nl,
    \+ functor(PD0, /, 2), % TODO: check other assrt processing code!
    PD0 =.. [H|Args0],
    extract_types(Args0, Args, DP1),
    list_to_prod(DP1, DP),
    \+ useless_type_annot(DP),
    !,
    PD =..[H|Args],
%       display(aa(assertion_body(PD,DP,CP,AP,GP,CO,ABody0))), nl,
    assertion_body(PD,DP,CP,AP,GP,CO,ABody).
norm_args(ABody, ABody).

extract_types([], [], []).
extract_types([A0|As0], [A|As], [T|Ts]) :-
    extract_type(A0, A, T),
    extract_types(As0, As, Ts).

extract_type(A, A, term) :- var(A), !.
extract_type(X :: Type0, A, Type) :- !,
    % A type annotation (X can be an argument or a moded argument)
    A = X, Type = Type0. 
extract_type(M, A, Type) :-
    functor(M, N, A),
    A1 is A + 1,
    functor(M1, N, A1),
    is_modedef(M1),
    % A moded argument, do nothing
    !,
    A = M, Type = term.
extract_type(X, A, Type) :-
    % Assume that it is a type
    A = '?',
    Type = X.

useless_type_annot(term).
useless_type_annot(A * B) :-
    useless_type_annot(A), useless_type_annot(B).

% ---------------------------------------------------------------------------
% TODO: Move to the assertion library to avoid this ugly hack.
%       I need it to distinguish modedefs from typedefs (for norm_args/2).

is_modedef('+'(_)).  is_modedef('+'(_,_)).
is_modedef('-'(_)).  is_modedef('-'(_,_)).
is_modedef('?'(_)).  is_modedef('?'(_,_)).
is_modedef(':'(_)).  is_modedef(':'(_,_)). % From pldoc (can bring problems...)
is_modedef('@'(_)).  is_modedef('@'(_,_)).
is_modedef(in(_)).   is_modedef(in(_,_)). 
is_modedef(out(_)).  is_modedef(out(_,_)).
is_modedef(go(_)).   is_modedef(go(_,_)). 

% ---------------------------------------------------------------------------
% Auxiliary for normalization

:- export(list_to_prod/2).
% From [A1,...,An] to (A1*...*An) ('*' is left associative).
list_to_prod([A|As], B) :-
    list_to_prod_(As, A, B).

list_to_prod_([], Acc, R) :- !, R = Acc.
list_to_prod_([A|As], Acc, R) :- 
    list_to_prod_(As, Acc * A, R).

:- export(norm_body_extra/3).
% Extended assrt_lib:norm_body/3 with 'is' for comments.
% TODO: factorize using operator priority?
% ------------ A  B   C  D  E --FormatId--------------------------- %ABCDE
norm_body_extra((PD::DP:CP=>AP is GP#CO),p,(PD::DP  :CP  =>AP  +GP  #CO)):-!.%11111I
norm_body_extra((PD::DP:CP=>AP  + GP#CO),p,(PD::DP  :CP  =>AP  +GP  #CO)):-!.%11111
norm_body_extra((PD::DP:CP=>AP is GP   ),p,(PD::DP  :CP  =>AP  +GP  #"")):-!.%11110I
norm_body_extra((PD::DP:CP=>AP  + GP   ),p,(PD::DP  :CP  =>AP  +GP  #"")):-!.%11110
norm_body_extra((PD::DP:CP=>AP      #CO),p,(PD::DP  :CP  =>AP  +true#CO)):-!.%11101
norm_body_extra((PD::DP:CP=>AP         ),p,(PD::DP  :CP  =>AP  +true#"")):-!.%11100
norm_body_extra((PD::DP:CP     is GP#CO),p,(PD::DP  :CP  =>true+GP  #CO)):-!.%11011I
norm_body_extra((PD::DP:CP      + GP#CO),p,(PD::DP  :CP  =>true+GP  #CO)):-!.%11011
norm_body_extra((PD::DP:CP     is GP   ),p,(PD::DP  :CP  =>true+GP  #"")):-!.%11010I
norm_body_extra((PD::DP:CP      + GP   ),p,(PD::DP  :CP  =>true+GP  #"")):-!.%11010
norm_body_extra((PD::DP:CP          #CO),p,(PD::DP  :CP  =>true+true#CO)):-!.%11001
norm_body_extra((PD::DP:CP             ),p,(PD::DP  :CP  =>true+true#"")):-!.%11000
norm_body_extra((PD::DP   =>AP is GP#CO),p,(PD::DP  :true=>AP  +GP  #CO)):-!.%10111I
norm_body_extra((PD::DP   =>AP  + GP#CO),p,(PD::DP  :true=>AP  +GP  #CO)):-!.%10111
norm_body_extra((PD::DP   =>AP is GP   ),p,(PD::DP  :true=>AP  +GP  #"")):-!.%10110I
norm_body_extra((PD::DP   =>AP  + GP   ),p,(PD::DP  :true=>AP  +GP  #"")):-!.%10110
norm_body_extra((PD::DP   =>AP      #CO),p,(PD::DP  :true=>AP  +true#CO)):-!.%10101
norm_body_extra((PD::DP   =>AP         ),p,(PD::DP  :true=>AP  +true#"")):-!.%10100
norm_body_extra((PD::DP        is GP#CO),p,(PD::DP  :true=>true+GP  #CO)):-!.%10011I
norm_body_extra((PD::DP         + GP#CO),p,(PD::DP  :true=>true+GP  #CO)):-!.%10011
norm_body_extra((PD::DP        is GP   ),p,(PD::DP  :true=>true+GP  #"")):-!.%10010I
norm_body_extra((PD::DP         + GP   ),p,(PD::DP  :true=>true+GP  #"")):-!.%10010
norm_body_extra((PD::DP             #CO),d,(PD::DP  :true=>true+true#CO)):-!.%10001
norm_body_extra((PD::DP                ),d,(PD::DP  :true=>true+true#"")):-!.%10000
norm_body_extra((PD    :CP=>AP is GP#CO),p,(PD::true:CP  =>AP  +GP  #CO)):-!.%01111I
norm_body_extra((PD    :CP=>AP  + GP#CO),p,(PD::true:CP  =>AP  +GP  #CO)):-!.%01111
norm_body_extra((PD    :CP=>AP is GP   ),p,(PD::true:CP  =>AP  +GP  #"")):-!.%01110I
norm_body_extra((PD    :CP=>AP  + GP   ),p,(PD::true:CP  =>AP  +GP  #"")):-!.%01110
norm_body_extra((PD    :CP=>AP      #CO),s,(PD::true:CP  =>AP  +true#CO)):-!.%01101
norm_body_extra((PD    :CP=>AP         ),s,(PD::true:CP  =>AP  +true#"")):-!.%01100
norm_body_extra((PD    :CP     is GP#CO),g,(PD::true:CP  =>true+GP  #CO)):-!.%01011I
norm_body_extra((PD    :CP      + GP#CO),g,(PD::true:CP  =>true+GP  #CO)):-!.%01011
norm_body_extra((PD    :CP     is GP   ),g,(PD::true:CP  =>true+GP  #"")):-!.%01010I
norm_body_extra((PD    :CP      + GP   ),g,(PD::true:CP  =>true+GP  #"")):-!.%01010
norm_body_extra((PD    :CP          #CO),c,(PD::true:CP  =>true+true#CO)):-!.%01001
norm_body_extra((PD    :CP             ),c,(PD::true:CP  =>true+true#"")):-!.%01000
norm_body_extra((PD       =>AP is GP#CO),p,(PD::true:true=>AP  +GP  #CO)):-!.%00111I
norm_body_extra((PD       =>AP  + GP#CO),p,(PD::true:true=>AP  +GP  #CO)):-!.%00111
norm_body_extra((PD       =>AP is GP   ),p,(PD::true:true=>AP  +GP  #"")):-!.%00110I
norm_body_extra((PD       =>AP  + GP   ),p,(PD::true:true=>AP  +GP  #"")):-!.%00110
norm_body_extra((PD       =>AP      #CO),s,(PD::true:true=>AP  +true#CO)):-!.%00101
norm_body_extra((PD       =>AP         ),s,(PD::true:true=>AP  +true#"")):-!.%00100
norm_body_extra((PD            is GP#CO),g,(PD::true:true=>true+GP  #CO)):-!.%00011I
norm_body_extra((PD             + GP#CO),g,(PD::true:true=>true+GP  #CO)):-!.%00011
norm_body_extra((PD            is GP   ),g,(PD::true:true=>true+GP  #"")):-!.%00010I
norm_body_extra((PD             + GP   ),g,(PD::true:true=>true+GP  #"")):-!.%00010
norm_body_extra((PD                 #CO),p,(PD::true:true=>true+true#CO)):-!.%00001
norm_body_extra((PD                    ),t,(PD::true:true=>true+true#"")):-!.%00000
% ---------------------------------------------------------------------------

:- export(assrt_replace_pd/4).
% Replace PD part of an assertion without normalizing it. NewPD can be a free variable
% and instantiated later.
%
% This predicate is useful for extending the assertion syntax (e.g., hiord assertions).
%
% E.g.,
%  ?- assrt_replace_pd((pred P(X) : num => int), NewPD, Assrt2, OldPD),
%     OldPD =.. [call,N|Args],
%     NewPD =.. ['\6\pvar'|Args].

% TODO: perhaps fixing normalize_assertions is simpler, at least for some case

assrt_replace_pd(Assrt, NewPD, Assrt2, OldPD) :- 
    ( Assrt =.. [AType,UBody] ->
        AStatus = ''
    ; Assrt =.. [AType,AStatus,UBody] ->
        true
    ; fail
    ),
    replace_pd(UBody, UBody2, OldPD, NewPD),
    ( AStatus = '' ->
        Assrt2  =.. [AType,UBody2]
    ; Assrt2 =.. [AType,AStatus,UBody2]
    ).

% Replace PD in an assertion body without normalizing it
replace_pd((PD0::DP:CP=>AP+GP#CO),(PD1::DP:CP=>AP+GP#CO),PD0,PD1):-!.%11111
replace_pd((PD0::DP:CP=>AP+GP   ),(PD1::DP:CP=>AP+GP   ),PD0,PD1):-!.%11110
replace_pd((PD0::DP:CP=>AP   #CO),(PD1::DP:CP=>AP   #CO),PD0,PD1):-!.%11101
replace_pd((PD0::DP:CP=>AP      ),(PD1::DP:CP=>AP      ),PD0,PD1):-!.%11100
replace_pd((PD0::DP:CP    +GP#CO),(PD1::DP:CP    +GP#CO),PD0,PD1):-!.%11011
replace_pd((PD0::DP:CP    +GP   ),(PD1::DP:CP    +GP   ),PD0,PD1):-!.%11010
replace_pd((PD0::DP:CP       #CO),(PD1::DP:CP       #CO),PD0,PD1):-!.%11001
replace_pd((PD0::DP:CP          ),(PD1::DP:CP          ),PD0,PD1):-!.%11000
replace_pd((PD0::DP   =>AP+GP#CO),(PD1::DP   =>AP+GP#CO),PD0,PD1):-!.%10111
replace_pd((PD0::DP   =>AP+GP   ),(PD1::DP   =>AP+GP   ),PD0,PD1):-!.%10110
replace_pd((PD0::DP   =>AP   #CO),(PD1::DP   =>AP   #CO),PD0,PD1):-!.%10101
replace_pd((PD0::DP   =>AP      ),(PD1::DP   =>AP      ),PD0,PD1):-!.%10100
replace_pd((PD0::DP       +GP#CO),(PD1::DP       +GP#CO),PD0,PD1):-!.%10011
replace_pd((PD0::DP       +GP   ),(PD1::DP       +GP   ),PD0,PD1):-!.%10010
replace_pd((PD0::DP          #CO),(PD1::DP          #CO),PD0,PD1):-!.%10001
replace_pd((PD0::DP             ),(PD1::DP             ),PD0,PD1):-!.%10000
replace_pd((PD0    :CP=>AP+GP#CO),(PD1    :CP=>AP+GP#CO),PD0,PD1):-!.%01111
replace_pd((PD0    :CP=>AP+GP   ),(PD1    :CP=>AP+GP   ),PD0,PD1):-!.%01110
replace_pd((PD0    :CP=>AP   #CO),(PD1    :CP=>AP   #CO),PD0,PD1):-!.%01101
replace_pd((PD0    :CP=>AP      ),(PD1    :CP=>AP      ),PD0,PD1):-!.%01100
replace_pd((PD0    :CP    +GP#CO),(PD1    :CP    +GP#CO),PD0,PD1):-!.%01011
replace_pd((PD0    :CP    +GP   ),(PD1    :CP    +GP   ),PD0,PD1):-!.%01010
replace_pd((PD0    :CP       #CO),(PD1    :CP       #CO),PD0,PD1):-!.%01001
replace_pd((PD0    :CP          ),(PD1    :CP          ),PD0,PD1):-!.%01000
replace_pd((PD0       =>AP+GP#CO),(PD1       =>AP+GP#CO),PD0,PD1):-!.%00111
replace_pd((PD0       =>AP+GP   ),(PD1       =>AP+GP   ),PD0,PD1):-!.%00110
replace_pd((PD0       =>AP   #CO),(PD1       =>AP   #CO),PD0,PD1):-!.%00101
replace_pd((PD0       =>AP      ),(PD1       =>AP      ),PD0,PD1):-!.%00100
replace_pd((PD0           +GP#CO),(PD1           +GP#CO),PD0,PD1):-!.%00011
replace_pd((PD0           +GP   ),(PD1           +GP   ),PD0,PD1):-!.%00010
replace_pd((PD0              #CO),(PD1              #CO),PD0,PD1):-!.%00001
replace_pd((PD0                 ),(PD1                 ),PD0,PD1):-!.%00000
