% (included file)
:- doc(section, "Clause sanity checks").
% This code implements a checker to detect some conditions that
% frequently are identified with programming mistakes:
%
%   - multiarity predicates
%   - discontiguous clauses
%   - singleton variables

% ---------------------------------------------------------------------------

:- multifile define_flag/3.
:- discontiguous(define_flag/3).
define_flag(single_var_warnings,    [on,  off], on).
define_flag(discontiguous_warnings, [on,  off], on).
define_flag(multi_arity_warnings,   [on,  off], on).

:- use_module(engine(runtime_control), [current_prolog_flag/2]).

% ---------------------------------------------------------------------------
:- doc(subsection, "Declarations").

% ---------------------------------------------------------------------------
% discontiguous/1 - Declare a predicate as discontiguous

decl__treatDom(discontiguous(_)).
decl__treat(discontiguous(L), Base,_M,_VNs, Ln0, Ln1) :- !,
    do_discontiguous(L, Base, Ln0, Ln1).

do_discontiguous(L, Base, Ln0, Ln1) :-
    sequence_contains(L, bad_spec_error(discontiguous, Ln0, Ln1), F, A),
      asserta_fact(discontiguous(F,A,Base)),
    fail.
do_discontiguous(_, _, _, _).

% ---------------------------------------------------------------------------
:- doc(subsection, "Clause check").

clause_check(F, A, Base,_Ln0,_Ln1) :-
    reading_pred(F, A, Base), !.
clause_check(F, A, Base, Ln0, Ln1) :-
    current_prolog_flag(multi_arity_warnings, on),
    pred_read(F, A0, Base),
    A0 =\= A,
    \+ (exports_meta_pred(Base, F, A), exports_meta_pred(Base, F, A0)),
    error_in_lns(Ln0, Ln1, warning, ['predicate ',~~(F/A),
                 ' is already defined with arity ',A0]),
    fail.
clause_check(F, A, Base, Ln0, Ln1) :-
    already_read_pred(F, A, Base),
    current_prolog_flag(discontiguous_warnings, on),
    \+ discontiguous(F, A, Base),
    error_in_lns(Ln0, Ln1, warning,
                 ['clauses of ',~~(F/A),' are discontiguous']),
    fail.
clause_check(F, A, Base,_Ln0,_Ln1) :-
    retractall_fact(reading_pred(_,_,Base)),
    asserta_fact(reading_pred(F,A,Base)).

exports_meta(Base, F, A) :-
    meta_pred(Base, F, A1, Meta),
    meta_inc_args(Meta, A1, A),
    exports_pred(Base, F, A1).

exports_meta_pred(Base, F, A) :- exports_pred(Base, F, A).
exports_meta_pred(Base, F, A) :- exports_meta(Base, F, A).

already_read_pred(F, A, Base) :- pred_read(F, A, Base), !.
already_read_pred(F, A, Base) :- asserta_fact(pred_read(F,A,Base)), fail.

singleton_check(Singletons, F, A, Ln0, Ln1) :-
    current_prolog_flag(single_var_warnings, on), !,
    no_underlines(Singletons, BadSingletons),
    singleton_check1(BadSingletons, F, A, Ln0, Ln1).
singleton_check(_, _, _, _, _).

no_underlines([], []).
no_underlines([N=_|Eqs], Ns) :-
    ( atom_concat('_', _, N) ->
          no_underlines(Eqs, Ns)
    ; Ns = [N|Ns_],
      no_underlines(Eqs, Ns_)
    ).

singleton_check1([], _, _, _, _) :- !.
singleton_check1(BadSingletons, F, A, Ln0, Ln1) :-
    error_in_lns(Ln0, Ln1, warning,
      [BadSingletons,' - singleton variables in ',~~(F/A)]).

