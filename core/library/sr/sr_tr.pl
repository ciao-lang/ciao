:- module(_, [], [assertions, datafacts]).

% ---------------------------------------------------------------------------

:- data default_sr/2.
% pred_sr(F, A, M, SearchRule)
:- data pred_sr/4.

% Declare the search rule for M:F/A, emit an error and fail on mismatch
decl_sr(F, A, M, SearchRule) :-
    ( pred_sr(F, A, M, SearchRule0) ->
        ( SearchRule = SearchRule0 -> true
        ; mod_error(M, ['mixing search rules for ', ~~(F/A)]),
          fail
        )
    ; assertz_fact(pred_sr(F, A, M, SearchRule))
    ).

% ---------------------------------------------------------------------------

sr_runtime(bf, library(sr/bf_rt)).
sr_runtime(af, library(sr/af_rt)).

:- export(sent_tr/3).
sent_tr(end_of_file, L, M) :-
    default_sr(M, SearchRule), ( SearchRule = bf ; SearchRule = af ),
    !,
    ( pred_sr(_, _, M, SearchRule) ->
        sr_runtime(SearchRule, Lib),
        L = [(:- include(Lib)), end_of_file]
    ; L = end_of_file, 
      ( module_error -> true
      ; true % TODO: recover warning? maybe not a good idea (message(warning, ['bf package: no breadth-first predicate defined']))
      )
    ),
    retractall_fact(default_sr(M,_)),
    retractall_fact(pred_sr(_,_,M,_)).
sent_tr(end_of_file, L, M) :-
    ( default_sr(M, bfall) -> SearchRule = bf
    ; default_sr(M, afall) -> SearchRule = af
    ; fail
    ),
    !,
    sr_runtime(SearchRule, Lib),
    L = [(:- include(Lib)), end_of_file],
    retractall_fact(default_sr(M,_)),
    retractall_fact(pred_sr(_,_,M,_)).
%
sent_tr((:- search_rule(SearchRule)), TrCls, M) :- !,
    retractall_fact(default_sr(M,_)),
    assertz_fact(default_sr(M,SearchRule)),
    TrCls = [].
sent_tr((:- _), _, _) :- !, fail. % skip other declarations
%
sent_tr(<-(H,B), TrCls, M) :- default_sr(M, bf), !,
    sr_clause_tr(bf, H, B, M, TrCls).
sent_tr(<-(H), TrCls, M) :- default_sr(M, bf), !,
    sr_clause_tr(bf, H, true, M, TrCls).
sent_tr(<-(H,B), TrCls, M) :- default_sr(M, af), !,
    sr_clause_tr(af, H, B, M, TrCls).
sent_tr(<-(H), TrCls, M) :- default_sr(M, af), !,
    sr_clause_tr(af, H, true, M, TrCls).
sent_tr(Cl, TrCl, M) :-
    cl_rule(Cl, H, B),
    ( default_sr(M, bfall) -> SearchRule = bf
    ; default_sr(M, afall) -> SearchRule = af
    ; SearchRule = df
    ),
    sr_clause_tr(SearchRule, H, B, M, TrCl).

cl_rule((H:-B), H, B) :- !.
cl_rule(H, H, true).

% treat as depth-first
sr_clause_tr(df, H, _B, M, TrCl) :- !,
    functor(H, F, A),
    ( decl_sr(F, A, M, df) ->
        fail % (OK, skip translation)
    ; cl_mismatch(TrCl) % search rule mismatch
    ).
sr_clause_tr(SearchRule, Head, Body, M, TrCls) :-
    functor(Head, F, A),
    ( \+ pred_sr(F, A, M, _) -> % wrapper on first occurrence
        sr_wrapper(SearchRule, F, A, TrCls, TrCls0)
    ; TrCls = TrCls0
    ),
    ( decl_sr(F, A, M, SearchRule) -> % (declare search rule)
        body_to_dl(Body, Bodylist, Bodyrest),
        TrCls0 = ['$bfcl'(Head, Bodylist, Bodyrest)]
    ; cl_mismatch(TrCls0) % search rule mismatch
    ).

sr_wrapper(df, _, _, Cls, Cls0) :- !, Cls=Cls0.
sr_wrapper(SearchRule, F, A, Cls, Cls0) :-
    functor(P, F, A),
    Cls = ['$bfpred'(P),(P :- '$sr_call'(SearchRule,P))|Cls0].

% consume clause, compilation will abort later
cl_mismatch([]).
    
% ---------------------------------------------------------------------------

body_to_dl((A, B), [A|Bs], R):- !, body_to_dl(B, Bs, R).
body_to_dl(true, R, R) :- !.
body_to_dl(A, [A|R], R).

% ---------------------------------------------------------------------------

:- use_module(engine(messages_basic), [message/2]).
:- use_module(library(compiler/c_itf), [module_error/0]).

% TODO: generalize c_itf:module_warning/1, use location
mod_error(Mod, Err) :-
    message(error, ['(in ', Mod, ') '|Err]),
    set_fact(module_error).
    
