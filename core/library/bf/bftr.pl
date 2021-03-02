:- module(_, [], [assertions, datafacts]).

% ---------------------------------------------------------------------------

% pred_sr(F, A, M, SearchRule)
:- data pred_sr/4.

% Declare the search rule for M:F/A, emit an error and fail on mismatch
decl_sr(F, A, M, SearchRule) :-
    ( pred_sr(F, A, M, SearchRule0) ->
        ( SearchRule = SearchRule0 -> true
        ; mod_error(M, ['mixing search rules for ', ~~(F/A)]),
          fail
        )
    ; asserta_fact(pred_sr(F, A, M, SearchRule))
    ).

% ---------------------------------------------------------------------------

:- export(bftr/3).
bftr(end_of_file, L, M) :- !,
    ( pred_sr(_, _, M, bf) ->
        L = [(:- include(library(bf/bf_rt))), end_of_file]
    ; L = end_of_file, 
      ( module_error -> true
      ; message(warning, ['bf package: no breadth-first predicate defined'])
      )
    ),
    retractall_fact(pred_sr(_,_,M,_)).
bftr(<-(H,B), TrCls, M) :- !,
    clausetr(H, B, M, TrCls).
bftr(<-(H), TrCls, M) :- !,
    clausetr(H, true, M, TrCls).
bftr(Cl, TrCl, M) :-
    cl_rule(Cl, H, _),
    functor(H, F, A),
    % annotate depth-first by default
    ( decl_sr(F, A, M, df) -> % (default)
        TrCl = Cl
    ; TrCl = [] % skip clause (compilation will abort anyway)
    ).

cl_rule((H:-B), H, B) :- !.
cl_rule(H, H, true).

clausetr(Head, Body, M, TrCls) :-
    functor(Head, F, A),
    % introduce wrapper on first occurence
    ( \+ pred_sr(F, A, M, _) ->
        functor(P, F, A),
        TrCls = ['$bfpred'(P),(P :- '$bf'([u([P],P)|L],L,P))| TrCl]
    ; TrCls = TrCl
    ),
    ( decl_sr(F, A, M, bf) ->
        body_to_dl(Body, Bodylist, Bodyrest),
        TrCl = ['$bfcl'(Head, Bodylist, Bodyrest)]
    ; TrCl = [] % skip clause (compilation will abort anyway)
    ).
    
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
    
