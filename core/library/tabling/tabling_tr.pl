:- module(tabling_tr, [do_term_expansion/3], [dynamic]).

:- use_module(engine(internals), [module_concat/3]).
:- use_module(engine(io_basic)).
:- use_module(engine(basic_props)).
:- use_module(library(lists), [reverse/2, append/3, length/2]).

:- dynamic 'trans$tabled'/2, 'trans$default'/1.

:- dynamic 'trans$prolog'/1, 'trans$tab'/2.
:- dynamic trans_conexpanding/0.

:- dynamic module/1, module_name/1.
%:- dynamic const_table_mod/1.
:- dynamic tclp_actived/0.
:- dynamic tclp_aggregates/1.
:- dynamic tclp_subsumption/0.

:- dynamic counter/1.

:- data found_hook/2.

reset(M):-
    retractall_fact(found_hook(M, _)).
add_hook(M, T):-
    current_fact(found_hook(M, T)), !.
add_hook(M, T):-
    asserta_fact(found_hook(M, T)), !.



do_term_expansion(0, _, Module) :-
    reset(_),
    assert('trans$tabled'(0, 0)), retractall('trans$tabled'(_, _)),
    assert('trans$default'((prolog))),
    (
        Module = user(_) ->
        Mod = "user"
    ;
        atom_codes(Module, Mod)
    ),
    assert(module(Mod)),
    assert(module_name(Mod)),
    assert(counter(0)), retractall(counter(_)), assert(counter(1)).

do_term_expansion(end_of_file, Tail0, _) :- !,
    retractall('trans$default'(_)),
    assert('trans$default'((prolog))),
    retractall('trans$prolog'(_)),
    retractall('trans$tab'(_, _)),
    retractall(module_name(_)),
    ( tclp_actived ->
        write_multifile_info(Tail0)
    ;
        Tail0 = _
    ),
    retractall(tclp_actived),
    retractall(tclp_aggregates(_)),
    retractall(tclp_subsumption).

do_term_expansion(':-'(Com), Clauses, _) :- !,
    expand_command(Com, Clauses).
%%      expand_command(Com,Clauses),
%%      display(Clauses), nl.

do_term_expansion(Clause, Clauses, _) :-
    (
        Clause = (Head :- Body) ->
        true
    ;
        Head = Clause,
        Body = true
    ),
    functor(Head, P, A),
    Pred = P/A,
    (
        'trans$tab'(P, A) ->
        convert_tabled_clause(Head, Body, Clauses)
    ;
        'trans$prolog'(Pred) ->
        Clauses = Clause
    ;
        P=current_store, A=1 ->
        add_hook(_, current_store),
        fail
    ;
        P=answer_check_entail, A=5 ->
        add_hook(_, check_lattice),
        fail
    ;
        'trans$default'(Default),
        (
            Default == (prolog) ->
            Clauses = Clause
        ;
            display('BUG tabling_tr: DEFAULT ENTERED'), nl
        )
    ).
%%      ),
%%      display(Clauses), nl.

write_multifile_info(Tail0) :-
    (
        tclp_aggregates(_) ->
        Tail0 = [
                    ( :- use_package(library(tabling/tab_aggregates)) ),
                      ( :- use_package(attr) ) | Tail1
                ]
    ;
        tclp_subsumption ->
        Tail0 = [
                    ( :- use_package(library(tabling/tab_subsumption)) ),
                      ( :- use_package(attr) ) | Tail1
                ]
    ;
        Tail0 = Tail1
    ),
    Tail1 = [
                ( :- multifile
                'tabling_rt:call_domain_projection'/2,
                'tabling_rt:answer_domain_projection'/2,
                'tabling_rt:call_store_projection'/3,
                'tabling_rt:answer_store_projection'/3,
                'tabling_rt:call_entail'/4,
                'tabling_rt:answer_check_entail'/5,
                'tabling_rt:apply_answer'/3,
                'tabling_rt:current_store'/1,
                'tabling_rt:reinstall_store'/3
                ),
                ( 'tabling_rt:call_domain_projection'(Vars, DomA) :-
                call_domain_projection(Vars, DomA) ),
                ( 'tabling_rt:answer_domain_projection'(Vars, DomA) :-
                answer_domain_projection(Vars, DomA) ),
                ( 'tabling_rt:call_store_projection'(Vars, DomA, Project) :-
                call_store_projection(Vars, DomA, Project) ),
                ( 'tabling_rt:answer_store_projection'(Vars, DomA, Answer) :-
                answer_store_projection(Vars, DomA, Answer) ),
                ( 'tabling_rt:call_entail'(_Vars, DomA, _DomB, StoreB) :-
                call_entail(DomA, StoreB) ),
                ( 'tabling_rt:answer_check_entail'(_Vars, DomA, _DomB, StoreB, Result) :-
                answer_check_entail(DomA, StoreB, Result) ),
                ( 'tabling_rt:apply_answer'(Vars, _DomA, StoreA) :-
                apply_answer(Vars, StoreA) ) 
                % ( 'tabling_rt:call_domain_projection'(Vars, DomA) :-
                % call_domain_projection(Vars, DomA) ),
                % ( 'tabling_rt:answer_domain_projection'(Vars, DomA) :-
                % answer_domain_projection(Vars, DomA) ),
                % ( 'tabling_rt:call_store_projection'(Vars, DomA, Project) :-
                % call_store_projection(Vars, DomA, Project) ),
                % ( 'tabling_rt:answer_store_projection'(Vars, DomA, Answer) :-
                % answer_store_projection(Vars, DomA, Answer) ),
                % ( 'tabling_rt:call_entail'(Vars, DomA, DomB, StoreB) :-
                % call_entail(Vars, DomA, DomB, StoreB) ),
                % ( 'tabling_rt:answer_check_entail'(Vars, DomA, DomB, StoreB, Result, NewDom) :-
                % answer_check_entail(Vars, DomA, DomB, StoreB, Result, NewDom) ),
                % ( 'tabling_rt:apply_answer'(Vars, DomA, StoreA) :-
                % apply_answer(Vars, DomA, StoreA) ) 
            | Tail2],
    (
        retract_fact(found_hook(_, current_store)) ->
        Tail2 = [
                    ( 'tabling_rt:current_store'(Orig) :-
                    current_store(Orig) ),
                    ( 'tabling_rt:reinstall_store'(Vars, DomA, StoreA) :-
                    reinstall_store(Vars, DomA, StoreA) ),          
                    end_of_file
                ]
    ;
        Tail2 = [
                    ('tabling_rt:current_store'(_) :- 
                    true),
                    ('tabling_rt:reinstall_store'(_,_,_) :- 
                    true),
                    end_of_file
                ]
    ).


expand_command(table(Preds), Clauses) :- !,
    expand_command_table(Preds, Clauses, []).


expand_command(table_subsumption(Preds), Clauses) :- !,
    assert(tclp_actived),
    assert(tclp_subsumption),
    expand_command_table(Preds, Clauses, []).


    
expand_command(active_tclp, _) :-
    !,
    (
        tclp_actived ->
        display('ERROR: several tclp activations.'), nl
    ;
        assert(tclp_actived)
    ).


expand_command(discontiguous(Preds), [( :- discontiguous NewPreds)]) :- !,
    new_preds(Preds, NewPreds).
new_preds((Name/A,Preds),(NewName/A, NewPreds)) :- !,
    name(Name, N),
    append("$", N, NN),
    name(NewName, NN),
    new_preds(Preds, NewPreds).
new_preds(Name/A, NewName/A) :-
    name(Name, N),
    append("$", N, NN),
    name(NewName, NN).
    

expand_command_table((Pred/A, Preds), Clauses0, Clauses) :- !,
    expand_command_table_one(Pred/A, Clauses0, Clauses1),
    expand_command_table(Preds, Clauses1, Clauses).
expand_command_table(Pred/A, Clauses0, Clauses) :- !,
    expand_command_table_one(Pred/A, Clauses0, Clauses).
expand_command_table((Pred, Preds), Clauses0, Clauses) :-
    struct(Pred), !,
    Pred =.. [Name| Arg], 
    length(Arg, Arity),
    assert(tclp_aggregates(Name/Arity:Arg)),
    expand_command_table_one(Name/Arity, Clauses0, Clauses1),
    expand_command_table(Preds, Clauses1, Clauses).
expand_command_table(Pred, Clauses0, Clauses) :- 
    struct(Pred), !,
    assert(tclp_actived),
    Pred =.. [Name| Arg],
    length(Arg, Arity),
    assert(tclp_aggregates(Name/Arity:Arg)),
    expand_command_table_one(Name/Arity, Clauses0, Clauses).

expand_command_table_one(Pspec, Clauses0, Clauses) :-
    (
        Pspec = P/A -> 
        true
    ;
        P = Pspec, A = 0
    ),
    functor(H, P, A),
    (
        'trans$tab'(P, A) ->
        Clauses0 = Clauses
    ;
        assert('trans$tab'(P, A)),
        retractall('trans$tabled'(P, A)),
        assert('trans$tabled'(P, A)),
        get_pred_init(H, PredInit),
        (
            tclp_subsumption ->
            Clauses0 = [( H :- 
                        make_attr(PredInit,PredInitAttr),
                        lookup_trie(PredInitAttr, Root, SF),
                        lookup_attr_call(Root, SF, PGen, CallSpace, LPrune),
                        execute_call(PredInit, SF, PGen, LPrune),
                        consume_answer(SF, PGen, AnsSpace, AttrVars),
                        reinstall_gen_space(SF, CallSpace),
                        consume_attr_answer(AnsSpace, AttrVars),
                        undo_attr(PredInitAttr, PredInit) )|Clauses]
        ;
            tclp_aggregates(P/A:Aggregates) ->
            Clauses0 = [( H :- 
                        make_attr(PredInit,PredInitAttr,Aggregates),
                        lookup_trie(PredInitAttr, Root, SF),
                        lookup_attr_call(Root, SF, PGen, CallSpace, LPrune),
                        execute_call(PredInit, SF, PGen, LPrune),
                        consume_answer(SF, PGen, AnsSpace, AttrVars),
                        reinstall_gen_space(SF, CallSpace),
                        consume_attr_answer(AnsSpace, AttrVars),
                        undo_attr(PredInitAttr, PredInit) )|Clauses]
        ;
            tclp_actived ->
            Clauses0 = [( H :- 
%                           tabled_call_attr(PredInit)
                        lookup_trie(PredInit, Root, SF),
                        lookup_attr_call(Root, SF, PGen, CallSpace, LPrune),
                        execute_call(PredInit, SF, PGen, LPrune),
                        consume_answer(SF, PGen, AnsSpace, AttrVars),
                        reinstall_gen_space(SF, CallSpace),
                        consume_attr_answer(AnsSpace, AttrVars)
                        )|Clauses]
        ;
            Clauses0 = [(H :- tabled_call(PredInit))|Clauses]
        )
    ).

convert_tabled_clause(Head, Body, (NewHead :- NBody)) :-
    new_trans_head(Head, NewHead), %Gets new head of the tabled clause
    conj_to_list(Body, Blist), %it tranforms the body into a list
    (
        tclp_actived ->
        convert_tabled_body_const(Blist, NBlist),
        list_to_conj(NBlist, NBody) %GConj is Guard as a set
    ;
        convert_tabled_body(Blist, NBlist),
        list_to_conj(NBlist, NBody) %GConj is Guard as a set
    ).

new_trans_head(Head, NewHead) :-
    functor(Head, P, Arity),
    name(P, Pl),
    append("$", Pl, MName),
    name(Npred, MName),
    functor(NewHead, Npred, Arity),
    put_args(NewHead, Head, Arity).

put_args(_, _, 0) :- !.
put_args(R, O, 1) :- !,
    arg(1, O, Arg),
    arg(1, R, Arg).
put_args(R, O, N) :- !,
    arg(N, O, Arg),
    arg(N, R, Arg),
    N1 is N - 1,
    put_args(R, O, N1).


%convert_tabled_body_const([], [new_answer_attr]) :- !.
convert_tabled_body_const([], [
%       retractall(counter(_)), assert(counter(0)),
    new_answer_attr
%       lookup_answer(_Node, _Attrs),
%       lookup_attr_answer(Node, Attrs, Space, _LPruneAns),
%       new_attr_answer(Node, Space, _LPruneAns)
    ]) :- !.

convert_tabled_body_const([HBody|RBody], NBody) :-
    convert_tabled_term_const(HBody, NHBody),
    convert_tabled_body_const(RBody, NRBody),
    append(NHBody, NRBody, NBody).

convert_tabled_term_const(T, NT) :-
    functor(T, NameT, ArityT),
    (
        'trans$tab'(NameT, ArityT) ->
        counter(N), N1 is N + 1, retractall(counter(_)), assert(counter(N1)), 
        get_pred_init(T, PredInit),
        (
            tclp_subsumption ->
            NT = [
                     make_attr(PredInit,PredInitAttr),
                     lookup_trie(PredInitAttr, Root, SF),
                     lookup_attr_call(Root, SF, PGen, CallSpace, LPrune),
                     execute_call(PredInit, SF, PGen, LPrune),
                     consume_answer(SF, PGen, AnsSpace, AttrVars),
                     reinstall_gen_space(SF, CallSpace),
                     consume_attr_answer(AnsSpace, AttrVars),
                     undo_attr(PredInitAttr, PredInit)]
        ;
            tclp_aggregates(NameT/ArityT:Aggregates) ->
            NT = [           
                     make_attr(PredInit,PredInitAttr,Aggregates),
                     lookup_trie(PredInitAttr, Root, SF),
                     lookup_attr_call(Root, SF, PGen, CallSpace, LPrune),
                     execute_call(PredInit, SF, PGen, LPrune),
                     consume_answer(SF, PGen, AnsSpace, AttrVars),
                     reinstall_gen_space(SF, CallSpace),
                     consume_attr_answer(AnsSpace, AttrVars),
                     undo_attr(PredInitAttr, PredInit)]
        ;
            %           const_table_mod(_M),
            % NT = [tabled_call_attr(PredInit)]
            NT = [  
                     % display(counter(N)),nl,
                     % T

%                        tabled_call_attr(PredInit)

                     lookup_trie(PredInit, Root, SF),
%                        display(a(N, PredInit)),nl,
                     lookup_attr_call(Root, SF, PGen, CallSpace, LPrune),
%                        display(b(N)),nl,
                     execute_call(PredInit, SF, PGen, LPrune),
%                        display(c(N)),nl,
                     consume_answer(SF, PGen, AnsSpace, AttrVars),
%                        display(d(N)),nl,
%                        display(s(SF,CallSpace, AnsSpace, AttrVars)),nl,
%                        ground(CallSpace),
                     reinstall_gen_space(SF, CallSpace),
%                        display(display(PGen, CallSpace)),
%                        display(e(N)),nl,
                     consume_attr_answer(AnsSpace, AttrVars)
%                        display(f(N)),nl
                     ]
        )
    ;
        NT = [T]
    ).

convert_tabled_body([], [new_answer]) :- !.

convert_tabled_body([HBody|RBody], [NHBody|NRBody]) :-
    convert_tabled_term(HBody, NHBody),
    convert_tabled_body(RBody, NRBody).

convert_tabled_term(T, NT) :-
    functor(T, NameT, ArityT),
    (
        'trans$tab'(NameT, ArityT) ->
        get_pred_init(T, AuxT),
        NT = tabled_call(AuxT)
    ;
        NT = T
    ).

get_pred_init(Call, ContPred) :-
    functor(Call, F, Arity),
    name(F,  FName),
    module_name(Module),
    append(Module,   ":$",   MAux),
    append(MAux,     FName, FullName),
    name(ContPredName, FullName),
    functor(ContPred, ContPredName, Arity),
    put_args(ContPred, Call, Arity).

%% TODO: change name/2 + append/3 by atom_concat/3
% get_pred_init(Call,ContPred) :-
%       functor(Call,F,Arity),
%       atom_concat(F,'0',F0),
%       module_name(Module),
%       module_concat(Module,F0,ContPredName),
%       functor(ContPred,ContPredName,Arity),
%       put_args(ContPred,Call,Arity).



conj_to_list(Term, List) :-
    conj_to_list_3(Term, List, []).
conj_to_list_3(Term, List0, List) :-
    ( Term = (T1, T2) ->
        conj_to_list_3(T1, List0, List1),
        conj_to_list_3(T2, List1, List)
    ; Term == true ->
        List0 = List
    ; List0 = [Term|List]
    ).

list_to_conj([],         true).
list_to_conj([Lit|List], G0) :-
    ( List == [] ->
        G0 = Lit
    ; G0 = (Lit, G),
        list_to_conj(List, G)
    ).
