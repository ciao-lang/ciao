:- module(p_printer, [
    % TODO: syntax/operators may not be OK! use print_program/1
    print_assrt/2, % from deepfind_src/assrt_string.pl
    print_assrt/3, % (like print_assrt/2, write from a different context)
    print_program/1
   ], [assertions, regtypes, hiord, datafacts]).

:- use_package(library(compiler/p_unit/p_unit_argnames)).

:- use_module(library(lists), [member/2, append/3]).
:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(library(format)).
:- use_module(library(messages)).
:- use_module(library(pretty_print), [pretty_print/4]).
:- use_module(library(assertions/assrt_write)).

:- use_module(library(compiler/p_unit),
              [get_comment/1, pr_key_get/2, get_commented_assertion/2,
               get_assertion/2, get_output_operator/3]).
:- use_module(library(compiler/p_unit/program_keys), [predkey_from_sg/2]).

:- use_module(library(assertions/assrt_lib), [assertion_body/7]).
:- use_module(library(compiler/p_unit/p_unit_db)).
:- use_module(library(compiler/p_unit/unexpand), [
    transform_clause_list/3,
    transform_head/3,
    transform_body/3,
    transform_assrt_body/3]).

:- use_module(engine(runtime_control), [push_prolog_flag/2, pop_prolog_flag/1]). % TODO: find a better solution

:- doc(bug, "translate_assertions/4 throws an error when the analysis
       returns unsolved cost equations instead of cost function").

% ---------------------------------------------------------------------------

% hooks: hook_compact_global_prop/2, hook_compact_calls_prop/2
:- include(library(compiler/p_unit/p_unit_hooks)).

% ---------------------------------------------------------------------------
:- doc(section, "Print program").

:- pred print_program(S)
    # "Prints the current program information into human readable format.".

print_program(S) :-
    with_mod_syntax(print_program_(S)). % TODO: mod syntax per module too

print_program_(S) :-
    % Output comments (at the beginning)
    print_comments(S),
    % Print the directives
    ( % (failure-driven loop) % TODO: add modules to comments and directives too
      curr_file(_,M), % TODO: base mod for unexpand, allow many for multiple output
        print_directives(S, M),
        print_module(S, M),
        fail
    ; true
    ).

print_comments(S) :-
    ( % (failure-driven loop)
      get_comment(C), % TODO: per module
        write_comment(C, S),
        fail
    ; true
    ).

print_directives(S, M) :-
    ( % (failure-driven loop)
      get_printable_directive(M, Cl),
        print_directive(Cl, S),
        fail
    ; true
    ).

print_module(S, M) :-
    % print the clauses and its assertions in the proper order
    ( % (failure-driven loop)
      pr_key_get(M, Goal),
        print_from_prkey(S, M, Goal),
        fail
    ; true
    ),
    % impl_defined predicates do not have clauses, and thus are not enumerated
    % by pr_key_get/2, because it is added by p_unit:add_clause/3. Why not
    % enumerate using itf_db?
    ( % (failure-driven loop)
      current_itf(impl_defines,Goal,M),
        print_from_prkey(S, M, Goal),
        fail
    ; true
    ).

% (only for redefining/1)
% % Print directive that may have been introduced during the output
% % TODO: this is weird... doing output twice could produce different results!
% print_new_directives(Direcs, S) :-
%     % (failure-driven loop)
%     ( get_printable_directives(Direcs2),
%       member(D2, Direcs2),
%         \+ member(D2, Direcs),
%         print_directive(D2, S),
%         fail
%     ; true
%     ).

% Print assertions and clauses for the given predicate
print_from_prkey(S, M, PrKey) :-
    ( % (failure-driven loop)
      get_commented_assertion(PrKey, CA),
        print_assrt(CA, S), nl(S),
        fail
    ; true
    ),
    ( % (failure-driven loop)
      get_assertion(PrKey, AsFiltered),
        print_assrt(AsFiltered, S), nl(S),
        fail
    ; true
    ),
    ( % (failure-driven loop)
      get_clause(PrKey, C),
        print_clause(C, M, S),
        fail
    ; true
    ),
    nl(S).

% TODO: SLOW! add reverse index from H to ClIds
get_clause(H, clause(H,B):ClId*Dict) :-
    current_fact(source_clause(ClId,clause(H,B),Dict)).

% ---------------------------------------------------------------------------

get_printable_directive(CurrMod, directive(Body):ClId*Dict) :-
    curr_file(Src, CurrMod),
    current_fact(source_clause(ClId, directive(Body), Dict)),
    \+ special_directive(Body),
    ( clause_locator(ClId, loc(Src, _, _)) -> true % declared in curr_file
    ; % TODO: duplicated for each module! add it!
      ClId = '\6\newdirective' % or added by add_directive/1 % TODO: kludge, add a separate db? (not source_clause/3)
    ).

special_directive(D) :-
    functor(D, F, _),
    special_directive_(F).

% TODO: any better way?
special_directive_(comp).
special_directive_(exit). 
special_directive_(entry).
special_directive_(true).
special_directive_(false).
special_directive_(success).
special_directive_(calls).
special_directive_(check).
special_directive_(checked).
special_directive_(pred). % Assertion 
special_directive_(module).
special_directive_(texec).
special_directive_(test).

% ---------------------------------------------------------------------------
:- doc(section, "Print directives").

print_directive(directive(Body):_ClId*Dic, S) :-
    pretty_print(S, directive(Body), [], Dic),
    nl(S).

% ---------------------------------------------------------------------------
:- doc(section, "Print clauses").

print_clause(clause(H1,B1):Clid*Dic, M, S) :-
    % TODO: do as transformation instead? (it simplifies something)
    ( hook_pp_info_clause(H1, B1, Hook) ->
        dump_clause(clause(H1, B1), Clid, Hook, clause(H, B))
    ; H = H1, B = B1
    ),
    clause_remove_litkey(clause(H, B), Cl1b),
    transform_clause_list([Cl1b], M, Cls2),
    pretty_print(S, Cls2, [], [Dic]).

clause_remove_litkey(clause(A, B), clause(A, BT)) :- !,
    body_remove_litkey(B, BT).
clause_remove_litkey(_, _) :-
    throw(ciaopp_bug(clause_remove_litkey/2)).

body_remove_litkey((L, Ls), (LT, LTs)) :- !,
    body_remove_litkey(L,  LT),
    body_remove_litkey(Ls, LTs).
body_remove_litkey((L ; Ls), (LT ; LTs)) :- !,
    body_remove_litkey(L,  LT),
    body_remove_litkey(Ls, LTs).
body_remove_litkey('->'(L, Ls), '->'(LT, LTs)) :- !,
    body_remove_litkey(L,  LT),
    body_remove_litkey(Ls, LTs).
body_remove_litkey('andprolog_rt:&'(L, Ls),'andprolog_rt:&'(LT, LTs)) :- !, % TODO: can it be moved elsewhere?
    body_remove_litkey(L,  LT),
    body_remove_litkey(Ls, LTs).
body_remove_litkey(':'(L, _), L) :- !.
body_remove_litkey(L, L).

% Insert program point info between body literals
dump_clause(clause(Head,Body),Clid,Hook,clause(Head,NewBody)):- 
    Body2 = (Body,'\6\cl_end_mark'(Clid)),
    dump_body(Body2,Hook,NewBody).

% TODO: simplify
dump_body(Cl, Hook, NCl) :-
    dump_body_(Cl, Hook, NCl),
    !.
dump_body(Cl, _, Cl) :-
    error_message("Internal error: dump_body: Unable to process ~p.",[Cl]).

dump_body_(B, Hook, NNB) :-
    dump_body__(B, Hook, NB),
    remove_true_literals(NB, NNB).

dump_body__(Lit, _Hook, Lit) :-
    var(Lit), !.
dump_body__(Lit, Hook, Out) :-
    ( Lit = (A, B)
    ; Lit = (A;B), Out = (NA;NB)
    ; Lit = (A->B), Out = (NA->NB)
    ),
    !,
    dump_body__(A, Hook, NA),
    dump_body__(B, Hook, NB),
    ( Lit = (_,_) -> literal_concat(NA, NB, Out) ; true ).
dump_body__(B, Hook, NB) :-
    dump_lit(B, Hook, NB),
    !.
dump_body__(A, _Hook, A) :-
    error_message("Internal error: dump_lit: Unable to process ~p.", [A]).

% TODO: be careful when removing 'true'! (code is not always equivalent)
% Remove 'true' from the body @var{L}. The result is @var{NL}.
remove_true_literals(A, A) :-
    var(A),
    !.
remove_true_literals((B,A), B) :-
    (A == (true:_) ; A == true),
    !.
remove_true_literals((A,B), C) :-
    (A == (true:_) ; A == true),
    !,
    remove_true_literals(B, C).
remove_true_literals((A,B), C) :-
    remove_true_literals(A, AC),
    !,
    remove_true_literals(B, BC),
    literal_concat(AC, BC, C).
remove_true_literals((A->B), (AC->BC)) :-
    !,
    remove_true_literals(A, AC),
    remove_true_literals(B, BC).
remove_true_literals((A;B), (A;C)) :-
    !,
    remove_true_literals(B, C).
remove_true_literals(A, A).

%%% --- New Version
% literal_concat((A,B), C, D) :-
%       (A = true ; A = true:_),
%       !,
%       literal_concat(B, C, D).
literal_concat(Lit, C, D) :-
    Lit == (A,B),
    !,
    literal_concat(B, C, D1),
    literal_concat(A, D1, D).
% literal_concat((A,B), C, (A,D)) :-
%       !,
%       literal_concat(B, C, D).
literal_concat(Lit, C, ((A;B),C)) :-
    Lit == (A;B),
    !.
literal_concat(A, B, B) :-
    (A == true ; A == true:_),
    !.
literal_concat(A, B, A) :-
    (B == true ; B == true:_),
    !.
literal_concat(A, B, (A,B)).

% TODO: refine
dump_lit('\6\cl_end_mark'(Clid), Hook, AtInfo) :- !,
    ( hook_pp_info_lit(Clid, Hook, AtInfo, true) -> true ; AtInfo = true ).
dump_lit(At:Key, Hook, AtInfo) :-
    hook_pp_info_lit(Key, Hook, AtInfo, At:Key),
    !.
dump_lit(At, _, At).

% ---------------------------------------------------------------------------
:- doc(section, "Print assertions").

:- export(add_srcloc_prop/0).
:- data add_srcloc_prop/0.

% Print the assertion from its own (module) context
print_assrt(A, S) :-
    A = as${ module => M },
    print_assrt(A, M, S).

% Print the assertion from a (possibly different) module context
print_assrt(A, M, S) :-
    A = as${
        status => Status,
        type => Type,
        head => Head,
        compat => Compat,
        call => Call,
        succ => Succ,
        comp => Comp0,
        dic => Dic,
        % comment => UserComment,
        fromwhere => From,
        locator => loc(Src, LB, LE)
    },
    ( var(Head) ->
        throw(error(unbound_head, print_assrt/2))
    ; true
    ),
    % --- add original line numbers in comp field
    ( add_srcloc_prop, Type \= texec -> 
        SrcLoc = srcloc(Head, Src, LB, LE),
        ( LB =\= 0, LE =\= 0 -> append(Comp0, [SrcLoc], Comp) ; Comp = Comp0 ) 
    ;
        Comp = Comp0
    ),
    % --- inverse rewrite program
    assertion_body(Head, Compat, Call, Succ, Comp, [], Body),
    % TODO: printing discards the comments of all assertions
    transform_head(Head, M, HeadT),
    transform_assrt_body(Body, M, BodyT),
    compact_assrt(BodyT, BodyT2),
    ( Dic = no ->
        % create_dict((HeadT:-BodyT), _VN),
        % dict2varnamesl(_VN, VN)
        create_dict_with_assrt_and_cl(Head, VN)
    ; VN = Dic
    ),
    ( (Type = entry ; Type = prop ; Type = texec) ->
        WriteStatus = nostatus
    ; WriteStatus = status
    ),
    ( From == commented ->
        ( Type == pred ->
            write_assertion_as_double_comment(S, HeadT, Status, Type, BodyT2, VN, WriteStatus)
        ; write_assertion_as_comment(S, HeadT, Status, Type, BodyT2, VN, WriteStatus)
        )
    %; Type == test ->
    %    true
    ; write_assertion(S, HeadT, Status, Type, BodyT2, VN, WriteStatus)
    ).

:- use_module(library(assertions/assrt_lib), [assertion_body/7]).

compact_assrt(In, Out) :-
    assertion_body(Pred, Compat, Call0, Succ, Comp0, Comm, In),
    compact_call_props(Call0, Call),
    compact_comp_props(Comp0, Comp),
    assertion_body(Pred, Compat, Call, Succ, Comp, Comm, Out).

:- meta_predicate compact_props(?, pred(2), ?).
compact_props([],   _,   []) :- !.
compact_props([A0|B0], CompactProp, [A|B]) :- !,
    compact_props(A0, CompactProp, A),
    compact_props(B0, CompactProp, B).
compact_props(A, CompactProp, B) :-
    CompactProp(A, B).

compact_call_props(Call0, Call) :-
    compact_props(Call0, compact_calls_prop, Call).

compact_calls_prop(A0, A) :- hook_compact_calls_prop(A0, A), !.
compact_calls_prop(A, A).

compact_comp_props(Comp0, Comp) :-
    compact_props(Comp0, comp_remove_first_argument, Comp1),
    compact_props(Comp1, compact_global_prop, Comp).

% TODO: rename by comp_remove_goal_arg or comp_unapply? (similar to prop_unapply)
comp_remove_first_argument(M:A, M:B) :- !,
    comp_remove_first_argument(A, B).
comp_remove_first_argument(A, B) :-
    A =.. [F, _|Args],
    !,
    B =.. [F|Args].
comp_remove_first_argument(A, B) :-
    A =.. [B].

compact_global_prop(C0, C) :- hook_compact_global_prop(C0, C), !.
compact_global_prop(C, C).

% ---------------------------------------------------------------------------

:- use_module(library(vndict)).

% TODO: we still see a lot of _NNN vars; is this not working properly?
% TODO: slow? simpler alternatives? (like get the Dic of the first assertion)

:- pred create_dict_with_assrt_and_cl(Head, NDic)
    : (term(Head),var(NDic))
# "For a given head @var{Head} (from assertion or a clause), a new
  dictionary @var{NDic} is returned using the dictionaries found in
  the assertions and clauses that match the same head. @var{NDic} is
  returned in Assertions Dictionary format. You would have to use
  @pred{varnamesl2dict/2} in order to transform it to Clauses
  Dictionary. Note that @var{Head} can have any constructor on its
  arguments, but those wont be completed.".

create_dict_with_assrt_and_cl(Head, NDic) :-
    functor(Head, FF, AA),
    functor(H, FF, AA),
    findall(hd(H,D), dict_for_head(H, D), HDLIST),
    Head =.. [_|Args],
    complete_goal_dic(HDLIST, Args, [], NDic).

dict_for_head(H, D) :-
    ( assertion_read(H,_,_,_,_,D,_,_,_), D\==no
    ; % TODO: SLOW! add reverse index from Head to ClIds
      current_fact(source_clause(_ClId,clause(H,_B),VND)),
      dict2varnamesl(VND, D)
    ).

:- pred complete_goal_dic(HDLIST, GoalArgsList, InitialDic, Dics)
# "@var{HDLIST} is a list with hd(Goal,Dict) as
   elements. @var{GoalArgList} are the list of arguments of the goal
   we want to complete the dictionary. @var{InitialDic} is the initial
   dictionary we could have. If you do not have it, just use an empty
   list, []. @var{Dics} is the dictionary returned as a list of pair
   atom=variable. Example:

@begin{verbatim}
?- complete_goal_dic( [hd(ap(A1,A2,A3),
                  [=('A',A1),=('B',A2),=('C',A3)])], [E,D,F], [], DF ).

D = A2,
DF = ['C'=A3,'B'=A2,'A'=A1],
E = A1,
F = A3 ? 
@end{verbatim}
".

complete_goal_dic([hd(G,D)|Hds], GoalArgs, Dic, DicS) :-
    G =.. [_|GArgs],
    complete_goal_dic__(GArgs, D, GoalArgs, Dic, Dic1),
    complete_goal_dic(Hds, GoalArgs, Dic1, DicS).
complete_goal_dic([], _, D, D).

complete_goal_dic__([GA|GAs], D, [GoalArg|GoalArgs], Dic, Dics) :-
    % GA has some name (A), which is not already used
    var(GA),
    ( member(A=GA0, D), GA == GA0 -> true ; fail ),
    \+ member(A=_, Dic),
    % no name for GoalArg yet
    var(GoalArg),
    \+ ( member(_=GoalArg0, Dic), GoalArg0 == GoalArg ),
    %
    GA = GoalArg,
    !,
    complete_goal_dic__(GAs, D, GoalArgs, [A=GA|Dic], Dics).
complete_goal_dic__([_|GAs], D, [_|GoalArgs], Dic, Dics) :-
    complete_goal_dic__(GAs, D, GoalArgs, Dic, Dics).
complete_goal_dic__([], _, _, Dic, Dic).

% ---------------------------------------------------------------------------
:- doc(section, "Print comments").

% E.g., write_comment("line1\nline2", user_output) produces:
%   % line1
%   % line2
% Trailing newlines for each comment string are ignored.

write_comment(C, S) :-
    comment_string(C, C2),
    format(S, "~s~n", [C2]).

comment_string(Cs, "% "||Cs2) :-
    comment_string_(Cs, Cs2).

comment_string_([], []) :- !.
comment_string_([0'\n], []) :- !. % remove last nl
comment_string_([0'\n|Cs], [0'\n|Cs2]) :- !, comment_string(Cs, Cs2).
comment_string_([C|Cs], [C|Cs2]) :- !, comment_string_(Cs, Cs2).

% ---------------------------------------------------------------------------
:- doc(section, "Call with an specific operator table").
% TODO: separate in itw own module

:- use_module(library(port_reify), [once_port_reify/2, port_call/1]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(compiler/c_itf), [define_ops/0]).
:- use_module(library(operators)).

:- export(with_mod_syntax/1).
:- meta_predicate with_mod_syntax(goal).
:- pred with_mod_syntax(G) # "Call @var{G} with the syntax definitions
   (operators) given by @pred{get_output_operator/3}".

with_mod_syntax(G) :-
    set_mod_operators(Prev),
    push_prolog_flag(write_strings, on), % TODO: make it optional?
    once_port_reify(call(G), Res),
    ( pop_prolog_flag(write_strings) -> true ; true ),
    undo_mod_operators(Prev),
    port_call(Res).

:- pred set_mod_operators(Prev) # "Set operators from
   @pred{get_output_operator/3}, saving the previous setting in @var{Prev}.".

set_mod_operators(Prev) :-
    findall(o(A,B,C), current_op(A, B, C), Prev),
    reset_ops,
    ( get_output_operator(A, B, C),
        op(A, B, C),
        fail
    ; true
    ),
    % TODO: do before or after get_output_operator/3 is used?
    standard_ops,
    define_ops.

:- pred undo_mod_operators(Prev) # "Restore previous operators from @var{Prev}.".
undo_mod_operators(Prev) :-
    reset_ops,
    ( member(o(A,B,C), Prev),
        op(A, B, C),
        fail
    ; true
    ).

reset_ops :-
    ( current_op(_, B, C),
        C \= ',', % (cannot redefine ','/2 in ISO)
        op(0, B, C),
        fail
    ; true
    ).

