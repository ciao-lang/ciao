:- module(tabling_rt,
        [
            print_counters/0,
            tabling_stats/0,
            abolish_all_tables/0,
            tabled_call/1,
            tabled_call_attr/1,
            new_answer/0,
            new_answer_attr/0,
            lookup_trie/3,
            execute_call/4,
            consume_answer/4,
%%              lookup_answer/2,
%%              new_attr_answer/3,
            lookup_attr_call/5, % from diff_tab
%%              lookup_attr_answer/4,
            reinstall_gen_space/2,
            consume_attr_answer/2,
            '$call_domain_projection'/2, % interface c<>pl
            '$answer_domain_projection'/2,
            '$call_store_projection'/3,
            '$answer_store_projection'/3,
            '$call_entail'/4,       
            '$answer_check_entail'/6,       
            '$apply_answer'/3,
            '$current_store'/1,
            '$reinstall_store'/3,
            set_tabling_flag/2,      % debug:    set_tabling_flag_c
            current_tabling_flag/2  % debug:    current_tabling_flag_c
        ],
        [assertions, hiord, regtypes, foreign_interface]).

:- use_module(library(write)).
:- use_module(library(lists)).
:- use_module(engine(basic_props)).

:- use_module(engine(hiord_rt),
        [
            '$meta_call'/1
        ]).

:- use_module(library(tabling/forward_trail)).

:- multifile
    'tabling_rt:call_domain_projection'/2,
    'tabling_rt:answer_domain_projection'/2,
    'tabling_rt:current_store'/1,
    'tabling_rt:call_store_projection'/3,
    'tabling_rt:answer_store_projection'/3,
    'tabling_rt:call_entail'/4,
    'tabling_rt:answer_check_entail'/5,
    'tabling_rt:reinstall_store'/3,
    'tabling_rt:apply_answer'/3.

'$call_domain_projection'(Vars, Dom) :- 
    'tabling_rt:call_domain_projection'(Vars, Dom).

'$call_store_projection'(Vars, DomA, ProjA) :-
    'tabling_rt:call_store_projection'(Vars, DomA, ProjA).

'$call_entail'(Vars, DomA, DomB, ProjB) :- 
    'tabling_rt:call_entail'(Vars, DomA, DomB, ProjB).

'$answer_domain_projection'(Vars, Dom) :- 
    'tabling_rt:answer_domain_projection'(Vars, Dom).

'$answer_store_projection'(Vars, DomA, ProjA) :- 
    'tabling_rt:answer_store_projection'(Vars, DomA, ProjA).

'$answer_check_entail'(Vars, DomA, DomB, ProjB, IntResult, NewDom) :- 
    'tabling_rt:answer_check_entail'(Vars, DomA, DomB, ProjB, Result),
    (
        Result = agg(NewDom) ->
        IntResult = 2
    ;
        IntResult = Result
    ).

'$apply_answer'(Vars, DomA, ProjA) :- 
    'tabling_rt:apply_answer'(Vars, DomA, ProjA).

'$current_store'(Orig) :-
    'tabling_rt:current_store'(Orig).

'$reinstall_store'(Vars, DomOrig, Orig) :- 
    'tabling_rt:reinstall_store'(Vars, DomOrig, Orig).


:- trust pred abolish_all_tables + foreign_low(abolish_all_tables_c) #
"Removes all tables currently in the system and frees all the memory
held by Ciao for these structures. Predicates that have been declared
as tabled remain so, but any information in their tables is
deleted. @pred{abolish_all_tables/0} works directly on the memory
structures allocated for table space. This makes it very fast for
abolishing a large volume of tables.".


%% tabled_call/1 and new_answer/0 standard predicate (without attributes)
:- trust pred tabled_call(+Call) :: cgoal +
    foreign_low(tabled_call_c) # "@pred{tabled_call/1} instruments calls
to the tabled predicate @var{Call}. It checks if the call is a
generator or a consumer using a trie structure.".

:- trust pred new_answer + foreign_low(new_answer_c) #
"@pred{new\_answer/0} adds an answer to the current generator and
 then fails.".


%% with attributes
tabled_call_attr(PredInit) :-
    lookup_trie(PredInit, Root, SF),
    lookup_attr_call(Root, SF, PGen, CallSpace, LPrune),
    execute_call(PredInit, SF, PGen, LPrune),
    consume_answer(SF, PGen, AnsSpace, AttrVars),
    reinstall_gen_space(SF, CallSpace),
    consume_attr_answer(AnsSpace, AttrVars).


:- trust pred lookup_trie(+Call, -SF, -Root) :: cgoal * int * int +
    foreign_low(lookup_trie_c) # "@pred{look_up_trie/3} instruments calls
to the tabled predicate @var{Call}. It checks if the call is a
generator or a consumer using a trie structure. @var{SF} is unified
with the substitution factor of @var{Call} and @var{Root} is unified
with the trie node of @var{Call}.".

:- trust pred execute_call(+Call, +SF, +Node, -LNodePrune) :: cgoal *
    int * int * list + foreign_low(execute_call_c) #
"@pred{execute_call/4} executes a tabled call and prunes
@var{LNodePrune} generators.  @var{SF} is the substitution factor of
@var{Call} and @var{Root} is the trie node of @var{Call}.".

:- trust pred consume_answer(+SF, +Node, -Space, -AttrVars) :: int * int *
    int * int + foreign_low(consume_answer_c) # "@pred{consume\_answer/4}
consumes answers from the table. @var{SF} is the substitution factor
of the tabled call, @var{Node} is the tabled call frame. @var{Space}
is a pointer to the answer constraint store and @var{AttrVars} are the
attributed variables of the tabled goal.".

:- trust pred new_answer_attr + foreign_low(new_answer_attr_c) # 
"@pred{new\_answer\_attr/0} looks for a 
previous answer which is identical up to variable renaming. ".

% :- trust pred lookup_answer(-Root, -Attrs) :: int * int +
%       foreign_low(lookup_answer_c) # "@pred{lookup\_answer/2} looks for a 
% previous answer which is identical up to variable renaming. @var{Root}
% is unified with the trie node corresponding with this answer and
% @var{Attrs} is unified with its attributed variables.".

% :- trust pred new_attr_answer(+Ans, +Space, +LPruneAns) :: int * int *
%       list + foreign_low(new_attr_answer_c) # "@pred{new\_attr\_answer/3}
% adds the answer @var{Ans} and its constraint store @var{Space} to the
% current generator and then fails. It also prunes the consumption of
% answers in @var{LPrune}.".



:- doc(initial_tabling/0, "Initializes the tabling module at the
beginning.").

:- trust pred initial_tabling + foreign_low(initial_tabling_c).

:- trust pred '$gen_tree_backtracking' +
    foreign_low(gen_tree_backtracking_c) #
"@pred{'$gen\_tree\_backtracking'/0} is executed before
    backtracking over a generator tree.".

:- trust pred '$pop_ptcp' + foreign_low(pop_ptcp_c) #
    "@pred{'$pop\_ptcp'/0} pops an element from ptcp stack.".

:- trust pred '$push_ptcp'(+PTCP) :: int + foreign_low(push_ptcp_c) #
    "@pred{'$push_ptcp'/1} push a generator pointer on the ptcp stack.".




:- trust pred lookup_attr_call(+Root, +SF, -Node, -CallSpace, -LNodePrune) ::
    int * int * int * int *int + foreign_low(lookup_attr_call_c)
# "Looks up a constrain tabled call.".

% :- trust pred lookup_attr_answer(+Root, +Attrs, -Space, -LPruneAns) ::
%       int * int * int * int + foreign_low(lookup_attr_answer_c)
% # "Looks up a constrain answer.".

:- trust pred reinstall_gen_space(+SF, +CallSpace) ::
    int * int + foreign_low(reinstall_gen_space_c)
# "Looks up a constrain tabled call.".

:- trust pred consume_attr_answer(+AnsSpace, +AttrsVars) ::
    int * int + foreign_low(consume_attr_answer_c)
# "Looks up a constrain tabled call.".



:- trust pred set_tabling_flag(-Flag,-Mode) :: 
    atm * atm + foreign_low(set_tabling_flag_c).
:- trust pred current_tabling_flag(-Flag,+Mode) :: 
    atm * atm + foreign_low(current_tabling_flag_c).

%% control statistics tabling
:- trust pred tabling_stats + foreign_low(tabling_stats_c).

%% control the answer strategy benchmark counters
%% :- extra_compiler_opts(['-DANS_COUNTER']).
%%:- trust pred print_counters + foreign_low(print_counters_c).
print_counters.

% :- use_module(library(messages)).
% suspend_gc :-
%       current_prolog_flag(gc, on),
%       set_prolog_flag(gc, off).

% :- initialization(suspend_gc).
:- initialization(initial_tabling).

%% :- extra_compiler_opts(['-DSWAPPING']).
%% :- extra_compiler_opts(['-DDEBUG_ALL -DSWAPPING']).
%% :- extra_compiler_opts(['-DDEBUG_ALL']).
%% :- extra_compiler_opts(['-DDEBUG_EXEC']).
%% :- extra_compiler_opts(['-DTRACE_REG']).

 :- extra_compiler_opts(['-g']).

:- use_foreign_source([ 'chat_tabling.c', 'debug.c']).
