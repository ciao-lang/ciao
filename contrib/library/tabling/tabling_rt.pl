:- module(tabling_rt,
        [
            abolish_all_tables/0,      
	    tabled_call/1,
	    lookup_trie/3,
	    execute_call/4,
	    consume_answer/4,
	    new_answer/0,
	    lookup_answer/2,
	    new_attr_answer/3
	], 
	[assertions,hiord,regtypes,foreign_interface]).

:- use_module(engine(hiord_rt), 
 	[
 	    '$meta_call'/1
 	]).

:- use_module(library(tabling/forward_trail)).

:- true pred abolish_all_tables + foreign_low(abolish_all_tables_c) #
"Removes all tables currently in the system and frees all the memory
held by Ciao for these structures. Predicates that have been declared
as tabled remain so, but any information in their tables is
deleted. @pred{abolish_all_tables/0} works directly on the memory
structures allocated for table space. This makes it very fast for
abolishing a large volume of tables.".

:- true pred tabled_call(+Call) :: callable +
foreign_low(tabled_call_c) # "@pred{tabled_call/1} instruments calls
to the tabled predicate @var{Call}. It checks if the call is a
generator or a consumer using a trie structure.".

:- true pred lookup_trie(+Call,-SF,-Root) :: callable * int * int +
foreign_low(lookup_trie_c) # "@pred{look_up_trie/3} instruments calls
to the tabled predicate @var{Call}. It checks if the call is a
generator or a consumer using a trie structure. @var{SF} is unified
with the substitution factor of @var{Call} and @var{Root} is unified
with the trie node of @var{Call}.".

:- true pred execute_call(+Call,+SF,+Node,-LNodePrune) :: callable *
int * int * list + foreign_low(execute_call_c) #
"@pred{execute_call/4} executes a tabled call and prunes
@var{LNodePrune} generators.  @var{SF} is the substitution factor of
@var{Call} and @var{Root} is the trie node of @var{Call}.".

:- true pred consume_answer(+SF,+Node,-Space,-AttrVars) :: int * int *
int * int + foreign_low(consume_answer_c) # "@pred{consume\_answer/4}
consumes answers from the table. @var{SF} is the substitution factor
of the tabled call, @var{Node} is the tabled call frame. @var{Space}
is a pointer to the answer constraint store and @var{AttrVars} are the
attributed variables of the tabled goal.".

:- true pred new_answer + foreign_low(new_answer_c) #
 "@pred{new\_answer/0} adds an answer to the current generator and
 then fails.".

:- true pred lookup_answer(-Root,-Attrs) :: int * int +
foreign_low(lookup_answer_c) # "@pred{lookup\_answer/2} looks for a 
previous answer which is identical up to variable renaming. @var{Root}
is unified with the trie node corresponding with this answer and
@var{Attrs} is unified with its attributed variables.".

:- true pred new_attr_answer(+Ans,+Space,+LPruneAns) :: int * int *
list + foreign_low(new_attr_answer_c) # "@pred{new\_attr\_answer/3}
adds the answer @var{Ans} and its constraint store @var{Space} to the
current generator and then fails. It also prunes the consumption of
answers in @var{LPrune}.".

:- doc(initial_tabling/0,"Initializes the tabling module at the
beginning.").

:- true pred initial_tabling + foreign_low(initial_tabling_c).

:- true pred '$gen_tree_backtracking' +
	foreign_low(gen_tree_backtracking_c) #
	"@pred{'$gen\_tree\_backtracking'/0} is executed before
	backtracking over a generator tree.".

:- true pred '$pop_ptcp' + foreign_low(pop_ptcp_c) #
 "@pred{'$pop\_ptcp'/0} pops an element from ptcp stack.".

:- true pred '$push_ptcp'(+PTCP) :: int + foreign_low(push_ptcp_c) #
"@pred{'$push_ptcp'/1} push a generator pointer on the ptcp stack.".

:- initialization(initial_tabling).

 %% :- extra_compiler_opts(['-DSWAPPING']).
 %% :- extra_compiler_opts(['-DDEBUG_ALL -DSWAPPING']).
 %% :- extra_compiler_opts(['-DDEBUG_ALL']).

:- use_foreign_source(['chat_tabling.c']).
