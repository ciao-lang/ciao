:- module(all_test, [t0/0, t1/0], [assertions]).

:- use_module(.(bignums_test)).
:- use_module(.(color_map_test)).
:- use_module(.(flat_test)).
:- use_module(.(guardians_test)).
:- use_module(.(hanoi_test)).
:- use_module(.(jugs_test)).
:- use_module(.(knights_test)).
:- use_module(.(module_test)).
:- use_module(.(mqu_test)).
:- use_module(.(qsort_test)).
%:- use_module(.(schedule_test)).
:- use_module(.(size_test)).
:- use_module(.(subst_exp_test)).
:- use_module(.(sudoku_test)).
:- use_module(.(zebra_argnames_test)).
%:- use_module(.(prof_lpdoc)).
:- use_module(.(wumpus_test)).

t0 :-
	(\+ bignums_test:t0 ->        true ; true),
	(\+ color_map_test:t0 ->      true ; true),
	(\+ flat_test:t0 ->           true ; true),
	(\+ guardians_test:t0 ->      true ; true),
	(\+ hanoi_test:t0 ->          true ; true),
	(\+ jugs_test:t0 ->           true ; true),
	(\+ knights_test:t0 ->        true ; true),
	(\+ module_test:t0 ->         true ; true),
	(\+ mqu_test:t0 ->            true ; true),
	(\+ qsort_test:t0 ->          true ; true),
% 	(\+ schedule_test:t0 ->       true ; true),
	(\+ size_test:t0 ->           true ; true),
	(\+ subst_exp_test:t0 ->      true ; true),
	(\+ sudoku_test:t0 ->         true ; true),
	(\+ zebra_argnames_test:t0 -> true ; true),
% 	(\+ prof_lpdoc:t0 ->          true ; true),
	(\+ wumpus_test:t0 ->         true ; true).


t1 :-
	(\+ bignums_test:t1 ->        true ; true),
	(\+ color_map_test:t1 ->      true ; true),
	(\+ flat_test:t1 ->           true ; true),
	(\+ guardians_test:t1 ->      true ; true),
	(\+ hanoi_test:t1 ->          true ; true),
	(\+ jugs_test:t1 ->           true ; true),
	(\+ knights_test:t1 ->        true ; true),
	(\+ module_test:t1 ->         true ; true),
	(\+ mqu_test:t1 ->            true ; true),
	(\+ qsort_test:t1 ->          true ; true),
% 	(\+ schedule_test:t1 ->       true ; true),
	(\+ size_test:t1 ->           true ; true),
	(\+ subst_exp_test:t1 ->      true ; true),
	(\+ sudoku_test:t1 ->         true ; true),
	(\+ zebra_argnames_test:t1 -> true ; true),
% 	(\+ prof_lpdoc:t1 ->          true ; true),
	(\+ wumpus_test:t1 ->         true ; true).
