:- module(and_sim_plan_rt,
        [
            print_info/0,
            precedences_info/1,
	    free_info/0,
	    sim_seq_memo/0,
	    sim_par_memo/0,
	    sim_par/0,
	    sim_seq/0,
	    sim_pred_s_amadeo/0,
	    sim_par_back/0,
	    insert_info/1
	], 
	[assertions,hiord,regtypes,foreign_interface]).

:- true pred print_info + foreign_low(print_info_c) # "Prints all the
        traces.".

:- true pred free_info + foreign_low(free_info_c) # "Frees all the
        traces.".

:- true pred sim_seq + foreign_low(sim_seq_c) # "Simulates sequential
        memoization.".

:- true pred sim_seq_memo + foreign_low(sim_seq_memo_c) # "Simulates
        sequential memoization.".

:- true pred sim_par_memo + foreign_low(sim_par_memo_c) # "Simulates
        sequential memoization.".

:- true pred sim_par + foreign_low(sim_par_c) # "Simulates sequential
        memoization.".

:- true pred sim_pred_s_amadeo + foreign_low(sim_pred_s_amadeo_c) #
        "Simulates sequential memoization.".

:- true pred sim_par_back + foreign_low(sim_par_back_c) #
        "Simulates sequential memoization.".

:- true pred insert_info(+Call) :: aux_par +
	foreign_low(insert_info_c) # "Inserts traces into C lenguaje
	format.".

:- true pred precedences_info(+Call) :: aux_par +
	foreign_low(precedences_info_c) # "Inserts precedences into C
	lenguaje format and NThreads.".

:- doc(initial/0,"Initializes the execution.").

:- true pred initial 
        + foreign_low(initial_c).

:- use_foreign_source(code).

:- initialization(initial).


:- regtype aux_par(T) # "@var{T} is to be defined.".
aux_par(_).

