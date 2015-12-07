:- module(gen_list,
        [
            gen_list/2
	],
	[]).

gen_list(0, []).
gen_list(M, [V|Ns]):- 
        M > 0,
        M1 is M - 1,
        V is M*M*M mod 7919,
        gen_list(M1, Ns).
