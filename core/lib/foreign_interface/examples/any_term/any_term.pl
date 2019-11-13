:- module(any_term,
    [custom_display_term/1,
     custom_create_term/2
    ],
    [foreign_interface]).

:- trust pred custom_display_term(in(X)) :: any_term + foreign.
:- trust pred custom_create_term(in(L), go(X)) :: c_int * any_term + (foreign,returns(X)).

:- use_foreign_source(any_term_c).
:- extra_compiler_opts(['-O2']).
