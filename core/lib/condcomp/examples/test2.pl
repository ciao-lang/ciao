:- module(_, [main/0], [condcomp]).

% Try to comment or uncomment the following lines:
:- compilation_fact(use_write(yes)).
%:- compilation_fact(use_write(no)).
:- compilation_fact(use_quote(yes)).
%:- compilation_fact(use_quote(no)).

:- if(use_write(yes)).
    :- use_module(library(write)).
    :- if(use_quote(yes)).
        pr(X) :- writeq(using_write(X)), nl.
    :- else.
        pr(X) :- write(using_write(X)), nl.
    :- endif.
:- else.
    :- if(use_quote(yes)).
        pr(X) :- displayq(using_display(X)), nl.
    :- else.
        pr(X) :- display(using_display(X)), nl.
    :- endif.
:- endif.

main :-
	pr('hello world').
