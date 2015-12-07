:- module(simple_db,_,[argnames,assertions,regtypes]).
:- use_module(library(aggregates)).

:- doc(title,"A simple database application using argument names").

:- data
product( id,    description,    brand,          quantity        ).
%       ----------------------------------------------------------
product(  1,    "Keyboard",     "Logitech",     6               ).
product(  2,    "Mouse",        "Logitech",     5               ).
product(  3,    "Monitor",      "Philips",      3               ).
product(  4,    "Laptop",       "Dell",         4               ).
% (${/} must go after argnames)
:- pred product${/}
   ::    int    * string        * string        * int.

% Compute the stock of products from a given brand.
% Note call to findall is equivalent to: findall(Q,product(_,_,Brand,Q),L).

brand_stock(Brand,Stock) :-
        findall(Q,product${brand=>Brand,quantity=>Q},L),
        sumlist(L,Stock).

sumlist([],0).
sumlist([X|T],S) :- 
        sumlist(T,S1),
        S is X + S1.

