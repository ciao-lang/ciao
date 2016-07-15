:- module(term_typing_rtc,[rtc_ground/1],[]).

% (rtcheck implementation for term_typing.pl)

% rtcheck version for term_typing:ground/1

rtc_ground(X) :- ground(X).
