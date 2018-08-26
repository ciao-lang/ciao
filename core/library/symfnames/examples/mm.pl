
:- module(mm,[p/1]).

:- use_module(engine(stream_basic)).
:- use_module(library(symfnames)).

p(X):- symfnames:open(file,read,S), read(S,X), close(S).
