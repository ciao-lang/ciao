:- module(mio,_,[expander]).

:- use_module(engine(io_basic)).
:- use_module(mio, []).

main('mio.pl').
adf(X):- 
    ( display(X) ->
      X==a
    ;
      X==b
    ).

