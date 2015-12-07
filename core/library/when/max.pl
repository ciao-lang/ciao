:- module(max, [max/3], []).

:- use_module(library(when)).

%% Two variables are ground

gmax(X, Y, Z):- 
        ground(X) ->            %% X is a number
        (
            ground(Y) ->        %% Then either Y or Z are ground
            gmax_1_2(X, Y, Z)
        ;
            gmax_1_3(X, Y, Z)
        )
 ;                              %% Y and Z must be ground (and numbers)
        gmax_1_3(Y, X, Z).      %% Note the argument swapping


gmax_1_2(X, Y, X):- X >= Y, !.         %% X is the greatest.
gmax_1_2(_, Y, Y).                     %% Y is the greatest.

gmax_1_3(X, Y, Z):- X < Z, Y = Z, !.   %% Y is the greatest
gmax_1_3(X, _Y, Z):- X > Z, !, fail.   %% Not possible: maximum is smaller!
gmax_1_3(X, Y, Z):-                    %% X = Z: we must wait until we know Y
        when(ground(Y), gmax_1_2(X, Y, Z)).


%% Max waits until the second and third variables are ground.
        
max(X, Y, Z):-
        when(
                (
                    (ground(X),ground(Y)) ;
                    (ground(X),ground(Z)) ;
                    (ground(Z),ground(Y))
                ),
                gmax(X, Y, Z)).
