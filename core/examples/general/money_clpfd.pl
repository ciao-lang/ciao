:- module(money_clpfd, [money/1], [clpfd]).

% SEND + MORE = MONEY using CLP(fd)
% ?- money(Dom).

money(Dom) :-
    Dom = [S,E,N,D,M,O,R,Y],
    domain(Dom, 0, 9),
    0 #< S, 0 #< M,
    all_different(Dom),
                S*1000 + E*100 + N*10 + D
              + M*1000 + O*100 + R*10 + E
                                       #=
      M*10000 + O*1000 + N*100 + E*10 + Y,
    labeling([], Dom).

