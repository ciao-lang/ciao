:- op(1150,  fx, (fun_eval)).
:- op(1150,  fx, (fun_return)).
% Experiment (related to types) --MH
% :- op(1150, xfx, (:=)).
% Old := priority
% :- op(1130, xfx, (:=)).
% New := priority
:- op( 980, xfx, (:=)). % priority between (::) and (,) (must be the same as in statevars.pl)
:- op(  50,  fx, (~)).
:- op( 910,  fx, (^^)).
:- op(  25,  fy, (^)).
:- op(1105, xfy, ('|')).
:- op(1050, xfy, (?)).
