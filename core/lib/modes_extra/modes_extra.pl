:- package(modes_extra).
:- use_package(modes).
:- use_package(assertions).
% add here your new mode definitions

:- op(500,  fx,(---)).

%% "ISO-like" modes
:- modedef  --(A) :  var(A) => nonvar(A). 
:- modedef ---(A) :  var(A).

%% Meta-variables
:- modedef in_var(A)  : var(A)  => var(A). % Used in CiaoPP code to declare variables that are unbounded in both input and output