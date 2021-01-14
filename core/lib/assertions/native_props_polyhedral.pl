% (included file)
:- doc(section, "Polyhedral constraints").

:- if(defined(optim_comp)).
:- else.
:- export(constraint/1).
:- doc(constraint(C), "@var{C} contains a list of linear
   (in)equalities that relate variables and @tt{int} values. For
   example, @tt{[A < B + 4]} is a constraint while @tt{[A < BC + 4]}
   or @tt{[A = 3.4, B >= C]} are not. Used by polyhedra-based
   analyses.").

:- prop constraint(C) + native
   # "@var{C} is a list of linear equations.".

% TODO: should we define this here? (term structure of a valid constraint/1)
constraint([]).
constraint([Cons|Rest]) :-
    constraint_(Cons),
    constraint(Rest).

constraint_(=(Lin_Expr1, Lin_Expr2)) :-
    lin_expr(Lin_Expr1),
    lin_expr(Lin_Expr2).
constraint_(=<(Lin_Expr1, Lin_Expr2)) :-
    lin_expr(Lin_Expr1),
    lin_expr(Lin_Expr2).
constraint_(>=(Lin_Expr1, Lin_Expr2)) :-
    lin_expr(Lin_Expr1),
    lin_expr(Lin_Expr2).
constraint_(<(Lin_Expr1, Lin_Expr2)) :-
    lin_expr(Lin_Expr1),
    lin_expr(Lin_Expr2).
constraint_(>(Lin_Expr1, Lin_Expr2)) :-
    lin_expr(Lin_Expr1),
    lin_expr(Lin_Expr2).

lin_expr(PPL_Var) :-
    ppl_var(PPL_Var), !.
lin_expr(Coeff) :-
    coefficient(Coeff).
% lin_expr(+(Lin_Expr), Vars, +(New_Lin_Expr)) :-
%       lin_expr(Lin_Expr, Vars, New_Lin_Expr).
lin_expr(+(Lin_Expr)) :-
    lin_expr(Lin_Expr).
lin_expr(-(Lin_Expr)) :-
    lin_expr(Lin_Expr).
lin_expr(+(Lin_Expr1, Lin_Expr2)) :-
    lin_expr(Lin_Expr1),
    lin_expr(Lin_Expr2).
lin_expr(-(Lin_Expr1, Lin_Expr2)) :-
    lin_expr(Lin_Expr1),
    lin_expr(Lin_Expr2).
lin_expr(*(Coeff, Lin_Expr)) :-
    coefficient(Coeff),
    lin_expr(Lin_Expr).
lin_expr(*(Lin_Expr, Coeff)) :-
    coefficient(Coeff),
    lin_expr(Lin_Expr).

ppl_var(Var) :-
    var(Var).

coefficient(Coeff) :-
    ground(Coeff),
    int(Coeff). % TODO: couldn't it be a num/1?
:- endif.
