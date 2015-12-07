%% ---------------------------------------------------------------------------
%% This file is part of the clpfd package for Ciao
%%
%% Copyright (C) 2006-2012 CLIP Group
%%
%% Originally written by:
%%   * Emilio Jesús Gallego Arias
%%
%% Modified by:
%%   * Rémy Haemmerlé
%%   * Jose F. Morales
%%
%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version 2
%% of the License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
%% ---------------------------------------------------------------------------

:- module(clpfd_tr, [trans_fd/2], [assertions]).

:- doc(title, "Translation of FD constraints").

:- include(.(clpfd_ops)).

%% Fixme, define allowable syntax.
% clpfd_expr(Var).

%% Translation of FD constraints:
%%
%% For a constraint E1 RelOp E2:
%%
%% * E1 and E2 are compiled into atomic expressions whose value is
%%   stored in V1 and V2. 
%% * The constraint V1 RelOp V2 is generated.
%% * See the special case of Var #= Expr in the code.
%%
%% There are specific cases for when some expression is a constant.
%%
%% Compiling an expression:
%%
%% * An expression of the form SubExp1 Op SubExp2 is compiled into:
%%   (a) Code C1 and C2 which calculates the value of SubExp1 and
%%       SubExp2 and leaves it into V1 and V2
%%   (b) Code which puts together the values of the subexpressions: 
%%       C #= V1 Op V2
%%
%% * The resulting code is thus (C1, C2, C #= V1 Op V2)


%% I am generating the basic calculating the expressions before the
%% constraint to be compiled in the hope that the expression
%% calculation can fail before being completed.  That may not be the
%% case in the applications which set up constraints which are not
%% supposed to fail, and then labeling is called.  But it may help if
%% the application is generating constraints and instantiating them on
%% the fly.

%% Note that constants are detected at *compile time*.  Groundness
%% analysis could detect which variables are will be bound to a
%% constant at run time and compile the constraints accordingly -- I
%% am not sure how much optimization we can expect from that.
%%
%% So, from a high-level point of view, what I would like to write
%% here is, instead of "number(T)" is "will_be_number_at_runtime(T)",
%% for which "number(T)" is a safe approximation.

%% Less than, one of them is a constant
trans_fd(T #< B, (CodeB, fd_constraints:'t<b'(T, ResB))) :-
        number(T), !,
	compile_fd_expr(B, CodeB, ResB).
trans_fd(A #< T, (CodeA, fd_constraints:'a<t'(ResA, T))) :-
        number(T), !,
        compile_fd_expr(A, CodeA, ResA).
%% General case
trans_fd(A #< B, (CodeA, CodeB, fd_constraints:'a<b'(ResA, ResB))) :-
	compile_fd_expr(A, CodeA, ResA),
	compile_fd_expr(B, CodeB, ResB).

%% Greater than in terms of less than
trans_fd(A #> B, R):- trans_fd(B #< A, R).

%% Less or equal: similar to the case above
trans_fd(A #=< T, (CodeA, fd_constraints:'a=<t'(ResA,T))) :-
        number(T), !,
	compile_fd_expr(A, CodeA, ResA).
trans_fd(T #=< B, (CodeB, fd_constraints:'t=<b'(T,ResB))) :-
        number(T), !,
	compile_fd_expr(B, CodeB, ResB).
trans_fd(A #=< B, (CodeA, CodeB, fd_constraints:'a=<b'(ResA,ResB))) :-
	compile_fd_expr(A, CodeA, ResA),
	compile_fd_expr(B, CodeB, ResB).

trans_fd(A #>= B, R) :- trans_fd(B #=< A, R).

%% Equality.  If one side of the equality is a variable, use the
%% translation of expressions for non-variable side and force the
%% variable where the expression result is stored to be the variable
%% in the equaliry constraint.
%trans_fd(X #= Y, Code) :-
%        isolate_var(X, Y, ABT, C), !,
%	compile_fd_expr(ABT, Code, C).
trans_fd(A #= B, (CodeA, CodeB, fd_constraints:'a=b'(ResA,ResB))) :-
	compile_fd_expr(A, CodeA, ResA),
	compile_fd_expr(B, CodeB, ResB).

%% Disequality.  Several specialized disequality indexicals are
%% defined in the indexicals library.  I am not sure they are useful
%% in general, but I am translating them anyway.

trans_fd(A #\= B, Code) :-
	nonvar(B), !,
	(
	    number(B) ->
	    compile_fd_expr(A, CodeA, ResA),
	    Code = (CodeA, fd_constraints:'a<>t'(ResA, B))
	;
	    B = (X + Y) ->
	    (
		detect_number(X, Y, C, T) ->   % Take care of B + T and T + B
		compile_fd_expr(A, CodeA, ResA),
		compile_fd_expr(C, CodeC, ResC),
		Code = (CodeA, CodeC, fd_constraints:'a<>b+t'(ResA,ResC,T))
	    ;
		compile_fd_expr(A, CodeA, ResA),
		compile_fd_expr(X, CodeX, ResX),
		compile_fd_expr(Y, CodeY, ResY),
		Code = (CodeA, CodeX, CodeY, fd_constraints:'a<>b+c'(ResA,ResX,ResY))
	    )
	;
	    B = (X - Y) ->
	    (
		detect_number(X, Y, C, T) ->   % Take care of B + T and T + B
		compile_fd_expr(A, CodeA, ResA),
		compile_fd_expr(C, CodeC, ResC), 
		Code = (CodeA, CodeC, 'a<>b-t'(ResA,ResC,T))
	    ;
		compile_fd_expr(A, CodeA, ResA),
		compile_fd_expr(X, CodeX, ResX),
		compile_fd_expr(Y, CodeY, ResY),
		Code = (CodeA, CodeX, CodeY, fd_constraints:'a<>b-c'(ResA,ResX,ResY))
	    )
	).
trans_fd(A #\= B, Res) :-
	nonvar(A), !, trans_fd(B #\= A, Res).
	
% General disequality
trans_fd(A #\= B, (CodeA, CodeB, fd_constraints:'a<>b'(ResA,ResB))) :-!,
	compile_fd_expr(A, CodeA, ResA),
	compile_fd_expr(B, CodeB, ResB).

%% Utility predicates: identify numbers and variables in order to
%% select the most appropriate indexical.
detect_number(A, B, A, B):- number(B), !.
detect_number(A, B, B, A):- number(A), !.

% isolate_var(A, B, A, B):- var(B), !.
% isolate_var(A, B, B, A):- var(A), !.


%% Compilation of expressions: return the code and the variable which
%% will hold the result.

%% Just a variable
compile_fd_expr(A, clpfd_rt:wrapper(A, X), X) :- var(A), !.

%% Just an integer.
compile_fd_expr(A, true, A) :- integer(A),	!.

%% Addition with a constant or with a general expression.
compile_fd_expr(X+Y, (CodeA, fd_term:new(ResC), fd_constraints:'a+t=c'(ResA, T, ResC)), ResC) :-
        detect_number(X, Y, A, T), !,
	compile_fd_expr(A, CodeA, ResA).
compile_fd_expr(A+B, (CodeA, CodeB, fd_term:new(Res), fd_constraints:'a+b=c'(ResA, ResB, Res)), Res) :- !,
	compile_fd_expr(A, CodeA, ResA),
	compile_fd_expr(B, CodeB, ResB).

%% Subtraction: constant and general expression
compile_fd_expr(X-Y, (CodeA, fd_term:new(Res), fd_constraints:'a-t=c'(ResA, T, Res)), Res) :-
        detect_number(X, Y, A, T), !,
	compile_fd_expr(A, CodeA, ResA).
compile_fd_expr(A-B, (CodeA, CodeB, fd_term:new(Res), fd_constraints:'a-b=c'(ResA, ResB, Res)), Res) :- !,
	compile_fd_expr(A, CodeA, ResA),
	compile_fd_expr(B, CodeB, ResB).

%% Multiplication by a number and general case
compile_fd_expr(X*Y, (CodeB, fd_term:new(Res), fd_constraints:'a=b*t'(Res, VarB, T)), Res) :- 
        detect_number(X, Y, B, T), !,
	compile_fd_expr(B, CodeB, VarB).
compile_fd_expr(B*C, (CodeB, CodeC, fd_term:new(Res), fd_constraints:'a=b*c'(Res, VarB, VarC)), Res) :- !,
	compile_fd_expr(B, CodeB, VarB),
	compile_fd_expr(C, CodeC, VarC).


