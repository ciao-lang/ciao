:- module(_, [functor1/2, true/1, true/2, menu_branch/3, menu_branch/4], []).

functor1(A , F) :- functor(A , F , _).

true(_).
true(A , A).

menu_branch(A , M , [ask_menu(M)|A]).
menu_branch(A , M , L , [ask_menu(M, L)|A]).
