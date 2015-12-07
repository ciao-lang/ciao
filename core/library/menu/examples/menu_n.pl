:- module(_, [menu_demo/0, test_menu/1], [menu]).

% Entry point.
menu_demo :-    menu(select).
% For testing: e.g., test_menu( br ) or test_menu( br(1) ).
test_menu(X) :- menu(X).

% label , title_text # flag_name - default_value :: post_hook <- guard.

select, 'Select menu level' # menu_level - naive :: select_menu_branch.
br(1) , 'This is the 1st question' # flag1 - vf11.
br(1) , 'This is the 2nd question' # flag2 - vf22.
% Example of false default value
br    , 'This is the 3rd question' # flag3 - vf32.

% 3 branches. opt3 is null branch
opt   , 'Type of Optimization'     # opt_br - opt1 :: opt_menu_branch.
opt1  , 'Opt1' # opt_flag1 - opt1.
opt2  , 'This appears in prev question is yes' # opt_flag1 - opt1 <- opt_guard.
opt2  , 'Ask prev question' # opt_flag2 - yes.


% Postcondition gets 2 arguments:
% - First a list of pair (flag-flag_value)
% - Returns in 2nd arg the modified list
opt_menu_branch( A , B) :-
 	member( opt_br=OptBr , A ),
 	menu_branch( A , OptBr , 0 , B ).

select_menu_branch( A , B) :-
 	member( menu_level=L , A ),
	( L == naive -> L1 = 0 ; L1 = 1 ),
 	menu_branch( A , br , L1 , B1 ),
 	menu_branch( B1 , opt , L1 , B ).


guard opt_guard( X ) :- 
	member( opt_flag2=Y , X ), Y == yes.


%% Definition of flags values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Better? menu_level := naive | expert.
/*
flag( menu_level , [ naive, expert ] , naive).
flag( flag1  , [ vf11 , vf12, vf13 ] , vf13 ).
flag( flag2  , [ vf21 , vf22, vf23 , vf24] , vf21 ).
flag( flag3  , [ on , off] , off ).
flag( opt_br , [ opt1, opt2 , opt3 ] , opt1 ).
flag( opt_flag1 , [ opt1, opt2 , opt3 ] , opt1 ).
flag( opt_flag2 , [ yes, no ] , opt1 ).

get_menu_default_option( O , Def ) :-
	flag( O , _ , Def ).

get_menu_help( _O , 'This Demos has no help!' ).

menu_is_valid_flag_value( A , B ) :-
	flag( A , V , _ ),
	member( B , V ).

menu_valid_flag_values( A , B ) :-
	flag( A , B , _ ).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



