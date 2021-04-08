:- use_package(argnames).

% Hooks for definition of menu of flags

:- discontiguous(menu_default/3).
:- multifile menu_default/3.

:- discontiguous menu_opt/6.
:- argnames menu_opt(menu, flag, message, guard, pre, post).
:- multifile menu_opt/6.
:- meta_predicate menu_opt(?, ?, ?, pred(1), pred(0), pred(2)).

:- discontiguous hook_menu_flag_values/3.
:- multifile hook_menu_flag_values/3.

:- discontiguous hook_menu_check_flag_value/3.
:- multifile hook_menu_check_flag_value/3.

:- discontiguous hook_menu_flag_help/3.
:- multifile hook_menu_flag_help/3.

:- discontiguous hook_menu_default_option/3.
:- multifile hook_menu_default_option/3.

% ---------------------------------------------------------------------------
% (Documentation)
% TODO: implement documentation of interfaces; see doccfg

% :- trust pred menu_default( Menu , Flag , DefaultValue ) :
%       term * atm * atm
% # "@var{Menu} is a term that has to correspond with the 1st argument
%   of @pred{Menu}. @var{Flag} is the desired flag to have a default
%   value. @var{DefaultValue} is the default value of @var{Flag}.".
% 
% :- trust pred menu_default( Menu , Flag , DefaultValue )  => 
%       atm  * atm * atm.
% :- trust pred menu_default( Menu , Flag , DefaultValue ) :
%       atm  * var * var
% # "This call mode can be used to ask which flags and its values
%   has a menu @var{menu}".
% 
% :- trust pred menu_default( Menu , Flag , DefaultValue ) :
%       atm  * atm * var
% # "This call mode can be used to ask which value have the flag
%   @var{Flag} in the menu @var{menu}".

% :- pred menu_opt( Menu , Flag , Text , Guard , BeforePrinting , SelectedHook ) :
%       term * atm * atm * cgoal * cgoal * cgoal
% # "@var{Menu} is a term that specifies the menu name. It can be an
%   atom or just a predicate of arity 1, where the 1st argument
%   indicates the menu level (i.e., ana(1) is the level 1 of 'ana'
%   menu). @var{Flag} is the flag that will be asked. 
% 
%   @var{Text} is the test that will be printed when asking the
%   @var{Flag}. 
% 
%   @var{Guard} is a predicate of arity 1 that is invoked to see if the
%   flag should be asked. The argument is the selected menu options till
%   moment in the way: [flag1=value1, flag2=value2, ...]. 
% 
%   @var{BeforePrinting} is a predicate of arity 0, that is invoked
%   whenever the menu option has been selected the validator menu
%   options chooser.
% 
%   @var{SelectedHook} is a predicate of arity 2, that is invoked
%   whenever the flag has been selected by the user. The 1st argument
%   are the current selected values, including the current flag, and in
%   the 2nd argument the possible modified list is expected.
% 
%   In summary, if @var{Guard} holds, then @var{BeforePrinting} is
%   executed (no action is taken whether it fails or not), and after the
%   user has types the option @var{SelectedHook} is invoked.".
% 
% :- trust pred menu_opt( Menu , Flag , Text , Guard , BeforePrinting ,
%       SelectedHook ) : term * term * term * term * term *term.

% :- pred hook_menu_flag_values(Menu, Flag, Values)
%       : atom * atom * var
%       => menu_flag_values(Values)
%   # "It is a hook. It is invoked whenever a menu question is
%   printed. @var{Values} is a term which specifies the possible
%   values. If @var{Values} is alist(List) -atom list-, then menu will
%   check if the typed value by user belongs to List. If @var{Values} is
%   a term ask(T,Flag), the menu will invoke
%   @pred{hook_menu_check_flag_value/3} hook to check if introduced
%   value is valid.".
% 
% :- pred hook_menu_check_flag_value(M, F, V) # "It is a hook. It is
%    invoked whenever the menu needs to check whether the answer
%    introduced for the menu @var{M} is correct. This happens when
%    @pred{hook_menu_flag_values/3} returns in its second argument
%    something different than alist(_).".
% 
% :- pred hook_menu_flag_help(M, F, H) # "It is a hook. It is invoked
%    whenever the user ask for a help description, @var{H}, of the flag
%    @var{F} in the menu @var{M}.".
% 
% :- pred hook_menu_default_option(M, F, D) # "It is a hook. It is
%    invoked whenever the menu needs to offer a default option to the
%    user in the menu @var{M} and it has not been neither introduced
%    before nor specified by @pred{menu_default/3}.".
