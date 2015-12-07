%%----------------------------------------------------------------------
%% TCL FROM PROLOG
%%
%% April 1999
%% Montse Iglesias
%%----------------------------------------------------------------------

:- module(tcl_command,[],[assertions]).

:- export(make_assigned_command/3).
:- export(make_display_command/2).
:- export(make_button_command/3).
:- export(make_label_command/3).
:- export(make_pack_command/2).
:- export(make_bind_command/4).
:- export(make_prolog_command/2).

:- export(test_command/2).

:- use_module(library(tcltk)).

%%------------------------------------------------------------------------

make_assigned_command(Variable,Content,Command):-
	Command = [set,Variable,Content].

make_display_command(Variable,Command):-
	atom_concat('$',Variable,Var),
	Command = [puts,Var].

make_button_command(Variable,Text_content,Command):-
	Text = dq(Text_content),
        atom_concat('.',Variable,Var),
	Command = [button,Var,'-text',Text].
%	Command1 = [pack,Var].

make_pack_command(Variable,Command):-
	atom_concat('.',Variable,Var),
	Command = [pack,Var].

make_label_command(Variable,Text_content,Command):-
	Text = dq(Text_content),
	atom_concat('.',Variable,Var),
	Command = [label,Var,'-text',Text].

make_prolog_command(Prolog_Event,Command):-
	Prolog = dq(write(execute(Prolog_Event))),
	Command = [prolog_one_event,Prolog].

make_bind_command(Variable,Tcl_Event,Prolog,Command):-
	make_prolog_command(Prolog,Prolog_command),
	Prolog_command1 = br(Prolog_command),
	atom_concat('.',Variable,Var),
	atom_concat('<',Tcl_Event,Tcl),
	atom_concat(Tcl,'>',Event),
	Command = [bind,Var,Event,Prolog_command1].

%%-----------------------------------------------------------------------
%%  Test these predicates
%%-----------------------------------------------------------------------

test_command(_,[]).
	
% execute all the commands of the list
test_command(I,[CommandList|Rest]):-
       tcl_eval(I,CommandList,_),
       test_command(I,Rest).
%      tcl_delete(I).
