%%------------------------------------------------------------------------
%%
%% MYCIN PROGRAMMING LIBRARY
%%
%% ERROR REPORTING
%%
%% AUTHOR : Angel Fernandez Pineda
%% DATE   : FEBRUARY 2000
%%
%%------------------------------------------------------------------------

:- module(mycin_error_reporting,
	[
	    start_of_messages/2,
	    end_of_messages/1,
	    message/3
	]).

%%------------------------------------------------------------------------

:- data message_of/5.          % Keep Tracking of error messages.
:- data doing_what/2.          % Currently doing DOING_MESSAGE.

:- use_module(library(compiler/c_itf), 
	[
	    module_error/0,
	    location/3,
	    module_from_base/2,
	    file_data/3
	]).

%%------------------------------------------------------------------------


start_of_messages(Module,DoingWhat) :-
	retractall_fact(message_of(Module,_,_,_,_)),
	retractall_fact(doing_what(Module,_)),
	asserta_fact(doing_what(Module,DoingWhat)).

end_of_messages(Module) :-
	current_prolog_flag(verbose_compilation,on),
	!,
	dump_messages(Module).

end_of_messages(Module) :-
	( message_of(Module,error,_,_,_) ; message_of(Module,warning,_,_,_) ),
	!,
	dump_messages(Module).

end_of_messages(_).

dump_messages(Module) :-
	doing_what(Module,Doing),
	message(['{'|Doing]),
	message_of(Module,Kind,LN0,LN1,Message),
	(LN0 \== '?' -> io_aux:message_lns(Kind,LN0,LN1,Message) ;
	                io_aux:message(Kind,Message) 
	),
	fail.

dump_messages(_) :-
	message(['}']).

:- set_prolog_flag(multi_arity_warnings,off).

message(Module,Kind,Message) :-
	module_from_base(Module,Base),
	file_data(Base,Source,_),
	location(Source,LN0,LN1),
	!,
	assertz_fact(message_of(Module,Kind,LN0,LN1,Message)),
	(Kind = error -> set_fact(module_error) ; true ).

message(Module,Kind,Message) :-
	assertz_fact(message_of(Module,Kind,'?','?',Message)),
	(Kind = error -> set_fact(module_error) ; true ).
