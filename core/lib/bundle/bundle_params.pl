:- module(bundle_params, [
		bundle_param_value/2,
		set_bundle_param_value/2,
		del_bundle_param_value/1
            ],
	    [assertions, regtypes, dcg, hiord]).

:- doc(title, "Bundle Configuration Parameters").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").
% (input of bundle configuration)

:- data param_value_/3.

bundle_param_value(Bundle:Name, Value) :-
	data_facts:current_fact(param_value_(Name, Bundle, Value)).

del_bundle_param_value(Bundle:Name) :-
	data_facts:retractall_fact(param_value_(Name, Bundle, _)).

set_bundle_param_value(Bundle:Name, Value) :-
	data_facts:retractall_fact(param_value_(Name, Bundle, _)),
	data_facts:asserta_fact(param_value_(Name, Bundle, Value)).



