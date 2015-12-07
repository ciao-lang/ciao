:- module(pcpe_rtquery_trans,[pcpe_rtquery_tr/2],[assertions]).

pcpe_rtquery_tr((:- pcpe_rtquery(Q)), [(:- pcpe_rtquery(Q))]):-
	callable(Q),!.
pcpe_rtquery_tr((:- pcpe_rtquery(Q)), []):-
	error(['The first argument of pcpe_rtquery(',Q,') must be a call to a predicate defined in this program.']).

	
