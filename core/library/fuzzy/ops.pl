:- op(1200,xfx,':~').   % Fuzzy Rule
:- op(1200,xf,':~').    % Fuzzy Fact

:- op(1200,xfx,':=').   % simple fuzzy prolog
:- op(1200,xf,':=').    % simple fuzzy prolog

:- op(1200,xfx,':#').   % definition of fuzzy predicate neg agreg etc
:- op(1175,fx,(=>)).    % implicacion fuzzy.
:- op(1150,fx,'fnot').  % fuzzy negation

:- op(1150, fx,aggr).   % declared associative aggregator
:- op(1120,xfy,'##').    % associative aggregator
:- op(1120,xfy,'<#').   % before apply aggregator
:- op(1120,xfy,'#>').   % after apply aggregator

:- op(1150,fx,'fuzzy'). % fuzzied
:- op(1190,fx,'fuzzy_predicate'). 
:- op(1190,fx,'fuzzy_discrete').
