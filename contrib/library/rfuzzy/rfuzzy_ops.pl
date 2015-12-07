% The following lines were previously in file rfops.pl.

:- op(1200,xfx,':~').    % Fuzzy Rule
% :- op(1200,xf,':~').     % Fuzzy Fact

:- op(1200,xfx,'value').   % Rfuzzy Fact
:- op(1175,xfx,'cred').    % Credibility value Rfuzzy.

%:- new_declaration(prop/1).
%:- op(1150,fx,(prop)).
%:- new_declaration(set_prop/1).
%:- op(1150,fx,(set_prop)). % Properties assignment Rfuzzy.

%:- op(1200,xfx,':=').      % simple fuzzy prolog
%:- op(1200,xf,':=').       % simple fuzzy prolog

:- op(1200,xfx,':#').      % definition of fuzzy predicate neg agreg etc
%:- op(1175,xfx,(=>)).      % implicacion fuzzy.
:- op(1175,xfx,'if').      % implicacion fuzzy.
:- op(1150,fx,'fnot').     % fuzzy negation

%:- op(1150, fx,aggr).      % declared associative aggregator
%:- op(1120,xfy,'##').      % associative aggregator
%:- op(1120,xfy,'<#').      % before apply aggregator
%:- op(1120,xfy,'#>').      % after apply aggregator

%:- op(1150,fx,'fuzzy'). % fuzzied
%:- op(1190,fx,'fuzzy_predicate'). 
%:- op(1190,fx,'fuzzy_discrete').

% ---------------------------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------------------------
 
%:- aggr min.
%:- aggr luka.
%:- aggr prod.
%:- aggr max.
%:- aggr dluka.
%:- aggr dprod.
%:- aggr iprod.
%:- aggr complement.

% ---------------------------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------------------------
