%%----------------------------------------------------------------------
%%
%% MYCIN RULEBASE EXAMPLE
%%
%% Angel Fernandez Pineda.
%%
%% DESCRIPTION:
%%
%% Our company is willing to develop a new software project. 
%% We would like to know how risky is such project.
%%
%%----------------------------------------------------------------------

:- module(project_risk,[],[mycin_rulebase]).

%% Import current knowledge about the project.

:- use_module(project_features).

%%----------------------------------------------------------------------
%% RULES
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Select the best organization pattern for the people involved on
%% development.
%%
%% Possible organization patterns are: by_applications, by_functions, 
%% weak_matrix, strong_matrix , by_projects.
%%
%%----------------------------------------------------------------------

:- export(org/1).

org(by_applications) cf 0.9 :-
	soft_reliability(high).

org(by_applications) cf 0.5 :-
	user_interaction(strong),
	duration(high),
	risk(high).

org(by_applications) cf -1 :-
	boss_availability(high).

%

org(by_functions) cf 0.3 :-
	technical_qualification(low).

org(by_functions) cf 0.85 :-
	tech_level(high),
	risk(high).

org(by_functions) cf -0.85 :-
	soft_reliability(high).

org(by_functions) cf -0.6 :-
	boss_availability(high).

org(by_functions) cf -1 :-
	user_interaction(strong).

org(by_functions) cf -0.75 :-
	duration(high).

org(by_functions) cf -0.15 :-
	new_employees(yes).

org(by_functions) cf -0.6 :-
	technical_qualification(low).

%

org(weak_matrix) cf 0.9 :-
	risk(high).

org(weak_matrix) cf 0.3 :-
	technical_qualification(high).

org(weak_matrix) cf 0.65 :-
	boss_availability(high),
	new_employees(yes),
	tech_level(high).

org(weak_matrix) cf -1 :-
	soft_reliability(high).

org(weak_matrix) cf -0.85 :-
	user_interaction(strong).

org(weak_matrix) cf -0.4 :-
	technical_qualification(low).

%

org(strong_matrix) cf 0.9 :-
	boss_availability(high),
	user_interaction(strong),
	tech_level(high).

org(strong_matrix) cf 0.2 :-
	technical_qualification(low),
	new_employees(yes).

org(strong_matrix) cf 0.8 :-
	risk(high).

%

org(by_projects) cf 0.8 :-
	boss_availability(high),
	user_interaction(strong),
	soft_reliability(high).

org(by_projects) cf -0.85 :-
	risk(high).

%%----------------------------------------------------------------------
%%
%% HOW RISKY IS THIS PROJECT
%%
%%----------------------------------------------------------------------

:- export(risk/1).

risk(high) cf 0.8 :-
	tech_level(high).

risk(high) cf 0.3 :-
	new_employees(yes).

risk(high) cf -0.35 :-
	boss_availability(high).

risk(high) cf -0.3 :-
	technical_qualification(high).

risk(high) cf -0.25 :-
	user_interaction(strong).

risk(high) cf 0.4 :-
	involves(some).

risk(high) cf 0.75 :-
	company_impact(high).

risk(high) cf 0.55 :-
	market_status(war),
	imitation_time(short).

rish(high) cf 0.37 :-
	market_status(offensive),
	imitation_time(short).

risk(high) cf -0.3 :-
	market_status(defensive),
	imitation_time(short).

risk(high) cf -0.4 :-
	market_status(peace).

%%----------------------------------------------------------------------
%%
%% CHECK WHETHER THIS PROJECT WILL HAVE A DIRECT IMPACT
%% ON OUR COMPANY'S "WAY OF LIFE"
%%
%%----------------------------------------------------------------------

company_impact(high) cf 0.65 :-
	kind(trans).

company_impact(high) cf 0.30 :-
	kind(sis).

company_impact(high) cf -0.15 :-
	new_employees(yes).


%%----------------------------------------------------------------------
%%
%% WHAT WILL BE OUR COMPANY'S STATUS IN RELATION WITH THE MARKET
%% (at the end of the project)
%%
%% war - our company will be trying to take over the 
%%       market against other company.
%% offensive - there will be no other company annoying us.
%% defensive - we will be in a weak situation.
%% peace     - things will stay the same.
%%----------------------------------------------------------------------

:- export(market_status/1).

market_status(war) cf 0.9 :-
	kind(info),
	company(proactive),
	other_companies(proactive).

market_status(war) cf 0.7 :-
	kind(trans),
	company(proactive),
	other_companies(proactive).

market_status(war) cf -0.3 :-
	barrier(yes).

market_status(peace) cf 0.95 :-
	company(reactive),
	other_companies(reactive).

market_status(offensive) cf 0.95 :-
	kind(info),
	company(proactive),
	other_companies(reactive).

market_status(offensive) cf 0.6 :-
	kind(trans),
	company(proactive),
	other_companies(reactive).

market_status(defensive) cf 1 :-
	company(reactive),
	other_companies(proactive).
