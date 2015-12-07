%%----------------------------------------------------------------------
%%
%% MYCIN RULEBASE EXAMPLE
%%
%% Angel Fernandez Pineda.
%%
%% DESCRIPTION:
%%
%% Our company is willing to develop a new software project. 
%% We would like to know how risky is such project. This file is
%% imported by project_risk.pl
%%
%%----------------------------------------------------------------------

:- module(project_features,[],[mycin_rulebase]).

%%----------------------------------------------------------------------
%% CURRENT KNOWLEDGE
%%----------------------------------------------------------------------

%% required reliability of resulting software.

:- export(soft_reliability/1).

soft_reliability(high) cf 0.5.

%% technical qualification of our employees

:- export(technical_qualification/1).

technical_qualification(high) cf 0.8.

%% Project director availability.

:- export(boss_availability/1).

boss_availability(high) cf 0.

%% interaction user-developer.

:- export(user_interaction/1).

user_interaction(strong) cf 0.7.
user_interaction(weak)  cf 0.

%% New employees are needed

:- export(new_employees/1).

new_employees(yes) cf 0.

%% Project duration

:- export(duration/1).

duration(long) cf 0.8.

%% level of required technology

:- export(tech_level/1).

tech_level(high) cf 0.5.

%% how many areas of our company are involved in the project

:- export(involves/1).

involves(some) cf 0.

%% Kind of software (transactional/informational)

:- export(kind/1).

kind(trans) cf -1.
kind(info)  cf 1.

%% How much time is need for other companies to imitate our project.

:- export(imitation_time/1).

imitation_time(short) cf 0.3.

%% Our company's market strategy (proactive / reactive)

:- export(company/1).

company(proactive) cf 0.7.
company(reactive)  cf 0.2.

%% Other companies market strategy

:- export(other_companies/1).

other_companies(proactive) cf 0.3.
other_companies(reactive)  cf 0.9.

%% This project is expected to keep other companies from entering our market

:- export(barrier/1).

barrier(yes) cf -1.
