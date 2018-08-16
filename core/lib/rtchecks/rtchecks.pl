:- package(rtchecks).

:- use_package(assertions).
:- use_package(hiord).
:- use_package(library(inliner/inliner_ops)). % TODO:T261

:- new_declaration(rtchecked/0).

% TODO: replace as a new_declaration(on) so that this can go to each rtc module (ask Manuel)
%   If we do that, then all definitions in propimpl shoud go to their corresponding modules
:- new_declaration(rtc_impl/2).

:- rtchecked. % TODO: clarify; better name (no conflicts with user)?

:- load_compilation_module(library(rtchecks/rtchecks_tr)).
:- add_sentence_trans(rtchecks_tr:rtchecks_sentence_tr/4, 8310).
:- add_goal_trans(rtchecks_tr:rtchecks_goal_tr/3, 8310).

:- set_prolog_flag(runtime_checks, yes).

%:- include(rtchecks_rt_propimpl).       % TODO: check scoping of the
%:- include('rtchecks_rt_propimpl.pl').  %       include/1 directive
:- include(library(rtchecks/rtchecks_rt_propimpl)).
:- use_module(engine(basic_props_rtc)).

