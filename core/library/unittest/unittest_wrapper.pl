% (included file)

% Declarations for unittest module wrappers

:- use_module(library(rtchecks/rtchecks_rt)).
:- use_module(library(rtchecks/rtchecks_basic)).
:- use_module(library(unittest/unittest_props)).

:- discontiguous test_entry/4.
:- multifile test_entry/4.
:- discontiguous test_check_pred/4.
:- multifile test_check_pred/4.
