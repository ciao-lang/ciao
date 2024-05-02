% (included file)

% Declarations for unittest module wrappers

:- use_module(library(rtchecks/rtchecks_rt)).
:- use_module(library(rtchecks/rtchecks_basic)).
:- use_module(library(unittest/unittest_props)).

:- discontiguous test_entry/3.
:- multifile test_entry/3.
:- discontiguous test_check_pred/3.
:- multifile test_check_pred/3.
