%:- module(_, [], []).

% This file loads compilation modules required to compile the
% compiler. They will be included statically in the bootstrap.

% TODO: merge with optim_comp version; this one is an included file so
%   that modules are in the scope of c_itf_internal.pl

:- use_module(library(condcomp/condcomp_tr)).

% TODO: really?
:- use_module(library(rtchecks/rtchecks_tr), []).
:- use_module(library(inliner/inliner_tr),   []).
