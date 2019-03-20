%:- module(_, [], []).

% This file loads compilation modules required to compile the
% compiler. They will be included statically in the bootstrap.

% TODO: merge with optim_comp version; this one is an included file so
%   that modules are in the scope of c_itf.pl

:- use_module(library(condcomp/condcomp_tr)).
%:- use_module(library(dcg/dcg_tr)).
%:- use_module(library(optparse/optparse_tr)).

% TODO: really?
:- use_module(library(rtchecks/rtchecks_tr), []).

