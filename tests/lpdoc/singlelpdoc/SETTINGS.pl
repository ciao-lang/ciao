:- module(_, _, [regtypes, fsyntax, assertions]).

:- include(lpdoclib('SETTINGS_schema')).
% ****************************************************************************
% This is an LPdoc configuration file. See SETTINGS_schema for documentation *
% ****************************************************************************

% TODO: lpdoc all is not working for this file. The .lpdoc file is
%       ignored unless lpdoc singlelpdoc.texic is put as target.

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(bundle/paths_extra), [fsR/2]).

:- reexport(ciao_docsrc(common/'LPDOCCOMMON')).
output_name(_) :- fail.
libtexinfo(_) :- fail.
datamode(_) :- fail.
execmode(_) :- fail.

filepath := ~fsR(bundle_src(ciao)/tests/lpdoc/singlelpdoc).

htmldir := ''.
% (not customized)
bibfile(_) :- fail.
docdir(_) :- fail.
infodir(_) :- fail.
mandir(_) :- fail.

doc_structure := 'singlelpdoc'.

doc_mainopts := no_patches.
doc_compopts := no_isoline|no_engmods|propmods|no_changelog.

%docformat := texi. % html or others are not working
docformat := texi|ps|pdf|manl|info|html.

