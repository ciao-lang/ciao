:- module(_, _, [assertions, regtypes, fsyntax]).

:- include(lpdoclib('SETTINGS_schema')).
% ****************************************************************************
% This is an LPdoc configuration file. See SETTINGS_schema for documentation *
% ****************************************************************************

:- doc(title, "Settings for Ciao IDE manual").
:- doc(author, "Jose F. Morales").
:- doc(filetype, user).

:- use_module(library(system)).
:- use_module(library(bundle/paths_extra), [fsR/2]).

datamode(_) :- fail.
execmode(_) :- fail.

% (not customized)
bibfile(_) :- fail.
htmldir(_) :- fail.
docdir(_) :- fail.
infodir(_) :- fail.
mandir(_) :- fail.

filepath := ~fsR(bundle_src(ide)/doc).

output_name := _ :- fail.

doc_structure := 'ide'.

commonopts := no_bugs|no_patches.
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

index := concept|lib|pred|prop|regtype|decl|author|global.

startpage := 1.

papertype := afourpaper.

libtexinfo := 'yes'.

docformat := texi|ps|pdf|manl|info|html.

