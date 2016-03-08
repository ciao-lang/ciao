:- module(_, _, [lpdoclib(doccfg)]).

% TODO: lpdoc all is not working for this file. The .lpdoc file is
%       ignored unless lpdoc singlelpdoc.texic is put as target.

filepath := ~fsR(bundle_src(ciao)/tests/lpdoc/singlelpdoc).

doc_structure := 'singlelpdoc'.

doc_mainopts := no_patches.
doc_compopts := no_isoline|no_engmods|propmods|no_changelog.

%docformat := texi. % html or others are not working
docformat := texi|ps|pdf|manl|info|html.

