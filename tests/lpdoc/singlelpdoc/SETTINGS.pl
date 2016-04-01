:- module(_, [], [lpdoclib(doccfg)]).

% TODO: lpdoc all is not working for this file. The .lpdoc file is
%       ignored unless lpdoc singlelpdoc.texic is put as target.

doc_structure := 'singlelpdoc'.

doc_mainopts := no_patches.
doc_compopts := no_isoline|no_engmods|propmods|no_changelog.

%docformat := texi. % html or others are not working
docformat := pdf|manl|info|html.

% TODO: port this manual
allow_markdown := no.
syntax_highlight := no.
