:- module(_, [], [lpdoclib(doccfg)]).

:- include(ciao_docsrc(common/'LPDOCCOMMON')).

output_name := 'ciao_internals'.

% TODO: use parent_bundle to share those defs
filepath := at_bundle(core, 'ciaoc').
filepath := at_bundle(core, 'lib/compiler').
filepath := at_bundle(core, 'engine').
filepath := ~ciaofilepath_common.

doc_structure := 
    'ciao-internals'-[
      % The compiler
%      'ciaoc',
      'compiler', % TODO: already in refman! include exemaker there too?
      'exemaker',
%      'c_itf',
      'c_itf_internal',
      'pl2wam',
      % Emulator generator
      'emugen/emugen_doc',
      % The default emulator (ciaoengine)
      'ciaoengine_doc'
    ].
%            callback.pl

doc_mainopts := no_patches.
% TODO: Added no_propuses because texindex breaks with very large
%       indices (due to internal, maybe arbitrary, limitations) --JF.
doc_compopts := no_isoline|no_engmods|no_propmods|no_changelog|no_propuses.

bibfile := ~ciao_bibfile.

% TODO: port this manual
allow_markdown := no.
syntax_highlight := no.
