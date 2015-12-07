:- module(_, _, [regtypes, fsyntax, assertions]).

:- include(lpdoclib('SETTINGS_schema')).
% ****************************************************************************
% This is an LPdoc configuration file. See SETTINGS_schema for documentation *
% ****************************************************************************

:- use_module(library(bundle/paths_extra), [fsR/2]).

:- reexport(ciao_docsrc(common/'LPDOCCOMMON')).
libtexinfo(_) :- fail.
datamode(_) :- fail.
execmode(_) :- fail.

% (not customized)
bibfile(_) :- fail.
htmldir(_) :- fail.
docdir(_) :- fail.
infodir(_) :- fail.
mandir(_) :- fail.

output_name := 'ciao_builder'.

% TODO: use parent_bundle to share those defs
filepath := ~fsR(bundle_src(builder)/'doc').
filepath := ~fsR(bundle_src(builder)/'src').
filepath := ~fsR(bundle_src(builder)/'cmds').
filepath := ~ciaofilepath_common.

doc_structure := 
        'ciao_builder_ref'-[
          % TODO: Missing lib/bundle/* modules
	  % TODO: Missing hooks for bundleconfig and bundlehooks
          %
	  % Driver of commands on bundles
          'ciao_builder'-[
            'ciaocl_help', % TODO: This is for internals
            'bundlehooks_holder',
            'builder_cmds'-[
	      % Build
              'ciaoc_aux',
              'builder_aux',
	      % Installation
	      'info_installer',
	      'register_in_script',
	      % Other operations on source
              'bundle_hash',
              'detcheader',
	      % Auxiliary
              'messages_aux',
              'interactive_aux'
            ],
	    % Configuration
            'bundle_configure',
	    % Packaging
            'pbundle_generator'-[
              'pbundle_meta',
              'pbundle_gen_bin',
              'pbundle_gen_mac',
              'pbundle_gen_rpm',
              'pbundle_gen_src',
              'pbundle_gen_win32'
            ]
          ]
        ].

%doc_mainopts := no_patches.
doc_mainopts := _ :- fail. % Allow patches in main changelog (those are the release notes)

% TODO: Added no_propuses because texindex breaks with very large
%       indices (due to internal, maybe arbitrary, limitations) --JF.
doc_compopts := no_isoline|no_engmods|propmods|no_changelog|no_propuses.


