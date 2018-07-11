% TODO:T253 include 'doc_structure' in alldocs

:- module(_, [], [lpdoclib(doccfg)]).

:- include(ciao_docsrc(common/'LPDOCCOMMON')).

output_name := 'actmod'.

filepath := at_bundle(core, 'doc/common'). % CiaoDesc, InstallCiao, InstallCiaoWin32, InstallTestUnix, InstallTestWin32bin, UserSetup, EmacsDownload, EmacsTesting
filepath := at_bundle(core, 'shell').
filepath := at_bundle(core, 'ciaoc').
filepath := at_bundle(core, 'engine').
filepath := at_bundle(core, 'cmds').
filepath := ~ciaofilepath_common.
%
filepath := at_bundle(ciao_emacs, 'elisp').

doc_structure := 
	  'actmod/actmod_doc'-[
	    'actmod/actmod_dist'-[
              'actmod/regp_filebased',
              'actmod/regp_platformbased',
              'actmod/regp_webbased'
	    ],
            'actmod/actmod_process',
            'actmod/actmod_rt'
            % actmod instance name registry protocols
%            % (internal)
%            'actmod/actmod_hooks',
%            'actmod/actmod_tr',
%            'actmod/actmod_holder',
%            'actmod/filebased_common',
%            'actmod/webbased_common',
%            'actmod/regp_platformserver',
%            'actmod/regp_webserver',
%            % (must go elsewhere)
%            'actmod/rundaemon',
%            % (fibers)
%            'fibers/stream_watchdog',
%            'fibers/fnct_rt',
%            'fibers/mexpand_extra'
          ].

%doc_mainopts := no_patches.
doc_mainopts := _ :- fail. % Allow patches in main changelog (those are the release notes)
% TODO: Added no_propuses because texindex breaks with very large
%       indices (due to internal, maybe arbitrary, limitations) --JF.
doc_compopts := no_isoline|no_engmods|propmods|no_changelog|no_propuses.

bibfile := ~ciao_bibfile.

% TODO: port this manual
allow_markdown := no.
syntax_highlight := no.
