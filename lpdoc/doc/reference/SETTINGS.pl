:- module(_, _, [assertions, regtypes, fsyntax]).

:- include(lpdoclib('SETTINGS_schema')).
% ****************************************************************************
% This is an LPdoc configuration file. See SETTINGS_schema for documentation *
% ****************************************************************************

:- doc(title, "LPdoc Manual Settings").

:- doc(module, "This file contains the definitions and configuration
   settings for the @apl{lpdoc} manual.").

:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Edison Mera").
:- doc(author, "Jose F. Morales").

:- doc(filetype, user).

% ----------------------------------------------------------------------------

:- use_module(library(bundle/paths_extra), [fsR/2]).

datamode(_) :- fail.
execmode(_) :- fail.

% (not customized)
bibfile(_) :- fail.
htmldir(_) :- fail.
docdir(_) :- fail.
infodir(_) :- fail.
mandir(_) :- fail.

filepath := ~fsR(bundle_src(lpdoc)/doc/readmes).
filepath := ~fsR(bundle_src(lpdoc)/doc/reference).
filepath := ~fsR(bundle_src(lpdoc)/src).
filepath := ~fsR(bundle_src(lpdoc)/examples).
%
filepath := ~fsR(bundle_src(ciao)/doc/common).

output_name := 'lpdoc'.

doc_structure := 
        'lpdoc_ref_man'-[
	  'Reference'-[
	    'Generating',
	    'comments',
	    'assertions/assertions_doc',
	    'assertions/assertions_props',
	    'regtypes/regtypes_doc',
	    'basic_props',
	    'assertions/native_props',
	    'metaprops/meta_props',
	    'lpdoc_examples',
	    'example_module',
	    'rtchecks_doc',
	    'unittest'-
               ['unittest/unittest_props',
		'unittestdecls_doc',
		% 'unittest/unittest_utils',
		'unittest/unittest_statistics',
		'unittest/unittest_examples'
	       ],
	    'lpdoc_install'
          ],
	  'Internals'-[
	    'docmaker',
	    'autodoc',
	    'autodoc_state',
	    'autodoc_doctree',
	    'autodoc_structure',
	    'autodoc_settings',
	    % Backends
	    'Backends'-[
	      'autodoc_texinfo',
	      'autodoc_html'-[
	        'autodoc_html_assets',
	        'autodoc_html_template'
              ],
	      'autodoc_man'
            ],
	    % Miscellanea and other support code
	    'autodoc_filesystem',
	    'autodoc_index',
	    'autodoc_refsdb',
	    'autodoc_errors',
	    'autodoc_bibrefs',
	    'autodoc_aux',
	    'autodoc_images'
% TODO: Compute local modules that are not included in the internal documentation? Emit warning?
%	    'pbundle_download'
          ]
        ].

commonopts := no_bugs|no_patches.
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

index := concept|lib|pred|prop|regtype|decl|author|global.

startpage := 1.

papertype := afourpaper.

libtexinfo := 'yes'.

docformat := texi|ps|pdf|manl|info|html.

