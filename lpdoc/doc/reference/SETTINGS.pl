:- module(_, [], [lpdoclib(doccfg)]).

%! \title Config for LPdoc reference manual
%  \author Manuel Hermenegildo
%  \author Edison Mera
%  \author Jose F. Morales

:- include(ciao_docsrc(common/'LPDOCCOMMON')).

filepath := '../readmes'.
filepath := '../../src'.
filepath := '../../examples'.
filepath := ~ciaofilepath_common.

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

bibfile := ~ciao_bibfile.

