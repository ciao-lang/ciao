:- module(_, [], [lpdoclib(doccfg)]).

%! \title Default configuration file for LPdoc
%  \author Ciao Development Team
%
%  \module This is a default configuration file for \apl{lpdoc},
%    typically used in the generation of documentation for single
%    modules. The defaults listed are typically suggestions. These
%    settings should be changed to suit your application.

:- doc(bug, "Definitions that are overriden by the emacs mode must fit
   in one line. Do not use emacs but LPdoc to generate this file").

filepath := '/home/clip/Systems/lpdoc/doc'|'/home/clip/Systems/ciao/doc/common'.

output_name := 'manual_name'.

doc_structure := 'main_module'.

commonopts := no_patches. % no_bugs|no_patches
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

bibfile := '/home/clip/bibtex/clip/clip'.
bibfile := '/home/clip/bibtex/clip/others'.

htmldir := '/home/clip/public_html/Local/lpdoc_docs'.
docdir := '/home/clip/public_html/Local/lpdoc_docs'.
infodir := '/home/clip/public_html/Local/lpdoc_docs'.
mandir := '/home/clip/public_html/Local/lpdoc_docs'.

% ----------------------------------------------------------------------------
% End of SETTINGS
