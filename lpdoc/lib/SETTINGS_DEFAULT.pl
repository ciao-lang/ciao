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

filepath := '/path/to/doc/sources'|'/other/path/to/doc/sources'.

output_name := 'manual_name'.

doc_structure := 'main_module'.

commonopts := no_patches. % no_bugs|no_patches
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

bibfile := '/path/to/somebib.bib'.
bibfile := '/path/to/otherbib.bib'.

htmldir := '/install/path/for/html'.
docdir := '/install/path/for/other/doc'.
infodir := '/install/path/for/info'.
mandir := '/install/path/for/man'.

% TODO: enable by default?
allow_markdown := no.
syntax_highlight := no.

% ----------------------------------------------------------------------------
% End of SETTINGS
