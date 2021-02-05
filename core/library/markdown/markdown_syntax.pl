:- module(markdown_syntax, [], [assertions, isomodes]).

:- doc(title, "LPdoc-flavored markdown: syntactic definitions").
:- doc(authors, "Jose F. Morales").
:- doc(authors, "Manuel Hermenegildo").

:- doc(module, "This module defines part of the syntactic definitions
   for the valid commands of the LPdoc-flavored markdown.").

:- doc(bug, "This could be moved/shared with LPdoc code (indeed, it is
   partially done)").

% Environments that behave like sections (take as body the rest of the
% text until a new section-like environment is found).

:- export(section_like_env/1).
section_like_env(module).
section_like_env(summary).
section_like_env(usage).
section_like_env(appendix).
section_like_env(copyright).
section_like_env(ack).
section_like_env(bug).
section_like_env(address).
% Note: this means that all text after a predicate comment
%   is really part of its documentation.
section_like_env(docpred(_, _)). % (internal) 
% (those are already treated by LPdoc)
%section_like_env(section).
%section_like_env(subsection).
%section_like_env(subsubsection).

% Type of arguments for some LPdoc commands (body)
%
% The valid types are:
%
%   - term :: block parsed as a Prolog term
%   - docstring :: block parsed as a docstring
%   - docstring_oneline :: line parsed as a docstring

:- export(cmd_type/2).
cmd_type(version_maintenance, term). % version_maintenance_type/1: on off dir(atom)
cmd_type(doinclude, term). % Name/Arity or list(Name/Arity)
cmd_type(hide, term). % Name/Arity or list(Name/Arity)
cmd_type(filetype, term). % filetype/1: module|user|include|package|part
cmd_type(nodoc, term). % nodoc atom 
cmd_type(title, docstring).
cmd_type(subtitle, docstring).
cmd_type(subtitle_extra, docstring).
cmd_type(ack, docstring).
cmd_type(usage, docstring).
cmd_type(appendix, docstring).
cmd_type(copyright, docstring).
cmd_type(credits, docstring).
cmd_type(address, docstring).
cmd_type(author, docstring).
cmd_type(summary, docstring).
cmd_type(module, docstring).
cmd_type(bug, docstring).
cmd_type(em, docstring).
cmd_type(bf, docstring).
cmd_type(tt, docstring).
cmd_type(text, docstring). % (internal) % TODO: use
cmd_type(section, docstring_oneline). % (internal)
cmd_type(subsection, docstring_oneline). % (internal)
cmd_type(subsubsection, docstring_oneline). % (internal)
cmd_type(item, docstring).
cmd_type(item(_), docstring). % (internal)
cmd_type(item_num(_), docstring). % (internal)
cmd_type(docpred(_, _), docstring). % (internal)
cmd_type(href, string).
cmd_type(href(_), docstring).
cmd_type(codeblock, string).
cmd_type(codeblock(_), string).
cmd_type(link, docstring). % (not in PLdoc)
cmd_type(link(_), string). % (not in PLdoc)
cmd_type(code, multiline). % (not in PLdoc)
cmd_type(var, string).
cmd_type(pred, string).
cmd_type(math, string).

:- export(front_cmd/1).
% LPdoc commands that can appear as front commands (at the beginning
% of a line)
front_cmd(version_maintenance).
front_cmd(doinclude).
front_cmd(hide).
front_cmd(filetype).
front_cmd(nodoc).
front_cmd(title).
front_cmd(subtitle).
front_cmd(subtitle_extra).
front_cmd(ack).
front_cmd(usage).
front_cmd(appendix).
front_cmd(copyright).
front_cmd(credits).
front_cmd(address).
front_cmd(author).
front_cmd(summary).
front_cmd(module).
front_cmd(bug).

:- export(decl_cmd/1).
% LPdoc commands that can appear as declaration commands, i.e.,
%     :- doc(..., ...).
decl_cmd(version_maintenance).
decl_cmd(doinclude).
decl_cmd(hide).
decl_cmd(filetype).
decl_cmd(nodoc).
decl_cmd(title).
decl_cmd(subtitle).
decl_cmd(subtitle_extra).
decl_cmd(ack).
decl_cmd(usage).
decl_cmd(appendix).
decl_cmd(copyright).
decl_cmd(credits).
decl_cmd(address).
decl_cmd(author).
decl_cmd(summary).
decl_cmd(module).
decl_cmd(bug).
decl_cmd(docpred(_, _)). % (internal)

% Commands that are printed using `@begin{cmd}...@end{cmd}`
:- export(block_cmd/1).
block_cmd(itemize).
block_cmd(enumerate).
block_cmd(description).
block_cmd(verbatim).

% Commands that are printed using `@cmd ...`
:- export(prefix_cmd/1).
prefix_cmd(item).
prefix_cmd(item(_)). % (internal)
prefix_cmd(item_num(_)). % (internal)

% Indentation for introducing code listings environments
:- export(code_env_indent/1).
code_env_indent(4).

% Special syntax for unary LPdoc commands
:- export(decorator_fxf/4).
decorator_fxf("**", "**", bf, any).
decorator_fxf("__", "__", bf, noalpha).
decorator_fxf("*", "*", em, any).
decorator_fxf("_", "_", em, noalpha).
decorator_fxf("[[", "]]", href, any).
decorator_fxf("`", "`", code, any). % Note: 'code' does not exist in LPdoc
decorator_fxf("$", "$", math, any).

% Special syntax for binary LPdoc commands
:- export(decorator_fxfxf/4).
decorator_fxfxf("[[", "][", "]]", href).
decorator_fxfxf("[", "](", ")", link). % Note: 'link' does not exist (reversed href, for markdown style)

% Reserved characters (can be escaped to avoid processing)
:- export(reserved_char/1).
reserved_char(0'*).
reserved_char(0'_).
reserved_char(0'[).
reserved_char(0']).
reserved_char(0'`).
reserved_char(0'$).
reserved_char(0'().
reserved_char(0')).
reserved_char(0'<). % TODO: not used yet
reserved_char(0'>). % TODO: not used yet

