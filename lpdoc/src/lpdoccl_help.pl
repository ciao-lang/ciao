:- module(_, [], [assertions, isomodes, regtypes, dcg]).

:- doc(title, "LPdoc Command-line Help").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(module, "Formatted help for the @apl{lpdoc} command line tool").

% ---------------------------------------------------------------------------

% (exports show_help/2 and show_help_cmd/2)
:- include(library(cmdline/cmdline_help)).

grp_details(_, _) :- fail. % none
cmd_details(_, _) :- fail. % none

% ---------------------------------------------------------------------------

top_cmd_name('lpdoc', "[<opts>] <cmd> [<args>]").
top_cmd_details([
  %2........................________________________________________________
  "General options:",
  "",
  "  -v                     Verbose",
  "  --trace-deps           Trace dependencies (for debugging)",
  "",
  "  -d Name=Value          Override the option Name (Name(Value)) from",
  "                         the configuration file",
  "",
  "  -cv,--comment-version  The source files contain version information",
  "                         If not specified lpdoc will asume the opposite",
  "",
  "  -f FILE                Uses file FILE as configuration file.",
  "                         Default is SETTINGS.pl",
  "  -c FILE                Process FILE as a separate (standalone) component",
  "",
  "  -T                     Start a LPdoc toplevel (single option)"
]).

% ---------------------------------------------------------------------------

grp_def(help, "Help") :- advanced.
%
cmd_grp(help, help).
cmd_usage(help, "", [
    %1_______________________________________________
    "Show help"
]).

grp_def(gen_grp, "Generate").
%
cmd_grp(all, gen_grp).
cmd_usage(all, "", [ % intermediate
    %1_______________________________________________
    "Generate all documentation formats (docformat/1)"
]).
cmd_grp(pdf, gen_grp).
cmd_usage(pdf, "", [ % intermediate
    %1_______________________________________________
    "Generate .pdf documentation"
]).
cmd_grp(ps, gen_grp).
cmd_usage(ps, "", [ % intermediate
    %1_______________________________________________
    "Generate .ps documentation"
]).
cmd_grp(html, gen_grp).
cmd_usage(html, "", [ % intermediate
    %1_______________________________________________
    "Generate .html documentation"
]).
cmd_grp(info, gen_grp).
cmd_usage(info, "", [ % intermediate
    %1_______________________________________________
    "Generate .info documentation"
]).
cmd_grp(manl, gen_grp).
cmd_usage(manl, "", [ % intermediate
    %1_______________________________________________
    "Generate .manl documentation"
]).

grp_def(view_grp, "Visualize").
%
cmd_grp(view, view_grp).
cmd_usage(view, "", [ % intermediate
    %1_______________________________________________
    "Visualize default format (.html)"
]).
cmd_grp(pdfview, view_grp).
cmd_usage(pdfview, "", [ % intermediate
    %1_______________________________________________
    "Visualize .pdf (with a default viewer)"
]).
cmd_grp(psview, view_grp).
cmd_usage(psview, "", [ % intermediate
    %1_______________________________________________
    "Visualize .ps (with a default viewer)"
]).
cmd_grp(htmlview, view_grp).
cmd_usage(htmlview, "", [ % intermediate
    %1_______________________________________________
    "Visualize .html (with a default viewer)"
]).
cmd_grp(infoview, view_grp).
cmd_usage(infoview, "", [ % intermediate
    %1_______________________________________________
    "Visualize .info (with a default viewer)"
]).
cmd_grp(manlview, view_grp).
cmd_usage(manlview, "", [ % intermediate
    %1_______________________________________________
    "Visualize .manl (with a default viewer)"
]).

grp_def(clean_grp, "Cleaning").
%
cmd_grp(clean, clean_grp).
cmd_usage(clean, "", [ % intermediate
    %1_______________________________________________
    "Clean all except .texi and targets (E.g., .pdf)"
]).
cmd_grp(docsclean, clean_grp).
cmd_usage(docsclean, "", [ % intermediate, temp_no_texi
    %1_______________________________________________
    "Clean all except .texi output"
]).
cmd_grp(distclean, clean_grp).
cmd_usage(distclean, "", [ % intermediate, texi
    %1_______________________________________________
    "Clean all except targets (e.g., .pdf)"
]).
cmd_grp(realclean, clean_grp).
cmd_usage(realclean, "", [ % intermediate, temp_no_texi, texi
    %1_______________________________________________
    "Clean all generated files"
]).
