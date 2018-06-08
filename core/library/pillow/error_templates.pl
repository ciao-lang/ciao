:- module(error_templates, [], [assertions,isomodes,dcg]).

:- doc(title, "HTML error responses").
:- doc(author, "The Ciao Development Team").

:- doc(module, "This module implements templates for generating HTML
   error responses.").

:- include(library(pillow/ops)). % for '$'/2 operator
:- use_module(library(pillow/html), [html2terms/2]).

:- export(html_error_response/2).
:- pred html_error_response(Contents, Str)
   # "Formats @var{Contents} (html_term) as a string @var{Str} using a
      error500 template.".

% HTTP error responses
html_error_response(Contents, Str) :-
	error500_template(Contents, HTML),
	html2terms(Str, HTML).

% % (old, simpler template)
% error500_template(Contents, HTML) :-
% 	HTML = [
%           start,
%           title("Error Report"), 
%           --,
%           h1(['Error:']),
%           --,
%           Contents,
%           --,
%           end
%         ].

% HTML template for internal server error
error500_template(Contents, HTML) :-
	Title = "Error 500 (Internal Server Error)",
	HTML = [
          declare("DOCTYPE html"),
          start,
          head([
              meta$['charset'="utf-8"],
              meta$['name'="viewport",
	            'content'="initial-scale=1, "||
                              "minimum-scale=1, "||
                              "width=device-width"],
	      title(Title)
          ]),
	  env(style, [type='text/css'], [
	      'html,code {',
	      '  font-family: arial, sans-serif;',
	      '  font-size:16px; line-height: 1.3em;',
	      '}',
	      'html {',
	      '  background: white; color: black;',
	      '}',
	      'body {',
	      '  margin: 10% auto 0;',
	      '  max-width:400px;',
	      '  min-height:200px;',
	      '  padding: 40px 0 20px',
	      '}'
          ]),
          begin(body, []),
	  b(Title), \\,
	  %
	  Contents,
          end(body),
	  end
        ].

