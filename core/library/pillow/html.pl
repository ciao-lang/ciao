:- module(html, [
    canonic_html_term/1, canonic_xml_term/1, html_term/1,
    output_html/1, html2terms/2, xml2terms/2, html_template/3
    ], [assertions,isomodes,dcg]).

:- doc(title, "HTML/XML parser and generator").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Sacha Varma").
:- doc(author, "The Ciao Development Team").

:- doc(module, "This module implements predicates for
   @concept{HTML}/@concept{XML} generation and parsing.").

:- doc(appendix, "The code uses input from from L. Naish's forms and
   Francisco Bueno's previous Chat interface.  Other people who have
   contributed are (please inform us if we leave out anybody): Markus
   Fromherz.").

:- include(library(pillow/ops)).
% 
:- use_module(library(stream_utils), [write_string/1]).
:- use_module(library(strings), [whitespace/2, whitespace0/2, string/3]).
:- use_module(library(lists), [reverse/2, list_lookup/4]).

% ---------------------------------------------------------------------------

:- doc(canonic_html_term/1, "A term representing HTML code in
   canonical, structured way.  It is a list of terms defined by the
   following predicate:
   @includedef{canonic_html_item/1} @includedef{tag_attrib/1}
   Each structure represents one HTML construction:
   @begin{description}

   @item{@bf{env(}@em{tag}@bf{,}@em{attribs}@bf{,}@em{terms}@bf{)}} An
   HTML environment, with name @em{tag}, list of attributes @em{attribs}
   and contents @em{terms}.

   @item{@bf{$(}@em{tag}@bf{,}@em{attribs}@bf{)}} An HTML element of
   name @em{tag} and list of attributes @em{attribs}.  @tt{($)/2} is
   defined by the pillow package as an infix, binary operator.

   @item{@bf{comment(}@em{string}@bf{)}} An HTML comment (translates
   to/from @tt{<!--}@em{string}@tt{-->}).

   @item{@bf{declare(}@em{string}@bf{)}} An HTML declaration, they are
   used only in the header (translates to/from
   @tt{<!}@em{string}@tt{>}).

   @item{@em{string}} Normal text is represented as a list of
   character codes.

   @end{description}

   For example, the term
   @begin{verbatim}
env(a,[href=\"www.therainforestsite.com\"],
      [\"Visit \",img$[src=\"TRFS.gif\"]])
   @end{verbatim}
   is output to (or parsed from):
   @begin{verbatim}
<a href=\"www.therainforestsite.com\">Visit <img src=\"TRFS.gif\"></a>
   @end{verbatim}").

:- prop canonic_html_term(HTMLTerm) + regtype
    # "@var{HTMLTerm} is a term representing HTML code in canonical form.".

canonic_html_term(T) :- list(T,canonic_html_item).

:- prop canonic_html_item/1 + regtype.

canonic_html_item(comment(S)) :- string(S).
canonic_html_item(declare(S)) :- string(S).
canonic_html_item(env(Tag,Atts,Terms)) :-
    atm(Tag),
    list(Atts,tag_attrib),
    canonic_html_term(Terms).
canonic_html_item('$'(Tag,Atts)) :-
    atm(Tag),
    list(Atts,tag_attrib).
canonic_html_item(S) :- string(S).

:- doc(canonic_xml_term/1, "A term representing XML code in
   canonical, structured way.  It is a list of terms defined by the
   following predicate (see @pred{tag_attrib/1} definition in
   @pred{canonic_html_term/1}): @includedef{canonic_xml_item/1} In
   addition to the structures defined by @pred{canonic_html_term/1}
   (the @tt{($)/2} structure appears only in malformed XML
   code), the following structures can be used: @begin{description}

   @item{@bf{elem(}@em{tag}@bf{,}@em{atts}@bf{)}} Specifies an XML empty
   element of name @em{tag} and list of attributes @em{atts}.
   For example, the term
   @begin{verbatim}
elem(arc,[weigh=""3"",begin=""n1"",end=""n2""])
   @end{verbatim}
   is output to (or parsed from):
   @begin{verbatim}
<arc weigh=""3"" begin=""n1"" end=""n2""/>
   @end{verbatim}

   @item{@bf{xmldecl(}@em{atts}@bf{)}} Specifies an XML declaration with
   attributes @em{atts} (translates to/from @tt{<?xml }@em{atts}@tt{?>})

   @end{description}").

:- prop canonic_xml_term(XMLTerm) + regtype
    # "@var{XMLTerm} is a term representing XML code in canonical form.".

canonic_xml_term(T) :- list(T,canonic_xml_item).

:- prop canonic_xml_item/1 + regtype.

canonic_xml_item(Term) :- canonic_html_item(Term).
canonic_xml_item(xmldecl(Atts)) :-
    list(Atts,tag_attrib).
canonic_xml_item(env(Tag,Atts,Terms)) :-
    atm(Tag),
    list(Atts,tag_attrib),
    canonic_xml_term(Terms).
canonic_xml_item(elem(Tag,Atts)) :-
    atm(Tag),
    list(Atts,tag_attrib).

:- prop tag_attrib/1 + regtype.

tag_attrib(Att) :- atm(Att).
tag_attrib((Att = Val)) :- atm(Att), string(Val).

:- doc(html_term/1, "A term which represents HTML or XML code in a
   structured way.  In addition to the structures defined by
   @pred{canonic_html_term/1} or @pred{canonic_xml_term/1}, the
   following structures can be used: @begin{description}

   @item{@bf{begin(}@em{tag}@bf{,}@em{atts}@bf{)}} It translates to the
   start of an HTML environment of name @em{tag} and attributes
   @em{atts}. There exists also a @bf{begin(@em{tag})} structure.
   Useful, in conjunction with the next structure, when including in a
   document output generated by an existing piece of code (e.g.
   @em{tag} = @tt{pre}).  Its use is otherwise discouraged.

   @item{@bf{end(}@em{tag}@bf{)}} Translates to the end of an HTML
   environment of name @em{tag}.

   @item{@bf{start}} Used at the beginning of a document (translates to
   @tt{<html>}).

   @item{@bf{end}} Used at the end of a document (translates to
   @tt{</html>}).

   @item{@tt{--}} Produces a horizontal rule (translates to @tt{<hr>}).

   @item{@bf{\\\\}} Produces a line break  (translates to @tt{<br>}).

   @item{@bf{$}}  Produces a paragraph break (translates to  @tt{<p>}).

   @item{@bf{image(}@em{address}@bf{)}} Used to include an image of
   address (URL) @em{address} (equivalent to @tt{img$[src=}@em{address}@tt{]}).

   @item{@bf{image(}@em{address}@bf{,}@em{atts}@bf{)}} As above with
   the list of attributes @em{atts}.

   @item{@bf{ref(}@em{address}@bf{,}@em{text}@bf{)}} Produces a
   hypertext link, @em{address} is the URL of the referenced resource,
   @em{text} is the text of the reference (equivalent to
   @tt{a([href=}@em{address}@tt{],}@em{text}@tt{)}).

   @item{@bf{label(}@em{name}@bf{,}@em{text}@bf{)}} Labels @em{text} as
   a target destination with label @em{name} (equivalent to
   @tt{a([name=}@em{name}@tt{],}@em{text}@tt{)}).

   @item{@bf{heading(}@em{n}@bf{,}@em{text}@bf{)}} Produces a heading of
   level @em{n} (between 1 and 6), @em{text} is the text to be used as
   heading.  Useful when one wants a heading level relative to another
   heading (equivalent to @tt{h}@em{n}@tt{(}@em{text}@tt{)}).

   @item{@bf{itemize(}@em{items}@bf{)}} Produces a list of bulleted
   items, @em{items} is a list of corresponding HTML terms (translates
   to a @tt{<ul>} environment).

   @item{@bf{enumerate(}@em{items}@bf{)}} Produces a list of numbered
    items, @em{items} is a list of corresponding HTML terms (translates
    to a @tt{<ol>} environment).

   @item{@bf{description(}@em{defs}@bf{)}} Produces a list of defined
   items, @em{defs} is a list whose elements are definitions, each of
   them being a Prolog sequence (composed by @tt{','/2} operators). The
   last element of the sequence is the definition, the other (if any)
   are the defined terms (translates to a @tt{<dl>} environment).

   @item{@bf{preformatted(}@em{text}@bf{)}} Used to include preformatted
   text, @em{text} is a list of HTML terms, each element of the list
   being a line of the resulting document (translates to a @tt{<pre>}
   environment).

   @item{@bf{verbatim(}@em{text}@bf{)}} Used to include text verbatim,
   special HTML characters (@tt{<,>,&,""} and space) are translated into its
   quoted HTML equivalent.

   @item{@bf{prolog_term(}@em{term}@bf{)}} Includes any prolog term
   @em{term}, represented in functional notation.  Variables are output
   as @tt{_}.

   @item{@bf{nl}} Used to include a newline in the HTML source (just to
   improve human readability).

   @item{@bf{entity(}@em{name}@bf{)}} Includes the entity of name
   @em{name} (ISO-8859-1 special character).

   @item{@bf{start_form(}@em{addr}@bf{,}@em{atts}@bf{)}} Specifies the
   beginning of a form. @em{addr} is the address (URL) of the program
   that will handle the form, and @em{atts} other attributes of the
   form, as the method used to invoke it. If @em{atts} is not present
   (there is only one argument) the method defaults to POST.

   @item{@bf{start_form}} Specifies the beginning of a form without
   assigning address to the handler.

   @item{@bf{end_form}} Specifies the end of a form.

   @item{@bf{checkbox(}@em{name}@bf{,}@em{state}@bf{)}} Specifies an
   input of type @tt{checkbox} with name @em{name}, @em{state} is
   @tt{on} if the checkbox is initially checked.

   @item{@bf{radio(}@em{name}@bf{,}@em{value}@bf{,}@em{selected}@bf{)}}
   Specifies an input of type @tt{radio} with name @em{name} (several
   radio buttons which are interlocked must share their name),
   @em{value} is the the value returned by the button, if
   @em{selected}=@em{value} the button is initially checked.

   @item{@bf{input(}@em{type}@bf{,}@em{atts}@bf{)}} Specifies an input
   of type @em{type} with a list of attributes @em{atts}.  Possible
   values of @em{type} are @tt{text}, @tt{hidden}, @tt{submit},
   @tt{reset}, \ldots

   @item{@bf{textinput(}@em{name}@bf{,}@em{atts}@bf{,}@em{text}@bf{)}}
   Specifies an input text area of name @em{name}. @em{text} provides
   the default text to be shown in the area, @em{atts} a list of
   attributes.

   @item{@bf{option(}@em{name}@bf{,}@em{val}@bf{,}@em{options}@bf{)}}
   Specifies a simple option selector of name @em{name}, @em{options}
   is the list of available options and @em{val} is the initial
   selected option (if @em{val} is not in @em{options} the first item
   is selected by default) (translates to a @tt{<select>} environment).

   @item{@bf{menu(}@em{name}@bf{,}@em{atts}@bf{,}@em{items}@bf{)}}
   Specifies a menu of name @em{name}, list of attributes @em{atts}
   and list of options @em{items}. The elements of the list @em{items}
   are marked with the prefix operator @tt{$} to indicate that they
   are selected (translates to a @tt{<select>} environment).

   @item{@em{name}@bf{(}@em{text}@bf{)}} A term with functor
   @em{name}/1, different from the special functors defined herein,
   represents an HTML environment of name @em{name} and included text
   @em{text}. For example, the term @begin{verbatim}
   address('clip@@clip.dia.fi.upm.es')
   @end{verbatim} is translated into
   the HTML source @begin{verbatim}
   <address>clip@@clip.dia.fi.upm.es</address>
   @end{verbatim}

   @item{@em{name}@bf{(}@em{atts}@bf{,}@em{text}@bf{)}} A term with
   functor @em{name}/2, different from the special functors defined
   herein, represents an HTML environment of name @em{name}, attributes
   @em{atts} and included text @em{text}. For example, the term
   @begin{verbatim}

   a([href='http://www.clip.dia.fi.upm.es/'],\"Clip home\")
   @end{verbatim} represents the HTML source @begin{verbatim}
   <a href=\"http://www.clip.dia.fi.upm.es/\">Clip home</a>
   @end{verbatim}

   @end{description}
  ").


:- prop html_term(HTMLTerm) + regtype
    # "@var{HTMLTerm} is a term representing HTML code.".

html_term(_).

% ---------------------------------------------------------------------------

% The idea is to have a CIAO/Prolog syntax description of a document as
% a term and then use html2terms/2 to translate the term into a string for
% writing and to translate a string to a term for reading

:- pred html_expansion(Term,Expansion)
    # "Hook predicate to define macros.  Expand occurrences of
      @var{Term} into @var{Expansion}, in @pred{output_html/1}.
      Take care to not transform something into itself!".

:- multifile html_expansion/2.

html_expansion(bf(X),b(X)).
html_expansion(it(X),i(X)).

:- doc(output_html(HTMLTerm), "Outputs @var{HTMLTerm}, interpreted
   as an @pred{html_term/1}, to current output stream.").

:- pred output_html(+html_term).

% Translate html format and send to current output
output_html(F) :-
    html_str(F,T,[]),
    write_string(T).

% HTML <-> Terms translation

:- doc(html2terms(String,Terms), "@var{String} is a character list
   containing HTML code and @var{Terms} is its prolog structured
   representation.").

:- pred html2terms(-string,+html_term)
    # "Translates an HTML-term into the HTML code it represents.".
:- pred html2terms(+string,?canonic_html_term)
    # "Translates HTML code into a structured HTML-term.".

% TODO: rename by html_term_string/2 and switch arg order?
html2terms(Chars, Terms) :-
    var(Chars), !,
    html_str(Terms, Chars, []).
html2terms(Chars, Terms) :-
    parse_html([], Terms, [], Chars, []).

% XML <-> Terms translation

:- doc(xml2terms(String,Terms), "@var{String} is a character list
   containing XML code and @var{Terms} is its prolog structured
   representation.").

:- pred xml2terms(-string,+html_term)
    # "Translates a XML-term into the XML code it represents.".
:- pred xml2terms(+string,?canonic_xml_term)
    # "Translates XML code into a structured XML-term.".

xml2terms(Chars, Terms) :-
    var(Chars), !,
    html_str(Terms, Chars, []). % Uses the same as HTML
xml2terms(Chars, Terms) :-
    parse_xml([], Terms, [], Chars, []).

% ---------------------------------------------------------------------------
%% Terms -> HTML/XML translation %%

html_str(X) --> {var(X)}, !,
    "<b>**Warning: free variable**</b>".
html_str(T) --> {html_expansion(T,NT)}, !,
    html_str(NT).
html_str(start) --> !, "<html>".
html_str(end)   --> !, "</html>".
html_str(--)  --> !, newline, "<hr>", newline.
html_str(\\) --> !, "<br>", newline.
html_str($)  --> !, newline, "<p>".
html_str(comment(C)) --> !,
    "<!-- ",atomic_or_string(C)," -->",
    newline.
html_str(declare(C)) --> !,
    "<!",atomic_or_string(C),">",
    newline.
% XML declaration
html_str(xmldecl(Atts)) --> !,
    "<?xml",
    html_atts(Atts),
    "?>".
html_str(image(Addr)) --> !,
    "<img",
    html_atts([src=Addr]),
    ">".
html_str(image(Addr,Atts)) --> !,
    "<img",
    html_atts([src=Addr|Atts]),
    ">".
html_str(ref(Addr,Text)) --> !,
    "<a",
    html_atts([href=Addr]),
    ">",
    html_str(Text),
    "</a>".
html_str(label(Label,Text)) --> !,
    "<a",
    html_atts([name=Label]),
    ">",
    html_str(Text),
    "</a>".
html_str(heading(L,X)) -->
    {number_codes(L,[N])}, !,
    html_env([0'h,N],X),
    newline.
html_str(itemize(L)) --> !,
    "<ul>",
    newline,
    html_items(L),
    "</ul>".
html_str(enumerate(L)) --> !,
    "<ol>",
    newline,
    html_items(L),
    "</ol>".
html_str(description(L)) --> !,
    "<dl>",
    newline,
    html_descriptions(L),
    "</dl>".
html_str(preformatted(X)) --> !,
    "<pre>",
    newline,
    preformatted_lines(X),
    "</pre>".
html_str(entity(Name)) --> !,
    "&",atomic_or_string(Name),";".
% Forms
html_str(start_form) --> !,
    "<form",
    html_atts([method="POST"]),
    ">",
    newline.
html_str(start_form(Addr)) --> !, 
    "<form",
    html_atts([method="POST", action=Addr]),
    ">",
    newline.
html_str(start_form(Addr,Atts)) --> !, 
    "<form",
    html_atts([action=Addr|Atts]),
    ">",
    newline.
html_str(end_form) --> !,
    "</form>", newline.
html_str(checkbox(Name,on)) --> !,
    "<input",
    html_atts([name=Name,type=checkbox,checked]),
    ">".
html_str(checkbox(Name,_)) --> !,
    "<input",
    html_atts([name=Name,type=checkbox]),
    ">".
html_str(radio(Name,Value,Value)) --> !,
    "<input",
    html_atts([name=Name,type=radio,value=Value,checked]),
    ">".
html_str(radio(Name,Value,_)) --> !,
    "<input",
    html_atts([name=Name,type=radio,value=Value]),
    ">".
html_str(input(Type,Atts)) --> !,
    "<input",
    html_atts([type=Type|Atts]),
    ">".
html_str(textinput(Name,Atts,Text)) --> !,
    "<textarea",
    html_atts([name=Name|Atts]),
    ">",
    textarea_data(Text),
    "</textarea>".
html_str(menu(Name,Atts,Items)) --> !,
    "<select",
    html_atts([name=Name|Atts]),
    ">", newline,
    html_options(Items),
    "</select>".
html_str(option(Name,Val,Options)) --> !,
    "<select",
    html_atts([name=Name]),
    ">", newline,
    html_one_option(Options, Val),
    "</select>".
html_str(prolog_term(T)) --> !,
    prolog_term(T).
% Constructs
html_str(verbatim(Text)) --> !,
    html_quoted(Text).
html_str(nl) --> !, newline. % Just to improve HTML source readability
html_str([]) --> !.
html_str([E|Es]) --> !,
    html_str(E),
    html_str(Es).
html_str(begin(T)) --> {atom(T), atom_codes(T,TS)}, !,
    "<",string(TS),">".
html_str(begin(T,Atts)) --> {atom(T), atom_codes(T,TS)}, !,
    "<",string(TS),
    html_atts(Atts),
    ">".
html_str(end(T)) --> {atom(T), atom_codes(T,TS)}, !,
    "</",string(TS),">".
html_str(env(Name,Atts,Text)) --> {atom(Name), atom_codes(Name,NS)}, !,
    html_env_atts(NS,Atts,Text).
html_str(T$Atts) --> {atom(T), atom_codes(T,TS)}, !,
    "<",string(TS),
    html_atts(Atts),
    ">".
% XML empty element
html_str(elem(N,Atts)) --> {atom(N), atom_codes(N,NS)}, !,
    "<",string(NS),
    html_atts(Atts),
    "/>".
html_str(F) --> {F =.. [Env,X], atom_codes(Env, ES)}, !,
    html_env(ES,X).
html_str(F) --> {F =.. [Env,Atts,X], atom_codes(Env, ES)}, !,
    html_env_atts(ES,Atts,X).
html_str(C) --> {integer(C), C >= 0, C =< 255}, !, [C].
html_str(T) -->
    prolog_term(T).

newline --> [10].

html_atts([]) --> [].
html_atts([A|As]) -->
    " ",
    html_att(A),
    html_atts(As).

html_att(A=V) --> {atom_codes(A,AS)}, !,
    string(AS),"=""",html_quoted_quote(V),"""".
html_att(A) -->  {atom_codes(A,AS)},
    string(AS).

html_quoted_quote(T) -->
    { atomic(T), \+ T = "" -> name(T,TS) ; TS = T },
    html_quoted_quote_chars(TS).

html_quoted_quote_chars([]) --> [].
html_quoted_quote_chars([C|T]) -->
    html_quoted_quote_char(C),
    html_quoted_quote_chars(T).

html_quoted_quote_char(0'") --> !, "&quot;".
html_quoted_quote_char(C)   --> [C].

html_env(E,I) -->
    "<",string(E),">",
    html_str(I),
    "</",string(E),">".

html_env_atts(E,Atts,I) -->
    "<",string(E),
    html_atts(Atts),
    ">",
    html_str(I),
    "</",string(E),">".

html_items([]) --> [].
html_items([It|Its]) -->
    "<li>",
    html_str(It),
    "</li>",
    newline,
    html_items(Its).

html_descriptions([]) --> [].
html_descriptions([D|Ds]) -->
    html_description(D),
    html_descriptions(Ds).

html_description((T,D)) --> !,
    "<dt>",
    html_str(T),
    "</dt>",
    newline,
    html_description(D).
html_description(D) -->
    "<dd>",
    html_str(D),
    "</dd>",
    newline.

preformatted_lines([]) --> [].
preformatted_lines([X|Xs]) -->
    html_str(X),
    newline,
    preformatted_lines(Xs).

html_options([]) --> [].
html_options([Op|Ops]) -->
    html_option(Op),
    newline,
    html_options(Ops).

html_option($Op) --> !,
    "<option selected>",html_quoted(Op),"</option>".
html_option(Op) -->
    "<option>",html_quoted(Op),"</option>".

html_one_option([], _) --> [].
html_one_option([Op|Ops], Sel) -->
    "<option",
    html_one_option_sel(Op, Sel),
    ">",html_quoted(Op),"</option>",
    newline,
    html_one_option(Ops, Sel).

html_one_option_sel(Op, Op) --> !, " selected".
html_one_option_sel(_, _) --> "".

html_quoted(T) -->
    {atomic(T) -> name(T,TS) ; TS = T},
    html_quoted_chars(TS).

html_quoted_chars([]) --> [].
html_quoted_chars([C|T]) -->
    html_quoted_char(C),
    html_quoted_chars(T).

html_quoted_char(0'>) --> !, "&gt;".
html_quoted_char(0'<) --> !, "&lt;".
html_quoted_char(0'&) --> !, "&amp;".
html_quoted_char(0'") --> !, "&quot;".
html_quoted_char(0'\n)--> !, "<br>".
html_quoted_char(C)   --> [C].

prolog_term(V) -->
    {var(V)}, !, "_".
prolog_term(T) -->
    {functor(T,F,A), atom_codes(F,FS)},
    string(FS), prolog_term_maybe_args(A,T).

prolog_term_maybe_args(0,_) --> !, "".
prolog_term_maybe_args(A,T) -->
    "(",
    prolog_term_args(1,A,T),
    ")".

prolog_term_args(N, N, T) --> !,
    {arg(N,T,A)},
    prolog_term(A).
prolog_term_args(N, M, T) -->
    {arg(N,T,A)},
    prolog_term(A),
    ",",
    {N1 is N+1},
    prolog_term_args(N1,M,T).

textarea_data('$empty') --> [], !.
textarea_data(X) -->
    {atomic(X), name(X,S)}, !,
    string(S).
textarea_data(L) -->
    html_lines(L), !.
textarea_data(S) -->
    string(S).

% ---------------------------------------------------------------------------

:- doc(html_template(Chars, Terms, Dict), "Interprets @var{Chars} as
   an HTML template returning in @var{Terms} the corresponding
   structured HTML-term, which includes variables, and unifying
   @var{Dict} with a dictionary of those variables (an incomplete list of
   @em{name}@tt{=}@em{Var} pairs).  An HTML template is standard HTML
   code, but in which ``slots'' can be defined and given an identifier.
   These slots represent parts of the HTML code in which other HTML code
   can be inserted, and are represented in the HTML-term as free
   variables.  There are two kinds of variables in templates:
   @begin{itemize}

   @item Variables representing page contents.  A variable with name
   @em{name} is defined with the special tag @tt{<V>}@em{name}@tt{</V>}.

   @item Variables representing tag attributes.  They occur as an
   attribute or an attribute value starting with @tt{_}, followed by its
   name, which must be formed by alphabetic characters.

   @end{itemize}

   As an example, suposse the following HTML template:
@begin{verbatim}
@includeverbatim{pillow/examples/template.html}
@end{verbatim}
   The following query in the Ciao toplevel shows how the template is
   parsed, and the dictionary returned:
@begin{verbatim}
?- file_to_string('template.html',_S), html_template(_S,Terms,Dict). 

Dict = [bgcolor=_A,content=_B|_],
Terms = [env(html,[],[\"
\",env(body,[bgcolor=_A],[\"
\",_B,\"
\"]),\"
\"]),\"
\"] ? 

yes
@end{verbatim}
  If a dictionary with values is supplied at call time, then variables
  are unified accordingly inside the template:
@begin{verbatim}
?- file_to_string('template.html',_S),
   html_template(_S,Terms,[content=b(\"hello world!\"),bgcolor=\"white\"]). 

Terms = [env(html,[],[\"
\",env(body,[bgcolor=\"white\"],[\"
\",b(\"hello world!\"),\"
\"]),\"
\"]),\"
\"] ? 

yes
@end{verbatim}
").

:- pred html_template(+string,?canonic_html_term,?list).

html_template(Chars, Terms, Dict) :-
    parse_html([], Terms, Dict, Chars, []).

% see a '<' - possible item
parse_html(Stack,NStack,Dict) -->
    "<",
    {tidy_string(Stack, Stack2)},
    html_unit(Stack2,Stack3,Dict), !,
    parse_html(Stack3,NStack,Dict).
% build on an open string
parse_html([Elem|Stack],NStack,Dict) -->
    {nonvar(Elem), Elem = string(S,T)},
    [C], !,
    {T = [C|TT]},
    parse_html([string(S,TT)|Stack],NStack,Dict).
% open a new string
parse_html(Stack,NStack,Dict) -->
    [C], !,
    parse_html([string([C|T],T)|Stack],NStack,Dict).
% base case - close open strings
parse_html(Stack,NStack,_Dict) --> "",
    {tidy_string(Stack,Stack2),
     reverse(Stack2,NStack)}.

% env terminators
html_unit(S,NS,Dict) -->
    "/",
    html_tag(N),
    whitespace0,
    ">",
    { poptokenstack(N,S,NS,Dict) },
    !.
% comment
html_unit(S,[comment(Text)|S],_Dict) -->
    "!--",
    string(Text),
    "-->",
    !.
% declaration
html_unit(S,[declare(Text)|S],_Dict) -->
    "!",
    string(Text),
    ">",
    !.
% items
html_unit(S,[N$A|S],Dict) -->
    html_tag(N),
    html_tag_atts(A,Dict),
    whitespace0,
    ">",
    !.

html_tag(N) -->
    loupalpha(C),
    html_tag_rest(Cs),
    { atom_codes(N,[C|Cs]) }.

html_tag_rest([C|Cs]) -->
    html_tag_char(C), !,
    html_tag_rest(Cs).
html_tag_rest([]) --> "".

html_tag_char(C) --> loupalpha(C), !.
html_tag_char(C) --> digit(C), !.
html_tag_char(0'.) --> ".".
html_tag_char(0'-) --> "-".


html_tag_atts([],_Dict) --> "".
html_tag_atts([A|As],Dict) -->
    whitespace,
    html_tag_att(A,Dict),
    html_tag_atts(As,Dict).

% template variable
html_tag_att(A,Dict) -->
    "_", !, html_tag(N),
    {list_lookup(Dict, (=), N, V)},
    html_opt_value(A, V, Dict).
html_tag_att(A,Dict) -->
    html_tag(N),
    html_opt_value(A, N, Dict).

html_opt_value(N = V, N, Dict) -->
    whitespace0,
    "=",
    whitespace0,
    html_value(V, Dict), !.
html_opt_value(N, N,_Dict) --> "".

% template variable
html_value(V, Dict) -->
    "_", !,
    html_tag(N),
    {list_lookup(Dict, (=), N, V)}.
% html_value(V,_Dict) --> http_lo_up_token(V). % People do not write valid HTML
html_value(V,_Dict) -->
    """", !,
    html_quoted_string(0'",V).
html_value(V,_Dict) -->
    "'", !,
    html_quoted_string(0'',V).
html_value(V,_Dict) --> html_lax_value(V).

html_quoted_string(Q, []) --> [Q], !.
html_quoted_string(Q, [C|Cs]) -->
    [C],
    html_quoted_string(Q, Cs).

html_lax_value([C|Cs]) --> [C], { C \== 0'> , C > 32 },
    html_lax_value(Cs).
html_lax_value([]) --> "".

poptokenstack(EnvTag,Stack,NStack,Dict) :-
    pop_ts(EnvTag,Stack,[],NStack,Dict),
    !.
poptokenstack(EnvTag,Stack,[SlashEnvTag$[]|Stack],_) :-
    atom_concat('/',EnvTag,SlashEnvTag).

pop_ts(EnvTag,[Elem|S],Insides,NS,Dict) :-
    ( nonvar(Elem), Elem = EnvTag$Atts ->
        elem_or_template_var(EnvTag,Atts,Insides,E,Dict),
        NS = [E|S]
    ; pop_ts(EnvTag,S,[Elem|Insides],NS,Dict)
    ).

elem_or_template_var('v',_,[NameS],Var,Dict) :-
    catch(atom_codes(Name,NameS), _, fail),
    list_lookup(Dict, (=), Name, Var),  !.
elem_or_template_var(EnvTag,Atts,Insides,env(EnvTag,Atts,Insides),_).

tidy_string([Elem|Stack],[L|Stack]) :-
    nonvar(Elem), Elem = string(L,T), !, T = [].
tidy_string(Stack,Stack).

% ---------------------------------------------------------------------------
%% XML -> Terms translation %%

parse_xml(Stack,NStack,Dict) -->
    "<",
    {tidy_string(Stack, Stack2)},
    xml_unit(Stack2,Stack3,Dict), !,
    parse_xml(Stack3,NStack,Dict).
% build on an open string
parse_xml([Elem|Stack],NStack,Dict) -->
    {nonvar(Elem), Elem = string(S,T)},
    [C], !,
    {T = [C|TT]},
    parse_xml([string(S,TT)|Stack],NStack,Dict).
% open a new string
parse_xml(Stack,NStack,Dict) -->
    [C], !,
    parse_xml([string([C|T],T)|Stack],NStack,Dict).
% base case - close open strings
parse_xml(Stack,NStack,_Dict) --> "",
    {tidy_string(Stack,Stack2),
     reverse(Stack2,NStack)}.

% env terminators
xml_unit(S,NS,Dict) -->
    "/",
    xml_tag(N),
    whitespace0,
    ">",
    { poptokenstack(N,S,NS,Dict) },
    !.
% comment
xml_unit(S,[comment(Text)|S],_Dict) -->
    "!--",
    string(Text),
    "-->",
    !.
% declaration
xml_unit(S,[declare(Text)|S],_Dict) -->
    "!",
    string(Text),
    ">",
    !.
% xml declarations
xml_unit(S,[xmldecl(Ats)|S],Dict) -->
    "?xml",
    xml_tag_atts(Ats,Dict),
    whitespace0,
    "?>",
    !.
% elements or env beginnings
xml_unit(S,[El|S],Dict) -->
    xml_tag(N),
    xml_tag_atts(A,Dict),
    whitespace0,
    elem_envbeg(El, N, A),
    ">",
    !.

elem_envbeg(elem(N,A), N, A) -->
    "/", !.
elem_envbeg(N$A,N, A) --> "".

xml_tag(N) -->
    xml_tag_start(C),
    xml_tag_rest(Cs),
    { atom_codes(N,[C|Cs]) }.

xml_tag_atts([],_Dict) --> "".
xml_tag_atts([A|As],Dict) -->
    whitespace,
    xml_tag_att(A,Dict),
    xml_tag_atts(As,Dict).

xml_tag_att(N=V,Dict) -->
    xml_tag(N),
    whitespace0,
    "=",
    whitespace0,
    xml_value(V, Dict).

xml_value(V, Dict) -->
    "_", !,
    xml_tag(N),
    {list_lookup(Dict, (=), N, V)}.
xml_value(V,_Dict) -->
    """", !,
    xml_quoted_string(0'",V).
xml_value(V,_Dict) -->
    "'", !,
    xml_quoted_string(0'',V).
xml_value(V,_Dict) --> % This is not correct syntax
    xml_bad_value(V).

xml_quoted_string(Q, []) --> [Q], !.
xml_quoted_string(Q, [0'&,0'q,0'u,0'o,0't,0';|Cs]) -->
    """", !,
    xml_quoted_string(Q, Cs).
xml_quoted_string(Q, [C|Cs]) -->
    [C],
    xml_quoted_string(Q, Cs).

xml_bad_value([]) --> "".
xml_bad_value([C|Cs]) -->
    [C],
    xml_bad_value(Cs).

xml_tag_start(C) --> loalpha(C), !.
xml_tag_start(C) --> upalpha(C), !.
xml_tag_start(0'_) --> "_".
xml_tag_start(0':) --> ":".

xml_tag_rest([C|Cs]) -->
    xml_tag_char(C), !,
    xml_tag_rest(Cs).
xml_tag_rest([]) --> "".

xml_tag_char(C) --> loalpha(C), !.
xml_tag_char(C) --> upalpha(C), !.
xml_tag_char(C) --> digit(C), !.
xml_tag_char(0'_) --> "_".
xml_tag_char(0':) --> ":".
xml_tag_char(0'.) --> ".".
xml_tag_char(0'-) --> "-".

% ---------------------------------------------------------------------------
% TODO: duplicated (or similar) in many modules

atomic_or_string(X) -->
    { atomic(X), name(X,S) }, !,
    string(S).
atomic_or_string(S) -->
    string(S).

loupalpha(C) --> loalpha(C), !.
loupalpha(C) --> upalpha(CU), { C is CU+0'a-0'A }.

loalpha(C) --> [C], {C >= 0'a, C =< 0'z}.

upalpha(C) --> [C], {C >= 0'A, C =< 0'Z}.

digit(C) --> [C], {C >= 0'0, C =< 0'9}.

html_crlf --> [13,10], !.
html_crlf --> [10].

html_line([]) -->
    html_crlf, !.
html_line([X|T]) -->
    [X],
    html_line(T).

html_lines([L|Ls]) -->
    html_line(L), !,
    html_lines(Ls).
html_lines([]) --> "".
