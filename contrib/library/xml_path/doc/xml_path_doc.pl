:- use_package([assertions,regtypes, isomodes]).
:- doc(nodoc,assertions).
:- doc(nodoc,regtypes).
:- doc(nodoc,isomodes).

:- use_module(library(xml_path/xml_path_types)).

% This line has been commented out to allow the documentation can be
% created. -- EMM

%:- include(library(xml_path/xml_path_syntax)).

:- doc(doinclude, canonic_xml_term/1).
:- doc(doinclude, canonic_xml_item/1).
:- doc(doinclude, tag_attrib/1).
:- doc(doinclude, canonic_xml_query/1).
:- doc(doinclude, canonic_xml_subquery/1).

:- doc(title,"XML query library").
:- doc(author, "Jos@'{e} Manuel G@'{o}mez P@'{e}rez").
:- doc(copyright,"@include{DocCopyright.lpdoc}").


:- doc(summary, "XML documents querying library").

:- doc(module, "This package provides a language suitable for
querying XML documents from a Prolog program. Constraint programming
expresions can be included in order to prune search as soon as
possible, i.e. upon constraint unsatisfability, improving
efficiency. Also, facilities are offered to improve search speed by
transforming XML documents into Prolog programs, hence reducing search
to just running the program and taking advantage of Prolog's indexing
capabilities.

Queries in an XML document have a recursive tree structructure that
permits to detail the search on the XML element sought, its
attributes, and its children. As a suffix, a constraint programming
expression can be added. Queries return value for the free variables
included (in case of success), and checks whether the XML document
structure matches that depicted by the query itself.

The operators introduced are described below:

@begin{itemize}


@item @em{@@} Delimits a subquery on an elment's attribute, such as
@tt{product@@val(product_name, \"car\")}, the first argument being the
attribute name and the second its value. Any of them can be free
variables, being possible to write queries like
@tt{product@@val(Name, \"car\")}, intended to find the 'Name' of
attributes of element product whose value is the string \"car\".

@item @em{::} The right-hand side of the subexpression delimited by
this operator is a query on the children elements of the element
described on its left-hand side.

@item @em{with} Declares the constraints the items sought must satisfy.

@end{itemize}

Some examples of this query language (more can be found in the
examples directory):

@begin{itemize}
@item Example A:
@end{itemize}

@noindent
@begin{verbatim}
@includeverbatim{xml_path/doc/exA}
@end{verbatim}

@begin{itemize}
@item Example B:
@end{itemize}

@noindent
@begin{verbatim}
@includeverbatim{xml_path/doc/exB}
@end{verbatim}

").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XML SEARCH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred xml_search(+Query, +Source, -Doc) : canonic_xml_query *
   canonic_xml_item * canonic_xml_item # "Checks a high level query
   @var{Query} against an XML document @var{Source}. If the query is
   successful it retuns in @var{Doc} the whole xml element(s) of the
   document that matched it.".

xml_search(_UserQuery, _SourceDoc, _Doc).

:- pred xml_parse(+Query, +Source, -Doc) : canonic_xml_query *
     canonic_xml_item * canonic_xml_item # "Checks a high level query
     @var{Query} against an XML document @var{Source}. If the query is
     successful it retuns in @var{Doc} the whole xml element(s) of the
     document that matched it. On the contrary as @pred{xml_search/3},
     the query can start at any level of the XML document, not
     necessarily at the root node.".

xml_parse(_Query, _SourceDoc, _SubDoc) .

:- pred xml_parse_match(+Query, +Source, -Match) : canonic_xml_query *
       canonic_xml_item * canonic_xml_item # "Checks a high level
       query @var{Query} against an XML document @var{Source}. If the
       query is successful it retuns in @var{Doc} the exact subtree of
       the xml document that matched it. On the contrary as
       @pred{'$xml_search_match/3}, the query can start at any level
       of the XML document, not necessarily at the root node.".

xml_parse_match(_Query, _SourceDoc, _Match) .

:- pred xml_search_match(+BasicQuery, +SourceDoc, -Match) :
      canonic_xml_query * canonic_xml_item * canonic_xml_item #
      "Checks query @var{Query} against an XML document
      @var{Source}. If the query is successful it retuns in @var{Doc}
      the exact subtree of the xml document that matched it.".

xml_search_match(_BasicQuery, _SourceDoc, _Match).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XML INDEXING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Retrieving data:

:- pred xml_index_query(+Query, -Id, -Match) : canonic_xml_query * atm
	* canonic_xml_item # " Matches a high level query @var{Query}
	against an XML document previously transformed into a Prolog
	program. @var{Id} identifies the resulting document
	@var{Match}, which is the exact match of the query against the
	XML document. ".

xml_index_query(_Query, _Id, _Doc) .


% Generating xml index: 

:- pred xml_index_to_file(SourceDoc, File) : canonic_xml_item * atm # "Transforms
   the XML document @var{SourceDoc} in a Prolog program which is
   output to file @var{File}.".

xml_index_to_file(_SourceDoc, _File) .


:- pred xml_index(SourceDoc) : canonic_xml_item # "Transforms the XML
   document @var{SourceDoc} in a Prolog program, generating the
   associated clauses, which are stored dynamically into the current
   process memory space.".

xml_index(_SourceDoc) .


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XML QUERY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred xml_query(+Query, +Doc, -Match) : canonic_xml_query *
  canonic_xml_item * canonic_xml_item # "Checks that XML document
  @var{Doc} is compliant with respect to the query @var{Query}
  expressed in the low level query language. The exact mapping of the
  query over the document is returned in @var{Match}".

xml_query(_Query, _Doc, _Match) .
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRANSFORMATION
