:- module(_, [], [assertions, regtypes, isomodes, hiord, regexp]).

:- doc(title, "Text Templates").

:- doc(author, "Jose F. Morales").
:- doc(author, "Ciao development team").

:- doc(module, "A template is a text marked-up with special tags that
   contain expressions. Template evaluation replaces tags with the
   evaluation of such expressions, w.r.t. a substitution of
   parameters.

   Tags must have the form @tt{@{@{}@em{Expr}@tt{@}@}}, where
   @em{Expr} is a parameter name.

   Example: given the file @tt{n.txt.skel}:
@begin{verbatim}
Dear {{Author}},

on behalf of the program committee of {{Conf}}, we are pleased to
inform you that your paper

{{Title}}

has been accepted for inclusion in the post-conference proceedings.
@end{verbatim}

   then the query:

@begin{verbatim}
?- eval_template_file('n.txt.skel', [
     'Author' = 'Alain',
     'Title' = 'The Birth of Prolog',
     'Conf' = 'CACM 93'
   ], 'n.txt').
@end{verbatim}

   produces the following text:

@begin{verbatim}
Dear Alain,

on behalf of the program committee of CACM 93, we are pleased to
inform you that your paper

The Birth of Prolog

has been accepted for inclusion in the post-conference proceedings.
@end{verbatim}
").

:- doc(bug, "Extend tag expression language. Probably we could turn
   inside-out a DCG goal to get both good performance and features.").
:- doc(bug, "The implementation can be improved.").

:- use_module(library(lists), [append/3]).
:- use_module(library(file_utils),
	[file_to_string/2, string_to_file/2]).
:- use_module(library(dict)).

:- export(eval_template_file/3).
:- pred eval_template_file(InFile, Subst, OutFile) # "Like
   @pred{eval_template_string/3}, using files.".

eval_template_file(InFile, Subst, OutFile) :-
	file_to_string(InFile, InString),
	eval_template_string(InString, Subst, OutString),
	string_to_file(OutString, OutFile).

:- export(eval_template_string/3).
:- pred eval_template_string(In, Subst, Out) # "Evaluate the template
   string @var{In} to generate @var{Out} using the values in
   @var{Subst}.".

eval_template_string(Str, Subst, Str2) :-
	params_to_dic(Subst, Dic),
	eval_template_string_(Dic, Str, Str2).

% Replace "{{Key}}" strings in input by values from Dic
eval_template_string_(Dic, Str, Str2) :-
 	parse_key(Str, Before, Key, After), !,
 	( dic_get(Dic, Key, Value) ->
	    % TODO: what about Value=[]?
 	    ( string(Value) -> ValueStr = Value
 	    ; atom(Value) -> atom_codes(Value, ValueStr)
 	    ; throw(unknown_value_in_replace_params(Value)) % TODO: good error?
 	    ),
 	    append(Before, S0, Str2),
 	    append(ValueStr, S1, S0)
 	; % not found, leave unchanged
 	  % TODO: should it complain instead?
 	  append(Before, S0, Str2),
	  atom_codes(Key, KeyStr),
 	  append("{{"||KeyStr, "}}"||S1, S0)
	),
 	eval_template_string_(Dic, After, S1).
eval_template_string_(_, Str, Str).

% Find the first match of {{Key}} in Str
parse_key(Str, Before, Key, After) :-
	append(Before, "{{"||Str0, Str),
	append(KeyStr, "}}"||After, Str0),
 	atom_codes(Key, KeyStr),
 	!.

params_to_dic([], _).
params_to_dic([K=V|KVs], Dic) :-
	dic_lookup(Dic, K, V),
	params_to_dic(KVs, Dic).

