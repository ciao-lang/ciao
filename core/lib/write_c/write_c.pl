:- module(write_c, [write_c/4], [dcg, assertions, hiord]).

:- use_module(library(write_c/write_tokens)).

:- doc(title, "The C language in Prolog terms.").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides predicates for writing C
	programs expressed with Prolog terms.").
	
:- op(550, xfx, [($)]). % _$_ notation is private.

% -----------------------------------------------------------------------------

write_c(Xs, Prefix, N0, N1) :-
	write_c_2(Xs, Prefix, N0, N1).

write_c_2([], _, N, N) :- !.
write_c_2([X|Xs], Prefix, N0, N2) :-
	name_vars(X, Prefix, N0, N1),
	declaration(X, Y, [format(new_line)]),
	write_tokens(Y),
	write_c_2(Xs, Prefix, N1, N2).

% -----------------------------------------------------------------------------

name_vars(X, Prefix, N1, N2) :-
	functor(X, _, A), !,
	name_vars_2(1, A, X, Prefix, N1, N2). 
name_vars(X, Prefix, N1, N2) :-
	var(X), !,
 	N2 is N1 + 1,
	X = identifier("~w__id~w", [Prefix, N1]).
% name_vars(X, Prefix, N1, N2) :-
% 	var(X), !,
%  	N2 is N1 + 1,
% 	number_codes(N1, C),
%  	atom_codes(Suffix, "__id"||C),
% 	atom_concat(Prefix, Suffix, X).
name_vars(_, _, N, N).

name_vars_2(I, N, X, Prefix, N1, N3) :-
	I =< N, !,
	I1 is I + 1,
	arg(I, X, A),
	name_vars(A, Prefix, N1, N2),
	name_vars_2(I1, N, X, Prefix, N2, N3).
name_vars_2(_, _, _, _, N, N). 

% -----------------------------------------------------------------------------

declaration(X) -->
	directive(X), !. % Extended C grammar !!!
declaration(X) -->
	function_declaration(X), !.
declaration(X) -->
	variable_declaration(X).

function_declaration(X) --> { canonize_declaration(X, Y) }, !,
	function_declaration(Y).
function_declaration(X$Y) -->
	specifiers(declaration, X), declarator(function, _, _, Y).

variable_declaration(X) --> { canonize_declaration(X, Y) }, !,
	variable_declaration(Y).
variable_declaration(X$Y) --> { \+ zero_sum(X) },
	specifiers(declaration, X), declarator(abstract, _, _, Y), !,
	[format(glue), ';'].
variable_declaration(X$Y) --> { \+ zero_sum(X) },
	specifiers(declaration, X),
	declarator(multiple(variable), named, _, Y), [format(glue), ';'].

struct_variable_declaration(X) --> { canonize_declaration(X, Y) }, !,
	struct_variable_declaration(Y).
struct_variable_declaration(X$Y) --> { \+ zero_sum(X) },
	specifiers(type, X),
	declarator(multiple(struct_variable), _, _, Y), [format(glue), ';'].

type_name_declaration(X) --> { canonize_declaration(X, Y) }, !,
	type_name_declaration(Y).
type_name_declaration(X$Y) --> { \+ zero_sum(X) },
	specifiers(type, X), declarator(variable, abstract, _, Y).

parameter_declaration(X) --> { canonize_declaration(X, Y) }, !,
	parameter_declaration(Y).
parameter_declaration(X$Y) --> { \+ zero_sum(X) }, 
	specifiers(declaration, X), declarator(variable, _, _, Y).

pointer_type_declaration(X, Mode) --> { canonize_declaration(X, Y) }, !,
	pointer_type_declaration(Y, Mode).
pointer_type_declaration(X$Y, Mode) -->
	specifiers(qualifier, X),
	declarator(variable, Mode, uninitialized, Y).

% -----------------------------------------------------------------------------

% Fails if yet canonized or anything was wrong.
canonize_declaration(_$_, _) :- !, fail.
canonize_declaration(X0:Y0#I, Y$Z#I) :- !,
	canonize_declaration(X0:Y0, Y$Z).
canonize_declaration(X0:Y0, Y$Z) :- !,
	split_specifiers(Y0, A, B0),
	flatten_sum(X0, X),
	flatten_sum(B0, B),
	canonize_2(B$X, Y1$Z),
	flatten_sum(A+Y1, Y).
canonize_declaration(X, Y) :-
	canonize_declaration(0:X, Y).

flatten_sum(A + B, D) :- !,
	flatten_sum(B, C),
	flatten_sum_2(A, C, D).
flatten_sum(A, A).

flatten_sum_2(A + B, C, D) :- !,
	flatten_sum_2(B, C, E),
	flatten_sum_2(A, E, D).
flatten_sum_2(0, B, B) :- !.
flatten_sum_2(A, 0, A) :- !.
flatten_sum_2(A, B, A + B).

canonize_2(X, Y) :-
	canonize_3(X, Z), !,
	canonize_2(Z, Y).
canonize_2(X, X).

canonize_3(X0$Y0, X$Y) :-
	is_declarator_1(X0, X, Y, Y0), !.
canonize_3(X0$Y0, X$Y) :-
	get_specifiers_and_declarator(X0, S0, D),
	nonvar(D),
	flatten_sum(S0, S),
	is_declarator_2(D, X, Y, S$Y0).

get_specifiers_and_declarator(X + Y, 0 + Z, X) :-
	is_declarator_2(X, _, _, _), !,
	get_specifiers_and_declarator(Y, Z, X).
get_specifiers_and_declarator(X + Y, X + Z, D) :- !, 
	get_specifiers_and_declarator(Y, Z, D).
get_specifiers_and_declarator(X, 0, X) :-
	is_declarator_2(X, _, _, _), !.
get_specifiers_and_declarator(X, X, _).

split_specifiers(X0 + Y0, X1 + Y1, X2 + Y2) :- !,
	split_specifiers(X0, X1, X2),
	split_specifiers(Y0, Y1, Y2).
split_specifiers(X, X, 0) :-
	is_storage_class_specifier(X), !.
split_specifiers(X, 0, X).

is_declarator_1(bitfield(X, Y), Y, bitfield(X, Z), Z).
is_declarator_1(array(X), X, array(Y), Y).
is_declarator_1(array(X, Y), Y, array(X, Z), Z).
is_declarator_1(function(X, Y), Y, function(X, Z), Z).
is_declarator_1(function(X, Y, Z), Z, function(X, Y, W), W).

is_declarator_2(pointer(X), X, pointer(Y), Y).

% -----------------------------------------------------------------------------

variable_declaration_list(X) -->
	list(X, variable_declaration, format(new_line)).

struct_variable_declaration_list(X) -->
	['{', format(inner), format(new_line)],
	nonempty_list(X, struct_variable_declaration, format(new_line)),
	[format(outer), format(new_line), '}'].

parameter_declaration_list(at_least(X)) --> !,
	list(X, parameter_declaration, ','), [',', '...'].
parameter_declaration_list(X) -->
	list(X, parameter_declaration, ',').

% -----------------------------------------------------------------------------

specifiers(declaration, X) --> !,
	declaration_specifier_list(X).
specifiers(type, X) --> !,
	type_specifier_or_qualifier_list(X).
specifiers(qualifier, X) --> !,
	type_qualifier_list(X).

declaration_specifier_list(X) -->
	sum(X, declaration_specifier).

declaration_specifier(X) -->
	storage_class_specifier(X), !.
declaration_specifier(X) -->
	type_specifier(X), !.
declaration_specifier(X) -->
	type_qualifier(X), !.
declaration_specifier(X) -->
	function_specifier(X).

type_specifier_or_qualifier_list(X) -->
	sum(X, type_specifier_or_qualifier).

type_specifier_or_qualifier(X) -->
	type_specifier(X), !.
type_specifier_or_qualifier(X) -->
	type_qualifier(X).

type_qualifier_list(X) -->
	sum(X, type_qualifier).

type_qualifier(X) --> { is_type_qualifier(X), ! },
	[X].

is_type_qualifier(const).
is_type_qualifier(restrict).
is_type_qualifier(volatile).

function_specifier(X) --> { is_function_specifier(X), ! },
	[X].

is_function_specifier(inline).

storage_class_specifier(X) --> { is_storage_class_specifier(X), ! },
	[X].

is_storage_class_specifier(typedef).
is_storage_class_specifier(extern).
is_storage_class_specifier(static).
is_storage_class_specifier(auto).
is_storage_class_specifier(register).

type_specifier(X) --> { is_type_specifier(X), ! },
	[X].
type_specifier(X) --> 
	struct_or_union_specifier(X), !.
type_specifier(X) --> 
	enum_specifier(X), !.
type_specifier(X) -->
	typedef_specifier(X).

is_type_specifier(void).
is_type_specifier(char).
is_type_specifier(short).
is_type_specifier(int).
is_type_specifier(long).
is_type_specifier(float).
is_type_specifier(double).
is_type_specifier(signed).
is_type_specifier(unsigned).
is_type_specifier('_bool').
is_type_specifier('_complex').
is_type_specifier('_imaginary').

struct_or_union_specifier(struct(X)) -->
	[struct], identifier(X), !.
struct_or_union_specifier(struct(X)) --> !,
	[struct], struct_variable_declaration_list(X).
struct_or_union_specifier(struct(X, Y)) --> !,
	[struct], identifier(X), struct_variable_declaration_list(Y).
struct_or_union_specifier(union(X)) -->
	[union], identifier(X), !.
struct_or_union_specifier(union(X)) --> !,
	[union], struct_variable_declaration_list(X).
struct_or_union_specifier(union(X, Y)) --> !,
	[union], identifier(X), struct_variable_declaration_list(Y).

enum_specifier(enum(X)) -->
	[enum], identifier(X), !.
enum_specifier(enum(X)) --> !,
	[enum], enumerator_list(X).
enum_specifier(enum(X, Y)) -->
	[enum], identifier(X), enumerator_list(Y).

typedef_specifier(X) -->
	identifier(X).

% -----------------------------------------------------------------------------

enumerator_list(X) -->
	['{', format(glue)],
	nonempty_list(X, enumerator, ','),
	[format(glue), '}'].

enumerator(X # Y) --> !,
	enumeration_constant(X), ['='], expression(conditional, Y).
enumerator(X) -->
	enumeration_constant(X).

enumeration_constant(X) -->
	identifier(X).

variable_initializer(X) -->
	expression(assignment, X), !.
variable_initializer(aggregate(X)) -->
	aggregate_initializer_list(X), !.

aggregate_initializer_list(X) -->
	['{', format(glue)],
	nonempty_list(X, aggregate_initializer, ','),
	[format(glue), '}'].

aggregate_initializer(X # Y) --> !,
	designator_list(X), ['='], variable_initializer(Y).
aggregate_initializer(X) -->
	variable_initializer(X).

designator_list(X) -->
	{ \+ zero_sum(X) }, 
	sum(X, designator).

designator(element(X)) --> !,
	['[', format(glue)], expression(conditional, X), [format(glue), ']'].
designator(member(X)) -->
	['.'], identifier(X).

% -----------------------------------------------------------------------------

declarator(T, Mode, Initialization, (X, Y)) --> { T=multiple(_) }, !,
	declarator(T, Mode, Initialization, X),
	[format(glue), ','], declarator(T, Mode, Initialization, Y).
declarator(multiple(T), Mode, Initialization, X) --> !,
	declarator(T, Mode, Initialization, X).
declarator(abstract, abstract, uninitialized, 0) --> !.
declarator(function, named, initialized, X # Y) --> !,
	variable_declarator(X, named), compound_statement(Y).
declarator(variable, named, initialized, X # Y) --> !,
	variable_declarator(X, named), ['='], variable_initializer(Y).
declarator(variable, Mode, uninitialized, X) --> !,
	variable_declarator(X, Mode).
declarator(struct_variable, Mode, uninitialized, X) -->
	struct_variable_declarator(X, Mode).

struct_variable_declarator(bitfield(X, 0), abstract) --> !,
	[':'], expression(conditional, X).
struct_variable_declarator(bitfield(X, Y), named) --> !,
	variable_declarator(Y, named), [':'], expression(conditional, X).
struct_variable_declarator(X, named) -->
	variable_declarator(X, named).

variable_declarator(X, Mode) -->
	variable_declarator_2(X, Mode, no).

variable_declarator_2(X, Mode, no) -->
	pointer_declarator_2(X, Mode, yes).

% pointer_declarator(X, Mode) -->
% 	pointer_declarator_2(X, Mode, no).

pointer_declarator_2(pointer(X), Mode, _) --> !,
	['*', format(glue)], pointer_type_declaration(X, Mode).
pointer_declarator_2(X, Mode, Halt) --> 
	function_declarator_2(X, Mode, Halt).

% function_declarator(X, Mode) -->
% 	function_declarator_2(X, Mode, no).

function_declarator_2(function(X, Y), Mode, _) --> !,
	array_declarator(Y, Mode), [format(glue), '(', format(glue)],
	parameter_declaration_list(X), [format(glue), ')'].
function_declarator_2(function(X, Y, Z), named, _) --> !,
	array_declarator(Z, named),
	[format(glue), '(', format(glue)],
	identifier_list(X),
	[format(glue), ')', format(inner2), format(new_line)],
	variable_declaration_list(Y),
	[format(outer2), format(new_line)].
function_declarator_2(X, Mode, Halt) -->
	array_declarator_2(X, Mode, Halt).

array_declarator(X, Mode) --> !,
	array_declarator_2(X, Mode, no).

array_declarator_2(array(X), Mode, _) -->
	array_declarator(X, Mode), !, [format(glue), '[', format(glue), ']'].
array_declarator_2(array(X, Y), Mode, _) --> !,
	array_declarator(Y, Mode), [format(glue), '[', format(glue)],
	expression(assignment, X), [format(glue), ']'].
array_declarator_2(X, Mode, Halt) -->
	name_declarator_2(X, Mode, Halt).

name_declarator_2(0, abstract, _) --> !.
name_declarator_2(X, named, _) -->
	identifier(X), !.
name_declarator_2(X, Mode, Halt) -->
	['(', format(glue)],
	variable_declarator_2(X, Mode, Halt),
	[format(glue), ')'].

% -----------------------------------------------------------------------------

statement(X) -->
	labeled_statement(X), !.
statement(X) -->
	compound_statement(X), !.
statement(X) -->
	selection_statement(X), !.
statement(X) -->
	iteration_statement(X), !.
statement(X) -->
	jump_statement(X), !.
statement(X) -->
	expression_statement(X).

labeled_statement(label(default, X)) --> !,
	[format(label), default, format(glue), ':', format(new_line)],
	statement(X).
labeled_statement(label(X, Y)) --> !,
	[format(label)], identifier(X),
	[format(glue), ':', format(new_line)],
	statement(Y).
labeled_statement(case(X, Y)) -->
	[format(label), case], expression(conditional, X),
	[format(glue), ':', format(new_line)],
	statement(Y).

compound_statement([]) -->
	['{', '}'].
compound_statement(X) -->
	['{', format(inner), format(new_line)],
	nonempty_list(X, block_item, format(new_line)),
	[format(outer), format(new_line), '}'].

block_item(X) -->
	statement(X), !.
block_item(X) -->
	variable_declaration(X).

expression_statement(X) -->
	expression(opt, X), [format(glue), ';'].

selection_statement(if(X, Y)) --> !,
	[if, '(', format(glue)], expression(root, X), [format(glue), ')'],
	statement(Y).
selection_statement(if(X, Y, Z)) --> !,
	[if, '(', format(glue)], expression(root, X), [format(glue), ')'],
	statement(Y), [else], statement(Z).
selection_statement(switch(X, Y))  -->
	[switch, format(glue), '(', format(glue)],
	expression(root, X),
	[format(glue), ')'],
	statement(Y).

iteration_statement(while(X, Y)) --> !,
	[while, '(', format(glue)], expression(root, X), [format(glue), ')'],
	statement(Y).
iteration_statement(do_while(X, Y)) --> !,
	[do], statement(X), [while, '(', format(glue)], expression(root, Y),
	[format(glue), ')', format(glue), ';'].
iteration_statement(for(X, Y, Z, W)) -->
	[for, '(', format(glue)],
	expression(opt, X), !, [format(glue), ';'],
	expression(opt, Y), [format(glue), ';'],
	expression(opt, Z), [format(glue), ')'], statement(W).
iteration_statement(for(X, Y, Z, W)) -->
	[for, '(', format(glue)],
	variable_declaration(X), [format(glue), ';'],
	expression(opt, Y), [format(glue), ';'],
	expression(opt, Z), [format(glue), ')'], statement(W).

jump_statement(goto(X)) --> !,
	[goto], identifier(X), [format(glue), ';'].
jump_statement(continue) --> !,
	[continue], [format(glue), ';'].
jump_statement(break) --> !,
	[break], [format(glue), ';'].
jump_statement(return) --> !,
	[return], [format(glue), ';'].
jump_statement(return(X)) -->
	[return], expression(root, X), [format(glue), ';'].

% -----------------------------------------------------------------------------

expression(opt, !) --> !.
expression(opt, X) --> !,
	expression(root, X).
expression(root, X) --> !,
	root_expression(X, no).
expression(assignment, X) --> !,
	assignment_expression(X, no).
expression(conditional, X) --> !, 
	conditional_expression(X, no).
expression(logical_OR, X) -->  !,
	logical_OR_expression(X, no).
expression(logical_AND, X) --> !,
	logical_AND_expression(X, no).
expression(inclusive_OR, X) --> !,
	inclusive_OR_expression(X, no).
expression(exclusive_OR, X) --> !,
	exclusive_OR_expression(X, no).
expression(bitwise_AND, X) --> !,
	bitwise_AND_expression(X, no).
expression(equality, X) --> !,
	equality_expression(X, no).
expression(relational, X) --> !,
	relational_expression(X, no).
expression(shift, X) --> !,
	shift_expression(X, no).
expression(additive, X) --> !,
	additive_expression(X, no).
expression(multiplicative, X) --> !,
	multiplicative_expression(X, no).
expression(cast, X) --> !,
	cast_expression(X, no).
expression(unary, X) --> !,
	unary_expression(X, no).
expression(postfix, X) --> !,
	postfix_expression(X, no).
expression(primary, X) --> !,
	primary_expression(X, no).

root_expression((X, Y), _) --> !,
	expression(root, X), [format(glue), ','], expression(root, Y).
root_expression(X, no) -->
	assignment_expression(X, yes).

assignment_expression(X, _) -->
	{ assignment_operator(X, Y, Z, W), ! },
	expression(unary, Y), [W], expression(assignment, Z).
assignment_expression(X, Halt) -->
	conditional_expression(X, Halt).

assignment_operator(X=Y, X, Y, '=').
assignment_operator(assign(X * Y), X, Y, '*=').
assignment_operator(assign(X / Y), X, Y, '/=').
assignment_operator(assign(X mod Y), X, Y, '%=').
assignment_operator(assign(X + Y), X, Y, '+=').
assignment_operator(assign(X - Y), X, Y, '-=').
assignment_operator(assign(X << Y), X, Y, '<<=').
assignment_operator(assign(X >> Y), X, Y, '>>=').
assignment_operator(assign(X /\ Y), X, Y, '&=').
assignment_operator(assign(X # Y), X, Y, '^=').
assignment_operator(assign(X \/ Y), X, Y, '|=').

conditional_expression(conditional(X, Y, Z), _) --> !,
	expression(logical_OR, X), ['?'],
	expression(root, Y), [':'], expression(conditional, Z).
conditional_expression(X, Halt) -->
	logical_OR_expression(X, Halt).

logical_OR_expression(logical(X \/ Y), _) --> !,
	expression(logical_OR, X), ['||'], expression(logical_AND, Y).
logical_OR_expression(X, Halt) -->
	logical_AND_expression(X, Halt).

logical_AND_expression(logical(X /\ Y), _) --> !,
	expression(logical_AND, X), ['&&'], expression(inclusive_OR, Y).
logical_AND_expression(X, Halt) -->
	inclusive_OR_expression(X, Halt).

inclusive_OR_expression(X \/ Y, _) --> !,
	expression(inclusive_OR, X), ['|'], expression(exclusive_OR, Y).
inclusive_OR_expression(X, Halt) -->
	exclusive_OR_expression(X, Halt).

exclusive_OR_expression(X # Y, _) --> !,
	expression(exclusive_OR, X), ['^'], expression(bitwise_AND, Y).
exclusive_OR_expression(X, Halt) -->
	bitwise_AND_expression(X, Halt).

bitwise_AND_expression(X /\ Y, _) --> !,
	expression(bitwise_AND, X), ['&'], expression(equality, Y).
bitwise_AND_expression(X, Halt) -->
	equality_expression(X, Halt).

equality_expression(X == Y, _) --> !,
	expression(equality, X), ['=='], expression(relational, Y).
equality_expression(X \== Y, _) --> !,
	expression(equality, X), ['!='], expression(relational, Y).
equality_expression(X, Halt) -->
	relational_expression(X, Halt).

relational_expression(X < Y, _) --> !,
	expression(relational, X), ['<'], expression(shift, Y).
relational_expression(X > Y, _) --> !,
	expression(relational, X), ['>'], expression(shift, Y).
relational_expression(X =< Y, _) --> !,
	expression(relational, X), ['<='], expression(shift, Y).
relational_expression(X >= Y, _) --> !,
	expression(relational, X), ['>='], expression(shift, Y).
relational_expression(X, Halt) -->
	shift_expression(X, Halt).

shift_expression(X << Y, _) --> !,
	expression(shift, X), ['<<'], expression(additive, Y).
shift_expression(X >> Y, _) --> !,
	expression(shift, X), ['>>'], expression(additive, Y).
shift_expression(X, Halt) -->
	additive_expression(X, Halt).

additive_expression(X + Y, _) --> !,
	expression(additive, X), ['+'], expression(multiplicative, Y).
additive_expression(X - Y, _) --> !,
	expression(additive, X), ['-'], expression(multiplicative, Y).
additive_expression(X, Halt) -->
	multiplicative_expression(X, Halt).

multiplicative_expression(X * Y, _) --> !,
	expression(multiplicative, X), ['*'], expression(cast, Y).
multiplicative_expression(X / Y, _) --> !,
	expression(multiplicative, X), ['/'], expression(cast, Y).
multiplicative_expression(X mod Y, _) --> !,
	expression(multiplicative, X), ['%'], expression(cast, Y).
multiplicative_expression(X, Halt) -->
	cast_expression(X, Halt).

cast_expression(cast(X, Y), _) --> !,
	['(', format(glue)],
	type_name_declaration(X),
	[format(glue), ')', format(glue)],
	expression(cast, Y).
cast_expression(X, Halt) -->
	unary_expression(X, Halt).

unary_expression(++X, _) --> !,
	['++', format(glue)], expression(unary, X).
unary_expression(--X, _) --> !,
	['--', format(glue)], expression(unary, X).
unary_expression(X, _) -->
	{ unary_operator(X, Y, Z), ! },
	[Z, format(glue)], expression(cast, Y).
unary_expression(size(X), _) -->
	[sizeof],
	[format(glue), '(', format(glue)],
	type_name_declaration(X), !,
	[format(glue), ')'].
unary_expression(size(X), _) --> !,
	[sizeof], expression(unary, X).
unary_expression(X, Halt) -->
	postfix_expression(X, Halt).

unary_operator(address(X), X, '&').
unary_operator('^'(X), X, '*').
unary_operator(+X, X, '+').
unary_operator(-X, X, '-').
unary_operator(\X, X, '~').
unary_operator(logical(\X), X, '!').

postfix_expression(element(X, Y), _) --> !,
	expression(postfix, Y),
	['[', format(glue)], expression(root, X), [format(glue), ']'].
postfix_expression(call(X, Y), _) --> !,
	expression(postfix, X),
	[format(glue), '(', format(glue)],
	argument_expression_list(Y),
	[format(glue), ')'].
postfix_expression(member(X, Y), _) --> !,
	expression(postfix, Y),
	[format(glue), '.', format(glue)], identifier(X).
postfix_expression(indirect_member(X, Y), _) --> !,
	expression(postfix, Y),
	[format(glue), '->', format(glue)], identifier(X).
postfix_expression(post(++X), _) --> !,
	expression(postfix, X),
	[format(glue), '++'].
postfix_expression(post(--X), _) --> !,
	expression(postfix, X),
	[format(glue), '--'].
postfix_expression(aggregate(X, Y), _) --> !,
	['(', format(glue)],
	type_name_declaration(X),
	[format(glue), ')'],
	aggregate_initializer(Y).
postfix_expression(X, Halt) -->
	primary_expression(X, Halt).

argument_expression_list(X) -->
	list(X, assignment_expression_list_2, ',').

assignment_expression_list_2(X) -->
	expression(assignment, X).

primary_expression(quoted(X), _) --> !, % Expanded C grammar !!!
	['(', format(glue)], expression(root, X), [format(glue), ')'].
primary_expression(X, _) --> 
	identifier(X), !.
primary_expression(X, _) --> 
	constant(X), !.
primary_expression(X, _) --> 
	string_literal(X), !.
primary_expression(X, Halt) -->
	['(', format(glue)], root_expression(X, Halt), [format(glue), ')'].

identifier_list(X) -->
	nonempty_list(X, identifier, ',').

identifier(X) --> { is_identifier(X) }, [X].

is_identifier(X) :-
	atom(X), !.
is_identifier(identifier(_, _)).

constant(X) --> { number(X) }, [X].

string_literal(X) --> { transparent_string(X, Y) }, [Y].

transparent_string([], []) :- !.
transparent_string("\n"||Xs, "\\n"||Ys) :- !,
	transparent_string(Xs, Ys).
transparent_string([X|Xs], [X|Ys]) :- integer(X),
	transparent_string(Xs, Ys).

% -----------------------------------------------------------------------------

directive(comment(X)) --> !,
	['/*', X, '*/'].
directive(format(new_line)) --> !,
	[].
directive(local_include(X)) --> !,
	['#include', '"', format(glue), X, format(glue), '"'].
directive(include(X)) --> !,
	['#include', '<', format(glue), X, format(glue), '>'].
directive(define(X, Y)) --> !,
	['#define', format(inner), format(one_line(yes))],
	identifier(X),
	( { Y = ! } ->
	    { true }
	; expression(primary, Y) ->
	    { true }
	; [format(new_line)], statement(Y)
	),
	[format(outer), format(one_line(no))].
directive(define(X, Y, Z)) --> !,
	['#define', format(inner), format(one_line(yes))],
	expression(postfix, call(X, Y)),
	( { Z = ! } ->
	    { true }
	; expression(primary, Z) ->
	    { true }
	; [format(new_line)], statement(Z)
	),
	[format(outer), format(one_line(no))].
directive(undefine(X)) --> !,
	['#undef'], identifier(X).
% TODO: #ifdef, #ifndef, #if defined(...., #error, and so on.
%
%

% -----------------------------------------------------------------------------
:- meta_predicate nonempty_list(?, pred(3), ?, ?, ?).	
nonempty_list([X], P, _) --> !,
	P(X).
nonempty_list([X|Xs], P, Separator) -->
	P(X), [format(glue), Separator], nonempty_list(Xs, P, Separator).

:- meta_predicate list(?, pred(3), ?, ?, ?).
list([], _, _) --> !.
list(Xs, P, Separator) --> nonempty_list(Xs, P, Separator).

:- meta_predicate sum(?, pred(3), ?, ?).
sum(X+Y, P) --> !,
	sum(X, P), sum(Y, P).
sum(0, _) --> !.
sum(X, P) -->
	P(X).

zero_sum(X+Y) :- !,
	zero_sum(X),
	zero_sum(Y).
zero_sum(0).
