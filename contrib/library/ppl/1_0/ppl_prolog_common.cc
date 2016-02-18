/* Common part of the Prolog interfaces: variables and non-inline functions.
   Copyright (C) 2001-2010 Roberto Bagnara <bagnara@cs.unipr.it>
   Copyright (C) 2010-2012 BUGSENG srl (http://bugseng.com)

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The PPL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.

For the most up-to-date information see the Parma Polyhedra Library
site: http://bugseng.com/products/ppl/ . */

#include "ppl_prolog_common.defs.hh"
#include <exception>
#include <stdexcept>
#include <sstream>
#include <climits>
#include <typeinfo>

namespace Parma_Polyhedra_Library {

namespace Interfaces {

namespace Prolog {

#if PROLOG_TRACK_ALLOCATION || NOISY_PROLOG_TRACK_ALLOCATION

Allocation_Tracker::Allocation_Tracker() {
}

Allocation_Tracker::~Allocation_Tracker() {
  Set::size_type n = s.size();
  if (n > 0)
    std::cerr
      << "Interfaces::Prolog::Allocation_Tracker: "
      << n << " object(s) leaked!"
      << std::endl;
}

Allocation_Tracker allocation_tracker;

#endif // PROLOG_TRACK_ALLOCATION || NOISY_PROLOG_TRACK_ALLOCATION



// For the out-of-memory exception.
Prolog_atom out_of_memory_exception_atom;

// For Prolog lists.
Prolog_atom a_nil;

// For variables.
Prolog_atom a_dollar_VAR;

// For linear expressions.
Prolog_atom a_plus;
Prolog_atom a_minus;
Prolog_atom a_asterisk;

// To represent rational numbers as fractions.
Prolog_atom a_slash;

// For constraints.
Prolog_atom a_less_than;
Prolog_atom a_equal_less_than;
Prolog_atom a_equal;
Prolog_atom a_greater_than_equal;
Prolog_atom a_greater_than;

// For congruences.
Prolog_atom a_is_congruent_to;
Prolog_atom a_modulo;

// For generators.
Prolog_atom a_line;
Prolog_atom a_ray;
Prolog_atom a_point;
Prolog_atom a_closure_point;

// For grid_generators.
Prolog_atom a_grid_line;
Prolog_atom a_parameter;
Prolog_atom a_grid_point;

// For artificial_parameters.
Prolog_atom a_divided_by;

// For the relation between a polyhedron and a constraint.
Prolog_atom a_is_disjoint;
Prolog_atom a_strictly_intersects;
Prolog_atom a_is_included;
Prolog_atom a_saturates;

// For the relation between a polyhedron and a generator.
Prolog_atom a_subsumes;

// Denotes a closed interval boundary.
Prolog_atom a_c;

// Denotes the empty set such as the empty interval or polyhedron.
Prolog_atom a_empty;

// Denotes the universe polyhedron.
Prolog_atom a_universe;

// Denotes the maximization mode for optimization problems.
Prolog_atom a_max;

// Denotes the minimization mode for optimization problems.
Prolog_atom a_min;

// Denote possible widths of bounded integer types.
Prolog_atom a_bits_8;
Prolog_atom a_bits_16;
Prolog_atom a_bits_32;
Prolog_atom a_bits_64;
Prolog_atom a_bits_128;

// Denote possible representations of bounded integer types.
Prolog_atom a_unsigned;
Prolog_atom a_signed_2_complement;

// Denote possible overflow behavior of bounded integer types.
Prolog_atom a_overflow_wraps;
Prolog_atom a_overflow_undefined;
Prolog_atom a_overflow_impossible;

// Denote possible outcomes of MIP and PIP problems solution attempts.
Prolog_atom a_unfeasible;
Prolog_atom a_unbounded;
Prolog_atom a_optimized;

// Denotes an open interval boundary.
Prolog_atom a_o;

// Denotes the constructor that turns two boundaries into a proper interval.
Prolog_atom a_i;

// Denote the -infinity and +infinity interval boundaries.
Prolog_atom a_minf;
Prolog_atom a_pinf;

// Denote complexity classes.
Prolog_atom a_polynomial;
Prolog_atom a_simplex;
Prolog_atom a_any;

// Denote control_parameters.
Prolog_atom a_pricing;
Prolog_atom a_pricing_steepest_edge_float;
Prolog_atom a_pricing_steepest_edge_exact;
Prolog_atom a_pricing_textbook;

Prolog_atom a_cutting_strategy;
Prolog_atom a_cutting_strategy_first;
Prolog_atom a_cutting_strategy_deepest;
Prolog_atom a_cutting_strategy_all;

Prolog_atom a_pivot_row_strategy;
Prolog_atom a_pivot_row_strategy_first;
Prolog_atom a_pivot_row_strategy_max_column;

// Default timeout exception atom.
Prolog_atom a_time_out;

// "Out of memory" exception atom.
Prolog_atom a_out_of_memory;

// Boolean constants.
Prolog_atom a_true;
Prolog_atom a_false;

// To build exception terms.
Prolog_atom a_ppl_overflow_error;
Prolog_atom a_ppl_domain_error;
Prolog_atom a_ppl_length_error;
Prolog_atom a_ppl_invalid_argument;
Prolog_atom a_ppl_logic_error;
Prolog_atom a_ppl_representation_error;
Prolog_atom a_expected;
Prolog_atom a_found;
Prolog_atom a_where;

const Prolog_Interface_Atom prolog_interface_atoms[] = {
  { &a_nil,                      "[]" },

  { &a_dollar_VAR,               "$VAR" },

  { &a_plus,                     "+" },
  { &a_minus,                    "-" },
  { &a_asterisk,                 "*" },

  { &a_slash,                    "/" },

  { &a_equal,                    "=" },
  { &a_greater_than_equal,       ">=" },
  { &a_equal_less_than,          "=<" },
  { &a_greater_than,             ">" },
  { &a_less_than,                "<" },

  { &a_is_congruent_to,          "=:=" },
  { &a_modulo,                   "/" },

  { &a_divided_by,               "/" },

  { &a_line,                     "line" },
  { &a_ray,                      "ray" },
  { &a_point,                    "point" },
  { &a_closure_point,            "closure_point" },

  { &a_grid_line,                "grid_line" },
  { &a_parameter,                "parameter" },
  { &a_grid_point,               "grid_point" },

  { &a_is_disjoint,              "is_disjoint" },
  { &a_strictly_intersects,      "strictly_intersects" },
  { &a_is_included,              "is_included" },
  { &a_saturates,                "saturates" },

  { &a_subsumes,                 "subsumes" },

  { &a_c,                        "c" },

  { &a_empty,                    "empty" },
  { &a_universe,                 "universe" },

  { &a_max,                      "max" },
  { &a_min,                      "min" },

  { &a_bits_8,                   "bits_8" },
  { &a_bits_16,                  "bits_16" },
  { &a_bits_32,                  "bits_32" },
  { &a_bits_64,                  "bits_64" },
  { &a_bits_128,                 "bits_128" },

  { &a_unsigned,                 "unsigned" },
  { &a_signed_2_complement,      "signed_2_complement" },

  { &a_overflow_wraps,           "overflow_wraps" },
  { &a_overflow_undefined,       "overflow_undefined" },
  { &a_overflow_impossible,      "overflow_impossible" },

  { &a_unfeasible,               "unfeasible" },
  { &a_unbounded,                "unbounded" },
  { &a_optimized,                "optimized" },

  { &a_o,                        "o" },
  { &a_i,                        "i" },

  { &a_minf,                     "minf" },
  { &a_pinf,                     "pinf" },

  { &a_polynomial,               "polynomial" },
  { &a_simplex,                  "simplex" },
  { &a_any,                      "any" },

  { &a_pricing,                  "pricing" },
  { &a_pricing_steepest_edge_float,
                                 "pricing_steepest_edge_float" },
  { &a_pricing_steepest_edge_exact,
                                 "pricing_steepest_edge_exact" },
  { &a_pricing_textbook,         "pricing_textbook" },

  { &a_cutting_strategy,         "cutting_strategy" },
  { &a_cutting_strategy_first,   "cutting_strategy_first" },
  { &a_cutting_strategy_deepest, "cutting_strategy_deepest" },
  { &a_cutting_strategy_all,     "cutting_strategy_all" },

  { &a_pivot_row_strategy,       "pivot_row_strategy" },
  { &a_pivot_row_strategy_first, "pivot_row_strategy_first" },
  { &a_pivot_row_strategy_max_column,
                                 "pivot_row_strategy_max_column" },

  { &a_time_out,                 "time_out" },
  { &a_out_of_memory,            "out_of_memory" },

  { &a_true,                     "true" },
  { &a_false,                    "false" },

  { &a_ppl_invalid_argument,     "ppl_invalid_argument" },
  { &a_ppl_overflow_error,       "ppl_overflow_error" },
  { &a_ppl_domain_error,         "ppl_domain_error" },
  { &a_ppl_length_error,         "ppl_length_error" },
  { &a_ppl_invalid_argument,     "ppl_invalid_argument" },
  { &a_ppl_logic_error,          "ppl_logic_error" },
  { &a_ppl_representation_error, "ppl_representation_error" },
  { &a_expected,                 "expected" },
  { &a_found,                    "found" },
  { &a_where,                    "where" },
  { 0,                           0 }
};

Prolog_term_ref
Prolog_atom_term_from_string(const char* s) {
  Prolog_term_ref t = Prolog_new_term_ref();
  Prolog_put_atom(t, Prolog_atom_from_string(s));
  return t;
}

void
handle_exception(const Prolog_unsigned_out_of_range& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref max = Prolog_new_term_ref();
  Prolog_put_ulong(max, e.max());
  Prolog_construct_compound(max,
			    Prolog_atom_from_string("unsigned_integer"
						    "_less_or_equal"),
			    max);
  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_construct_compound(expected, a_expected, max);

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));

  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
handle_exception(const not_unsigned_integer& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_construct_compound(expected, a_expected,
			    Prolog_atom_term_from_string("unsigned_integer"));

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));

  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
handle_exception(const non_linear& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_construct_compound(expected, a_expected,
			    Prolog_atom_term_from_string
			    ("linear_expression_or_constraint"));

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));

  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
handle_exception(const not_a_variable& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found,
			    e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_construct_compound(expected, a_expected,
			    Prolog_atom_term_from_string
			    ("$VAR(unsigned_integer)"));

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));

  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
handle_exception(const not_an_integer& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_construct_compound(expected, a_expected,
			    Prolog_atom_term_from_string("integer"));

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));

  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
handle_exception(const ppl_handle_mismatch& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_construct_compound(expected, a_expected,
			    Prolog_atom_term_from_string("handle"));

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));

  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
handle_exception(const not_an_optimization_mode& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_put_atom(expected, a_nil);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("max"), expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("min"), expected);
  Prolog_construct_compound(expected, a_expected, expected);

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));
  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
handle_exception(const not_a_complexity_class& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_put_atom(expected, a_nil);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("polynomial"), expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("simplex"), expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("any"), expected);
  Prolog_construct_compound(expected, a_expected, expected);

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));
  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
  handle_exception(const not_a_control_parameter_name& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_put_atom(expected, a_nil);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("pricing"), expected);

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));
  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
  handle_exception(const not_a_control_parameter_value& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_put_atom(expected, a_nil);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("pricing_steepest_edge_float"),
                        expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("pricing_steepest_edge_exact"),
                        expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("pricing_textbook"),
                        expected);

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));
  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
  handle_exception(const not_a_pip_problem_control_parameter_name& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_put_atom(expected, a_nil);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("cutting_strategy"),
                        expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("pivot_row_strategy"),
                        expected);

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));
  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
  handle_exception(const not_a_pip_problem_control_parameter_value& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_put_atom(expected, a_nil);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("cutting_strategy_first"),
                        expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("cutting_strategy_deepest"),
                        expected);
   Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("cutting_strategy_all"),
                        expected);
   Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("pivot_row_strategy_first"),
                        expected);
    Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("pivot_row_strategy_max_column"),
                        expected);
 Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));
  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
handle_exception(const not_universe_or_empty& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_put_atom(expected, a_nil);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("universe"), expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("empty"), expected);
  Prolog_construct_compound(expected, a_expected, expected);

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));
  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
handle_exception(const not_a_boolean& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_put_atom(expected, a_nil);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("true"), expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("false"), expected);
  Prolog_construct_compound(expected, a_expected, expected);

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));
  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
handle_exception(const not_a_bounded_integer_type_width& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_put_atom(expected, a_nil);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("bits_8"), expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("bits_16"), expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("bits_32"), expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("bits_64"), expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("bits_128"), expected);
  Prolog_construct_compound(expected, a_expected, expected);

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));
  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
handle_exception(const not_a_bounded_integer_type_representation& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_put_atom(expected, a_nil);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("unsigned"), expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("signed_2_complement"),
                        expected);
  Prolog_construct_compound(expected, a_expected, expected);

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));
  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
handle_exception(const not_a_bounded_integer_type_overflow& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_put_atom(expected, a_nil);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("overflow_wraps"),
                        expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("overflow_undefined"),
                        expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("overflow_impossible"),
                        expected);
  Prolog_construct_compound(expected, a_expected, expected);

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));
  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
handle_exception(const not_a_relation& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_put_atom(expected, a_nil);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("="), expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string(">="), expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("=<"), expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string(">"), expected);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string("<"), expected);
  Prolog_construct_compound(expected, a_expected, expected);

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));
  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
handle_exception(const not_a_nil_terminated_list& e) {
  Prolog_term_ref found = Prolog_new_term_ref();
  Prolog_construct_compound(found, a_found, e.term());

  Prolog_term_ref expected = Prolog_new_term_ref();
  Prolog_put_atom(expected, a_nil);
  Prolog_construct_cons(expected,
			Prolog_atom_term_from_string
                        ("Prolog_list"), expected);
  Prolog_construct_compound(expected, a_expected, expected);

  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string(e.where()));
  Prolog_term_ref exception_term = Prolog_new_term_ref();
  Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			    found, expected, where);
  Prolog_raise_exception(exception_term);
}

void
handle_exception(const PPL_integer_out_of_range& e) {
  Prolog_term_ref where = Prolog_new_term_ref();
  Prolog_construct_compound(where, a_where,
			    Prolog_atom_term_from_string("Coefficient_to_integer_term"));

  Prolog_term_ref exception_term = Prolog_new_term_ref();
  std::ostringstream s;
  s << e.value();
  std::string str = s.str();
  Prolog_construct_compound(exception_term, a_ppl_representation_error,
			    Prolog_atom_term_from_string(str.c_str()),
			    where);
  Prolog_raise_exception(exception_term);
}

void
handle_exception(const unknown_interface_error& e) {
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_put_atom_chars(et, e.where());
  Prolog_raise_exception(et);
}

void
handle_exception(const std::overflow_error& e) {
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_construct_compound(et, a_ppl_overflow_error,
			    Prolog_atom_term_from_string(e.what()));
  Prolog_raise_exception(et);
}

void
handle_exception(const std::domain_error& e) {
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_construct_compound(et, a_ppl_domain_error,
			    Prolog_atom_term_from_string(e.what()));
  Prolog_raise_exception(et);
}

void
handle_exception(const std::length_error& e) {
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_construct_compound(et, a_ppl_length_error,
			    Prolog_atom_term_from_string(e.what()));
  Prolog_raise_exception(et);
}

void
handle_exception(const std::invalid_argument& e) {
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_construct_compound(et, a_ppl_invalid_argument,
			    Prolog_atom_term_from_string(e.what()));
  Prolog_raise_exception(et);
}

void
handle_exception(const std::logic_error& e) {
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_construct_compound(et, a_ppl_logic_error,
			    Prolog_atom_term_from_string(e.what()));
  Prolog_raise_exception(et);
}

void
handle_exception(const std::bad_alloc&) {
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_put_atom(et, out_of_memory_exception_atom);
  Prolog_raise_exception(et);
}

void
handle_exception(const std::exception& e) {
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_put_atom_chars(et, e.what());
  Prolog_raise_exception(et);
}

void
handle_exception() {
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_put_atom_chars(et, "PPL bug: unknown exception raised");
  Prolog_raise_exception(et);
}

Parma_Polyhedra_Library::Watchdog* p_timeout_object = 0;

typedef
Parma_Polyhedra_Library::Threshold_Watcher
<Parma_Polyhedra_Library::Weightwatch_Traits> Weightwatch;

Weightwatch* p_deterministic_timeout_object = 0;

void
reset_timeout() {
  if (p_timeout_object) {
    delete p_timeout_object;
    p_timeout_object = 0;
    abandon_expensive_computations = 0;
  }
}

void
reset_deterministic_timeout() {
  if (p_deterministic_timeout_object) {
    delete p_deterministic_timeout_object;
    p_deterministic_timeout_object = 0;
    abandon_expensive_computations = 0;
  }
}

Prolog_atom timeout_exception_atom;

void
handle_exception(const timeout_exception&) {
  assert(p_timeout_object);
  reset_timeout();
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_put_atom(et, timeout_exception_atom);
  Prolog_raise_exception(et);
}

void
handle_exception(const deterministic_timeout_exception&) {
  assert(p_deterministic_timeout_object);
  reset_deterministic_timeout();
  Prolog_term_ref et = Prolog_new_term_ref();
  Prolog_put_atom(et, timeout_exception_atom);
  Prolog_raise_exception(et);
}

Prolog_term_ref
variable_term(dimension_type varid) {
  Prolog_term_ref v = Prolog_new_term_ref();
  Prolog_put_ulong(v, varid);
  Prolog_term_ref t = Prolog_new_term_ref();
  Prolog_construct_compound(t, a_dollar_VAR, v);
  return t;
}

Prolog_atom
term_to_boolean(Prolog_term_ref t, const char* where) {
  if (Prolog_is_atom(t)) {
    Prolog_atom name;
    if (Prolog_get_atom_name(t, &name)
	&& (name == a_true || name == a_false))
      return name;
  }
  throw not_a_boolean(t, where);
}

Prolog_atom
term_to_universe_or_empty(Prolog_term_ref t, const char* where) {
  if (Prolog_is_atom(t)) {
    Prolog_atom name;
    if (Prolog_get_atom_name(t, &name)
	&& (name == a_universe || name == a_empty))
      return name;
  }
  throw not_universe_or_empty(t, where);
}

Coefficient
integer_term_to_Coefficient(Prolog_term_ref t) {
  PPL_DIRTY_TEMP_COEFFICIENT(n);
  assert(Prolog_is_integer(t));
  if (!Prolog_get_Coefficient(t, n))
    abort();
  return n;
}

Prolog_term_ref
Coefficient_to_integer_term(const Coefficient& n) {
  Prolog_term_ref t = Prolog_new_term_ref();
  if (!Prolog_put_Coefficient(t, n))
    abort();
  return t;
}

bool
unify_long(Prolog_term_ref t, long l) {
  Prolog_term_ref t_l = Prolog_new_term_ref();
  return Prolog_put_long(t_l, l) && Prolog_unify(t, t_l);
}

bool
unify_ulong(Prolog_term_ref t, unsigned long l) {
  Prolog_term_ref t_l = Prolog_new_term_ref();
  return Prolog_put_ulong(t_l, l) && Prolog_unify(t, t_l);
}

Linear_Expression
build_linear_expression(Prolog_term_ref t, const char* where) {
  if (Prolog_is_integer(t))
    return Linear_Expression(integer_term_to_Coefficient(t));
  else if (Prolog_is_compound(t)) {
    Prolog_atom functor;
    int arity;
    Prolog_get_compound_name_arity(t, &functor, &arity);
    switch (arity) {
    case 1:
      {
	Prolog_term_ref arg = Prolog_new_term_ref();
	Prolog_get_arg(1, t, arg);
	if (functor == a_minus)
	  // Unary minus.
	  return -build_linear_expression(arg, where);
	else if (functor == a_dollar_VAR)
	  // Variable.
	  return Variable(term_to_unsigned<dimension_type>(arg, where));
      }
      break;
    case 2:
      {
	Prolog_term_ref arg1 = Prolog_new_term_ref();
	Prolog_term_ref arg2 = Prolog_new_term_ref();
	Prolog_get_arg(1, t, arg1);
	Prolog_get_arg(2, t, arg2);
	if (functor == a_plus)
	  // Plus.
	  if (Prolog_is_integer(arg1))
	    return integer_term_to_Coefficient(arg1)
	      + build_linear_expression(arg2, where);
	  else if (Prolog_is_integer(arg2))
	    return build_linear_expression(arg1, where)
	      + integer_term_to_Coefficient(arg2);
	  else
	    return build_linear_expression(arg1, where)
	      + build_linear_expression(arg2, where);
	else if (functor == a_minus)
	  // Minus.
	  if (Prolog_is_integer(arg1))
	    return integer_term_to_Coefficient(arg1)
	      - build_linear_expression(arg2, where);
	  else if (Prolog_is_integer(arg2))
	    return build_linear_expression(arg1, where)
	      - integer_term_to_Coefficient(arg2);
	  else
	    return build_linear_expression(arg1, where)
	      - build_linear_expression(arg2, where);
	else if (functor == a_asterisk) {
	  // Times.
	  if (Prolog_is_integer(arg1))
	    return integer_term_to_Coefficient(arg1)
	      * build_linear_expression(arg2, where);
	  else if (Prolog_is_integer(arg2))
	    return build_linear_expression(arg1, where)
	      * integer_term_to_Coefficient(arg2);
	}
      }
    }
  }
  // Invalid.
  throw non_linear(t, where);
}

Constraint
build_constraint(Prolog_term_ref t, const char* where) {
  if (Prolog_is_compound(t)) {
    Prolog_atom functor;
    int arity;
    Prolog_get_compound_name_arity(t, &functor, &arity);
    if (arity == 2) {
      Prolog_term_ref arg1 = Prolog_new_term_ref();
      Prolog_term_ref arg2 = Prolog_new_term_ref();
      Prolog_get_arg(1, t, arg1);
      Prolog_get_arg(2, t, arg2);
      if (functor == a_equal)
	// =
	if (Prolog_is_integer(arg1))
	  return integer_term_to_Coefficient(arg1)
	    == build_linear_expression(arg2, where);
	else if (Prolog_is_integer(arg2))
	  return build_linear_expression(arg1, where)
	    == integer_term_to_Coefficient(arg2);
	else
	  return build_linear_expression(arg1, where)
	    == build_linear_expression(arg2, where);
      else if (functor == a_equal_less_than)
	// =<
	if (Prolog_is_integer(arg1))
	  return integer_term_to_Coefficient(arg1)
	    <= build_linear_expression(arg2, where);
	else if (Prolog_is_integer(arg2))
	  return build_linear_expression(arg1, where)
	    <= integer_term_to_Coefficient(arg2);
	else
	  return build_linear_expression(arg1, where)
	    <= build_linear_expression(arg2, where);
      else if (functor == a_greater_than_equal)
	// >=
	if (Prolog_is_integer(arg1))
	  return integer_term_to_Coefficient(arg1)
	    >= build_linear_expression(arg2, where);
	else if (Prolog_is_integer(arg2))
	  return build_linear_expression(arg1, where)
	    >= integer_term_to_Coefficient(arg2);
	else
	  return build_linear_expression(arg1, where)
	    >= build_linear_expression(arg2, where);
      else if (functor == a_less_than)
	// <
	if (Prolog_is_integer(arg1))
	  return integer_term_to_Coefficient(arg1)
	    < build_linear_expression(arg2, where);
	else if (Prolog_is_integer(arg2))
	  return build_linear_expression(arg1, where)
	    < integer_term_to_Coefficient(arg2);
	else
	  return build_linear_expression(arg1, where)
	    < build_linear_expression(arg2, where);
      else if (functor == a_greater_than) {
	// >
	if (Prolog_is_integer(arg1))
	  return integer_term_to_Coefficient(arg1)
	    > build_linear_expression(arg2, where);
	else if (Prolog_is_integer(arg2))
	  return build_linear_expression(arg1, where)
	    > integer_term_to_Coefficient(arg2);
	else
	  return build_linear_expression(arg1, where)
	    > build_linear_expression(arg2, where);
      }
    }
  }
  // Invalid.
  throw non_linear(t, where);
}

Congruence
build_congruence(Prolog_term_ref t, const char* where) {
  if (Prolog_is_compound(t)) {
    Prolog_atom functor;
    int arity;
    Prolog_get_compound_name_arity(t, &functor, &arity);
    if (arity == 2) {
      Prolog_term_ref arg1 = Prolog_new_term_ref();
      Prolog_term_ref arg2 = Prolog_new_term_ref();
      Prolog_get_arg(1, t, arg1);
      Prolog_get_arg(2, t, arg2);
      if (functor == a_modulo) {
        // /
	if (Prolog_is_integer(arg2)) {
          Prolog_atom functor1;
          int arity1;
          Prolog_get_compound_name_arity(arg1, &functor1, &arity1);
          if (arity1 == 2) {
            if (functor1 == a_is_congruent_to) {
      	      // =:=
              Prolog_term_ref arg11 = Prolog_new_term_ref();
              Prolog_term_ref arg12 = Prolog_new_term_ref();
              Prolog_get_arg(1, arg1, arg11);
              Prolog_get_arg(2, arg1, arg12);
              if (Prolog_is_integer(arg12))
	        return (build_linear_expression(arg11, where)
			%= integer_term_to_Coefficient(arg12))
		  / integer_term_to_Coefficient(arg2);
	      else
	        return (build_linear_expression(arg11, where)
			%= build_linear_expression(arg12, where))
		  / integer_term_to_Coefficient(arg2);
	    }
	  }
	}
      }
      else
        if (functor == a_is_congruent_to)
      	  // =:=
          if (Prolog_is_integer(arg2))
	    return build_linear_expression(arg1, where)
	      %= integer_term_to_Coefficient(arg2);
	  else
	    return build_linear_expression(arg1, where)
	      %= build_linear_expression(arg2, where);
	else
          if (functor == a_equal) {
	    // =
	    if (Prolog_is_integer(arg1))
	      return (build_linear_expression(arg2, where)
		      %= integer_term_to_Coefficient(arg1)) / 0;
	    else if (Prolog_is_integer(arg2))
	      return (build_linear_expression(arg1, where)
		      %= integer_term_to_Coefficient(arg2)) / 0;
	    else
	      return (build_linear_expression(arg1, where)
		      %= build_linear_expression(arg2, where)) / 0;
	  }
    }
  }
  // Invalid.
  throw non_linear(t, where);
}

Generator
build_generator(Prolog_term_ref t, const char* where) {
  if (Prolog_is_compound(t)) {
    Prolog_atom functor;
    int arity;
    Prolog_get_compound_name_arity(t, &functor, &arity);
    if (arity == 1) {
      Prolog_term_ref arg = Prolog_new_term_ref();
      Prolog_get_arg(1, t, arg);
      if (functor == a_line)
	return Generator::line(build_linear_expression(arg, where));
      else if (functor == a_ray)
	return Generator::ray(build_linear_expression(arg, where));
      else if (functor == a_point)
	return Generator::point(build_linear_expression(arg, where));
      else if (functor == a_closure_point)
	return Generator::closure_point(build_linear_expression(arg, where));
    }
    else if (arity == 2) {
      Prolog_term_ref arg1 = Prolog_new_term_ref();
      Prolog_term_ref arg2 = Prolog_new_term_ref();
      Prolog_get_arg(1, t, arg1);
      Prolog_get_arg(2, t, arg2);
      if (Prolog_is_integer(arg2)) {
        if (functor == a_point)
	  return Generator::point(build_linear_expression(arg1, where),
				  integer_term_to_Coefficient(arg2));
        else if (functor == a_closure_point)
	  return Generator::closure_point(build_linear_expression(arg1, where),
					  integer_term_to_Coefficient(arg2));
      }
    }
  }
  // Invalid.
  throw non_linear(t, where);
}

Grid_Generator
build_grid_generator(Prolog_term_ref t, const char* where) {
  if (Prolog_is_compound(t)) {
    Prolog_atom functor;
    int arity;
    Prolog_get_compound_name_arity(t, &functor, &arity);
    if (arity == 1) {
      Prolog_term_ref arg = Prolog_new_term_ref();
      Prolog_get_arg(1, t, arg);
      if (functor == a_grid_line)
	return Grid_Generator::grid_line(build_linear_expression(arg, where));
      else if (functor == a_parameter)
	return Grid_Generator::parameter(build_linear_expression(arg, where));
      else if (functor == a_grid_point)
	return Grid_Generator::grid_point(build_linear_expression(arg, where));
    }
    else if (arity == 2) {
      Prolog_term_ref arg1 = Prolog_new_term_ref();
      Prolog_term_ref arg2 = Prolog_new_term_ref();
      Prolog_get_arg(1, t, arg1);
      Prolog_get_arg(2, t, arg2);
      if (Prolog_is_integer(arg2)) {
        if (functor == a_grid_point)
	  return Grid_Generator::grid_point(build_linear_expression(arg1,
								    where),
					    integer_term_to_Coefficient(arg2));
        else if (functor == a_parameter)
	  return Grid_Generator::parameter(build_linear_expression(arg1,
								   where),
					  integer_term_to_Coefficient(arg2));
      }
    }
  }
  // Invalid.
  throw non_linear(t, where);
}

template <typename R>
Prolog_term_ref
get_homogeneous_expression(const R& r) {
  Prolog_term_ref so_far = Prolog_new_term_ref();
  PPL_DIRTY_TEMP_COEFFICIENT(coefficient);
  dimension_type varid = 0;
  dimension_type space_dimension = r.space_dimension();
  while (varid < space_dimension
	 && (coefficient = r.coefficient(Variable(varid))) == 0)
    ++varid;
  if (varid >= space_dimension) {
    Prolog_put_long(so_far, 0);
  }
  else {
    Prolog_construct_compound(so_far, a_asterisk,
			      Coefficient_to_integer_term(coefficient),
			      variable_term(varid));
    while (true) {
      ++varid;
      while (varid < space_dimension
	     && (coefficient = r.coefficient(Variable(varid))) == 0)
	++varid;
      if (varid >= space_dimension)
	break;
      else {
	Prolog_term_ref addendum = Prolog_new_term_ref();
	Prolog_construct_compound(addendum, a_asterisk,
				  Coefficient_to_integer_term(coefficient),
				  variable_term(varid));
	Prolog_term_ref new_so_far = Prolog_new_term_ref();
	Prolog_construct_compound(new_so_far, a_plus,
				  so_far, addendum);
	so_far = new_so_far;
      }
    }
  }
  return so_far;
}

Prolog_term_ref
get_linear_expression(const Linear_Expression& le) {
  Prolog_term_ref t_homo = get_homogeneous_expression(le);
  if (le.inhomogeneous_term() == 0)
    return t_homo;
  else {
    Prolog_term_ref t_in
      = Coefficient_to_integer_term(le.inhomogeneous_term());
    if (unify_long(t_homo, 0))
      return t_in;
    else {
      Prolog_term_ref t_le = Prolog_new_term_ref();
      Prolog_construct_compound(t_le, a_plus, t_homo, t_in);
      return t_le;
    }
  }
}

Prolog_term_ref
constraint_term(const Constraint& c) {
  Prolog_atom relation = 0;
  switch (c.type()) {
  case Constraint::EQUALITY:
    relation = a_equal;
    break;
  case Constraint::NONSTRICT_INEQUALITY:
    relation = a_greater_than_equal;
    break;
  case Constraint::STRICT_INEQUALITY:
    relation = a_greater_than;
    break;
  default:
    throw unknown_interface_error("generator_term()");
  }
  Prolog_term_ref t = Prolog_new_term_ref();
  Prolog_construct_compound
    (t,
     relation,
     get_homogeneous_expression(c),
     Coefficient_to_integer_term(-c.inhomogeneous_term()));
  return t;
}

Prolog_term_ref
congruence_term(const Congruence& cg) {
  Prolog_atom relation1 = a_is_congruent_to;
  Prolog_atom relation2 = a_modulo;
  Prolog_term_ref t_tmp = Prolog_new_term_ref();
  Prolog_term_ref t = Prolog_new_term_ref();
  Prolog_construct_compound
    (t_tmp,
     relation1,
     get_homogeneous_expression(cg),
     Coefficient_to_integer_term(-cg.inhomogeneous_term()));
  Prolog_construct_compound
    (t,
     relation2,
     t_tmp,
     Coefficient_to_integer_term(cg.modulus()));
  return t;
}

Prolog_term_ref
generator_term(const Generator& g) {
  Prolog_term_ref t = Prolog_new_term_ref();
  Prolog_atom constructor = 0;
  switch (g.type()) {
  case Generator::LINE:
    constructor = a_line;
    break;
  case Generator::RAY:
    constructor = a_ray;
    break;
  case Generator::POINT:
    {
      constructor = a_point;
      const Coefficient& divisor = g.divisor();
      if (divisor == 1)
	break;
      else {
	Prolog_construct_compound(t, constructor,
				  get_homogeneous_expression(g),
				  Coefficient_to_integer_term(divisor));
	return t;
      }
    }
  case Generator::CLOSURE_POINT:
    {
      constructor = a_closure_point;
      const Coefficient& divisor = g.divisor();
      if (divisor == 1)
	break;
      else {
	Prolog_construct_compound(t, constructor,
				  get_homogeneous_expression(g),
				  Coefficient_to_integer_term(divisor));
	return t;
      }
    }
  default:
    throw unknown_interface_error("generator_term()");
  }
  Prolog_construct_compound(t, constructor, get_homogeneous_expression(g));
  return t;
}

Prolog_term_ref
grid_generator_term(const Grid_Generator& g) {
  Prolog_term_ref t = Prolog_new_term_ref();
  Prolog_atom constructor = 0;
  switch (g.type()) {
  case Grid_Generator::LINE:
    constructor = a_grid_line;
    break;
  case Grid_Generator::PARAMETER:
    {
      constructor = a_parameter;
      const Coefficient& divisor = g.divisor();
      if (divisor == 1)
	break;
      else {
	Prolog_construct_compound(t, constructor,
				  get_homogeneous_expression(g),
				  Coefficient_to_integer_term(divisor));
	return t;
      }
    }
  case Grid_Generator::POINT:
    {
      constructor = a_grid_point;
      const Coefficient& divisor = g.divisor();
      if (divisor == 1)
	break;
      else {
	Prolog_construct_compound(t, constructor,
				  get_homogeneous_expression(g),
				  Coefficient_to_integer_term(divisor));
	return t;
      }
    }
  default:
    throw unknown_interface_error("grid_generator_term()");
  }
  Prolog_construct_compound(t, constructor, get_homogeneous_expression(g));
  return t;
}

Prolog_term_ref
artificial_parameter_term(const PIP_Tree_Node::Artificial_Parameter& art) {
  Prolog_term_ref t = Prolog_new_term_ref();
  Prolog_construct_compound(t, a_divided_by,
                            get_linear_expression(art),
                            Coefficient_to_integer_term(art.denominator()));
  return t;
}

Variable
term_to_Variable(Prolog_term_ref t, const char* where) {
  if (Prolog_is_compound(t)) {
    Prolog_atom functor;
    int arity;
    Prolog_get_compound_name_arity(t, &functor, &arity);
    if (functor == a_dollar_VAR && arity == 1) {
      Prolog_term_ref arg = Prolog_new_term_ref();
      Prolog_get_arg(1, t, arg);
      return
	Variable(term_to_unsigned<dimension_type>(arg, "term_to_Variable"));
    }
  }
  throw not_a_variable(t, where);
}

Coefficient
term_to_Coefficient(Prolog_term_ref t, const char* where) {
  if (Prolog_is_integer(t))
    return integer_term_to_Coefficient(t);
  else
    throw not_an_integer(t, where);
}

Prolog_atom
term_to_bounded_integer_type_width(Prolog_term_ref t, const char* where) {
  if (Prolog_is_atom(t)) {
    Prolog_atom name;
    if (Prolog_get_atom_name(t, &name)
	&& (name == a_bits_8 || name == a_bits_16
            || name == a_bits_32 || name == a_bits_64
            || name == a_bits_128))
      return name;
  }
  throw not_a_bounded_integer_type_width(t, where);
}

Prolog_atom
term_to_bounded_integer_type_representation(Prolog_term_ref t,
                                            const char* where) {
  if (Prolog_is_atom(t)) {
    Prolog_atom name;
    if (Prolog_get_atom_name(t, &name)
	&& (name == a_unsigned || name == a_signed_2_complement))
      return name;
  }
  throw not_a_bounded_integer_type_representation(t, where);
}

Prolog_atom
term_to_bounded_integer_type_overflow(Prolog_term_ref t,
                                      const char* where) {
  if (Prolog_is_atom(t)) {
    Prolog_atom name;
    if (Prolog_get_atom_name(t, &name)
	&& (name == a_overflow_wraps
            || name == a_overflow_undefined
            || name == a_overflow_impossible))
      return name;
  }
  throw not_a_bounded_integer_type_overflow(t, where);
}

Prolog_atom
term_to_optimization_mode(Prolog_term_ref t, const char* where) {
  if (Prolog_is_atom(t)) {
    Prolog_atom name;
    if (Prolog_get_atom_name(t, &name)
	&& (name == a_max || name == a_min))
      return name;
  }
  throw not_an_optimization_mode(t, where);
}

Prolog_atom
term_to_control_parameter_name(Prolog_term_ref t, const char* where) {
  if (Prolog_is_atom(t)) {
    Prolog_atom name;
    if (Prolog_get_atom_name(t, &name)
	&& (name == a_pricing || name == a_cutting_strategy))
      return name;
  }
  throw not_a_control_parameter_name(t, where);
}

Prolog_atom
term_to_pip_problem_control_parameter_name(Prolog_term_ref t, const char* where) {
  if (Prolog_is_atom(t)) {
    Prolog_atom name;
    if (Prolog_get_atom_name(t, &name)
	&& (name == a_cutting_strategy || name == a_pivot_row_strategy))
      return name;
  }
  throw not_a_pip_problem_control_parameter_name(t, where);
}

Prolog_atom
term_to_control_parameter_value(Prolog_term_ref t, const char* where) {
  if (Prolog_is_atom(t)) {
    Prolog_atom name;
    if (Prolog_get_atom_name(t, &name)
	&& (name == a_pricing_steepest_edge_float
            || name == a_pricing_steepest_edge_exact
            || name == a_pricing_textbook
            || name == a_cutting_strategy_first
            || name == a_cutting_strategy_deepest))
      return name;
  }
  throw not_a_control_parameter_value(t, where);
}

Prolog_atom
term_to_pip_problem_control_parameter_value(Prolog_term_ref t,
                                            const char* where) {
  if (Prolog_is_atom(t)) {
    Prolog_atom name;
    if (Prolog_get_atom_name(t, &name)
	&& (name == a_cutting_strategy_first
            || name == a_cutting_strategy_deepest
            || name == a_cutting_strategy_all
            || name == a_pivot_row_strategy_first
            || name == a_pivot_row_strategy_max_column))
      return name;
  }
  throw not_a_pip_problem_control_parameter_value(t, where);
}

bool Prolog_interface_initialized = false;

void
check_nil_terminating(Prolog_term_ref t, const char* where) {
  if (Prolog_is_atom(t)) {
    Prolog_atom a;
    Prolog_get_atom_name(t, &a);
    if (a == a_nil)
      return;
  }
  throw not_a_nil_terminated_list(t, where);
}

inline dimension_type
max_representable_dimension(dimension_type d) {
  return
    Prolog_has_unbounded_integers
    ? d
    : std::min(d, static_cast<dimension_type>(Prolog_max_integer));
}

bool
term_to_boundary(Prolog_term_ref t_b, Boundary_Kind kind,
		 bool& finite, bool& closed,
		 Coefficient& n, Coefficient& d) {
  if (!Prolog_is_compound(t_b))
    return false;

  Prolog_atom functor;
  int arity;

  Prolog_get_compound_name_arity(t_b, &functor, &arity);
  // A boundary term is either of the form c(Limit) or o(Limit).
  if (arity != 1 || (functor != a_c && functor != a_o))
    return false;

  Prolog_atom open_closed_atom = functor;

  Prolog_term_ref t_limit = Prolog_new_term_ref();
  Prolog_get_arg(1, t_b, t_limit);
  if (Prolog_is_integer(t_limit)) {
    // A finite, integral limit.
    finite = true;
    closed = (open_closed_atom == a_c);
    n = integer_term_to_Coefficient(t_limit);
    d = 1;
  }
  else if (Prolog_is_atom(t_limit)) {
    Prolog_atom a;
    Prolog_get_atom_name(t_limit, &a);
    Prolog_atom allowed_infinity = (kind == LOWER_BOUNDARY ? a_minf : a_pinf);
    // Only open bounds may be unbounded.
    if (a != allowed_infinity || open_closed_atom != a_o)
      return false;

    finite = false;
  }
  else if (Prolog_is_compound(t_limit)) {
    Prolog_get_compound_name_arity(t_limit, &functor, &arity);
    if (arity != 2 || functor != a_slash)
      return false;

    Prolog_term_ref t_n = Prolog_new_term_ref();
    Prolog_term_ref t_d = Prolog_new_term_ref();
    Prolog_get_arg(1, t_limit, t_n);
    Prolog_get_arg(2, t_limit, t_d);

    if (!Prolog_is_integer(t_n) || !Prolog_is_integer(t_d))
      return false;
    else {
      finite = true;
      closed = (open_closed_atom == a_c);
      n = integer_term_to_Coefficient(t_n);
      d = integer_term_to_Coefficient(t_d);
      // Catch negative denominators and divisions by zero here.
      if (d <= 0)
        return false;
    }
  }
  return true;
}

Prolog_atom
term_to_relation(Prolog_term_ref t, const char* where) {
  if (Prolog_is_atom(t)) {
    Prolog_atom name;
    if (Prolog_get_atom_name(t, &name)
	&& (name == a_equal
	    || name == a_greater_than_equal
	    || name == a_equal_less_than
	    || name == a_greater_than
	    || name == a_less_than))
      return name;
  }
  throw not_a_relation(t, where);
}

Relation_Symbol
term_to_relation_symbol(Prolog_term_ref t_r, const char* where) {
  Prolog_atom ra = term_to_relation(t_r, where);
  Relation_Symbol r;
  if (ra == a_less_than)
    r = LESS_THAN;
  else if (ra == a_equal_less_than)
    r = LESS_OR_EQUAL;
  else if (ra == a_equal)
    r = EQUAL;
  else if (ra == a_greater_than_equal)
    r = GREATER_OR_EQUAL;
  else {
    assert(ra == a_greater_than);
    r = GREATER_THAN;
  }
  return r;
}

Prolog_term_ref
rational_term(const Rational_Box::interval_type::boundary_type& q) {
  Prolog_term_ref t = Prolog_new_term_ref();
  PPL_DIRTY_TEMP_COEFFICIENT(numerator);
  PPL_DIRTY_TEMP_COEFFICIENT(denominator);
  numerator = q.get_num();
  denominator = q.get_den();
  if (denominator == 1)
    Prolog_put_Coefficient(t, numerator);
  else
    Prolog_construct_compound(t, a_slash,
			      Coefficient_to_integer_term(numerator),
			      Coefficient_to_integer_term(denominator));
  return t;
}

Prolog_term_ref
interval_term(const Rational_Box::interval_type& i) {
  Prolog_term_ref t = Prolog_new_term_ref();
  if (i.is_empty())
    Prolog_put_atom(t, a_empty);
  else {
    // Lower bound.
    const Prolog_atom& l_oc = i.lower_is_open() ? a_o : a_c;
    Prolog_term_ref l_b = Prolog_new_term_ref();
    if (i.lower_is_boundary_infinity())
      Prolog_put_atom(l_b, a_minf);
    else
      Prolog_put_term(l_b, rational_term(i.lower()));
    Prolog_term_ref l_t = Prolog_new_term_ref();
    Prolog_construct_compound(l_t, l_oc, l_b);

    // Upper bound.
    const Prolog_atom& u_oc = i.upper_is_open() ? a_o : a_c;
    Prolog_term_ref u_b = Prolog_new_term_ref();
    if (i.upper_is_boundary_infinity())
      Prolog_put_atom(u_b, a_pinf);
    else
      Prolog_put_term(u_b, rational_term(i.upper()));
    Prolog_term_ref u_t = Prolog_new_term_ref();
    Prolog_construct_compound(u_t, u_oc, u_b);

    Prolog_construct_compound(t, a_i, l_t, u_t);
  }
  return t;
}

Prolog_atom
term_to_complexity_class(Prolog_term_ref t, const char* where) {
  if (Prolog_is_atom(t)) {
    Prolog_atom name;
    if (Prolog_get_atom_name(t, &name)
	&& (name == a_polynomial || name == a_simplex || name == a_any))
      return name;
  }
  throw not_a_complexity_class(t, where);
}

} // namespace Prolog

} // namespace Interfaces

} // namespace Parma_Polyhedra_Library

using namespace Parma_Polyhedra_Library::Interfaces::Prolog;

extern "C" Prolog_foreign_return_type
ppl_version_major(Prolog_term_ref t_v) {
  try {
    if (unify_ulong(t_v, version_major()))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_version_minor(Prolog_term_ref t_v) {
  try {
    if (unify_ulong(t_v, version_minor()))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_version_revision(Prolog_term_ref t_v) {
  try {
    if (unify_ulong(t_v, version_revision()))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_version_beta(Prolog_term_ref t_v) {
  try {
    if (unify_ulong(t_v, version_beta()))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_version(Prolog_term_ref t_v) {
  try {
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_atom_chars(tmp, version());
    if (Prolog_unify(t_v, tmp))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_banner(Prolog_term_ref t_b) {
  try {
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_atom_chars(tmp, banner());
    if (Prolog_unify(t_b, tmp))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_max_space_dimension(Prolog_term_ref t_msd) {
  try {
    if (unify_ulong(t_msd, max_representable_dimension(max_space_dimension())))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_initialize() {
  try {
    if (Prolog_interface_initialized)
      return PROLOG_SUCCESS;
    // Initialize the core library.
    initialize();
    for (size_t i = 0; prolog_interface_atoms[i].p_atom != 0; ++i) {
      Prolog_atom a = Prolog_atom_from_string(prolog_interface_atoms[i].name);
      *prolog_interface_atoms[i].p_atom = a;
    }
    timeout_exception_atom = a_time_out;
    out_of_memory_exception_atom = a_out_of_memory;
    ppl_Prolog_sysdep_init();
    Prolog_interface_initialized = true;
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_finalize() {
  try {
    if (!Prolog_interface_initialized)
      return PROLOG_SUCCESS;

    Prolog_interface_initialized = false;
    // Finalize the core library.
    finalize();
    // Release the pending timeout object, if any.
    reset_timeout();
    ppl_Prolog_sysdep_deinit();
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_set_rounding_for_PPL() {
  try {
    set_rounding_for_PPL();
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_restore_pre_PPL_rounding() {
  try {
    restore_pre_PPL_rounding();
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_irrational_precision(Prolog_term_ref t_p) {
  try {
    if (unify_ulong(t_p, irrational_precision()))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_set_irrational_precision(Prolog_term_ref t_p) {
  try {
    unsigned p
      = term_to_unsigned<unsigned>(t_p, "ppl_set_irrational_precision/1");
    set_irrational_precision(p);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_set_timeout_exception_atom(Prolog_term_ref t_tea) {
  try {
    if (Prolog_is_atom(t_tea)) {
      Prolog_atom tea;
      if (Prolog_get_atom_name(t_tea, &tea)) {
	timeout_exception_atom = tea;
	return PROLOG_SUCCESS;
      }
    }
    Prolog_term_ref found = Prolog_new_term_ref();
    Prolog_construct_compound(found, a_found, t_tea);

    Prolog_term_ref expected = Prolog_new_term_ref();
    Prolog_construct_compound(expected, a_expected,
			      Prolog_atom_term_from_string("atom"));

    Prolog_term_ref where = Prolog_new_term_ref();
    Prolog_construct_compound(where, a_where,
			      Prolog_atom_term_from_string
			      ("ppl_set_timeout_exception_atom"));

    Prolog_term_ref exception_term = Prolog_new_term_ref();
    Prolog_construct_compound(exception_term, a_ppl_invalid_argument,
			      found, expected, where);
    Prolog_raise_exception(exception_term);
    return PROLOG_FAILURE;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_timeout_exception_atom(Prolog_term_ref t) {
  try {
    Prolog_term_ref t_tea = Prolog_new_term_ref();
    Prolog_put_atom(t_tea, timeout_exception_atom);
    return Prolog_unify(t_tea, t) ? PROLOG_SUCCESS : PROLOG_FAILURE;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_set_timeout(Prolog_term_ref t_csecs) {
  try {
    // In case a timeout was already set.
    reset_timeout();
    static timeout_exception e;
    unsigned csecs = term_to_unsigned<unsigned>(t_csecs, "ppl_set_timeout/1");
    p_timeout_object =
      new Parma_Polyhedra_Library::Watchdog(csecs,
                                            abandon_expensive_computations,
                                            e);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_reset_timeout() {
  try {
    reset_timeout();
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_set_deterministic_timeout(Prolog_term_ref t_unscaled_weight,
                              Prolog_term_ref t_scale) {
  try {
    // In case a deterministic timeout was already set.
    reset_deterministic_timeout();
    static deterministic_timeout_exception e;
    unsigned long unscaled_weight
      = term_to_unsigned<unsigned long>(t_unscaled_weight,
                                        "ppl_set_deterministic_timeout/2");
    unsigned scale
      = term_to_unsigned<unsigned>(t_scale, "ppl_set_deterministic_timeout/2");
    typedef Parma_Polyhedra_Library::Weightwatch_Traits Traits;
    p_deterministic_timeout_object
      = new Weightwatch(Traits::compute_delta(unscaled_weight, scale),
                        abandon_expensive_computations, e);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_reset_deterministic_timeout() {
  try {
    reset_deterministic_timeout();
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_Coefficient_bits(Prolog_term_ref t_bits) {
  try {
    if (unify_ulong(t_bits, PPL_COEFFICIENT_BITS))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_Coefficient_is_bounded() {
  try {
    if (std::numeric_limits<Coefficient>::is_bounded)
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_Coefficient_min(Prolog_term_ref t_min) {
  try {
    if (std::numeric_limits<Coefficient>::is_bounded) {
      PPL_DIRTY_TEMP_COEFFICIENT(min);
      min = std::numeric_limits<Coefficient>::min();
      if (Prolog_has_unbounded_integers
	  || (min >= Prolog_min_integer && min <= Prolog_min_integer))
	return Prolog_unify_Coefficient(t_min, min)
	  ? PROLOG_SUCCESS : PROLOG_FAILURE;
    }
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_Coefficient_max(Prolog_term_ref t_max) {
  try {
    if (std::numeric_limits<Coefficient>::is_bounded) {
      PPL_DIRTY_TEMP_COEFFICIENT(max);
      max = std::numeric_limits<Coefficient>::max();
      if (Prolog_has_unbounded_integers
	  || (max >= Prolog_min_integer && max <= Prolog_min_integer))
	return Prolog_unify_Coefficient(t_max, max)
	  ? PROLOG_SUCCESS : PROLOG_FAILURE;
    }
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_MIP_Problem_from_space_dimension
(Prolog_term_ref t_nd, Prolog_term_ref t_mip) {
  static const char* where = "ppl_MIP_Problem_from_space_dimension/2";
  try {
    dimension_type d = term_to_unsigned<dimension_type>(t_nd, where);
    MIP_Problem* mip = new MIP_Problem(d);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, mip);
    if (Prolog_unify(t_mip, tmp)) {
      PPL_REGISTER(mip);
      return PROLOG_SUCCESS;
    }
    else
      delete mip;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_MIP_Problem(Prolog_term_ref t_nd,
		    Prolog_term_ref t_clist,
		    Prolog_term_ref t_le_expr,
		    Prolog_term_ref t_opt,
		    Prolog_term_ref t_mip) {
  static const char* where = "ppl_new_MIP_Problem/5";
  try {
    Constraint_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();
    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_constraint(c, where));
    }
    // Check the list is properly terminated.
    check_nil_terminating(t_clist, where);

    const Linear_Expression le = build_linear_expression(t_le_expr, where);
    Prolog_atom opt = term_to_optimization_mode(t_opt, where);
    Optimization_Mode mode = (opt == a_max) ? MAXIMIZATION : MINIMIZATION;

    MIP_Problem* mip
      = new MIP_Problem(term_to_unsigned<dimension_type>(t_nd, where),
			cs, le, mode);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, mip);
    if (Prolog_unify(t_mip, tmp)) {
      PPL_REGISTER(mip);
      return PROLOG_SUCCESS;
    }
    else
      delete mip;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_MIP_Problem_from_MIP_Problem(Prolog_term_ref t_mip_source,
				     Prolog_term_ref t_mip) {
  static const char* where = "ppl_new_MIP_Problem_from_MIP_Problem/2";
  try {
    const MIP_Problem* mip_source
      = static_cast<const MIP_Problem*>
      (term_to_handle<MIP_Problem>(t_mip_source, where));
    PPL_CHECK(mip_source);
    MIP_Problem* mip = new MIP_Problem(*mip_source);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, mip);
    if (Prolog_unify(t_mip, tmp)) {
      PPL_REGISTER(mip);
      return PROLOG_SUCCESS;
    }
    else
      delete mip;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_swap(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_MIP_Problem_swap/2";
  try {
    MIP_Problem* lhs = term_to_handle<MIP_Problem>(t_lhs, where);
    MIP_Problem* rhs = term_to_handle<MIP_Problem>(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    swap(*lhs, *rhs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_delete_MIP_Problem(Prolog_term_ref t_mip) {
  static const char* where = "ppl_delete_MIP_Problem/1";
  try {
    const MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_UNREGISTER(mip);
    delete mip;
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_space_dimension(Prolog_term_ref t_mip, Prolog_term_ref t_sd) {
  static const char* where = "ppl_MIP_Problem_space_dimension/2";
  try {
    const MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);
    if (unify_ulong(t_sd, mip->space_dimension()))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_integer_space_dimensions(Prolog_term_ref t_mip,
					 Prolog_term_ref t_vlist) {
  static const char* where = "ppl_MIP_Problem_integer_space_dimensions/2";
  try {
    const MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    const Variables_Set& i_vars = mip->integer_space_dimensions();

    for (Variables_Set::const_iterator i = i_vars.begin(),
	   i_end = i_vars.end(); i != i_end; ++i)
      Prolog_construct_cons(tail, variable_term(*i), tail);

    if (Prolog_unify(t_vlist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_constraints(Prolog_term_ref t_mip,
			    Prolog_term_ref t_clist) {
  static const char* where = "ppl_MIP_Problem_constraints/2";
  try {
    const MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    for (MIP_Problem::const_iterator i = mip->constraints_begin(),
	   i_end = mip->constraints_end(); i != i_end; ++i)
      Prolog_construct_cons(tail, constraint_term(*i), tail);

    if (Prolog_unify(t_clist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_objective_function(Prolog_term_ref t_mip,
				   Prolog_term_ref t_le_expr) {
  static const char* where = "ppl_MIP_Problem_objective_function/2";
  try {
    const MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);

    const Linear_Expression& le = mip->objective_function();
    Prolog_term_ref t = get_linear_expression(le);

    if (Prolog_unify(t_le_expr, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_optimization_mode(Prolog_term_ref t_mip,
				  Prolog_term_ref t_opt) {
  static const char* where = "ppl_MIP_Problem_optimization_mode/2";
  try {
    MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);

    Optimization_Mode mode = mip->optimization_mode();
    Prolog_term_ref t = Prolog_new_term_ref();
    Prolog_atom a = (mode == MAXIMIZATION) ? a_max : a_min;
    Prolog_put_atom(t, a);
    if (Prolog_unify(t_opt, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_clear(Prolog_term_ref t_mip) {
  static const char* where = "ppl_MIP_Problem_clear/1";
  try {
    MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);
    mip->clear();
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_add_space_dimensions_and_embed
(Prolog_term_ref t_mip, Prolog_term_ref t_nnd) {
  static const char* where
    = "ppl_MIP_Problem_add_space_dimensions_and_embed/2";
  try {
    MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);
    dimension_type d = term_to_unsigned<dimension_type>(t_nnd, where);
    mip->add_space_dimensions_and_embed(d);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_add_to_integer_space_dimensions(Prolog_term_ref t_mip,
						Prolog_term_ref t_vlist) {
  static const char* where
    = "ppl_MIP_Problem_add_to_integer_space_dimensions/2";
  try {
    MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);
    Variables_Set i_vars;
    Prolog_term_ref v = Prolog_new_term_ref();

    while (Prolog_is_cons(t_vlist)) {
      Prolog_get_cons(t_vlist, v, t_vlist);
      i_vars.insert(term_to_Variable(v, where).id());
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_vlist, where);

    mip->add_to_integer_space_dimensions(i_vars);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_add_constraint(Prolog_term_ref t_mip, Prolog_term_ref t_c) {
  static const char* where = "ppl_MIP_Problem_add_constraint/2";
  try {
    MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);
    mip->add_constraint(build_constraint(t_c, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_add_constraints(Prolog_term_ref t_mip,
				Prolog_term_ref t_clist) {
  static const char* where = "ppl_MIP_Problem_add_constraints/2";
  try {
    MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);
    Constraint_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_constraint(c, where));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist, where);

    mip->add_constraints(cs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_set_objective_function(Prolog_term_ref t_mip,
				       Prolog_term_ref t_le_expr) {
  static const char* where = "ppl_MIP_Problem_set_objective_function/2";
  try {
    MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);
    mip->set_objective_function(build_linear_expression(t_le_expr, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_set_optimization_mode(Prolog_term_ref t_mip,
				      Prolog_term_ref t_opt) {
  static const char* where = "ppl_MIP_Problem_set_optimization_mode/2";
  try {
    MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);

    Prolog_atom opt = term_to_optimization_mode(t_opt, where);
    Optimization_Mode mode = (opt == a_max) ? MAXIMIZATION : MINIMIZATION;
    mip->set_optimization_mode(mode);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_set_control_parameter(Prolog_term_ref t_mip,
				      Prolog_term_ref t_cp_value) {
  static const char* where = "ppl_MIP_Problem_set_control_parameter/2";
  try {
    MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);

    Prolog_atom cp_value = term_to_control_parameter_value(t_cp_value, where);
    if (cp_value == a_pricing_steepest_edge_float)
      mip->set_control_parameter(MIP_Problem::PRICING_STEEPEST_EDGE_FLOAT);
    else if (cp_value == a_pricing_steepest_edge_exact)
      mip->set_control_parameter(MIP_Problem::PRICING_STEEPEST_EDGE_EXACT);
    else if (cp_value == a_pricing_textbook)
      mip->set_control_parameter(MIP_Problem::PRICING_TEXTBOOK);
    else
      throw unknown_interface_error("ppl_MIP_Problem_get_control_parameter()");
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_get_control_parameter(Prolog_term_ref t_mip,
                                      Prolog_term_ref t_cp_name,
                                      Prolog_term_ref t_cp_value) {
  static const char* where = "ppl_MIP_Problem_get_control_parameter/3";
  try {
    MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);
    Prolog_atom cp_name = term_to_control_parameter_name(t_cp_name, where);
    MIP_Problem::Control_Parameter_Value ppl_cp_value;
    if (cp_name == a_pricing)
      ppl_cp_value = mip->get_control_parameter(MIP_Problem::PRICING);
    else
      throw unknown_interface_error("ppl_MIP_Problem_get_control_parameter()");

    Prolog_term_ref t = Prolog_new_term_ref();
    Prolog_atom a;
    switch (ppl_cp_value) {
    case MIP_Problem::PRICING_STEEPEST_EDGE_FLOAT:
      a = a_pricing_steepest_edge_float;
      break;
    case MIP_Problem::PRICING_STEEPEST_EDGE_EXACT:
      a = a_pricing_steepest_edge_exact;
      break;
    case MIP_Problem::PRICING_TEXTBOOK:
      a = a_pricing_textbook;
      break;
    default:
      throw unknown_interface_error("ppl_MIP_Problem_get_control_parameter()");
    }
    Prolog_put_atom(t, a);
    if (Prolog_unify(t_cp_value, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_is_satisfiable(Prolog_term_ref t_mip) {
  static const char* where = "ppl_MIP_Problem_is_satisfiable/1";
  try {
    const MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);
    if (mip->is_satisfiable())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_solve(Prolog_term_ref t_mip, Prolog_term_ref t_status) {
  static const char* where = "ppl_MIP_Problem_solve/2";
  try {
    const MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);

    Prolog_atom a;
    switch (mip->solve()) {
    case UNFEASIBLE_MIP_PROBLEM:
      a = a_unfeasible;
      break;
    case UNBOUNDED_MIP_PROBLEM:
      a = a_unbounded;
      break;
    case OPTIMIZED_MIP_PROBLEM:
      a = a_optimized;
      break;
    default:
      throw unknown_interface_error("ppl_MIP_Problem_solve()");
    }
    Prolog_term_ref t = Prolog_new_term_ref();
    Prolog_put_atom(t, a);
    if (Prolog_unify(t_status, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_feasible_point(Prolog_term_ref t_mip,
			       Prolog_term_ref t_g) {
  static const char* where = "ppl_MIP_Problem_feasible_point/2";
  try {
    const MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);
    const Generator& g = mip->feasible_point();
    if (Prolog_unify(t_g, generator_term(g)))
      return PROLOG_SUCCESS;
 }
 CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_optimizing_point(Prolog_term_ref t_mip,
				 Prolog_term_ref t_g) {
  static const char* where = "ppl_MIP_Problem_optimizing_point/2";
  try {
    const MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);
    const Generator& g = mip->optimizing_point();
    if (Prolog_unify(t_g, generator_term(g)))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_optimal_value(Prolog_term_ref t_mip,
			      Prolog_term_ref t_n,
			      Prolog_term_ref t_d) {
  static const char* where = "ppl_MIP_Problem_optimal_value/3";
  try {
    const MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);
    PPL_DIRTY_TEMP_COEFFICIENT(n);
    PPL_DIRTY_TEMP_COEFFICIENT(d);
    mip->optimal_value(n, d);
    if (Prolog_unify_Coefficient(t_n, n)
	&& Prolog_unify_Coefficient(t_d, d))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_evaluate_objective_function(Prolog_term_ref t_mip,
					    Prolog_term_ref t_g,
					    Prolog_term_ref t_n,
					    Prolog_term_ref t_d) {
  static const char* where = "ppl_MIP_Problem_evaluate_objective_function/4";
  try {
    const MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);
    PPL_DIRTY_TEMP_COEFFICIENT(n);
    PPL_DIRTY_TEMP_COEFFICIENT(d);
    mip->evaluate_objective_function(build_generator(t_g, where), n, d);
    if (Prolog_unify_Coefficient(t_n, n)
	&& Prolog_unify_Coefficient(t_d, d))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_OK(Prolog_term_ref t_mip) {
  static const char* where = "ppl_MIP_Problem_OK/1";
  try {
    const MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);
    if (mip->OK())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_MIP_Problem_ascii_dump(Prolog_term_ref t_mip) {
  static const char* where = "ppl_MIP_Problem_ascii_dump/1";
  try {
    const MIP_Problem* mip = term_to_handle<MIP_Problem>(t_mip, where);
    PPL_CHECK(mip);
    mip->ascii_dump(std::cout);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_PIP_Problem_from_space_dimension
(Prolog_term_ref t_nd, Prolog_term_ref t_pip) {
  static const char* where = "ppl_PIP_Problem_from_space_dimension/2";
  try {
    dimension_type d = term_to_unsigned<dimension_type>(t_nd, where);
    PIP_Problem* pip = new PIP_Problem(d);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, pip);
    if (Prolog_unify(t_pip, tmp)) {
      PPL_REGISTER(pip);
      return PROLOG_SUCCESS;
    }
    else
      delete pip;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_PIP_Problem(Prolog_term_ref t_dim,
                    Prolog_term_ref t_cs,
                    Prolog_term_ref t_params,
                    Prolog_term_ref t_pip) {
  static const char* where = "ppl_new_PIP_Problem/4";
  try {
    dimension_type dim = term_to_unsigned<dimension_type>(t_dim, where);
    Constraint_System cs;
    Prolog_term_ref t_c = Prolog_new_term_ref();
    while (Prolog_is_cons(t_cs)) {
      Prolog_get_cons(t_cs, t_c, t_cs);
      cs.insert(build_constraint(t_c, where));
    }
    // Check the list is properly terminated.
    check_nil_terminating(t_cs, where);

    Variables_Set params;
    Prolog_term_ref t_par = Prolog_new_term_ref();
    while (Prolog_is_cons(t_params)) {
      Prolog_get_cons(t_params, t_par, t_params);
      params.insert(term_to_Variable(t_par, where).id());
    }
    // Check the list is properly terminated.
    check_nil_terminating(t_params, where);

    PIP_Problem* pip = new PIP_Problem(dim, cs.begin(), cs.end(), params);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, pip);
    if (Prolog_unify(t_pip, tmp)) {
      PPL_REGISTER(pip);
      return PROLOG_SUCCESS;
    }
    else
      delete pip;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_new_PIP_Problem_from_PIP_Problem(Prolog_term_ref t_pip_source,
				     Prolog_term_ref t_pip) {
  static const char* where = "ppl_new_PIP_Problem_from_PIP_Problem/2";
  try {
    const PIP_Problem* pip_source
      = static_cast<const PIP_Problem*>
      (term_to_handle<PIP_Problem>(t_pip_source, where));
    PPL_CHECK(pip_source);
    PIP_Problem* pip = new PIP_Problem(*pip_source);
    Prolog_term_ref tmp = Prolog_new_term_ref();
    Prolog_put_address(tmp, pip);
    if (Prolog_unify(t_pip, tmp)) {
      PPL_REGISTER(pip);
      return PROLOG_SUCCESS;
    }
    else
      delete pip;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_swap(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs) {
  static const char* where = "ppl_PIP_Problem_swap/2";
  try {
    PIP_Problem* lhs = term_to_handle<PIP_Problem>(t_lhs, where);
    PIP_Problem* rhs = term_to_handle<PIP_Problem>(t_rhs, where);
    PPL_CHECK(lhs);
    PPL_CHECK(rhs);
    swap(*lhs, *rhs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_delete_PIP_Problem(Prolog_term_ref t_pip) {
  static const char* where = "ppl_delete_PIP_Problem/1";
  try {
    const PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_UNREGISTER(pip);
    delete pip;
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_space_dimension(Prolog_term_ref t_pip, Prolog_term_ref t_sd) {
  static const char* where = "ppl_PIP_Problem_space_dimension/2";
  try {
    const PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);
    if (unify_ulong(t_sd, pip->space_dimension()))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_parameter_space_dimensions(Prolog_term_ref t_pip,
                                           Prolog_term_ref t_vlist) {
  static const char* where = "ppl_PIP_Problem_parameter_space_dimensions/2";
  try {
    const PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    const Variables_Set& params = pip->parameter_space_dimensions();

    for (Variables_Set::const_iterator i = params.begin(),
	   i_end = params.end(); i != i_end; ++i)
      Prolog_construct_cons(tail, variable_term(*i), tail);

    if (Prolog_unify(t_vlist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_constraints(Prolog_term_ref t_pip,
			    Prolog_term_ref t_clist) {
  static const char* where = "ppl_PIP_Problem_constraints/2";
  try {
    const PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    for (PIP_Problem::const_iterator i = pip->constraints_begin(),
	   i_end = pip->constraints_end(); i != i_end; ++i)
      Prolog_construct_cons(tail, constraint_term(*i), tail);

    if (Prolog_unify(t_clist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_clear(Prolog_term_ref t_pip) {
  static const char* where = "ppl_PIP_Problem_clear/1";
  try {
    PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);
    pip->clear();
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_add_space_dimensions_and_embed
(Prolog_term_ref t_pip,
 Prolog_term_ref t_num_vars,
 Prolog_term_ref t_num_params) {
  static const char* where
    = "ppl_PIP_Problem_add_space_dimensions_and_embed/3";
  try {
    PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);
    dimension_type nv = term_to_unsigned<dimension_type>(t_num_vars, where);
    dimension_type np = term_to_unsigned<dimension_type>(t_num_params, where);
    pip->add_space_dimensions_and_embed(nv, np);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_add_to_parameter_space_dimensions(Prolog_term_ref t_pip,
                                                  Prolog_term_ref t_vlist) {
  static const char* where
    = "ppl_PIP_Problem_add_to_parameter_space_dimensions/2";
  try {
    PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);
    Variables_Set params;
    Prolog_term_ref v = Prolog_new_term_ref();

    while (Prolog_is_cons(t_vlist)) {
      Prolog_get_cons(t_vlist, v, t_vlist);
      params.insert(term_to_Variable(v, where).id());
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_vlist, where);

    pip->add_to_parameter_space_dimensions(params);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_add_constraint(Prolog_term_ref t_pip, Prolog_term_ref t_c) {
  static const char* where = "ppl_PIP_Problem_add_constraint/2";
  try {
    PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);
    pip->add_constraint(build_constraint(t_c, where));
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_add_constraints(Prolog_term_ref t_pip,
				Prolog_term_ref t_clist) {
  static const char* where = "ppl_PIP_Problem_add_constraints/2";
  try {
    PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);
    Constraint_System cs;
    Prolog_term_ref c = Prolog_new_term_ref();

    while (Prolog_is_cons(t_clist)) {
      Prolog_get_cons(t_clist, c, t_clist);
      cs.insert(build_constraint(c, where));
    }

    // Check the list is properly terminated.
    check_nil_terminating(t_clist, where);

    pip->add_constraints(cs);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_get_control_parameter(Prolog_term_ref t_pip,
                                      Prolog_term_ref t_cp_name,
                                      Prolog_term_ref t_cp_value) {
  static const char* where = "ppl_PIP_Problem_get_control_parameter/3";
  try {
    PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);
    Prolog_atom cp_name = term_to_pip_problem_control_parameter_name(t_cp_name, where);
    PIP_Problem::Control_Parameter_Value ppl_cp_value;
    Prolog_atom a;
    if (cp_name == a_cutting_strategy) {
      ppl_cp_value
        = pip->get_control_parameter(PIP_Problem::CUTTING_STRATEGY);
      switch (ppl_cp_value) {
      case PIP_Problem::CUTTING_STRATEGY_FIRST:
        a = a_cutting_strategy_first;
        break;
      case PIP_Problem::CUTTING_STRATEGY_DEEPEST:
        a = a_cutting_strategy_deepest;
        break;
      case PIP_Problem::CUTTING_STRATEGY_ALL:
        a = a_cutting_strategy_all;
        break;
      default:
        throw unknown_interface_error(
          "ppl_PIP_Problem_get_control_parameter()");
      }
    }
    else if (cp_name == a_pivot_row_strategy) {
      ppl_cp_value
        = pip->get_control_parameter(PIP_Problem::PIVOT_ROW_STRATEGY);
      switch (ppl_cp_value) {
      case PIP_Problem::PIVOT_ROW_STRATEGY_FIRST:
        a = a_pivot_row_strategy_first;
        break;
      case PIP_Problem::PIVOT_ROW_STRATEGY_MAX_COLUMN:
        a = a_pivot_row_strategy_max_column;
        break;
      default:
        throw unknown_interface_error(
          "ppl_PIP_Problem_get_control_parameter()");
      }
    }
    else
      throw unknown_interface_error("ppl_PIP_Problem_get_control_parameter()");

    Prolog_term_ref t = Prolog_new_term_ref();
    Prolog_put_atom(t, a);
    if (Prolog_unify(t_cp_value, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_set_control_parameter(Prolog_term_ref t_pip,
				      Prolog_term_ref t_cp_value) {
  static const char* where = "ppl_PIP_Problem_set_control_parameter/2";
  try {
    PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);

    Prolog_atom cp_value = term_to_pip_problem_control_parameter_value(t_cp_value, where);
    if (cp_value == a_cutting_strategy_first)
      pip->set_control_parameter(PIP_Problem::CUTTING_STRATEGY_FIRST);
    else if (cp_value == a_cutting_strategy_deepest)
      pip->set_control_parameter(PIP_Problem::CUTTING_STRATEGY_DEEPEST);
    else if (cp_value == a_cutting_strategy_all)
      pip->set_control_parameter(PIP_Problem::CUTTING_STRATEGY_ALL);
    else if (cp_value == a_pivot_row_strategy_first)
      pip->set_control_parameter(PIP_Problem::PIVOT_ROW_STRATEGY_FIRST);
    else if (cp_value == a_pivot_row_strategy_max_column)
      pip->set_control_parameter(PIP_Problem::PIVOT_ROW_STRATEGY_MAX_COLUMN);
    else
      throw unknown_interface_error("ppl_PIP_Problem_set_control_parameter()");
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_has_big_parameter_dimension(Prolog_term_ref t_pip,
                                            Prolog_term_ref t_d) {
  static const char* where = "ppl_PIP_Problem_get_big_parameter_dimension/2";
  try {
    PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);
    dimension_type dim = pip->get_big_parameter_dimension();
    if (dim == not_a_dimension())
      return PROLOG_FAILURE;
    if (unify_ulong(t_d, dim))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_set_big_parameter_dimension(Prolog_term_ref t_pip,
                                            Prolog_term_ref t_d) {
  static const char* where = "ppl_MIP_Problem_set_big_parameter_dimension/2";
  try {
    PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);
    dimension_type d = term_to_unsigned<dimension_type>(t_d, where);
    pip->set_big_parameter_dimension(d);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_is_satisfiable(Prolog_term_ref t_pip) {
  static const char* where = "ppl_PIP_Problem_is_satisfiable/1";
  try {
    const PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);
    if (pip->is_satisfiable())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_solve(Prolog_term_ref t_pip, Prolog_term_ref t_status) {
  static const char* where = "ppl_PIP_Problem_solve/2";
  try {
    const PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);

    Prolog_atom a;
    switch (pip->solve()) {
    case UNFEASIBLE_PIP_PROBLEM:
      a = a_unfeasible;
      break;
    case OPTIMIZED_PIP_PROBLEM:
      a = a_optimized;
      break;
    default:
      throw unknown_interface_error("ppl_PIP_Problem_solve()");
    }
    Prolog_term_ref t = Prolog_new_term_ref();
    Prolog_put_atom(t, a);
    if (Prolog_unify(t_status, t))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_solution(Prolog_term_ref t_pip,
                         Prolog_term_ref t_pip_tree) {
  static const char* where = "ppl_PIP_Problem_solution/2";
  try {
    const PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);
    PIP_Tree_Node* sol = const_cast<PIP_Tree_Node*>(pip->solution());
    Prolog_term_ref t_sol = Prolog_new_term_ref();
    Prolog_put_address(t_sol, sol);
    if (Prolog_unify(t_pip_tree, t_sol)) {
      PPL_WEAK_REGISTER(sol);
      return PROLOG_SUCCESS;
    }
 }
 CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_optimizing_solution(Prolog_term_ref t_pip,
                                    Prolog_term_ref t_pip_tree) {
  static const char* where = "ppl_PIP_Problem_optimizing_solution/2";
  try {
    const PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);
    PIP_Tree_Node* sol = const_cast<PIP_Tree_Node*>(pip->optimizing_solution());
    Prolog_term_ref t_sol = Prolog_new_term_ref();
    Prolog_put_address(t_sol, sol);
    if (Prolog_unify(t_pip_tree, t_sol)) {
      PPL_WEAK_REGISTER(sol);
      return PROLOG_SUCCESS;
    }
 }
 CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_OK(Prolog_term_ref t_pip) {
  static const char* where = "ppl_PIP_Problem_OK/1";
  try {
    const PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);
    if (pip->OK())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Problem_ascii_dump(Prolog_term_ref t_pip) {
  static const char* where = "ppl_PIP_Problem_ascii_dump/1";
  try {
    const PIP_Problem* pip = term_to_handle<PIP_Problem>(t_pip, where);
    PPL_CHECK(pip);
    pip->ascii_dump(std::cout);
    return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Tree_Node_constraints(Prolog_term_ref t_pip,
			      Prolog_term_ref t_cs) {
  static const char* where = "ppl_PIP_Tree_Node_constraints/2";
  try {
    const PIP_Tree_Node* pip = term_to_handle<PIP_Tree_Node>(t_pip, where);
    PPL_CHECK(pip);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    const Constraint_System& ppl_cs = pip->constraints();
    for (Constraint_System::const_iterator i = ppl_cs.begin(),
           ppl_cs_end = ppl_cs.end(); i != ppl_cs_end; ++i)
      Prolog_construct_cons(tail, constraint_term(*i), tail);

    if (Prolog_unify(t_cs, tail)) {
      return PROLOG_SUCCESS;
    }
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Tree_Node_is_solution(Prolog_term_ref t_pip) {
  static const char* where = "ppl_PIP_Tree_Node_as_solution/2";
  try {
    const PIP_Tree_Node* pip = term_to_handle<PIP_Tree_Node>(t_pip, where);
    PPL_CHECK(pip);

    if (pip != 0 && pip->as_solution() != 0)
      return PROLOG_SUCCESS;
    return PROLOG_FAILURE;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Tree_Node_is_decision(Prolog_term_ref t_pip) {
  static const char* where = "ppl_PIP_Tree_Node_as_decision/2";
  try {
    const PIP_Tree_Node* pip = term_to_handle<PIP_Tree_Node>(t_pip, where);
    PPL_CHECK(pip);

    if (pip != 0 && pip->as_decision() != 0)
      return PROLOG_SUCCESS;
    return PROLOG_FAILURE;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Tree_Node_is_bottom(Prolog_term_ref t_pip) {
  static const char* where = "ppl_PIP_Tree_Node_as_decision/2";
  try {
    const PIP_Tree_Node* pip = term_to_handle<PIP_Tree_Node>(t_pip, where);
    PPL_CHECK(pip);

    if (pip == 0)
      return PROLOG_SUCCESS;
    return PROLOG_FAILURE;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Tree_Node_artificials(Prolog_term_ref t_tree_node,
                              Prolog_term_ref t_artlist) {
  static const char* where = "ppl_PIP_Tree_Node_artificials/2";
  try {
    const PIP_Tree_Node* node
      = term_to_handle<PIP_Tree_Node>(t_tree_node, where);
    PPL_CHECK(node);

    Prolog_term_ref tail = Prolog_new_term_ref();
    Prolog_put_atom(tail, a_nil);
    for (PIP_Tree_Node::Artificial_Parameter_Sequence::const_iterator
           i = node->art_parameter_begin(),
           arts_end = node->art_parameter_end(); i != arts_end; ++i)
      Prolog_construct_cons(tail, artificial_parameter_term(*i), tail);

    if (Prolog_unify(t_artlist, tail))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Tree_Node_parametric_values(Prolog_term_ref t_pip,
			                    Prolog_term_ref t_var,
			                    Prolog_term_ref t_le) {
  static const char* where = "ppl_PIP_Solution_Node_get_parametric_values/3";
  try {
    const PIP_Solution_Node* pip
      = term_to_handle<PIP_Solution_Node>(t_pip, where);
    PPL_CHECK(pip);
    Variable var = term_to_Variable(t_var, where);
    if (Prolog_unify(t_le, get_linear_expression(pip->parametric_values(var))))
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Tree_Node_true_child(Prolog_term_ref t_pip,
			             Prolog_term_ref t_ptree) {
  static const char* where = "ppl_PIP_Decision_Node_get_true_child/2";
  try {
    const PIP_Decision_Node* pip
      = term_to_handle<PIP_Decision_Node>(t_pip, where);
    PPL_CHECK(pip);
    bool b = true;
    PIP_Tree_Node* ppl_ptree = const_cast<PIP_Tree_Node*>(pip->child_node(b));
    Prolog_term_ref t_ppl_ptree = Prolog_new_term_ref();
    Prolog_put_address(t_ppl_ptree, ppl_ptree);
    if (Prolog_unify(t_ptree, t_ppl_ptree)) {
      PPL_WEAK_REGISTER(ppl_ptree);
      return PROLOG_SUCCESS;
    }
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Tree_Node_false_child(Prolog_term_ref t_pip,
			              Prolog_term_ref t_ptree) {
  static const char* where = "ppl_PIP_Decision_Node_get_false_child/2";
  try {
    const PIP_Decision_Node* pip
      = term_to_handle<PIP_Decision_Node>(t_pip, where);
    PPL_CHECK(pip);
    bool b = false;
    PIP_Tree_Node* ppl_ptree = const_cast<PIP_Tree_Node*>(pip->child_node(b));
    Prolog_term_ref t_ppl_ptree = Prolog_new_term_ref();
    Prolog_put_address(t_ppl_ptree, ppl_ptree);
    if (Prolog_unify(t_ptree, t_ppl_ptree)) {
      PPL_WEAK_REGISTER(ppl_ptree);
      return PROLOG_SUCCESS;
    }
  }
  CATCH_ALL;
}

extern "C" Prolog_foreign_return_type
ppl_PIP_Tree_Node_OK(Prolog_term_ref t_pip) {
  static const char* where = "ppl_PIP_Tree_Node_OK/1";
  try {
    const PIP_Tree_Node* pip = term_to_handle<PIP_Tree_Node>(t_pip, where);
    PPL_CHECK(pip);
    if (pip->OK())
      return PROLOG_SUCCESS;
  }
  CATCH_ALL;
}
