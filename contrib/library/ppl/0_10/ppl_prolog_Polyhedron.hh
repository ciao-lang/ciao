/* Prolog Polyhedron interface code: declarations.
   Copyright (C) 2001-2008 Roberto Bagnara <bagnara@cs.unipr.it>

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
site: http://www.cs.unipr.it/ppl/ . */

extern "C" Prolog_foreign_return_type
  ppl_delete_Polyhedron(Prolog_term_ref t_ph);
extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_space_dimension(Prolog_term_ref t_nd,
                                               Prolog_term_ref t_uoe,
                                               Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_space_dimension(Prolog_term_ref t_nd,
                                               Prolog_term_ref t_uoe,
                                               Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_C_Polyhedron(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_NNC_Polyhedron(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_Grid(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_Rational_Box(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_BD_Shape_mpz_class(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_BD_Shape_mpq_class(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_Octagonal_Shape_mpz_class(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_Octagonal_Shape_mpq_class(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_Double_Box(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_BD_Shape_double(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_Octagonal_Shape_double(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_C_Polyhedron(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_NNC_Polyhedron(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_Grid(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_Rational_Box(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_BD_Shape_mpz_class(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_BD_Shape_mpq_class(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_Double_Box(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_BD_Shape_double(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_Octagonal_Shape_double(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_C_Polyhedron_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_NNC_Polyhedron_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_Grid_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_Rational_Box_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_BD_Shape_mpz_class_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_BD_Shape_mpq_class_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_Double_Box_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_BD_Shape_double_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_C_Polyhedron_from_Octagonal_Shape_double_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_C_Polyhedron_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_Grid_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_Rational_Box_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_BD_Shape_mpz_class_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_BD_Shape_mpq_class_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_Double_Box_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_BD_Shape_double_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_NNC_Polyhedron_from_Octagonal_Shape_double_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
  ppl_new_C_Polyhedron_from_constraints(Prolog_term_ref t_clist,
						    Prolog_term_ref t_ph);
extern "C" Prolog_foreign_return_type
  ppl_new_NNC_Polyhedron_from_constraints(Prolog_term_ref t_clist,
						    Prolog_term_ref t_ph);
extern "C" Prolog_foreign_return_type
  ppl_new_C_Polyhedron_from_congruences(Prolog_term_ref t_clist,
						    Prolog_term_ref t_ph);
extern "C" Prolog_foreign_return_type
  ppl_new_NNC_Polyhedron_from_congruences(Prolog_term_ref t_clist,
						    Prolog_term_ref t_ph);
extern "C" Prolog_foreign_return_type
  ppl_new_C_Polyhedron_from_generators(Prolog_term_ref t_clist,
						    Prolog_term_ref t_ph);
extern "C" Prolog_foreign_return_type
  ppl_new_NNC_Polyhedron_from_generators(Prolog_term_ref t_clist,
						    Prolog_term_ref t_ph);
extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_swap(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_space_dimension(Prolog_term_ref t_ph, Prolog_term_ref t_sd);
extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_affine_dimension(Prolog_term_ref t_ph, Prolog_term_ref t_sd);
extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_relation_with_constraint(Prolog_term_ref t_ph,
						 Prolog_term_ref t_c,
						 Prolog_term_ref t_r);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_relation_with_generator(Prolog_term_ref t_ph,
						 Prolog_term_ref t_c,
						 Prolog_term_ref t_r);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_relation_with_congruence(Prolog_term_ref t_ph,
						 Prolog_term_ref t_c,
						 Prolog_term_ref t_r);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_get_constraints(Prolog_term_ref t_ph,
				   Prolog_term_ref t_glist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_get_congruences(Prolog_term_ref t_ph,
				   Prolog_term_ref t_glist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_get_generators(Prolog_term_ref t_ph,
				   Prolog_term_ref t_glist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_get_minimized_constraints(Prolog_term_ref t_ph,
					     Prolog_term_ref t_glist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_get_minimized_congruences(Prolog_term_ref t_ph,
					     Prolog_term_ref t_glist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_get_minimized_generators(Prolog_term_ref t_ph,
					     Prolog_term_ref t_glist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_is_empty(Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_is_universe(Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_is_bounded(Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_contains_integer_point(Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_is_topologically_closed(Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_is_discrete(Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_topological_closure_assign(Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_bounds_from_above(Prolog_term_ref t_ph,
				       Prolog_term_ref t_expr);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_bounds_from_below(Prolog_term_ref t_ph,
				       Prolog_term_ref t_expr);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_maximize(Prolog_term_ref t_ph, Prolog_term_ref t_le_expr,
		       Prolog_term_ref t_n,  Prolog_term_ref t_d,
		       Prolog_term_ref t_maxmin);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_minimize(Prolog_term_ref t_ph, Prolog_term_ref t_le_expr,
		       Prolog_term_ref t_n,  Prolog_term_ref t_d,
		       Prolog_term_ref t_maxmin);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_maximize_with_point(Prolog_term_ref t_ph,
				  Prolog_term_ref t_le_expr,
				  Prolog_term_ref t_n,
                                  Prolog_term_ref t_d,
				  Prolog_term_ref t_maxmin,
                                  Prolog_term_ref t_g);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_minimize_with_point(Prolog_term_ref t_ph,
				  Prolog_term_ref t_le_expr,
				  Prolog_term_ref t_n,
                                  Prolog_term_ref t_d,
				  Prolog_term_ref t_maxmin,
                                  Prolog_term_ref t_g);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_contains_Polyhedron(Prolog_term_ref t_lhs,
				   Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_strictly_contains_Polyhedron(Prolog_term_ref t_lhs,
				   Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_is_disjoint_from_Polyhedron(Prolog_term_ref t_lhs,
				   Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_equals_Polyhedron(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_OK(Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_add_constraint(Prolog_term_ref t_ph, Prolog_term_ref t_c);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_add_congruence(Prolog_term_ref t_ph, Prolog_term_ref t_c);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_add_generator(Prolog_term_ref t_ph, Prolog_term_ref t_c);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_add_constraints(Prolog_term_ref t_ph,
				   Prolog_term_ref t_clist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_add_congruences(Prolog_term_ref t_ph,
				   Prolog_term_ref t_clist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_add_generators(Prolog_term_ref t_ph,
				   Prolog_term_ref t_clist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_refine_with_constraint(Prolog_term_ref t_ph, Prolog_term_ref t_c);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_refine_with_congruence(Prolog_term_ref t_ph, Prolog_term_ref t_c);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_refine_with_constraints(Prolog_term_ref t_ph,
				   Prolog_term_ref t_clist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_refine_with_congruences(Prolog_term_ref t_ph,
				   Prolog_term_ref t_clist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_intersection_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_upper_bound_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_difference_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_concatenate_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_time_elapse_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_poly_hull_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_poly_difference_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_upper_bound_assign_if_exact
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_poly_hull_assign_if_exact
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_simplify_using_context_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_b);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_constrains(Prolog_term_ref t_ph,
                          Prolog_term_ref t_v);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_unconstrain_space_dimension(Prolog_term_ref t_ph,
                           Prolog_term_ref t_v);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_unconstrain_space_dimensions(Prolog_term_ref t_ph,
                           Prolog_term_ref t_vlist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_affine_image
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_le, Prolog_term_ref t_d);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_affine_preimage
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_le, Prolog_term_ref t_d);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_bounded_affine_image
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_lb_le, Prolog_term_ref t_ub_le,
   Prolog_term_ref t_d);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_bounded_affine_preimage
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_lb_le, Prolog_term_ref t_ub_le,
   Prolog_term_ref t_d);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_generalized_affine_image
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_r, Prolog_term_ref t_le,
   Prolog_term_ref t_d);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_generalized_affine_preimage
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_r, Prolog_term_ref t_le,
   Prolog_term_ref t_d);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_generalized_affine_image_lhs_rhs
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_lhs, Prolog_term_ref t_r, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_generalized_affine_preimage_lhs_rhs
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_lhs, Prolog_term_ref t_r, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_add_space_dimensions_and_embed
  (Prolog_term_ref t_ph, Prolog_term_ref t_nnd);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_add_space_dimensions_and_project
  (Prolog_term_ref t_ph, Prolog_term_ref t_nnd);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_remove_space_dimensions
  (Prolog_term_ref t_ph, Prolog_term_ref t_vlist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_remove_higher_space_dimensions
  (Prolog_term_ref t_ph, Prolog_term_ref t_nd);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_expand_space_dimension
  (Prolog_term_ref t_ph, Prolog_term_ref t_v, Prolog_term_ref t_nd);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_fold_space_dimensions
  (Prolog_term_ref t_ph, Prolog_term_ref t_vlist, Prolog_term_ref t_v);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_map_space_dimensions
  (Prolog_term_ref t_ph, Prolog_term_ref t_pfunc);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_ascii_dump
  (Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_external_memory_in_bytes(Prolog_term_ref t_pps,
			 Prolog_term_ref t_m);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_total_memory_in_bytes(Prolog_term_ref t_pps,
			 Prolog_term_ref t_m);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_BHRZ03_widening_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs,
   Prolog_term_ref t_ti, Prolog_term_ref t_to);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_H79_widening_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs,
   Prolog_term_ref t_ti, Prolog_term_ref t_to);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_BHRZ03_widening_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_H79_widening_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_widening_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs,
   Prolog_term_ref t_ti, Prolog_term_ref t_to);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_widening_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist,
   Prolog_term_ref t_ti, Prolog_term_ref t_to);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist,
   Prolog_term_ref t_ti, Prolog_term_ref t_to);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist,
   Prolog_term_ref t_ti, Prolog_term_ref t_to);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist,
   Prolog_term_ref t_ti, Prolog_term_ref t_to);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_limited_H79_extrapolation_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_bounded_H79_extrapolation_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist);

extern "C" Prolog_foreign_return_type
ppl_Polyhedron_linear_partition(Prolog_term_ref t_ph,
			     Prolog_term_ref t_qh,
			     Prolog_term_ref t_inters,
			     Prolog_term_ref t_pset);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_intersection_assign_and_minimize
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_poly_hull_assign_and_minimize
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_add_constraint_and_minimize(Prolog_term_ref t_ph,
					       Prolog_term_ref t_c);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_add_congruence_and_minimize(Prolog_term_ref t_ph,
					       Prolog_term_ref t_c);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_add_generator_and_minimize(Prolog_term_ref t_ph,
					       Prolog_term_ref t_c);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_add_constraints_and_minimize(Prolog_term_ref t_ph,
						Prolog_term_ref t_clist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_add_congruences_and_minimize(Prolog_term_ref t_ph,
						Prolog_term_ref t_clist);

extern "C" Prolog_foreign_return_type
  ppl_Polyhedron_add_generators_and_minimize(Prolog_term_ref t_ph,
						Prolog_term_ref t_clist);



