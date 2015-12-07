/* Prolog Octagonal_Shape_mpq_class interface code: declarations.
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

extern "C" Prolog_foreign_return_type
  ppl_delete_Octagonal_Shape_mpq_class(Prolog_term_ref t_ph);



extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_space_dimension(Prolog_term_ref t_nd,
                                               Prolog_term_ref t_uoe,
                                               Prolog_term_ref t_ph);





extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_C_Polyhedron(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_Grid(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class(
                     Prolog_term_ref t_ph_source, Prolog_term_ref t_ph);






extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_C_Polyhedron_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_Grid_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);

extern "C" Prolog_foreign_return_type
ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity(
                     Prolog_term_ref t_ph_source,
                     Prolog_term_ref t_ph,
                     Prolog_term_ref t_cc);






extern "C" Prolog_foreign_return_type
  ppl_new_Octagonal_Shape_mpq_class_from_constraints(Prolog_term_ref t_clist,
						    Prolog_term_ref t_ph);
extern "C" Prolog_foreign_return_type
  ppl_new_Octagonal_Shape_mpq_class_from_congruences(Prolog_term_ref t_clist,
						    Prolog_term_ref t_ph);
extern "C" Prolog_foreign_return_type
  ppl_new_Octagonal_Shape_mpq_class_from_generators(Prolog_term_ref t_clist,
						    Prolog_term_ref t_ph);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_swap(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_space_dimension(Prolog_term_ref t_ph, Prolog_term_ref t_sd);
extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_affine_dimension(Prolog_term_ref t_ph, Prolog_term_ref t_sd);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_relation_with_constraint(Prolog_term_ref t_ph,
						 Prolog_term_ref t_c,
						 Prolog_term_ref t_r);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_relation_with_generator(Prolog_term_ref t_ph,
						 Prolog_term_ref t_c,
						 Prolog_term_ref t_r);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_relation_with_congruence(Prolog_term_ref t_ph,
						 Prolog_term_ref t_c,
						 Prolog_term_ref t_r);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_get_constraints(Prolog_term_ref t_ph,
				   Prolog_term_ref t_glist);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_get_congruences(Prolog_term_ref t_ph,
				   Prolog_term_ref t_glist);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_get_minimized_constraints(Prolog_term_ref t_ph,
					     Prolog_term_ref t_glist);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_get_minimized_congruences(Prolog_term_ref t_ph,
					     Prolog_term_ref t_glist);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_is_empty(Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_is_universe(Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_is_bounded(Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_contains_integer_point(Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_is_topologically_closed(Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_is_discrete(Prolog_term_ref t_ph);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_topological_closure_assign(Prolog_term_ref t_ph);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_bounds_from_above(Prolog_term_ref t_ph,
				       Prolog_term_ref t_expr);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_bounds_from_below(Prolog_term_ref t_ph,
				       Prolog_term_ref t_expr);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_maximize(Prolog_term_ref t_ph, Prolog_term_ref t_le_expr,
		       Prolog_term_ref t_n,  Prolog_term_ref t_d,
		       Prolog_term_ref t_maxmin);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_minimize(Prolog_term_ref t_ph, Prolog_term_ref t_le_expr,
		       Prolog_term_ref t_n,  Prolog_term_ref t_d,
		       Prolog_term_ref t_maxmin);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_maximize_with_point(Prolog_term_ref t_ph,
				  Prolog_term_ref t_le_expr,
				  Prolog_term_ref t_n,
                                  Prolog_term_ref t_d,
				  Prolog_term_ref t_maxmin,
                                  Prolog_term_ref t_g);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_minimize_with_point(Prolog_term_ref t_ph,
				  Prolog_term_ref t_le_expr,
				  Prolog_term_ref t_n,
                                  Prolog_term_ref t_d,
				  Prolog_term_ref t_maxmin,
                                  Prolog_term_ref t_g);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_frequency(Prolog_term_ref t_ph, Prolog_term_ref t_le_expr,
		       Prolog_term_ref t_freqn,  Prolog_term_ref t_freqd,
		       Prolog_term_ref t_valn,  Prolog_term_ref t_vald);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_contains_Octagonal_Shape_mpq_class(Prolog_term_ref t_lhs,
				   Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_strictly_contains_Octagonal_Shape_mpq_class(Prolog_term_ref t_lhs,
				   Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_is_disjoint_from_Octagonal_Shape_mpq_class(Prolog_term_ref t_lhs,
				   Prolog_term_ref t_rhs);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_equals_Octagonal_Shape_mpq_class(Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_OK(Prolog_term_ref t_ph);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_add_constraint(Prolog_term_ref t_ph, Prolog_term_ref t_c);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_add_congruence(Prolog_term_ref t_ph, Prolog_term_ref t_c);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_add_constraints(Prolog_term_ref t_ph,
				   Prolog_term_ref t_clist);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_add_congruences(Prolog_term_ref t_ph,
				   Prolog_term_ref t_clist);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_refine_with_constraint(Prolog_term_ref t_ph, Prolog_term_ref t_c);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_refine_with_congruence(Prolog_term_ref t_ph, Prolog_term_ref t_c);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_refine_with_constraints(Prolog_term_ref t_ph,
				   Prolog_term_ref t_clist);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_refine_with_congruences(Prolog_term_ref t_ph,
				   Prolog_term_ref t_clist);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_intersection_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_upper_bound_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_difference_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_concatenate_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_time_elapse_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_upper_bound_assign_if_exact
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_simplify_using_context_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_b);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_constrains(Prolog_term_ref t_ph,
                          Prolog_term_ref t_v);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimension(Prolog_term_ref t_ph,
                           Prolog_term_ref t_v);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimensions(Prolog_term_ref t_ph,
                           Prolog_term_ref t_vlist);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_affine_image
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_le, Prolog_term_ref t_d);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_affine_preimage
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_le, Prolog_term_ref t_d);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_bounded_affine_image
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_lb_le, Prolog_term_ref t_ub_le,
   Prolog_term_ref t_d);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_bounded_affine_preimage
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_lb_le, Prolog_term_ref t_ub_le,
   Prolog_term_ref t_d);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_generalized_affine_image
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_r, Prolog_term_ref t_le,
   Prolog_term_ref t_d);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_generalized_affine_preimage
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_v, Prolog_term_ref t_r, Prolog_term_ref t_le,
   Prolog_term_ref t_d);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_generalized_affine_image_lhs_rhs
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_lhs, Prolog_term_ref t_r, Prolog_term_ref t_rhs);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_generalized_affine_preimage_lhs_rhs
  (Prolog_term_ref t_ph,
   Prolog_term_ref t_lhs, Prolog_term_ref t_r, Prolog_term_ref t_rhs);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_add_space_dimensions_and_embed
  (Prolog_term_ref t_ph, Prolog_term_ref t_nnd);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_add_space_dimensions_and_project
  (Prolog_term_ref t_ph, Prolog_term_ref t_nnd);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_remove_space_dimensions
  (Prolog_term_ref t_ph, Prolog_term_ref t_vlist);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_remove_higher_space_dimensions
  (Prolog_term_ref t_ph, Prolog_term_ref t_nd);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_expand_space_dimension
  (Prolog_term_ref t_ph, Prolog_term_ref t_v, Prolog_term_ref t_nd);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_fold_space_dimensions
  (Prolog_term_ref t_ph, Prolog_term_ref t_vlist, Prolog_term_ref t_v);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_map_space_dimensions
  (Prolog_term_ref t_ph, Prolog_term_ref t_pfunc);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_drop_some_non_integer_points
  (Prolog_term_ref t_ph, Prolog_term_ref t_cc);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_drop_some_non_integer_points_2
  (Prolog_term_ref t_ph, Prolog_term_ref t_vlist, Prolog_term_ref t_cc);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_ascii_dump
  (Prolog_term_ref t_ph);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_external_memory_in_bytes(Prolog_term_ref t_pps,
			 Prolog_term_ref t_m);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_total_memory_in_bytes(Prolog_term_ref t_pps,
			 Prolog_term_ref t_m);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_BHMZ05_widening_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs,
   Prolog_term_ref t_ti, Prolog_term_ref t_to);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_BHMZ05_widening_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_widening_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs,
   Prolog_term_ref t_ti, Prolog_term_ref t_to);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_widening_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);




extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist,
   Prolog_term_ref t_ti, Prolog_term_ref t_to);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist,
   Prolog_term_ref t_ti, Prolog_term_ref t_to);






extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_limited_BHMZ05_extrapolation_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist);

extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_limited_CC76_extrapolation_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs, Prolog_term_ref t_clist);






extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_CC76_extrapolation_assign_with_tokens
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs,
   Prolog_term_ref t_ti, Prolog_term_ref t_to);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_CC76_extrapolation_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_CC76_narrowing_assign
  (Prolog_term_ref t_lhs, Prolog_term_ref t_rhs);





extern "C" Prolog_foreign_return_type
ppl_Octagonal_Shape_mpq_class_linear_partition(Prolog_term_ref t_ph,
			     Prolog_term_ref t_qh,
			     Prolog_term_ref t_inters,
			     Prolog_term_ref t_pset);





extern "C" Prolog_foreign_return_type
  ppl_Octagonal_Shape_mpq_class_wrap_assign
     (Prolog_term_ref t_ph,
      Prolog_term_ref t_vars,
      Prolog_term_ref t_w,
      Prolog_term_ref t_r,
      Prolog_term_ref t_o,
      Prolog_term_ref t_cs,
      Prolog_term_ref t_complexity,
      Prolog_term_ref t_ind);




extern "C" Prolog_foreign_return_type
  ppl_termination_test_MS_Octagonal_Shape_mpq_class(Prolog_term_ref t_pset);

extern "C" Prolog_foreign_return_type
  ppl_termination_test_PR_Octagonal_Shape_mpq_class(Prolog_term_ref t_pset);






extern "C" Prolog_foreign_return_type
  ppl_one_affine_ranking_function_MS_Octagonal_Shape_mpq_class
     (Prolog_term_ref t_pset,
      Prolog_term_ref t_g);

extern "C" Prolog_foreign_return_type
  ppl_one_affine_ranking_function_PR_Octagonal_Shape_mpq_class
     (Prolog_term_ref t_pset,
      Prolog_term_ref t_g);






extern "C" Prolog_foreign_return_type
  ppl_all_affine_ranking_functions_MS_Octagonal_Shape_mpq_class
     (Prolog_term_ref t_pset,
      Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
  ppl_all_affine_ranking_functions_PR_Octagonal_Shape_mpq_class
     (Prolog_term_ref t_pset,
      Prolog_term_ref t_ph);






extern "C" Prolog_foreign_return_type
  ppl_all_affine_quasi_ranking_functions_MS_Octagonal_Shape_mpq_class
     (Prolog_term_ref t_pset,
      Prolog_term_ref t_ph_decreasing,
      Prolog_term_ref t_ph_bounded);





extern "C" Prolog_foreign_return_type
  ppl_termination_test_MS_Octagonal_Shape_mpq_class_2
     (Prolog_term_ref t_pset_before,
      Prolog_term_ref t_pset_after);

extern "C" Prolog_foreign_return_type
  ppl_termination_test_PR_Octagonal_Shape_mpq_class_2
     (Prolog_term_ref t_pset_before,
      Prolog_term_ref t_pset_after);






extern "C" Prolog_foreign_return_type
  ppl_one_affine_ranking_function_MS_Octagonal_Shape_mpq_class_2
     (Prolog_term_ref t_pset_before,
      Prolog_term_ref t_pset_after,
      Prolog_term_ref t_g);

extern "C" Prolog_foreign_return_type
  ppl_one_affine_ranking_function_PR_Octagonal_Shape_mpq_class_2
     (Prolog_term_ref t_pset_before,
      Prolog_term_ref t_pset_after,
      Prolog_term_ref t_g);






extern "C" Prolog_foreign_return_type
  ppl_all_affine_ranking_functions_MS_Octagonal_Shape_mpq_class_2
     (Prolog_term_ref t_pset_before,
      Prolog_term_ref t_pset_after,
      Prolog_term_ref t_ph);

extern "C" Prolog_foreign_return_type
  ppl_all_affine_ranking_functions_PR_Octagonal_Shape_mpq_class_2
     (Prolog_term_ref t_pset_before,
      Prolog_term_ref t_pset_after,
      Prolog_term_ref t_ph);






extern "C" Prolog_foreign_return_type
  ppl_all_affine_quasi_ranking_functions_MS_Octagonal_Shape_mpq_class_2
     (Prolog_term_ref t_pset_before,
      Prolog_term_ref t_pset_after,
      Prolog_term_ref t_ph_decreasing,
      Prolog_term_ref t_ph_bounded);







