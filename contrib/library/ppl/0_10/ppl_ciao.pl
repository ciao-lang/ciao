/* Ciao Prolog interface: Ciao Prolog part.
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

% January 2009. Patched by R@'{e}my Haemmerl@'{e} for CLIP  

%:- module(ppl_ciao,
:- export(
[
	  ppl_version_major/1,
	  ppl_version_minor/1,
	  ppl_version_revision/1,
	  ppl_version_beta/1,
	  ppl_version/1,
	  ppl_banner/1,
	  ppl_max_space_dimension/1,
	  ppl_Coefficient_is_bounded/0,
	  ppl_Coefficient_max/1,
	  ppl_Coefficient_min/1,
	  ppl_initialize/0,
	  ppl_finalize/0,
	  ppl_set_rounding_for_PPL/0,
	  ppl_restore_pre_PPL_rounding/0,
	  ppl_set_timeout_exception_atom/1,
	  ppl_timeout_exception_atom/1,
	  ppl_set_timeout/1,
	  ppl_reset_timeout/0,
	  ppl_new_MIP_Problem_from_space_dimension/2,
	  ppl_new_MIP_Problem/5,
	  ppl_new_MIP_Problem_from_MIP_Problem/2,
	  ppl_MIP_Problem_swap/2,
	  ppl_delete_MIP_Problem/1,
	  ppl_MIP_Problem_space_dimension/2,
	  ppl_MIP_Problem_integer_space_dimensions/2,
	  ppl_MIP_Problem_constraints/2,
	  ppl_MIP_Problem_objective_function/2,
	  ppl_MIP_Problem_optimization_mode/2,
	  ppl_MIP_Problem_clear/1,
	  ppl_MIP_Problem_add_space_dimensions_and_embed/2,
	  ppl_MIP_Problem_add_to_integer_space_dimensions/2,
	  ppl_MIP_Problem_add_constraint/2,
	  ppl_MIP_Problem_add_constraints/2,
	  ppl_MIP_Problem_set_objective_function/2,
	  ppl_MIP_Problem_set_optimization_mode/2,
	  ppl_MIP_Problem_set_control_parameter/2,
	  ppl_MIP_Problem_get_control_parameter/3,
	  ppl_MIP_Problem_is_satisfiable/1,
	  ppl_MIP_Problem_solve/2,
	  ppl_MIP_Problem_feasible_point/2,
	  ppl_MIP_Problem_optimizing_point/2,
	  ppl_MIP_Problem_optimal_value/3,
	  ppl_MIP_Problem_evaluate_objective_function/4,
	  ppl_MIP_Problem_OK/1,
	  ppl_delete_Polyhedron/1,
	  ppl_new_C_Polyhedron_from_space_dimension/3,
	  ppl_new_NNC_Polyhedron_from_space_dimension/3,
	  ppl_new_C_Polyhedron_from_C_Polyhedron/2,
	  ppl_new_C_Polyhedron_from_NNC_Polyhedron/2,
	  ppl_new_C_Polyhedron_from_Grid/2,
	  ppl_new_C_Polyhedron_from_Rational_Box/2,
	  ppl_new_C_Polyhedron_from_BD_Shape_mpz_class/2,
	  ppl_new_C_Polyhedron_from_BD_Shape_mpq_class/2,
	  ppl_new_C_Polyhedron_from_Octagonal_Shape_mpz_class/2,
	  ppl_new_C_Polyhedron_from_Octagonal_Shape_mpq_class/2,
	  ppl_new_C_Polyhedron_from_Double_Box/2,
	  ppl_new_C_Polyhedron_from_BD_Shape_double/2,
	  ppl_new_C_Polyhedron_from_Octagonal_Shape_double/2,
	  ppl_new_NNC_Polyhedron_from_C_Polyhedron/2,
	  ppl_new_NNC_Polyhedron_from_NNC_Polyhedron/2,
	  ppl_new_NNC_Polyhedron_from_Grid/2,
	  ppl_new_NNC_Polyhedron_from_Rational_Box/2,
	  ppl_new_NNC_Polyhedron_from_BD_Shape_mpz_class/2,
	  ppl_new_NNC_Polyhedron_from_BD_Shape_mpq_class/2,
	  ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class/2,
	  ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class/2,
	  ppl_new_NNC_Polyhedron_from_Double_Box/2,
	  ppl_new_NNC_Polyhedron_from_BD_Shape_double/2,
	  ppl_new_NNC_Polyhedron_from_Octagonal_Shape_double/2,
	  ppl_new_C_Polyhedron_from_C_Polyhedron_with_complexity/3,
	  ppl_new_C_Polyhedron_from_NNC_Polyhedron_with_complexity/3,
	  ppl_new_C_Polyhedron_from_Grid_with_complexity/3,
	  ppl_new_C_Polyhedron_from_Rational_Box_with_complexity/3,
	  ppl_new_C_Polyhedron_from_BD_Shape_mpz_class_with_complexity/3,
	  ppl_new_C_Polyhedron_from_BD_Shape_mpq_class_with_complexity/3,
	  ppl_new_C_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity/3,
	  ppl_new_C_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity/3,
	  ppl_new_C_Polyhedron_from_Double_Box_with_complexity/3,
	  ppl_new_C_Polyhedron_from_BD_Shape_double_with_complexity/3,
	  ppl_new_C_Polyhedron_from_Octagonal_Shape_double_with_complexity/3,
	  ppl_new_NNC_Polyhedron_from_C_Polyhedron_with_complexity/3,
	  ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity/3,
	  ppl_new_NNC_Polyhedron_from_Grid_with_complexity/3,
	  ppl_new_NNC_Polyhedron_from_Rational_Box_with_complexity/3,
	  ppl_new_NNC_Polyhedron_from_BD_Shape_mpz_class_with_complexity/3,
	  ppl_new_NNC_Polyhedron_from_BD_Shape_mpq_class_with_complexity/3,
	  ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity/3,
	  ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity/3,
	  ppl_new_NNC_Polyhedron_from_Double_Box_with_complexity/3,
	  ppl_new_NNC_Polyhedron_from_BD_Shape_double_with_complexity/3,
	  ppl_new_NNC_Polyhedron_from_Octagonal_Shape_double_with_complexity/3,
	  ppl_new_C_Polyhedron_from_constraints/2,
	  ppl_new_NNC_Polyhedron_from_constraints/2,
	  ppl_new_C_Polyhedron_from_congruences/2,
	  ppl_new_NNC_Polyhedron_from_congruences/2,
	  ppl_new_C_Polyhedron_from_generators/2,
	  ppl_new_NNC_Polyhedron_from_generators/2,
	  ppl_Polyhedron_swap/2,
	  ppl_Polyhedron_space_dimension/2,
	  ppl_Polyhedron_affine_dimension/2,
	  ppl_Polyhedron_relation_with_constraint/3,
	  ppl_Polyhedron_relation_with_generator/3,
	  ppl_Polyhedron_relation_with_congruence/3,
	  ppl_Polyhedron_get_constraints/2,
	  ppl_Polyhedron_get_congruences/2,
	  ppl_Polyhedron_get_generators/2,
	  ppl_Polyhedron_get_minimized_constraints/2,
	  ppl_Polyhedron_get_minimized_congruences/2,
	  ppl_Polyhedron_get_minimized_generators/2,
	  ppl_Polyhedron_is_empty/1,
	  ppl_Polyhedron_is_universe/1,
	  ppl_Polyhedron_is_bounded/1,
	  ppl_Polyhedron_contains_integer_point/1,
	  ppl_Polyhedron_is_topologically_closed/1,
	  ppl_Polyhedron_is_discrete/1,
	  ppl_Polyhedron_topological_closure_assign/1,
	  ppl_Polyhedron_bounds_from_above/2,
	  ppl_Polyhedron_bounds_from_below/2,
	  ppl_Polyhedron_maximize/5,
	  ppl_Polyhedron_minimize/5,
	  ppl_Polyhedron_maximize_with_point/6,
	  ppl_Polyhedron_minimize_with_point/6,
	  ppl_Polyhedron_contains_Polyhedron/2,
	  ppl_Polyhedron_strictly_contains_Polyhedron/2,
	  ppl_Polyhedron_is_disjoint_from_Polyhedron/2,
	  ppl_Polyhedron_equals_Polyhedron/2,
	  ppl_Polyhedron_OK/1,
	  ppl_Polyhedron_add_constraint/2,
	  ppl_Polyhedron_add_congruence/2,
	  ppl_Polyhedron_add_generator/2,
	  ppl_Polyhedron_add_constraints/2,
	  ppl_Polyhedron_add_congruences/2,
	  ppl_Polyhedron_add_generators/2,
	  ppl_Polyhedron_refine_with_constraint/2,
	  ppl_Polyhedron_refine_with_congruence/2,
	  ppl_Polyhedron_refine_with_constraints/2,
	  ppl_Polyhedron_refine_with_congruences/2,
	  ppl_Polyhedron_intersection_assign/2,
	  ppl_Polyhedron_upper_bound_assign/2,
	  ppl_Polyhedron_difference_assign/2,
	  ppl_Polyhedron_concatenate_assign/2,
	  ppl_Polyhedron_time_elapse_assign/2,
	  ppl_Polyhedron_poly_hull_assign/2,
	  ppl_Polyhedron_poly_difference_assign/2,
	  ppl_Polyhedron_upper_bound_assign_if_exact/2,
	  ppl_Polyhedron_poly_hull_assign_if_exact/2,
	  ppl_Polyhedron_simplify_using_context_assign/3,
	  ppl_Polyhedron_constrains/2,
	  ppl_Polyhedron_unconstrain_space_dimension/2,
	  ppl_Polyhedron_unconstrain_space_dimensions/2,
	  ppl_Polyhedron_affine_image/4,
	  ppl_Polyhedron_affine_preimage/4,
	  ppl_Polyhedron_bounded_affine_image/5,
	  ppl_Polyhedron_bounded_affine_preimage/5,
	  ppl_Polyhedron_generalized_affine_image/5,
	  ppl_Polyhedron_generalized_affine_preimage/5,
	  ppl_Polyhedron_generalized_affine_image_lhs_rhs/4,
	  ppl_Polyhedron_generalized_affine_preimage_lhs_rhs/4,
	  ppl_Polyhedron_add_space_dimensions_and_embed/2,
	  ppl_Polyhedron_add_space_dimensions_and_project/2,
	  ppl_Polyhedron_remove_space_dimensions/2,
	  ppl_Polyhedron_remove_higher_space_dimensions/2,
	  ppl_Polyhedron_expand_space_dimension/3,
	  ppl_Polyhedron_fold_space_dimensions/3,
	  ppl_Polyhedron_map_space_dimensions/2,
	  ppl_Polyhedron_ascii_dump/1,
	  ppl_Polyhedron_external_memory_in_bytes/2,
	  ppl_Polyhedron_total_memory_in_bytes/2,
	  ppl_Polyhedron_BHRZ03_widening_assign_with_tokens/4,
	  ppl_Polyhedron_H79_widening_assign_with_tokens/4,
	  ppl_Polyhedron_BHRZ03_widening_assign/2,
	  ppl_Polyhedron_H79_widening_assign/2,
	  ppl_Polyhedron_widening_assign_with_tokens/4,
	  ppl_Polyhedron_widening_assign/2,
	  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens/5,
	  ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens/5,
	  ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens/5,
	  ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens/5,
	  ppl_Polyhedron_limited_BHRZ03_extrapolation_assign/3,
	  ppl_Polyhedron_limited_H79_extrapolation_assign/3,
	  ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign/3,
	  ppl_Polyhedron_bounded_H79_extrapolation_assign/3,
	  ppl_Polyhedron_linear_partition/4,
	  ppl_Polyhedron_intersection_assign_and_minimize/2,
	  ppl_Polyhedron_poly_hull_assign_and_minimize/2,
	  ppl_Polyhedron_add_constraint_and_minimize/2,
	  ppl_Polyhedron_add_congruence_and_minimize/2,
	  ppl_Polyhedron_add_generator_and_minimize/2,
	  ppl_Polyhedron_add_constraints_and_minimize/2,
	  ppl_Polyhedron_add_congruences_and_minimize/2,
	  ppl_Polyhedron_add_generators_and_minimize/2
,
	  ppl_delete_Grid/1,
	  ppl_new_Grid_from_space_dimension/3,
	  ppl_new_Grid_from_C_Polyhedron/2,
	  ppl_new_Grid_from_NNC_Polyhedron/2,
	  ppl_new_Grid_from_Grid/2,
	  ppl_new_Grid_from_Rational_Box/2,
	  ppl_new_Grid_from_BD_Shape_mpz_class/2,
	  ppl_new_Grid_from_BD_Shape_mpq_class/2,
	  ppl_new_Grid_from_Octagonal_Shape_mpz_class/2,
	  ppl_new_Grid_from_Octagonal_Shape_mpq_class/2,
	  ppl_new_Grid_from_Double_Box/2,
	  ppl_new_Grid_from_BD_Shape_double/2,
	  ppl_new_Grid_from_Octagonal_Shape_double/2,
	  ppl_new_Grid_from_C_Polyhedron_with_complexity/3,
	  ppl_new_Grid_from_NNC_Polyhedron_with_complexity/3,
	  ppl_new_Grid_from_Grid_with_complexity/3,
	  ppl_new_Grid_from_Rational_Box_with_complexity/3,
	  ppl_new_Grid_from_BD_Shape_mpz_class_with_complexity/3,
	  ppl_new_Grid_from_BD_Shape_mpq_class_with_complexity/3,
	  ppl_new_Grid_from_Octagonal_Shape_mpz_class_with_complexity/3,
	  ppl_new_Grid_from_Octagonal_Shape_mpq_class_with_complexity/3,
	  ppl_new_Grid_from_Double_Box_with_complexity/3,
	  ppl_new_Grid_from_BD_Shape_double_with_complexity/3,
	  ppl_new_Grid_from_Octagonal_Shape_double_with_complexity/3,
	  ppl_new_Grid_from_constraints/2,
	  ppl_new_Grid_from_grid_generators/2,
	  ppl_new_Grid_from_congruences/2,
	  ppl_Grid_swap/2,
	  ppl_Grid_space_dimension/2,
	  ppl_Grid_affine_dimension/2,
	  ppl_Grid_relation_with_constraint/3,
	  ppl_Grid_relation_with_generator/3,
	  ppl_Grid_relation_with_congruence/3,
	  ppl_Grid_relation_with_grid_generator/3,
	  ppl_Grid_get_constraints/2,
	  ppl_Grid_get_congruences/2,
	  ppl_Grid_get_grid_generators/2,
	  ppl_Grid_get_minimized_constraints/2,
	  ppl_Grid_get_minimized_congruences/2,
	  ppl_Grid_get_minimized_grid_generators/2,
	  ppl_Grid_is_empty/1,
	  ppl_Grid_is_universe/1,
	  ppl_Grid_is_bounded/1,
	  ppl_Grid_contains_integer_point/1,
	  ppl_Grid_is_topologically_closed/1,
	  ppl_Grid_is_discrete/1,
	  ppl_Grid_topological_closure_assign/1,
	  ppl_Grid_bounds_from_above/2,
	  ppl_Grid_bounds_from_below/2,
	  ppl_Grid_maximize/5,
	  ppl_Grid_minimize/5,
	  ppl_Grid_maximize_with_point/6,
	  ppl_Grid_minimize_with_point/6,
	  ppl_Grid_contains_Grid/2,
	  ppl_Grid_strictly_contains_Grid/2,
	  ppl_Grid_is_disjoint_from_Grid/2,
	  ppl_Grid_equals_Grid/2,
	  ppl_Grid_OK/1,
	  ppl_Grid_add_constraint/2,
	  ppl_Grid_add_congruence/2,
	  ppl_Grid_add_grid_generator/2,
	  ppl_Grid_add_constraints/2,
	  ppl_Grid_add_congruences/2,
	  ppl_Grid_add_grid_generators/2,
	  ppl_Grid_refine_with_constraint/2,
	  ppl_Grid_refine_with_congruence/2,
	  ppl_Grid_refine_with_constraints/2,
	  ppl_Grid_refine_with_congruences/2,
	  ppl_Grid_intersection_assign/2,
	  ppl_Grid_upper_bound_assign/2,
	  ppl_Grid_difference_assign/2,
	  ppl_Grid_concatenate_assign/2,
	  ppl_Grid_time_elapse_assign/2,
	  ppl_Grid_upper_bound_assign_if_exact/2,
	  ppl_Grid_simplify_using_context_assign/3,
	  ppl_Grid_constrains/2,
	  ppl_Grid_unconstrain_space_dimension/2,
	  ppl_Grid_unconstrain_space_dimensions/2,
	  ppl_Grid_affine_image/4,
	  ppl_Grid_affine_preimage/4,
	  ppl_Grid_bounded_affine_image/5,
	  ppl_Grid_bounded_affine_preimage/5,
	  ppl_Grid_generalized_affine_image/5,
	  ppl_Grid_generalized_affine_preimage/5,
	  ppl_Grid_generalized_affine_image_lhs_rhs/4,
	  ppl_Grid_generalized_affine_preimage_lhs_rhs/4,
	  ppl_Grid_generalized_affine_image_with_congruence/6,
	  ppl_Grid_generalized_affine_preimage_with_congruence/6,
	  ppl_Grid_generalized_affine_image_lhs_rhs_with_congruence/5,
	  ppl_Grid_generalized_affine_preimage_lhs_rhs_with_congruence/5,
	  ppl_Grid_add_space_dimensions_and_embed/2,
	  ppl_Grid_add_space_dimensions_and_project/2,
	  ppl_Grid_remove_space_dimensions/2,
	  ppl_Grid_remove_higher_space_dimensions/2,
	  ppl_Grid_expand_space_dimension/3,
	  ppl_Grid_fold_space_dimensions/3,
	  ppl_Grid_map_space_dimensions/2,
	  ppl_Grid_ascii_dump/1,
	  ppl_Grid_external_memory_in_bytes/2,
	  ppl_Grid_total_memory_in_bytes/2,
	  ppl_Grid_congruence_widening_assign_with_tokens/4,
	  ppl_Grid_generator_widening_assign_with_tokens/4,
	  ppl_Grid_congruence_widening_assign/2,
	  ppl_Grid_generator_widening_assign/2,
	  ppl_Grid_widening_assign_with_tokens/4,
	  ppl_Grid_widening_assign/2,
	  ppl_Grid_limited_congruence_extrapolation_assign_with_tokens/5,
	  ppl_Grid_limited_generator_extrapolation_assign_with_tokens/5,
	  ppl_Grid_limited_congruence_extrapolation_assign/3,
	  ppl_Grid_limited_generator_extrapolation_assign/3,
	  ppl_delete_Rational_Box/1,
	  ppl_new_Rational_Box_from_space_dimension/3,
	  ppl_new_Rational_Box_from_C_Polyhedron/2,
	  ppl_new_Rational_Box_from_NNC_Polyhedron/2,
	  ppl_new_Rational_Box_from_Grid/2,
	  ppl_new_Rational_Box_from_Rational_Box/2,
	  ppl_new_Rational_Box_from_BD_Shape_mpz_class/2,
	  ppl_new_Rational_Box_from_BD_Shape_mpq_class/2,
	  ppl_new_Rational_Box_from_Octagonal_Shape_mpz_class/2,
	  ppl_new_Rational_Box_from_Octagonal_Shape_mpq_class/2,
	  ppl_new_Rational_Box_from_Double_Box/2,
	  ppl_new_Rational_Box_from_BD_Shape_double/2,
	  ppl_new_Rational_Box_from_Octagonal_Shape_double/2,
	  ppl_new_Rational_Box_from_C_Polyhedron_with_complexity/3,
	  ppl_new_Rational_Box_from_NNC_Polyhedron_with_complexity/3,
	  ppl_new_Rational_Box_from_Grid_with_complexity/3,
	  ppl_new_Rational_Box_from_Rational_Box_with_complexity/3,
	  ppl_new_Rational_Box_from_BD_Shape_mpz_class_with_complexity/3,
	  ppl_new_Rational_Box_from_BD_Shape_mpq_class_with_complexity/3,
	  ppl_new_Rational_Box_from_Octagonal_Shape_mpz_class_with_complexity/3,
	  ppl_new_Rational_Box_from_Octagonal_Shape_mpq_class_with_complexity/3,
	  ppl_new_Rational_Box_from_Double_Box_with_complexity/3,
	  ppl_new_Rational_Box_from_BD_Shape_double_with_complexity/3,
	  ppl_new_Rational_Box_from_Octagonal_Shape_double_with_complexity/3,
	  ppl_new_Rational_Box_from_constraints/2,
	  ppl_new_Rational_Box_from_congruences/2,
	  ppl_new_Rational_Box_from_generators/2,
	  ppl_Rational_Box_swap/2,
	  ppl_Rational_Box_space_dimension/2,
	  ppl_Rational_Box_affine_dimension/2,
	  ppl_Rational_Box_relation_with_constraint/3,
	  ppl_Rational_Box_relation_with_generator/3,
	  ppl_Rational_Box_relation_with_congruence/3,
	  ppl_Rational_Box_get_constraints/2,
	  ppl_Rational_Box_get_congruences/2,
	  ppl_Rational_Box_get_minimized_constraints/2,
	  ppl_Rational_Box_get_minimized_congruences/2,
	  ppl_Rational_Box_is_empty/1,
	  ppl_Rational_Box_is_universe/1,
	  ppl_Rational_Box_is_bounded/1,
	  ppl_Rational_Box_contains_integer_point/1,
	  ppl_Rational_Box_is_topologically_closed/1,
	  ppl_Rational_Box_is_discrete/1,
	  ppl_Rational_Box_topological_closure_assign/1,
	  ppl_Rational_Box_bounds_from_above/2,
	  ppl_Rational_Box_bounds_from_below/2,
	  ppl_Rational_Box_maximize/5,
	  ppl_Rational_Box_minimize/5,
	  ppl_Rational_Box_maximize_with_point/6,
	  ppl_Rational_Box_minimize_with_point/6,
	  ppl_Rational_Box_contains_Rational_Box/2,
	  ppl_Rational_Box_strictly_contains_Rational_Box/2,
	  ppl_Rational_Box_is_disjoint_from_Rational_Box/2,
	  ppl_Rational_Box_equals_Rational_Box/2,
	  ppl_Rational_Box_OK/1,
	  ppl_Rational_Box_add_constraint/2,
	  ppl_Rational_Box_add_congruence/2,
	  ppl_Rational_Box_add_constraints/2,
	  ppl_Rational_Box_add_congruences/2,
	  ppl_Rational_Box_refine_with_constraint/2,
	  ppl_Rational_Box_refine_with_congruence/2,
	  ppl_Rational_Box_refine_with_constraints/2,
	  ppl_Rational_Box_refine_with_congruences/2,
	  ppl_Rational_Box_intersection_assign/2,
	  ppl_Rational_Box_upper_bound_assign/2,
	  ppl_Rational_Box_difference_assign/2,
	  ppl_Rational_Box_concatenate_assign/2,
	  ppl_Rational_Box_time_elapse_assign/2,
	  ppl_Rational_Box_upper_bound_assign_if_exact/2,
	  ppl_Rational_Box_simplify_using_context_assign/3,
	  ppl_Rational_Box_constrains/2,
	  ppl_Rational_Box_unconstrain_space_dimension/2,
	  ppl_Rational_Box_unconstrain_space_dimensions/2,
	  ppl_Rational_Box_affine_image/4,
	  ppl_Rational_Box_affine_preimage/4,
	  ppl_Rational_Box_bounded_affine_image/5,
	  ppl_Rational_Box_bounded_affine_preimage/5,
	  ppl_Rational_Box_generalized_affine_image/5,
	  ppl_Rational_Box_generalized_affine_preimage/5,
	  ppl_Rational_Box_generalized_affine_image_lhs_rhs/4,
	  ppl_Rational_Box_generalized_affine_preimage_lhs_rhs/4,
	  ppl_Rational_Box_add_space_dimensions_and_embed/2,
	  ppl_Rational_Box_add_space_dimensions_and_project/2,
	  ppl_Rational_Box_remove_space_dimensions/2,
	  ppl_Rational_Box_remove_higher_space_dimensions/2,
	  ppl_Rational_Box_expand_space_dimension/3,
	  ppl_Rational_Box_fold_space_dimensions/3,
	  ppl_Rational_Box_map_space_dimensions/2,
	  ppl_Rational_Box_ascii_dump/1,
	  ppl_Rational_Box_external_memory_in_bytes/2,
	  ppl_Rational_Box_total_memory_in_bytes/2,
	  ppl_Rational_Box_CC76_widening_assign_with_tokens/4,
	  ppl_Rational_Box_CC76_widening_assign/2,
	  ppl_Rational_Box_widening_assign_with_tokens/4,
	  ppl_Rational_Box_widening_assign/2,
	  ppl_Rational_Box_limited_CC76_extrapolation_assign_with_tokens/5,
	  ppl_Rational_Box_limited_CC76_extrapolation_assign/3,
	  ppl_Rational_Box_linear_partition/4,
	  ppl_delete_BD_Shape_mpz_class/1,
	  ppl_new_BD_Shape_mpz_class_from_space_dimension/3,
	  ppl_new_BD_Shape_mpz_class_from_C_Polyhedron/2,
	  ppl_new_BD_Shape_mpz_class_from_NNC_Polyhedron/2,
	  ppl_new_BD_Shape_mpz_class_from_Grid/2,
	  ppl_new_BD_Shape_mpz_class_from_Rational_Box/2,
	  ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpz_class/2,
	  ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpq_class/2,
	  ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpz_class/2,
	  ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpq_class/2,
	  ppl_new_BD_Shape_mpz_class_from_Double_Box/2,
	  ppl_new_BD_Shape_mpz_class_from_BD_Shape_double/2,
	  ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_double/2,
	  ppl_new_BD_Shape_mpz_class_from_C_Polyhedron_with_complexity/3,
	  ppl_new_BD_Shape_mpz_class_from_NNC_Polyhedron_with_complexity/3,
	  ppl_new_BD_Shape_mpz_class_from_Grid_with_complexity/3,
	  ppl_new_BD_Shape_mpz_class_from_Rational_Box_with_complexity/3,
	  ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpz_class_with_complexity/3,
	  ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpq_class_with_complexity/3,
	  ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpz_class_with_complexity/3,
	  ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpq_class_with_complexity/3,
	  ppl_new_BD_Shape_mpz_class_from_Double_Box_with_complexity/3,
	  ppl_new_BD_Shape_mpz_class_from_BD_Shape_double_with_complexity/3,
	  ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_double_with_complexity/3,
	  ppl_new_BD_Shape_mpz_class_from_constraints/2,
	  ppl_new_BD_Shape_mpz_class_from_congruences/2,
	  ppl_new_BD_Shape_mpz_class_from_generators/2,
	  ppl_BD_Shape_mpz_class_swap/2,
	  ppl_BD_Shape_mpz_class_space_dimension/2,
	  ppl_BD_Shape_mpz_class_affine_dimension/2,
	  ppl_BD_Shape_mpz_class_relation_with_constraint/3,
	  ppl_BD_Shape_mpz_class_relation_with_generator/3,
	  ppl_BD_Shape_mpz_class_relation_with_congruence/3,
	  ppl_BD_Shape_mpz_class_get_constraints/2,
	  ppl_BD_Shape_mpz_class_get_congruences/2,
	  ppl_BD_Shape_mpz_class_get_minimized_constraints/2,
	  ppl_BD_Shape_mpz_class_get_minimized_congruences/2,
	  ppl_BD_Shape_mpz_class_is_empty/1,
	  ppl_BD_Shape_mpz_class_is_universe/1,
	  ppl_BD_Shape_mpz_class_is_bounded/1,
	  ppl_BD_Shape_mpz_class_contains_integer_point/1,
	  ppl_BD_Shape_mpz_class_is_topologically_closed/1,
	  ppl_BD_Shape_mpz_class_is_discrete/1,
	  ppl_BD_Shape_mpz_class_topological_closure_assign/1,
	  ppl_BD_Shape_mpz_class_bounds_from_above/2,
	  ppl_BD_Shape_mpz_class_bounds_from_below/2,
	  ppl_BD_Shape_mpz_class_maximize/5,
	  ppl_BD_Shape_mpz_class_minimize/5,
	  ppl_BD_Shape_mpz_class_maximize_with_point/6,
	  ppl_BD_Shape_mpz_class_minimize_with_point/6,
	  ppl_BD_Shape_mpz_class_contains_BD_Shape_mpz_class/2,
	  ppl_BD_Shape_mpz_class_strictly_contains_BD_Shape_mpz_class/2,
	  ppl_BD_Shape_mpz_class_is_disjoint_from_BD_Shape_mpz_class/2,
	  ppl_BD_Shape_mpz_class_equals_BD_Shape_mpz_class/2,
	  ppl_BD_Shape_mpz_class_OK/1,
	  ppl_BD_Shape_mpz_class_add_constraint/2,
	  ppl_BD_Shape_mpz_class_add_congruence/2,
	  ppl_BD_Shape_mpz_class_add_constraints/2,
	  ppl_BD_Shape_mpz_class_add_congruences/2,
	  ppl_BD_Shape_mpz_class_refine_with_constraint/2,
	  ppl_BD_Shape_mpz_class_refine_with_congruence/2,
	  ppl_BD_Shape_mpz_class_refine_with_constraints/2,
	  ppl_BD_Shape_mpz_class_refine_with_congruences/2,
	  ppl_BD_Shape_mpz_class_intersection_assign/2,
	  ppl_BD_Shape_mpz_class_upper_bound_assign/2,
	  ppl_BD_Shape_mpz_class_difference_assign/2,
	  ppl_BD_Shape_mpz_class_concatenate_assign/2,
	  ppl_BD_Shape_mpz_class_time_elapse_assign/2,
	  ppl_BD_Shape_mpz_class_upper_bound_assign_if_exact/2,
	  ppl_BD_Shape_mpz_class_simplify_using_context_assign/3,
	  ppl_BD_Shape_mpz_class_constrains/2,
	  ppl_BD_Shape_mpz_class_unconstrain_space_dimension/2,
	  ppl_BD_Shape_mpz_class_unconstrain_space_dimensions/2,
	  ppl_BD_Shape_mpz_class_affine_image/4,
	  ppl_BD_Shape_mpz_class_affine_preimage/4,
	  ppl_BD_Shape_mpz_class_bounded_affine_image/5,
	  ppl_BD_Shape_mpz_class_bounded_affine_preimage/5,
	  ppl_BD_Shape_mpz_class_generalized_affine_image/5,
	  ppl_BD_Shape_mpz_class_generalized_affine_preimage/5,
	  ppl_BD_Shape_mpz_class_generalized_affine_image_lhs_rhs/4,
	  ppl_BD_Shape_mpz_class_generalized_affine_preimage_lhs_rhs/4,
	  ppl_BD_Shape_mpz_class_add_space_dimensions_and_embed/2,
	  ppl_BD_Shape_mpz_class_add_space_dimensions_and_project/2,
	  ppl_BD_Shape_mpz_class_remove_space_dimensions/2,
	  ppl_BD_Shape_mpz_class_remove_higher_space_dimensions/2,
	  ppl_BD_Shape_mpz_class_expand_space_dimension/3,
	  ppl_BD_Shape_mpz_class_fold_space_dimensions/3,
	  ppl_BD_Shape_mpz_class_map_space_dimensions/2,
	  ppl_BD_Shape_mpz_class_ascii_dump/1,
	  ppl_BD_Shape_mpz_class_external_memory_in_bytes/2,
	  ppl_BD_Shape_mpz_class_total_memory_in_bytes/2,
	  ppl_BD_Shape_mpz_class_BHMZ05_widening_assign_with_tokens/4,
	  ppl_BD_Shape_mpz_class_H79_widening_assign_with_tokens/4,
	  ppl_BD_Shape_mpz_class_BHMZ05_widening_assign/2,
	  ppl_BD_Shape_mpz_class_H79_widening_assign/2,
	  ppl_BD_Shape_mpz_class_widening_assign_with_tokens/4,
	  ppl_BD_Shape_mpz_class_widening_assign/2,
	  ppl_BD_Shape_mpz_class_limited_BHMZ05_extrapolation_assign_with_tokens/5,
	  ppl_BD_Shape_mpz_class_limited_H79_extrapolation_assign_with_tokens/5,
	  ppl_BD_Shape_mpz_class_limited_CC76_extrapolation_assign_with_tokens/5,
	  ppl_BD_Shape_mpz_class_limited_BHMZ05_extrapolation_assign/3,
	  ppl_BD_Shape_mpz_class_limited_H79_extrapolation_assign/3,
	  ppl_BD_Shape_mpz_class_limited_CC76_extrapolation_assign/3,
	  ppl_BD_Shape_mpz_class_CC76_extrapolation_assign_with_tokens/4,
	  ppl_BD_Shape_mpz_class_CC76_extrapolation_assign/2,
	  ppl_BD_Shape_mpz_class_CC76_narrowing_assign/2,
	  ppl_BD_Shape_mpz_class_linear_partition/4,
	  ppl_delete_BD_Shape_mpq_class/1,
	  ppl_new_BD_Shape_mpq_class_from_space_dimension/3,
	  ppl_new_BD_Shape_mpq_class_from_C_Polyhedron/2,
	  ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron/2,
	  ppl_new_BD_Shape_mpq_class_from_Grid/2,
	  ppl_new_BD_Shape_mpq_class_from_Rational_Box/2,
	  ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class/2,
	  ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class/2,
	  ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class/2,
	  ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class/2,
	  ppl_new_BD_Shape_mpq_class_from_Double_Box/2,
	  ppl_new_BD_Shape_mpq_class_from_BD_Shape_double/2,
	  ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_double/2,
	  ppl_new_BD_Shape_mpq_class_from_C_Polyhedron_with_complexity/3,
	  ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron_with_complexity/3,
	  ppl_new_BD_Shape_mpq_class_from_Grid_with_complexity/3,
	  ppl_new_BD_Shape_mpq_class_from_Rational_Box_with_complexity/3,
	  ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity/3,
	  ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity/3,
	  ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity/3,
	  ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity/3,
	  ppl_new_BD_Shape_mpq_class_from_Double_Box_with_complexity/3,
	  ppl_new_BD_Shape_mpq_class_from_BD_Shape_double_with_complexity/3,
	  ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_double_with_complexity/3,
	  ppl_new_BD_Shape_mpq_class_from_constraints/2,
	  ppl_new_BD_Shape_mpq_class_from_congruences/2,
	  ppl_new_BD_Shape_mpq_class_from_generators/2,
	  ppl_BD_Shape_mpq_class_swap/2,
	  ppl_BD_Shape_mpq_class_space_dimension/2,
	  ppl_BD_Shape_mpq_class_affine_dimension/2,
	  ppl_BD_Shape_mpq_class_relation_with_constraint/3,
	  ppl_BD_Shape_mpq_class_relation_with_generator/3,
	  ppl_BD_Shape_mpq_class_relation_with_congruence/3,
	  ppl_BD_Shape_mpq_class_get_constraints/2,
	  ppl_BD_Shape_mpq_class_get_congruences/2,
	  ppl_BD_Shape_mpq_class_get_minimized_constraints/2,
	  ppl_BD_Shape_mpq_class_get_minimized_congruences/2,
	  ppl_BD_Shape_mpq_class_is_empty/1,
	  ppl_BD_Shape_mpq_class_is_universe/1,
	  ppl_BD_Shape_mpq_class_is_bounded/1,
	  ppl_BD_Shape_mpq_class_contains_integer_point/1,
	  ppl_BD_Shape_mpq_class_is_topologically_closed/1,
	  ppl_BD_Shape_mpq_class_is_discrete/1,
	  ppl_BD_Shape_mpq_class_topological_closure_assign/1,
	  ppl_BD_Shape_mpq_class_bounds_from_above/2,
	  ppl_BD_Shape_mpq_class_bounds_from_below/2,
	  ppl_BD_Shape_mpq_class_maximize/5,
	  ppl_BD_Shape_mpq_class_minimize/5,
	  ppl_BD_Shape_mpq_class_maximize_with_point/6,
	  ppl_BD_Shape_mpq_class_minimize_with_point/6,
	  ppl_BD_Shape_mpq_class_contains_BD_Shape_mpq_class/2,
	  ppl_BD_Shape_mpq_class_strictly_contains_BD_Shape_mpq_class/2,
	  ppl_BD_Shape_mpq_class_is_disjoint_from_BD_Shape_mpq_class/2,
	  ppl_BD_Shape_mpq_class_equals_BD_Shape_mpq_class/2,
	  ppl_BD_Shape_mpq_class_OK/1,
	  ppl_BD_Shape_mpq_class_add_constraint/2,
	  ppl_BD_Shape_mpq_class_add_congruence/2,
	  ppl_BD_Shape_mpq_class_add_constraints/2,
	  ppl_BD_Shape_mpq_class_add_congruences/2,
	  ppl_BD_Shape_mpq_class_refine_with_constraint/2,
	  ppl_BD_Shape_mpq_class_refine_with_congruence/2,
	  ppl_BD_Shape_mpq_class_refine_with_constraints/2,
	  ppl_BD_Shape_mpq_class_refine_with_congruences/2,
	  ppl_BD_Shape_mpq_class_intersection_assign/2,
	  ppl_BD_Shape_mpq_class_upper_bound_assign/2,
	  ppl_BD_Shape_mpq_class_difference_assign/2,
	  ppl_BD_Shape_mpq_class_concatenate_assign/2,
	  ppl_BD_Shape_mpq_class_time_elapse_assign/2,
	  ppl_BD_Shape_mpq_class_upper_bound_assign_if_exact/2,
	  ppl_BD_Shape_mpq_class_simplify_using_context_assign/3,
	  ppl_BD_Shape_mpq_class_constrains/2,
	  ppl_BD_Shape_mpq_class_unconstrain_space_dimension/2,
	  ppl_BD_Shape_mpq_class_unconstrain_space_dimensions/2,
	  ppl_BD_Shape_mpq_class_affine_image/4,
	  ppl_BD_Shape_mpq_class_affine_preimage/4,
	  ppl_BD_Shape_mpq_class_bounded_affine_image/5,
	  ppl_BD_Shape_mpq_class_bounded_affine_preimage/5,
	  ppl_BD_Shape_mpq_class_generalized_affine_image/5,
	  ppl_BD_Shape_mpq_class_generalized_affine_preimage/5,
	  ppl_BD_Shape_mpq_class_generalized_affine_image_lhs_rhs/4,
	  ppl_BD_Shape_mpq_class_generalized_affine_preimage_lhs_rhs/4,
	  ppl_BD_Shape_mpq_class_add_space_dimensions_and_embed/2,
	  ppl_BD_Shape_mpq_class_add_space_dimensions_and_project/2,
	  ppl_BD_Shape_mpq_class_remove_space_dimensions/2,
	  ppl_BD_Shape_mpq_class_remove_higher_space_dimensions/2,
	  ppl_BD_Shape_mpq_class_expand_space_dimension/3,
	  ppl_BD_Shape_mpq_class_fold_space_dimensions/3,
	  ppl_BD_Shape_mpq_class_map_space_dimensions/2,
	  ppl_BD_Shape_mpq_class_ascii_dump/1,
	  ppl_BD_Shape_mpq_class_external_memory_in_bytes/2,
	  ppl_BD_Shape_mpq_class_total_memory_in_bytes/2,
	  ppl_BD_Shape_mpq_class_BHMZ05_widening_assign_with_tokens/4,
	  ppl_BD_Shape_mpq_class_H79_widening_assign_with_tokens/4,
	  ppl_BD_Shape_mpq_class_BHMZ05_widening_assign/2,
	  ppl_BD_Shape_mpq_class_H79_widening_assign/2,
	  ppl_BD_Shape_mpq_class_widening_assign_with_tokens/4,
	  ppl_BD_Shape_mpq_class_widening_assign/2,
	  ppl_BD_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens/5,
	  ppl_BD_Shape_mpq_class_limited_H79_extrapolation_assign_with_tokens/5,
	  ppl_BD_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens/5,
	  ppl_BD_Shape_mpq_class_limited_BHMZ05_extrapolation_assign/3,
	  ppl_BD_Shape_mpq_class_limited_H79_extrapolation_assign/3,
	  ppl_BD_Shape_mpq_class_limited_CC76_extrapolation_assign/3,
	  ppl_BD_Shape_mpq_class_CC76_extrapolation_assign_with_tokens/4,
	  ppl_BD_Shape_mpq_class_CC76_extrapolation_assign/2,
	  ppl_BD_Shape_mpq_class_CC76_narrowing_assign/2,
	  ppl_BD_Shape_mpq_class_linear_partition/4,
	  ppl_delete_Octagonal_Shape_mpz_class/1,
	  ppl_new_Octagonal_Shape_mpz_class_from_space_dimension/3,
	  ppl_new_Octagonal_Shape_mpz_class_from_C_Polyhedron/2,
	  ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron/2,
	  ppl_new_Octagonal_Shape_mpz_class_from_Grid/2,
	  ppl_new_Octagonal_Shape_mpz_class_from_Rational_Box/2,
	  ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpz_class/2,
	  ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpq_class/2,
	  ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class/2,
	  ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpq_class/2,
	  ppl_new_Octagonal_Shape_mpz_class_from_Double_Box/2,
	  ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_double/2,
	  ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_double/2,
	  ppl_new_Octagonal_Shape_mpz_class_from_C_Polyhedron_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpz_class_from_Grid_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpz_class_from_Rational_Box_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpz_class_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpq_class_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpq_class_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpz_class_from_Double_Box_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_double_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_double_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpz_class_from_constraints/2,
	  ppl_new_Octagonal_Shape_mpz_class_from_congruences/2,
	  ppl_new_Octagonal_Shape_mpz_class_from_generators/2,
	  ppl_Octagonal_Shape_mpz_class_swap/2,
	  ppl_Octagonal_Shape_mpz_class_space_dimension/2,
	  ppl_Octagonal_Shape_mpz_class_affine_dimension/2,
	  ppl_Octagonal_Shape_mpz_class_relation_with_constraint/3,
	  ppl_Octagonal_Shape_mpz_class_relation_with_generator/3,
	  ppl_Octagonal_Shape_mpz_class_relation_with_congruence/3,
	  ppl_Octagonal_Shape_mpz_class_get_constraints/2,
	  ppl_Octagonal_Shape_mpz_class_get_congruences/2,
	  ppl_Octagonal_Shape_mpz_class_get_minimized_constraints/2,
	  ppl_Octagonal_Shape_mpz_class_get_minimized_congruences/2,
	  ppl_Octagonal_Shape_mpz_class_is_empty/1,
	  ppl_Octagonal_Shape_mpz_class_is_universe/1,
	  ppl_Octagonal_Shape_mpz_class_is_bounded/1,
	  ppl_Octagonal_Shape_mpz_class_contains_integer_point/1,
	  ppl_Octagonal_Shape_mpz_class_is_topologically_closed/1,
	  ppl_Octagonal_Shape_mpz_class_is_discrete/1,
	  ppl_Octagonal_Shape_mpz_class_topological_closure_assign/1,
	  ppl_Octagonal_Shape_mpz_class_bounds_from_above/2,
	  ppl_Octagonal_Shape_mpz_class_bounds_from_below/2,
	  ppl_Octagonal_Shape_mpz_class_maximize/5,
	  ppl_Octagonal_Shape_mpz_class_minimize/5,
	  ppl_Octagonal_Shape_mpz_class_maximize_with_point/6,
	  ppl_Octagonal_Shape_mpz_class_minimize_with_point/6,
	  ppl_Octagonal_Shape_mpz_class_contains_Octagonal_Shape_mpz_class/2,
	  ppl_Octagonal_Shape_mpz_class_strictly_contains_Octagonal_Shape_mpz_class/2,
	  ppl_Octagonal_Shape_mpz_class_is_disjoint_from_Octagonal_Shape_mpz_class/2,
	  ppl_Octagonal_Shape_mpz_class_equals_Octagonal_Shape_mpz_class/2,
	  ppl_Octagonal_Shape_mpz_class_OK/1,
	  ppl_Octagonal_Shape_mpz_class_add_constraint/2,
	  ppl_Octagonal_Shape_mpz_class_add_congruence/2,
	  ppl_Octagonal_Shape_mpz_class_add_constraints/2,
	  ppl_Octagonal_Shape_mpz_class_add_congruences/2,
	  ppl_Octagonal_Shape_mpz_class_refine_with_constraint/2,
	  ppl_Octagonal_Shape_mpz_class_refine_with_congruence/2,
	  ppl_Octagonal_Shape_mpz_class_refine_with_constraints/2,
	  ppl_Octagonal_Shape_mpz_class_refine_with_congruences/2,
	  ppl_Octagonal_Shape_mpz_class_intersection_assign/2,
	  ppl_Octagonal_Shape_mpz_class_upper_bound_assign/2,
	  ppl_Octagonal_Shape_mpz_class_difference_assign/2,
	  ppl_Octagonal_Shape_mpz_class_concatenate_assign/2,
	  ppl_Octagonal_Shape_mpz_class_time_elapse_assign/2,
	  ppl_Octagonal_Shape_mpz_class_upper_bound_assign_if_exact/2,
	  ppl_Octagonal_Shape_mpz_class_simplify_using_context_assign/3,
	  ppl_Octagonal_Shape_mpz_class_constrains/2,
	  ppl_Octagonal_Shape_mpz_class_unconstrain_space_dimension/2,
	  ppl_Octagonal_Shape_mpz_class_unconstrain_space_dimensions/2,
	  ppl_Octagonal_Shape_mpz_class_affine_image/4,
	  ppl_Octagonal_Shape_mpz_class_affine_preimage/4,
	  ppl_Octagonal_Shape_mpz_class_bounded_affine_image/5,
	  ppl_Octagonal_Shape_mpz_class_bounded_affine_preimage/5,
	  ppl_Octagonal_Shape_mpz_class_generalized_affine_image/5,
	  ppl_Octagonal_Shape_mpz_class_generalized_affine_preimage/5,
	  ppl_Octagonal_Shape_mpz_class_generalized_affine_image_lhs_rhs/4,
	  ppl_Octagonal_Shape_mpz_class_generalized_affine_preimage_lhs_rhs/4,
	  ppl_Octagonal_Shape_mpz_class_add_space_dimensions_and_embed/2,
	  ppl_Octagonal_Shape_mpz_class_add_space_dimensions_and_project/2,
	  ppl_Octagonal_Shape_mpz_class_remove_space_dimensions/2,
	  ppl_Octagonal_Shape_mpz_class_remove_higher_space_dimensions/2,
	  ppl_Octagonal_Shape_mpz_class_expand_space_dimension/3,
	  ppl_Octagonal_Shape_mpz_class_fold_space_dimensions/3,
	  ppl_Octagonal_Shape_mpz_class_map_space_dimensions/2,
	  ppl_Octagonal_Shape_mpz_class_ascii_dump/1,
	  ppl_Octagonal_Shape_mpz_class_external_memory_in_bytes/2,
	  ppl_Octagonal_Shape_mpz_class_total_memory_in_bytes/2,
	  ppl_Octagonal_Shape_mpz_class_BHMZ05_widening_assign_with_tokens/4,
	  ppl_Octagonal_Shape_mpz_class_BHMZ05_widening_assign/2,
	  ppl_Octagonal_Shape_mpz_class_widening_assign_with_tokens/4,
	  ppl_Octagonal_Shape_mpz_class_widening_assign/2,
	  ppl_Octagonal_Shape_mpz_class_limited_BHMZ05_extrapolation_assign_with_tokens/5,
	  ppl_Octagonal_Shape_mpz_class_limited_CC76_extrapolation_assign_with_tokens/5,
	  ppl_Octagonal_Shape_mpz_class_limited_BHMZ05_extrapolation_assign/3,
	  ppl_Octagonal_Shape_mpz_class_limited_CC76_extrapolation_assign/3,
	  ppl_Octagonal_Shape_mpz_class_CC76_extrapolation_assign_with_tokens/4,
	  ppl_Octagonal_Shape_mpz_class_CC76_extrapolation_assign/2,
	  ppl_Octagonal_Shape_mpz_class_CC76_narrowing_assign/2,
	  ppl_Octagonal_Shape_mpz_class_linear_partition/4,
	  ppl_delete_Octagonal_Shape_mpq_class/1,
	  ppl_new_Octagonal_Shape_mpq_class_from_space_dimension/3,
	  ppl_new_Octagonal_Shape_mpq_class_from_C_Polyhedron/2,
	  ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron/2,
	  ppl_new_Octagonal_Shape_mpq_class_from_Grid/2,
	  ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box/2,
	  ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class/2,
	  ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class/2,
	  ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class/2,
	  ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class/2,
	  ppl_new_Octagonal_Shape_mpq_class_from_Double_Box/2,
	  ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_double/2,
	  ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_double/2,
	  ppl_new_Octagonal_Shape_mpq_class_from_C_Polyhedron_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpq_class_from_Grid_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpq_class_from_Double_Box_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_double_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_double_with_complexity/3,
	  ppl_new_Octagonal_Shape_mpq_class_from_constraints/2,
	  ppl_new_Octagonal_Shape_mpq_class_from_congruences/2,
	  ppl_new_Octagonal_Shape_mpq_class_from_generators/2,
	  ppl_Octagonal_Shape_mpq_class_swap/2,
	  ppl_Octagonal_Shape_mpq_class_space_dimension/2,
	  ppl_Octagonal_Shape_mpq_class_affine_dimension/2,
	  ppl_Octagonal_Shape_mpq_class_relation_with_constraint/3,
	  ppl_Octagonal_Shape_mpq_class_relation_with_generator/3,
	  ppl_Octagonal_Shape_mpq_class_relation_with_congruence/3,
	  ppl_Octagonal_Shape_mpq_class_get_constraints/2,
	  ppl_Octagonal_Shape_mpq_class_get_congruences/2,
	  ppl_Octagonal_Shape_mpq_class_get_minimized_constraints/2,
	  ppl_Octagonal_Shape_mpq_class_get_minimized_congruences/2,
	  ppl_Octagonal_Shape_mpq_class_is_empty/1,
	  ppl_Octagonal_Shape_mpq_class_is_universe/1,
	  ppl_Octagonal_Shape_mpq_class_is_bounded/1,
	  ppl_Octagonal_Shape_mpq_class_contains_integer_point/1,
	  ppl_Octagonal_Shape_mpq_class_is_topologically_closed/1,
	  ppl_Octagonal_Shape_mpq_class_is_discrete/1,
	  ppl_Octagonal_Shape_mpq_class_topological_closure_assign/1,
	  ppl_Octagonal_Shape_mpq_class_bounds_from_above/2,
	  ppl_Octagonal_Shape_mpq_class_bounds_from_below/2,
	  ppl_Octagonal_Shape_mpq_class_maximize/5,
	  ppl_Octagonal_Shape_mpq_class_minimize/5,
	  ppl_Octagonal_Shape_mpq_class_maximize_with_point/6,
	  ppl_Octagonal_Shape_mpq_class_minimize_with_point/6,
	  ppl_Octagonal_Shape_mpq_class_contains_Octagonal_Shape_mpq_class/2,
	  ppl_Octagonal_Shape_mpq_class_strictly_contains_Octagonal_Shape_mpq_class/2,
	  ppl_Octagonal_Shape_mpq_class_is_disjoint_from_Octagonal_Shape_mpq_class/2,
	  ppl_Octagonal_Shape_mpq_class_equals_Octagonal_Shape_mpq_class/2,
	  ppl_Octagonal_Shape_mpq_class_OK/1,
	  ppl_Octagonal_Shape_mpq_class_add_constraint/2,
	  ppl_Octagonal_Shape_mpq_class_add_congruence/2,
	  ppl_Octagonal_Shape_mpq_class_add_constraints/2,
	  ppl_Octagonal_Shape_mpq_class_add_congruences/2,
	  ppl_Octagonal_Shape_mpq_class_refine_with_constraint/2,
	  ppl_Octagonal_Shape_mpq_class_refine_with_congruence/2,
	  ppl_Octagonal_Shape_mpq_class_refine_with_constraints/2,
	  ppl_Octagonal_Shape_mpq_class_refine_with_congruences/2,
	  ppl_Octagonal_Shape_mpq_class_intersection_assign/2,
	  ppl_Octagonal_Shape_mpq_class_upper_bound_assign/2,
	  ppl_Octagonal_Shape_mpq_class_difference_assign/2,
	  ppl_Octagonal_Shape_mpq_class_concatenate_assign/2,
	  ppl_Octagonal_Shape_mpq_class_time_elapse_assign/2,
	  ppl_Octagonal_Shape_mpq_class_upper_bound_assign_if_exact/2,
	  ppl_Octagonal_Shape_mpq_class_simplify_using_context_assign/3,
	  ppl_Octagonal_Shape_mpq_class_constrains/2,
	  ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimension/2,
	  ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimensions/2,
	  ppl_Octagonal_Shape_mpq_class_affine_image/4,
	  ppl_Octagonal_Shape_mpq_class_affine_preimage/4,
	  ppl_Octagonal_Shape_mpq_class_bounded_affine_image/5,
	  ppl_Octagonal_Shape_mpq_class_bounded_affine_preimage/5,
	  ppl_Octagonal_Shape_mpq_class_generalized_affine_image/5,
	  ppl_Octagonal_Shape_mpq_class_generalized_affine_preimage/5,
	  ppl_Octagonal_Shape_mpq_class_generalized_affine_image_lhs_rhs/4,
	  ppl_Octagonal_Shape_mpq_class_generalized_affine_preimage_lhs_rhs/4,
	  ppl_Octagonal_Shape_mpq_class_add_space_dimensions_and_embed/2,
	  ppl_Octagonal_Shape_mpq_class_add_space_dimensions_and_project/2,
	  ppl_Octagonal_Shape_mpq_class_remove_space_dimensions/2,
	  ppl_Octagonal_Shape_mpq_class_remove_higher_space_dimensions/2,
	  ppl_Octagonal_Shape_mpq_class_expand_space_dimension/3,
	  ppl_Octagonal_Shape_mpq_class_fold_space_dimensions/3,
	  ppl_Octagonal_Shape_mpq_class_map_space_dimensions/2,
	  ppl_Octagonal_Shape_mpq_class_ascii_dump/1,
	  ppl_Octagonal_Shape_mpq_class_external_memory_in_bytes/2,
	  ppl_Octagonal_Shape_mpq_class_total_memory_in_bytes/2,
	  ppl_Octagonal_Shape_mpq_class_BHMZ05_widening_assign_with_tokens/4,
	  ppl_Octagonal_Shape_mpq_class_BHMZ05_widening_assign/2,
	  ppl_Octagonal_Shape_mpq_class_widening_assign_with_tokens/4,
	  ppl_Octagonal_Shape_mpq_class_widening_assign/2,
	  ppl_Octagonal_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens/5,
	  ppl_Octagonal_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens/5,
	  ppl_Octagonal_Shape_mpq_class_limited_BHMZ05_extrapolation_assign/3,
	  ppl_Octagonal_Shape_mpq_class_limited_CC76_extrapolation_assign/3,
	  ppl_Octagonal_Shape_mpq_class_CC76_extrapolation_assign_with_tokens/4,
	  ppl_Octagonal_Shape_mpq_class_CC76_extrapolation_assign/2,
	  ppl_Octagonal_Shape_mpq_class_CC76_narrowing_assign/2,
	  ppl_Octagonal_Shape_mpq_class_linear_partition/4,
	  ppl_delete_Constraints_Product_C_Polyhedron_Grid/1,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_space_dimension/3,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_C_Polyhedron/2,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_NNC_Polyhedron/2,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Grid/2,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Rational_Box/2,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpz_class/2,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpq_class/2,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpz_class/2,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpq_class/2,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Double_Box/2,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_double/2,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_double/2,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Constraints_Product_C_Polyhedron_Grid/2,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_C_Polyhedron_with_complexity/3,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_NNC_Polyhedron_with_complexity/3,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Grid_with_complexity/3,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Rational_Box_with_complexity/3,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpz_class_with_complexity/3,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpq_class_with_complexity/3,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpz_class_with_complexity/3,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpq_class_with_complexity/3,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Double_Box_with_complexity/3,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_double_with_complexity/3,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_double_with_complexity/3,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Constraints_Product_C_Polyhedron_Grid_with_complexity/3,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_constraints/2,
	  ppl_new_Constraints_Product_C_Polyhedron_Grid_from_congruences/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_swap/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_space_dimension/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_affine_dimension/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_constraint/3,
	  ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_generator/3,
	  ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_congruence/3,
	  ppl_Constraints_Product_C_Polyhedron_Grid_is_empty/1,
	  ppl_Constraints_Product_C_Polyhedron_Grid_is_universe/1,
	  ppl_Constraints_Product_C_Polyhedron_Grid_is_bounded/1,
	  ppl_Constraints_Product_C_Polyhedron_Grid_is_topologically_closed/1,
	  ppl_Constraints_Product_C_Polyhedron_Grid_is_discrete/1,
	  ppl_Constraints_Product_C_Polyhedron_Grid_topological_closure_assign/1,
	  ppl_Constraints_Product_C_Polyhedron_Grid_bounds_from_above/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_bounds_from_below/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_maximize/5,
	  ppl_Constraints_Product_C_Polyhedron_Grid_minimize/5,
	  ppl_Constraints_Product_C_Polyhedron_Grid_maximize_with_point/6,
	  ppl_Constraints_Product_C_Polyhedron_Grid_minimize_with_point/6,
	  ppl_Constraints_Product_C_Polyhedron_Grid_contains_Constraints_Product_C_Polyhedron_Grid/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_strictly_contains_Constraints_Product_C_Polyhedron_Grid/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_is_disjoint_from_Constraints_Product_C_Polyhedron_Grid/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_equals_Constraints_Product_C_Polyhedron_Grid/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_OK/1,
	  ppl_Constraints_Product_C_Polyhedron_Grid_add_constraint/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_add_congruence/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_add_constraints/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_add_congruences/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_refine_with_constraint/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_refine_with_congruence/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_refine_with_constraints/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_refine_with_congruences/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_intersection_assign/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_upper_bound_assign/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_difference_assign/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_concatenate_assign/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_time_elapse_assign/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_upper_bound_assign_if_exact/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_constrains/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_unconstrain_space_dimension/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_unconstrain_space_dimensions/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_affine_image/4,
	  ppl_Constraints_Product_C_Polyhedron_Grid_affine_preimage/4,
	  ppl_Constraints_Product_C_Polyhedron_Grid_bounded_affine_image/5,
	  ppl_Constraints_Product_C_Polyhedron_Grid_bounded_affine_preimage/5,
	  ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_image/5,
	  ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_preimage/5,
	  ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_image_lhs_rhs/4,
	  ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_preimage_lhs_rhs/4,
	  ppl_Constraints_Product_C_Polyhedron_Grid_add_space_dimensions_and_embed/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_add_space_dimensions_and_project/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_remove_space_dimensions/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_remove_higher_space_dimensions/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_expand_space_dimension/3,
	  ppl_Constraints_Product_C_Polyhedron_Grid_fold_space_dimensions/3,
	  ppl_Constraints_Product_C_Polyhedron_Grid_map_space_dimensions/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_ascii_dump/1,
	  ppl_Constraints_Product_C_Polyhedron_Grid_external_memory_in_bytes/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_total_memory_in_bytes/2,
	  ppl_Constraints_Product_C_Polyhedron_Grid_widening_assign_with_tokens/4,
	  ppl_Constraints_Product_C_Polyhedron_Grid_widening_assign/2,
	  ppl_delete_Pointset_Powerset_C_Polyhedron/1,
	  ppl_new_Pointset_Powerset_C_Polyhedron_from_space_dimension/3,
	  ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron/2,
	  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron/2,
	  ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron_with_complexity/3,
	  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron_with_complexity/3,
	  ppl_new_Pointset_Powerset_C_Polyhedron_from_constraints/2,
	  ppl_new_Pointset_Powerset_C_Polyhedron_from_congruences/2,
	  ppl_Pointset_Powerset_C_Polyhedron_swap/2,
	  ppl_Pointset_Powerset_C_Polyhedron_space_dimension/2,
	  ppl_Pointset_Powerset_C_Polyhedron_affine_dimension/2,
	  ppl_Pointset_Powerset_C_Polyhedron_relation_with_constraint/3,
	  ppl_Pointset_Powerset_C_Polyhedron_relation_with_generator/3,
	  ppl_Pointset_Powerset_C_Polyhedron_relation_with_congruence/3,
	  ppl_Pointset_Powerset_C_Polyhedron_is_empty/1,
	  ppl_Pointset_Powerset_C_Polyhedron_is_universe/1,
	  ppl_Pointset_Powerset_C_Polyhedron_is_bounded/1,
	  ppl_Pointset_Powerset_C_Polyhedron_contains_integer_point/1,
	  ppl_Pointset_Powerset_C_Polyhedron_is_topologically_closed/1,
	  ppl_Pointset_Powerset_C_Polyhedron_is_discrete/1,
	  ppl_Pointset_Powerset_C_Polyhedron_pairwise_reduce/1,
	  ppl_Pointset_Powerset_C_Polyhedron_omega_reduce/1,
	  ppl_Pointset_Powerset_C_Polyhedron_topological_closure_assign/1,
	  ppl_Pointset_Powerset_C_Polyhedron_bounds_from_above/2,
	  ppl_Pointset_Powerset_C_Polyhedron_bounds_from_below/2,
	  ppl_Pointset_Powerset_C_Polyhedron_maximize/5,
	  ppl_Pointset_Powerset_C_Polyhedron_minimize/5,
	  ppl_Pointset_Powerset_C_Polyhedron_maximize_with_point/6,
	  ppl_Pointset_Powerset_C_Polyhedron_minimize_with_point/6,
	  ppl_Pointset_Powerset_C_Polyhedron_contains_Pointset_Powerset_C_Polyhedron/2,
	  ppl_Pointset_Powerset_C_Polyhedron_strictly_contains_Pointset_Powerset_C_Polyhedron/2,
	  ppl_Pointset_Powerset_C_Polyhedron_is_disjoint_from_Pointset_Powerset_C_Polyhedron/2,
	  ppl_Pointset_Powerset_C_Polyhedron_geometrically_covers_Pointset_Powerset_C_Polyhedron/2,
	  ppl_Pointset_Powerset_C_Polyhedron_geometrically_equals_Pointset_Powerset_C_Polyhedron/2,
	  ppl_Pointset_Powerset_C_Polyhedron_equals_Pointset_Powerset_C_Polyhedron/2,
	  ppl_Pointset_Powerset_C_Polyhedron_OK/1,
	  ppl_Pointset_Powerset_C_Polyhedron_add_constraint/2,
	  ppl_Pointset_Powerset_C_Polyhedron_add_congruence/2,
	  ppl_Pointset_Powerset_C_Polyhedron_add_constraints/2,
	  ppl_Pointset_Powerset_C_Polyhedron_add_congruences/2,
	  ppl_Pointset_Powerset_C_Polyhedron_refine_with_constraint/2,
	  ppl_Pointset_Powerset_C_Polyhedron_refine_with_congruence/2,
	  ppl_Pointset_Powerset_C_Polyhedron_refine_with_constraints/2,
	  ppl_Pointset_Powerset_C_Polyhedron_refine_with_congruences/2,
	  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign/2,
	  ppl_Pointset_Powerset_C_Polyhedron_upper_bound_assign/2,
	  ppl_Pointset_Powerset_C_Polyhedron_difference_assign/2,
	  ppl_Pointset_Powerset_C_Polyhedron_concatenate_assign/2,
	  ppl_Pointset_Powerset_C_Polyhedron_time_elapse_assign/2,
	  ppl_Pointset_Powerset_C_Polyhedron_upper_bound_assign_if_exact/2,
	  ppl_Pointset_Powerset_C_Polyhedron_simplify_using_context_assign/3,
	  ppl_Pointset_Powerset_C_Polyhedron_constrains/2,
	  ppl_Pointset_Powerset_C_Polyhedron_unconstrain_space_dimension/2,
	  ppl_Pointset_Powerset_C_Polyhedron_unconstrain_space_dimensions/2,
	  ppl_Pointset_Powerset_C_Polyhedron_affine_image/4,
	  ppl_Pointset_Powerset_C_Polyhedron_affine_preimage/4,
	  ppl_Pointset_Powerset_C_Polyhedron_bounded_affine_image/5,
	  ppl_Pointset_Powerset_C_Polyhedron_bounded_affine_preimage/5,
	  ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_image/5,
	  ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_preimage/5,
	  ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_image_lhs_rhs/4,
	  ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_preimage_lhs_rhs/4,
	  ppl_Pointset_Powerset_C_Polyhedron_add_space_dimensions_and_embed/2,
	  ppl_Pointset_Powerset_C_Polyhedron_add_space_dimensions_and_project/2,
	  ppl_Pointset_Powerset_C_Polyhedron_remove_space_dimensions/2,
	  ppl_Pointset_Powerset_C_Polyhedron_remove_higher_space_dimensions/2,
	  ppl_Pointset_Powerset_C_Polyhedron_expand_space_dimension/3,
	  ppl_Pointset_Powerset_C_Polyhedron_fold_space_dimensions/3,
	  ppl_Pointset_Powerset_C_Polyhedron_map_space_dimensions/2,
	  ppl_Pointset_Powerset_C_Polyhedron_ascii_dump/1,
	  ppl_Pointset_Powerset_C_Polyhedron_external_memory_in_bytes/2,
	  ppl_Pointset_Powerset_C_Polyhedron_total_memory_in_bytes/2,
	  ppl_Pointset_Powerset_C_Polyhedron_size/2,
	  ppl_new_Pointset_Powerset_C_Polyhedron_iterator_from_iterator/2,
	  ppl_Pointset_Powerset_C_Polyhedron_begin_iterator/2,
	  ppl_Pointset_Powerset_C_Polyhedron_end_iterator/2,
	  ppl_Pointset_Powerset_C_Polyhedron_iterator_equals_iterator/2,
	  ppl_Pointset_Powerset_C_Polyhedron_increment_iterator/1,
	  ppl_Pointset_Powerset_C_Polyhedron_decrement_iterator/1,
	  ppl_Pointset_Powerset_C_Polyhedron_get_disjunct/2,
	  ppl_delete_Pointset_Powerset_C_Polyhedron_iterator/1,
	  ppl_Pointset_Powerset_C_Polyhedron_add_disjunct/2,
	  ppl_Pointset_Powerset_C_Polyhedron_drop_disjunct/2,
	  ppl_Pointset_Powerset_C_Polyhedron_drop_disjuncts/3,
	  ppl_Pointset_Powerset_C_Polyhedron_BHZ03_BHRZ03_BHRZ03_widening_assign/2,
	  ppl_Pointset_Powerset_C_Polyhedron_BHZ03_H79_H79_widening_assign/2,
	  ppl_Pointset_Powerset_C_Polyhedron_BGP99_BHRZ03_extrapolation_assign/3,
	  ppl_Pointset_Powerset_C_Polyhedron_BGP99_H79_extrapolation_assign/3,
	  ppl_delete_Pointset_Powerset_NNC_Polyhedron/1,
	  ppl_new_Pointset_Powerset_NNC_Polyhedron_from_space_dimension/3,
	  ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron/2,
	  ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron/2,
	  ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron_with_complexity/3,
	  ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity/3,
	  ppl_new_Pointset_Powerset_NNC_Polyhedron_from_constraints/2,
	  ppl_new_Pointset_Powerset_NNC_Polyhedron_from_congruences/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_swap/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_space_dimension/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_affine_dimension/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_constraint/3,
	  ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_generator/3,
	  ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_congruence/3,
	  ppl_Pointset_Powerset_NNC_Polyhedron_is_empty/1,
	  ppl_Pointset_Powerset_NNC_Polyhedron_is_universe/1,
	  ppl_Pointset_Powerset_NNC_Polyhedron_is_bounded/1,
	  ppl_Pointset_Powerset_NNC_Polyhedron_contains_integer_point/1,
	  ppl_Pointset_Powerset_NNC_Polyhedron_is_topologically_closed/1,
	  ppl_Pointset_Powerset_NNC_Polyhedron_is_discrete/1,
	  ppl_Pointset_Powerset_NNC_Polyhedron_pairwise_reduce/1,
	  ppl_Pointset_Powerset_NNC_Polyhedron_omega_reduce/1,
	  ppl_Pointset_Powerset_NNC_Polyhedron_topological_closure_assign/1,
	  ppl_Pointset_Powerset_NNC_Polyhedron_bounds_from_above/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_bounds_from_below/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_maximize/5,
	  ppl_Pointset_Powerset_NNC_Polyhedron_minimize/5,
	  ppl_Pointset_Powerset_NNC_Polyhedron_maximize_with_point/6,
	  ppl_Pointset_Powerset_NNC_Polyhedron_minimize_with_point/6,
	  ppl_Pointset_Powerset_NNC_Polyhedron_contains_Pointset_Powerset_NNC_Polyhedron/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_strictly_contains_Pointset_Powerset_NNC_Polyhedron/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_is_disjoint_from_Pointset_Powerset_NNC_Polyhedron/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_covers_Pointset_Powerset_NNC_Polyhedron/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_equals_Pointset_Powerset_NNC_Polyhedron/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_equals_Pointset_Powerset_NNC_Polyhedron/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_OK/1,
	  ppl_Pointset_Powerset_NNC_Polyhedron_add_constraint/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_add_congruence/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_add_constraints/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_add_congruences/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_refine_with_constraint/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_refine_with_congruence/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_refine_with_constraints/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_refine_with_congruences/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_intersection_assign/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_upper_bound_assign/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_difference_assign/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_concatenate_assign/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_time_elapse_assign/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_upper_bound_assign_if_exact/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_simplify_using_context_assign/3,
	  ppl_Pointset_Powerset_NNC_Polyhedron_constrains/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimension/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimensions/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_affine_image/4,
	  ppl_Pointset_Powerset_NNC_Polyhedron_affine_preimage/4,
	  ppl_Pointset_Powerset_NNC_Polyhedron_bounded_affine_image/5,
	  ppl_Pointset_Powerset_NNC_Polyhedron_bounded_affine_preimage/5,
	  ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_image/5,
	  ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_preimage/5,
	  ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_image_lhs_rhs/4,
	  ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_preimage_lhs_rhs/4,
	  ppl_Pointset_Powerset_NNC_Polyhedron_add_space_dimensions_and_embed/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_add_space_dimensions_and_project/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_remove_space_dimensions/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_remove_higher_space_dimensions/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_expand_space_dimension/3,
	  ppl_Pointset_Powerset_NNC_Polyhedron_fold_space_dimensions/3,
	  ppl_Pointset_Powerset_NNC_Polyhedron_map_space_dimensions/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_ascii_dump/1,
	  ppl_Pointset_Powerset_NNC_Polyhedron_external_memory_in_bytes/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_total_memory_in_bytes/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_size/2,
	  ppl_new_Pointset_Powerset_NNC_Polyhedron_iterator_from_iterator/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_begin_iterator/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_end_iterator/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_iterator_equals_iterator/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_increment_iterator/1,
	  ppl_Pointset_Powerset_NNC_Polyhedron_decrement_iterator/1,
	  ppl_Pointset_Powerset_NNC_Polyhedron_get_disjunct/2,
	  ppl_delete_Pointset_Powerset_NNC_Polyhedron_iterator/1,
	  ppl_Pointset_Powerset_NNC_Polyhedron_add_disjunct/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_drop_disjunct/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_drop_disjuncts/3,
	  ppl_Pointset_Powerset_NNC_Polyhedron_BHZ03_BHRZ03_BHRZ03_widening_assign/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_BHZ03_H79_H79_widening_assign/2,
	  ppl_Pointset_Powerset_NNC_Polyhedron_BGP99_BHRZ03_extrapolation_assign/3,
	  ppl_Pointset_Powerset_NNC_Polyhedron_BGP99_H79_extrapolation_assign/3,
	  ppl_delete_Double_Box/1,
	  ppl_new_Double_Box_from_space_dimension/3,
	  ppl_new_Double_Box_from_C_Polyhedron/2,
	  ppl_new_Double_Box_from_NNC_Polyhedron/2,
	  ppl_new_Double_Box_from_Grid/2,
	  ppl_new_Double_Box_from_Rational_Box/2,
	  ppl_new_Double_Box_from_BD_Shape_mpz_class/2,
	  ppl_new_Double_Box_from_BD_Shape_mpq_class/2,
	  ppl_new_Double_Box_from_Octagonal_Shape_mpz_class/2,
	  ppl_new_Double_Box_from_Octagonal_Shape_mpq_class/2,
	  ppl_new_Double_Box_from_Double_Box/2,
	  ppl_new_Double_Box_from_BD_Shape_double/2,
	  ppl_new_Double_Box_from_Octagonal_Shape_double/2,
	  ppl_new_Double_Box_from_C_Polyhedron_with_complexity/3,
	  ppl_new_Double_Box_from_NNC_Polyhedron_with_complexity/3,
	  ppl_new_Double_Box_from_Grid_with_complexity/3,
	  ppl_new_Double_Box_from_Rational_Box_with_complexity/3,
	  ppl_new_Double_Box_from_BD_Shape_mpz_class_with_complexity/3,
	  ppl_new_Double_Box_from_BD_Shape_mpq_class_with_complexity/3,
	  ppl_new_Double_Box_from_Octagonal_Shape_mpz_class_with_complexity/3,
	  ppl_new_Double_Box_from_Octagonal_Shape_mpq_class_with_complexity/3,
	  ppl_new_Double_Box_from_Double_Box_with_complexity/3,
	  ppl_new_Double_Box_from_BD_Shape_double_with_complexity/3,
	  ppl_new_Double_Box_from_Octagonal_Shape_double_with_complexity/3,
	  ppl_new_Double_Box_from_constraints/2,
	  ppl_new_Double_Box_from_congruences/2,
	  ppl_new_Double_Box_from_generators/2,
	  ppl_Double_Box_swap/2,
	  ppl_Double_Box_space_dimension/2,
	  ppl_Double_Box_affine_dimension/2,
	  ppl_Double_Box_relation_with_constraint/3,
	  ppl_Double_Box_relation_with_generator/3,
	  ppl_Double_Box_relation_with_congruence/3,
	  ppl_Double_Box_get_constraints/2,
	  ppl_Double_Box_get_congruences/2,
	  ppl_Double_Box_get_minimized_constraints/2,
	  ppl_Double_Box_get_minimized_congruences/2,
	  ppl_Double_Box_is_empty/1,
	  ppl_Double_Box_is_universe/1,
	  ppl_Double_Box_is_bounded/1,
	  ppl_Double_Box_contains_integer_point/1,
	  ppl_Double_Box_is_topologically_closed/1,
	  ppl_Double_Box_is_discrete/1,
	  ppl_Double_Box_topological_closure_assign/1,
	  ppl_Double_Box_bounds_from_above/2,
	  ppl_Double_Box_bounds_from_below/2,
	  ppl_Double_Box_maximize/5,
	  ppl_Double_Box_minimize/5,
	  ppl_Double_Box_maximize_with_point/6,
	  ppl_Double_Box_minimize_with_point/6,
	  ppl_Double_Box_contains_Double_Box/2,
	  ppl_Double_Box_strictly_contains_Double_Box/2,
	  ppl_Double_Box_is_disjoint_from_Double_Box/2,
	  ppl_Double_Box_equals_Double_Box/2,
	  ppl_Double_Box_OK/1,
	  ppl_Double_Box_add_constraint/2,
	  ppl_Double_Box_add_congruence/2,
	  ppl_Double_Box_add_constraints/2,
	  ppl_Double_Box_add_congruences/2,
	  ppl_Double_Box_refine_with_constraint/2,
	  ppl_Double_Box_refine_with_congruence/2,
	  ppl_Double_Box_refine_with_constraints/2,
	  ppl_Double_Box_refine_with_congruences/2,
	  ppl_Double_Box_intersection_assign/2,
	  ppl_Double_Box_upper_bound_assign/2,
	  ppl_Double_Box_difference_assign/2,
	  ppl_Double_Box_concatenate_assign/2,
	  ppl_Double_Box_time_elapse_assign/2,
	  ppl_Double_Box_upper_bound_assign_if_exact/2,
	  ppl_Double_Box_simplify_using_context_assign/3,
	  ppl_Double_Box_constrains/2,
	  ppl_Double_Box_unconstrain_space_dimension/2,
	  ppl_Double_Box_unconstrain_space_dimensions/2,
	  ppl_Double_Box_affine_image/4,
	  ppl_Double_Box_affine_preimage/4,
	  ppl_Double_Box_bounded_affine_image/5,
	  ppl_Double_Box_bounded_affine_preimage/5,
	  ppl_Double_Box_generalized_affine_image/5,
	  ppl_Double_Box_generalized_affine_preimage/5,
	  ppl_Double_Box_generalized_affine_image_lhs_rhs/4,
	  ppl_Double_Box_generalized_affine_preimage_lhs_rhs/4,
	  ppl_Double_Box_add_space_dimensions_and_embed/2,
	  ppl_Double_Box_add_space_dimensions_and_project/2,
	  ppl_Double_Box_remove_space_dimensions/2,
	  ppl_Double_Box_remove_higher_space_dimensions/2,
	  ppl_Double_Box_expand_space_dimension/3,
	  ppl_Double_Box_fold_space_dimensions/3,
	  ppl_Double_Box_map_space_dimensions/2,
	  ppl_Double_Box_ascii_dump/1,
	  ppl_Double_Box_external_memory_in_bytes/2,
	  ppl_Double_Box_total_memory_in_bytes/2,
	  ppl_Double_Box_CC76_widening_assign_with_tokens/4,
	  ppl_Double_Box_CC76_widening_assign/2,
	  ppl_Double_Box_widening_assign_with_tokens/4,
	  ppl_Double_Box_widening_assign/2,
	  ppl_Double_Box_limited_CC76_extrapolation_assign_with_tokens/5,
	  ppl_Double_Box_limited_CC76_extrapolation_assign/3,
	  ppl_Double_Box_linear_partition/4,
	  ppl_delete_BD_Shape_double/1,
	  ppl_new_BD_Shape_double_from_space_dimension/3,
	  ppl_new_BD_Shape_double_from_C_Polyhedron/2,
	  ppl_new_BD_Shape_double_from_NNC_Polyhedron/2,
	  ppl_new_BD_Shape_double_from_Grid/2,
	  ppl_new_BD_Shape_double_from_Rational_Box/2,
	  ppl_new_BD_Shape_double_from_BD_Shape_mpz_class/2,
	  ppl_new_BD_Shape_double_from_BD_Shape_mpq_class/2,
	  ppl_new_BD_Shape_double_from_Octagonal_Shape_mpz_class/2,
	  ppl_new_BD_Shape_double_from_Octagonal_Shape_mpq_class/2,
	  ppl_new_BD_Shape_double_from_Double_Box/2,
	  ppl_new_BD_Shape_double_from_BD_Shape_double/2,
	  ppl_new_BD_Shape_double_from_Octagonal_Shape_double/2,
	  ppl_new_BD_Shape_double_from_C_Polyhedron_with_complexity/3,
	  ppl_new_BD_Shape_double_from_NNC_Polyhedron_with_complexity/3,
	  ppl_new_BD_Shape_double_from_Grid_with_complexity/3,
	  ppl_new_BD_Shape_double_from_Rational_Box_with_complexity/3,
	  ppl_new_BD_Shape_double_from_BD_Shape_mpz_class_with_complexity/3,
	  ppl_new_BD_Shape_double_from_BD_Shape_mpq_class_with_complexity/3,
	  ppl_new_BD_Shape_double_from_Octagonal_Shape_mpz_class_with_complexity/3,
	  ppl_new_BD_Shape_double_from_Octagonal_Shape_mpq_class_with_complexity/3,
	  ppl_new_BD_Shape_double_from_Double_Box_with_complexity/3,
	  ppl_new_BD_Shape_double_from_BD_Shape_double_with_complexity/3,
	  ppl_new_BD_Shape_double_from_Octagonal_Shape_double_with_complexity/3,
	  ppl_new_BD_Shape_double_from_constraints/2,
	  ppl_new_BD_Shape_double_from_congruences/2,
	  ppl_new_BD_Shape_double_from_generators/2,
	  ppl_BD_Shape_double_swap/2,
	  ppl_BD_Shape_double_space_dimension/2,
	  ppl_BD_Shape_double_affine_dimension/2,
	  ppl_BD_Shape_double_relation_with_constraint/3,
	  ppl_BD_Shape_double_relation_with_generator/3,
	  ppl_BD_Shape_double_relation_with_congruence/3,
	  ppl_BD_Shape_double_get_constraints/2,
	  ppl_BD_Shape_double_get_congruences/2,
	  ppl_BD_Shape_double_get_minimized_constraints/2,
	  ppl_BD_Shape_double_get_minimized_congruences/2,
	  ppl_BD_Shape_double_is_empty/1,
	  ppl_BD_Shape_double_is_universe/1,
	  ppl_BD_Shape_double_is_bounded/1,
	  ppl_BD_Shape_double_contains_integer_point/1,
	  ppl_BD_Shape_double_is_topologically_closed/1,
	  ppl_BD_Shape_double_is_discrete/1,
	  ppl_BD_Shape_double_topological_closure_assign/1,
	  ppl_BD_Shape_double_bounds_from_above/2,
	  ppl_BD_Shape_double_bounds_from_below/2,
	  ppl_BD_Shape_double_maximize/5,
	  ppl_BD_Shape_double_minimize/5,
	  ppl_BD_Shape_double_maximize_with_point/6,
	  ppl_BD_Shape_double_minimize_with_point/6,
	  ppl_BD_Shape_double_contains_BD_Shape_double/2,
	  ppl_BD_Shape_double_strictly_contains_BD_Shape_double/2,
	  ppl_BD_Shape_double_is_disjoint_from_BD_Shape_double/2,
	  ppl_BD_Shape_double_equals_BD_Shape_double/2,
	  ppl_BD_Shape_double_OK/1,
	  ppl_BD_Shape_double_add_constraint/2,
	  ppl_BD_Shape_double_add_congruence/2,
	  ppl_BD_Shape_double_add_constraints/2,
	  ppl_BD_Shape_double_add_congruences/2,
	  ppl_BD_Shape_double_refine_with_constraint/2,
	  ppl_BD_Shape_double_refine_with_congruence/2,
	  ppl_BD_Shape_double_refine_with_constraints/2,
	  ppl_BD_Shape_double_refine_with_congruences/2,
	  ppl_BD_Shape_double_intersection_assign/2,
	  ppl_BD_Shape_double_upper_bound_assign/2,
	  ppl_BD_Shape_double_difference_assign/2,
	  ppl_BD_Shape_double_concatenate_assign/2,
	  ppl_BD_Shape_double_time_elapse_assign/2,
	  ppl_BD_Shape_double_upper_bound_assign_if_exact/2,
	  ppl_BD_Shape_double_simplify_using_context_assign/3,
	  ppl_BD_Shape_double_constrains/2,
	  ppl_BD_Shape_double_unconstrain_space_dimension/2,
	  ppl_BD_Shape_double_unconstrain_space_dimensions/2,
	  ppl_BD_Shape_double_affine_image/4,
	  ppl_BD_Shape_double_affine_preimage/4,
	  ppl_BD_Shape_double_bounded_affine_image/5,
	  ppl_BD_Shape_double_bounded_affine_preimage/5,
	  ppl_BD_Shape_double_generalized_affine_image/5,
	  ppl_BD_Shape_double_generalized_affine_preimage/5,
	  ppl_BD_Shape_double_generalized_affine_image_lhs_rhs/4,
	  ppl_BD_Shape_double_generalized_affine_preimage_lhs_rhs/4,
	  ppl_BD_Shape_double_add_space_dimensions_and_embed/2,
	  ppl_BD_Shape_double_add_space_dimensions_and_project/2,
	  ppl_BD_Shape_double_remove_space_dimensions/2,
	  ppl_BD_Shape_double_remove_higher_space_dimensions/2,
	  ppl_BD_Shape_double_expand_space_dimension/3,
	  ppl_BD_Shape_double_fold_space_dimensions/3,
	  ppl_BD_Shape_double_map_space_dimensions/2,
	  ppl_BD_Shape_double_ascii_dump/1,
	  ppl_BD_Shape_double_external_memory_in_bytes/2,
	  ppl_BD_Shape_double_total_memory_in_bytes/2,
	  ppl_BD_Shape_double_BHMZ05_widening_assign_with_tokens/4,
	  ppl_BD_Shape_double_H79_widening_assign_with_tokens/4,
	  ppl_BD_Shape_double_BHMZ05_widening_assign/2,
	  ppl_BD_Shape_double_H79_widening_assign/2,
	  ppl_BD_Shape_double_widening_assign_with_tokens/4,
	  ppl_BD_Shape_double_widening_assign/2,
	  ppl_BD_Shape_double_limited_BHMZ05_extrapolation_assign_with_tokens/5,
	  ppl_BD_Shape_double_limited_H79_extrapolation_assign_with_tokens/5,
	  ppl_BD_Shape_double_limited_CC76_extrapolation_assign_with_tokens/5,
	  ppl_BD_Shape_double_limited_BHMZ05_extrapolation_assign/3,
	  ppl_BD_Shape_double_limited_H79_extrapolation_assign/3,
	  ppl_BD_Shape_double_limited_CC76_extrapolation_assign/3,
	  ppl_BD_Shape_double_CC76_extrapolation_assign_with_tokens/4,
	  ppl_BD_Shape_double_CC76_extrapolation_assign/2,
	  ppl_BD_Shape_double_CC76_narrowing_assign/2,
	  ppl_BD_Shape_double_linear_partition/4,
	  ppl_delete_Octagonal_Shape_double/1,
	  ppl_new_Octagonal_Shape_double_from_space_dimension/3,
	  ppl_new_Octagonal_Shape_double_from_C_Polyhedron/2,
	  ppl_new_Octagonal_Shape_double_from_NNC_Polyhedron/2,
	  ppl_new_Octagonal_Shape_double_from_Grid/2,
	  ppl_new_Octagonal_Shape_double_from_Rational_Box/2,
	  ppl_new_Octagonal_Shape_double_from_BD_Shape_mpz_class/2,
	  ppl_new_Octagonal_Shape_double_from_BD_Shape_mpq_class/2,
	  ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpz_class/2,
	  ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpq_class/2,
	  ppl_new_Octagonal_Shape_double_from_Double_Box/2,
	  ppl_new_Octagonal_Shape_double_from_BD_Shape_double/2,
	  ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_double/2,
	  ppl_new_Octagonal_Shape_double_from_C_Polyhedron_with_complexity/3,
	  ppl_new_Octagonal_Shape_double_from_NNC_Polyhedron_with_complexity/3,
	  ppl_new_Octagonal_Shape_double_from_Grid_with_complexity/3,
	  ppl_new_Octagonal_Shape_double_from_Rational_Box_with_complexity/3,
	  ppl_new_Octagonal_Shape_double_from_BD_Shape_mpz_class_with_complexity/3,
	  ppl_new_Octagonal_Shape_double_from_BD_Shape_mpq_class_with_complexity/3,
	  ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpz_class_with_complexity/3,
	  ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpq_class_with_complexity/3,
	  ppl_new_Octagonal_Shape_double_from_Double_Box_with_complexity/3,
	  ppl_new_Octagonal_Shape_double_from_BD_Shape_double_with_complexity/3,
	  ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_double_with_complexity/3,
	  ppl_new_Octagonal_Shape_double_from_constraints/2,
	  ppl_new_Octagonal_Shape_double_from_congruences/2,
	  ppl_new_Octagonal_Shape_double_from_generators/2,
	  ppl_Octagonal_Shape_double_swap/2,
	  ppl_Octagonal_Shape_double_space_dimension/2,
	  ppl_Octagonal_Shape_double_affine_dimension/2,
	  ppl_Octagonal_Shape_double_relation_with_constraint/3,
	  ppl_Octagonal_Shape_double_relation_with_generator/3,
	  ppl_Octagonal_Shape_double_relation_with_congruence/3,
	  ppl_Octagonal_Shape_double_get_constraints/2,
	  ppl_Octagonal_Shape_double_get_congruences/2,
	  ppl_Octagonal_Shape_double_get_minimized_constraints/2,
	  ppl_Octagonal_Shape_double_get_minimized_congruences/2,
	  ppl_Octagonal_Shape_double_is_empty/1,
	  ppl_Octagonal_Shape_double_is_universe/1,
	  ppl_Octagonal_Shape_double_is_bounded/1,
	  ppl_Octagonal_Shape_double_contains_integer_point/1,
	  ppl_Octagonal_Shape_double_is_topologically_closed/1,
	  ppl_Octagonal_Shape_double_is_discrete/1,
	  ppl_Octagonal_Shape_double_topological_closure_assign/1,
	  ppl_Octagonal_Shape_double_bounds_from_above/2,
	  ppl_Octagonal_Shape_double_bounds_from_below/2,
	  ppl_Octagonal_Shape_double_maximize/5,
	  ppl_Octagonal_Shape_double_minimize/5,
	  ppl_Octagonal_Shape_double_maximize_with_point/6,
	  ppl_Octagonal_Shape_double_minimize_with_point/6,
	  ppl_Octagonal_Shape_double_contains_Octagonal_Shape_double/2,
	  ppl_Octagonal_Shape_double_strictly_contains_Octagonal_Shape_double/2,
	  ppl_Octagonal_Shape_double_is_disjoint_from_Octagonal_Shape_double/2,
	  ppl_Octagonal_Shape_double_equals_Octagonal_Shape_double/2,
	  ppl_Octagonal_Shape_double_OK/1,
	  ppl_Octagonal_Shape_double_add_constraint/2,
	  ppl_Octagonal_Shape_double_add_congruence/2,
	  ppl_Octagonal_Shape_double_add_constraints/2,
	  ppl_Octagonal_Shape_double_add_congruences/2,
	  ppl_Octagonal_Shape_double_refine_with_constraint/2,
	  ppl_Octagonal_Shape_double_refine_with_congruence/2,
	  ppl_Octagonal_Shape_double_refine_with_constraints/2,
	  ppl_Octagonal_Shape_double_refine_with_congruences/2,
	  ppl_Octagonal_Shape_double_intersection_assign/2,
	  ppl_Octagonal_Shape_double_upper_bound_assign/2,
	  ppl_Octagonal_Shape_double_difference_assign/2,
	  ppl_Octagonal_Shape_double_concatenate_assign/2,
	  ppl_Octagonal_Shape_double_time_elapse_assign/2,
	  ppl_Octagonal_Shape_double_upper_bound_assign_if_exact/2,
	  ppl_Octagonal_Shape_double_simplify_using_context_assign/3,
	  ppl_Octagonal_Shape_double_constrains/2,
	  ppl_Octagonal_Shape_double_unconstrain_space_dimension/2,
	  ppl_Octagonal_Shape_double_unconstrain_space_dimensions/2,
	  ppl_Octagonal_Shape_double_affine_image/4,
	  ppl_Octagonal_Shape_double_affine_preimage/4,
	  ppl_Octagonal_Shape_double_bounded_affine_image/5,
	  ppl_Octagonal_Shape_double_bounded_affine_preimage/5,
	  ppl_Octagonal_Shape_double_generalized_affine_image/5,
	  ppl_Octagonal_Shape_double_generalized_affine_preimage/5,
	  ppl_Octagonal_Shape_double_generalized_affine_image_lhs_rhs/4,
	  ppl_Octagonal_Shape_double_generalized_affine_preimage_lhs_rhs/4,
	  ppl_Octagonal_Shape_double_add_space_dimensions_and_embed/2,
	  ppl_Octagonal_Shape_double_add_space_dimensions_and_project/2,
	  ppl_Octagonal_Shape_double_remove_space_dimensions/2,
	  ppl_Octagonal_Shape_double_remove_higher_space_dimensions/2,
	  ppl_Octagonal_Shape_double_expand_space_dimension/3,
	  ppl_Octagonal_Shape_double_fold_space_dimensions/3,
	  ppl_Octagonal_Shape_double_map_space_dimensions/2,
	  ppl_Octagonal_Shape_double_ascii_dump/1,
	  ppl_Octagonal_Shape_double_external_memory_in_bytes/2,
	  ppl_Octagonal_Shape_double_total_memory_in_bytes/2,
	  ppl_Octagonal_Shape_double_BHMZ05_widening_assign_with_tokens/4,
	  ppl_Octagonal_Shape_double_BHMZ05_widening_assign/2,
	  ppl_Octagonal_Shape_double_widening_assign_with_tokens/4,
	  ppl_Octagonal_Shape_double_widening_assign/2,
	  ppl_Octagonal_Shape_double_limited_BHMZ05_extrapolation_assign_with_tokens/5,
	  ppl_Octagonal_Shape_double_limited_CC76_extrapolation_assign_with_tokens/5,
	  ppl_Octagonal_Shape_double_limited_BHMZ05_extrapolation_assign/3,
	  ppl_Octagonal_Shape_double_limited_CC76_extrapolation_assign/3,
	  ppl_Octagonal_Shape_double_CC76_extrapolation_assign_with_tokens/4,
	  ppl_Octagonal_Shape_double_CC76_extrapolation_assign/2,
	  ppl_Octagonal_Shape_double_CC76_narrowing_assign/2,
	  ppl_Octagonal_Shape_double_linear_partition/4
]).
:-use_package([
        assertions,
        basicmodes,
        regtypes,
        foreign_interface
]).


:- true pred ppl_version_major_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_version_major)).

ppl_version_major(Term1) :-
   ppl_version_major_2(Term1, 1).

:- true pred ppl_version_minor_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_version_minor)).

ppl_version_minor(Term1) :-
   ppl_version_minor_2(Term1, 1).

:- true pred ppl_version_revision_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_version_revision)).

ppl_version_revision(Term1) :-
   ppl_version_revision_2(Term1, 1).

:- true pred ppl_version_beta_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_version_beta)).

ppl_version_beta(Term1) :-
   ppl_version_beta_2(Term1, 1).

:- true pred ppl_version_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_version)).

ppl_version(Term1) :-
   ppl_version_2(Term1, 1).

:- true pred ppl_banner_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_banner)).

ppl_banner(Term1) :-
   ppl_banner_2(Term1, 1).

:- true pred ppl_max_space_dimension_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_max_space_dimension)).

ppl_max_space_dimension(Term1) :-
   ppl_max_space_dimension_2(Term1, 1).

:- true pred ppl_Coefficient_is_bounded_2(go(Success))
          ::  int
  + (returns(Success), foreign(ppl_Coefficient_is_bounded)).

ppl_Coefficient_is_bounded :-
   ppl_Coefficient_is_bounded_2(1).

:- true pred ppl_Coefficient_max_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Coefficient_max)).

ppl_Coefficient_max(Term1) :-
   ppl_Coefficient_max_2(Term1, 1).

:- true pred ppl_Coefficient_min_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Coefficient_min)).

ppl_Coefficient_min(Term1) :-
   ppl_Coefficient_min_2(Term1, 1).

:- true pred ppl_initialize +  foreign.

:- true pred ppl_finalize +  foreign.

:- true pred ppl_set_rounding_for_PPL +  foreign.

:- true pred ppl_restore_pre_PPL_rounding +  foreign.

:- true pred ppl_set_timeout_exception_atom(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_timeout_exception_atom_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_timeout_exception_atom)).

ppl_timeout_exception_atom(Term1) :-
   ppl_timeout_exception_atom_2(Term1, 1).

:- true pred ppl_set_timeout(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_reset_timeout +  foreign.

:- true pred ppl_new_MIP_Problem_from_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_MIP_Problem_from_space_dimension)).

ppl_new_MIP_Problem_from_space_dimension(Term1, Term2) :-
   ppl_new_MIP_Problem_from_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_new_MIP_Problem_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_MIP_Problem)).

ppl_new_MIP_Problem(Term1, Term2, Term3, Term4, Term5) :-
   ppl_new_MIP_Problem_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_new_MIP_Problem_from_MIP_Problem_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_MIP_Problem_from_MIP_Problem)).

ppl_new_MIP_Problem_from_MIP_Problem(Term1, Term2) :-
   ppl_new_MIP_Problem_from_MIP_Problem_2(Term1, Term2, 1).

:- true pred ppl_MIP_Problem_swap(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_delete_MIP_Problem(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_MIP_Problem_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_space_dimension)).

ppl_MIP_Problem_space_dimension(Term1, Term2) :-
   ppl_MIP_Problem_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_MIP_Problem_integer_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_integer_space_dimensions)).

ppl_MIP_Problem_integer_space_dimensions(Term1, Term2) :-
   ppl_MIP_Problem_integer_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_MIP_Problem_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_constraints)).

ppl_MIP_Problem_constraints(Term1, Term2) :-
   ppl_MIP_Problem_constraints_2(Term1, Term2, 1).

:- true pred ppl_MIP_Problem_objective_function_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_objective_function)).

ppl_MIP_Problem_objective_function(Term1, Term2) :-
   ppl_MIP_Problem_objective_function_2(Term1, Term2, 1).

:- true pred ppl_MIP_Problem_optimization_mode_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_optimization_mode)).

ppl_MIP_Problem_optimization_mode(Term1, Term2) :-
   ppl_MIP_Problem_optimization_mode_2(Term1, Term2, 1).

:- true pred ppl_MIP_Problem_clear_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_clear)).

ppl_MIP_Problem_clear(Term1) :-
   ppl_MIP_Problem_clear_2(Term1, 1).

:- true pred ppl_MIP_Problem_add_space_dimensions_and_embed_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_add_space_dimensions_and_embed)).

ppl_MIP_Problem_add_space_dimensions_and_embed(Term1, Term2) :-
   ppl_MIP_Problem_add_space_dimensions_and_embed_2(Term1, Term2, 1).

:- true pred ppl_MIP_Problem_add_to_integer_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_add_to_integer_space_dimensions)).

ppl_MIP_Problem_add_to_integer_space_dimensions(Term1, Term2) :-
   ppl_MIP_Problem_add_to_integer_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_MIP_Problem_add_constraint_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_add_constraint)).

ppl_MIP_Problem_add_constraint(Term1, Term2) :-
   ppl_MIP_Problem_add_constraint_2(Term1, Term2, 1).

:- true pred ppl_MIP_Problem_add_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_add_constraints)).

ppl_MIP_Problem_add_constraints(Term1, Term2) :-
   ppl_MIP_Problem_add_constraints_2(Term1, Term2, 1).

:- true pred ppl_MIP_Problem_set_objective_function_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_set_objective_function)).

ppl_MIP_Problem_set_objective_function(Term1, Term2) :-
   ppl_MIP_Problem_set_objective_function_2(Term1, Term2, 1).

:- true pred ppl_MIP_Problem_set_optimization_mode_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_set_optimization_mode)).

ppl_MIP_Problem_set_optimization_mode(Term1, Term2) :-
   ppl_MIP_Problem_set_optimization_mode_2(Term1, Term2, 1).

:- true pred ppl_MIP_Problem_set_control_parameter(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_MIP_Problem_get_control_parameter_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_get_control_parameter)).

ppl_MIP_Problem_get_control_parameter(Term1, Term2, Term3) :-
   ppl_MIP_Problem_get_control_parameter_2(Term1, Term2, Term3, 1).

:- true pred ppl_MIP_Problem_is_satisfiable_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_is_satisfiable)).

ppl_MIP_Problem_is_satisfiable(Term1) :-
   ppl_MIP_Problem_is_satisfiable_2(Term1, 1).

:- true pred ppl_MIP_Problem_solve_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_solve)).

ppl_MIP_Problem_solve(Term1, Term2) :-
   ppl_MIP_Problem_solve_2(Term1, Term2, 1).

:- true pred ppl_MIP_Problem_feasible_point_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_feasible_point)).

ppl_MIP_Problem_feasible_point(Term1, Term2) :-
   ppl_MIP_Problem_feasible_point_2(Term1, Term2, 1).

:- true pred ppl_MIP_Problem_optimizing_point_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_optimizing_point)).

ppl_MIP_Problem_optimizing_point(Term1, Term2) :-
   ppl_MIP_Problem_optimizing_point_2(Term1, Term2, 1).

:- true pred ppl_MIP_Problem_optimal_value_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_optimal_value)).

ppl_MIP_Problem_optimal_value(Term1, Term2, Term3) :-
   ppl_MIP_Problem_optimal_value_2(Term1, Term2, Term3, 1).

:- true pred ppl_MIP_Problem_evaluate_objective_function_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_evaluate_objective_function)).

ppl_MIP_Problem_evaluate_objective_function(Term1, Term2, Term3, Term4) :-
   ppl_MIP_Problem_evaluate_objective_function_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_MIP_Problem_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_MIP_Problem_OK)).

ppl_MIP_Problem_OK(Term1) :-
   ppl_MIP_Problem_OK_2(Term1, 1).

:- true pred ppl_delete_Polyhedron(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_new_C_Polyhedron_from_space_dimension_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_space_dimension)).

ppl_new_C_Polyhedron_from_space_dimension(Term1, Term2, Term3) :-
   ppl_new_C_Polyhedron_from_space_dimension_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_NNC_Polyhedron_from_space_dimension_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_space_dimension)).

ppl_new_NNC_Polyhedron_from_space_dimension(Term1, Term2, Term3) :-
   ppl_new_NNC_Polyhedron_from_space_dimension_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_C_Polyhedron_from_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_C_Polyhedron)).

ppl_new_C_Polyhedron_from_C_Polyhedron(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_NNC_Polyhedron)).

ppl_new_C_Polyhedron_from_NNC_Polyhedron(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_Grid)).

ppl_new_C_Polyhedron_from_Grid(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_Grid_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_Rational_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_Rational_Box)).

ppl_new_C_Polyhedron_from_Rational_Box(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_Rational_Box_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_BD_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_BD_Shape_mpz_class)).

ppl_new_C_Polyhedron_from_BD_Shape_mpz_class(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_BD_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_BD_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_BD_Shape_mpq_class)).

ppl_new_C_Polyhedron_from_BD_Shape_mpq_class(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_BD_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_Octagonal_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_Octagonal_Shape_mpz_class)).

ppl_new_C_Polyhedron_from_Octagonal_Shape_mpz_class(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_Octagonal_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_Octagonal_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_Octagonal_Shape_mpq_class)).

ppl_new_C_Polyhedron_from_Octagonal_Shape_mpq_class(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_Octagonal_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_Double_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_Double_Box)).

ppl_new_C_Polyhedron_from_Double_Box(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_Double_Box_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_BD_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_BD_Shape_double)).

ppl_new_C_Polyhedron_from_BD_Shape_double(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_BD_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_Octagonal_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_Octagonal_Shape_double)).

ppl_new_C_Polyhedron_from_Octagonal_Shape_double(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_Octagonal_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_C_Polyhedron)).

ppl_new_NNC_Polyhedron_from_C_Polyhedron(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_NNC_Polyhedron)).

ppl_new_NNC_Polyhedron_from_NNC_Polyhedron(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_Grid)).

ppl_new_NNC_Polyhedron_from_Grid(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_Grid_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_Rational_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_Rational_Box)).

ppl_new_NNC_Polyhedron_from_Rational_Box(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_Rational_Box_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_BD_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_BD_Shape_mpz_class)).

ppl_new_NNC_Polyhedron_from_BD_Shape_mpz_class(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_BD_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_BD_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_BD_Shape_mpq_class)).

ppl_new_NNC_Polyhedron_from_BD_Shape_mpq_class(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_BD_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class)).

ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class)).

ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_Double_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_Double_Box)).

ppl_new_NNC_Polyhedron_from_Double_Box(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_Double_Box_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_BD_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_BD_Shape_double)).

ppl_new_NNC_Polyhedron_from_BD_Shape_double(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_BD_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_Octagonal_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_Octagonal_Shape_double)).

ppl_new_NNC_Polyhedron_from_Octagonal_Shape_double(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_Octagonal_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_C_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_C_Polyhedron_with_complexity)).

ppl_new_C_Polyhedron_from_C_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_C_Polyhedron_from_C_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_C_Polyhedron_from_NNC_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_NNC_Polyhedron_with_complexity)).

ppl_new_C_Polyhedron_from_NNC_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_C_Polyhedron_from_NNC_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_C_Polyhedron_from_Grid_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_Grid_with_complexity)).

ppl_new_C_Polyhedron_from_Grid_with_complexity(Term1, Term2, Term3) :-
   ppl_new_C_Polyhedron_from_Grid_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_C_Polyhedron_from_Rational_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_Rational_Box_with_complexity)).

ppl_new_C_Polyhedron_from_Rational_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_C_Polyhedron_from_Rational_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_C_Polyhedron_from_BD_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_BD_Shape_mpz_class_with_complexity)).

ppl_new_C_Polyhedron_from_BD_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_C_Polyhedron_from_BD_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_C_Polyhedron_from_BD_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_BD_Shape_mpq_class_with_complexity)).

ppl_new_C_Polyhedron_from_BD_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_C_Polyhedron_from_BD_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_C_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity)).

ppl_new_C_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_C_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_C_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity)).

ppl_new_C_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_C_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_C_Polyhedron_from_Double_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_Double_Box_with_complexity)).

ppl_new_C_Polyhedron_from_Double_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_C_Polyhedron_from_Double_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_C_Polyhedron_from_BD_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_BD_Shape_double_with_complexity)).

ppl_new_C_Polyhedron_from_BD_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_C_Polyhedron_from_BD_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_C_Polyhedron_from_Octagonal_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_Octagonal_Shape_double_with_complexity)).

ppl_new_C_Polyhedron_from_Octagonal_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_C_Polyhedron_from_Octagonal_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_NNC_Polyhedron_from_C_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_C_Polyhedron_with_complexity)).

ppl_new_NNC_Polyhedron_from_C_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_NNC_Polyhedron_from_C_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity)).

ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_NNC_Polyhedron_from_Grid_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_Grid_with_complexity)).

ppl_new_NNC_Polyhedron_from_Grid_with_complexity(Term1, Term2, Term3) :-
   ppl_new_NNC_Polyhedron_from_Grid_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_NNC_Polyhedron_from_Rational_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_Rational_Box_with_complexity)).

ppl_new_NNC_Polyhedron_from_Rational_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_NNC_Polyhedron_from_Rational_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_NNC_Polyhedron_from_BD_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_BD_Shape_mpz_class_with_complexity)).

ppl_new_NNC_Polyhedron_from_BD_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_NNC_Polyhedron_from_BD_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_NNC_Polyhedron_from_BD_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_BD_Shape_mpq_class_with_complexity)).

ppl_new_NNC_Polyhedron_from_BD_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_NNC_Polyhedron_from_BD_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity)).

ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity)).

ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_NNC_Polyhedron_from_Double_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_Double_Box_with_complexity)).

ppl_new_NNC_Polyhedron_from_Double_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_NNC_Polyhedron_from_Double_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_NNC_Polyhedron_from_BD_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_BD_Shape_double_with_complexity)).

ppl_new_NNC_Polyhedron_from_BD_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_NNC_Polyhedron_from_BD_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_NNC_Polyhedron_from_Octagonal_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_Octagonal_Shape_double_with_complexity)).

ppl_new_NNC_Polyhedron_from_Octagonal_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_NNC_Polyhedron_from_Octagonal_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_C_Polyhedron_from_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_constraints)).

ppl_new_C_Polyhedron_from_constraints(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_constraints_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_constraints)).

ppl_new_NNC_Polyhedron_from_constraints(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_constraints_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_congruences)).

ppl_new_C_Polyhedron_from_congruences(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_congruences_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_congruences)).

ppl_new_NNC_Polyhedron_from_congruences(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_congruences_2(Term1, Term2, 1).

:- true pred ppl_new_C_Polyhedron_from_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_generators)).

ppl_new_C_Polyhedron_from_generators(Term1, Term2) :-
   ppl_new_C_Polyhedron_from_generators_2(Term1, Term2, 1).

:- true pred ppl_new_NNC_Polyhedron_from_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_generators)).

ppl_new_NNC_Polyhedron_from_generators(Term1, Term2) :-
   ppl_new_NNC_Polyhedron_from_generators_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_swap(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_space_dimension)).

ppl_Polyhedron_space_dimension(Term1, Term2) :-
   ppl_Polyhedron_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_affine_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_affine_dimension)).

ppl_Polyhedron_affine_dimension(Term1, Term2) :-
   ppl_Polyhedron_affine_dimension_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_relation_with_constraint_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_relation_with_constraint)).

ppl_Polyhedron_relation_with_constraint(Term1, Term2, Term3) :-
   ppl_Polyhedron_relation_with_constraint_2(Term1, Term2, Term3, 1).

:- true pred ppl_Polyhedron_relation_with_generator_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_relation_with_generator)).

ppl_Polyhedron_relation_with_generator(Term1, Term2, Term3) :-
   ppl_Polyhedron_relation_with_generator_2(Term1, Term2, Term3, 1).

:- true pred ppl_Polyhedron_relation_with_congruence_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_relation_with_congruence)).

ppl_Polyhedron_relation_with_congruence(Term1, Term2, Term3) :-
   ppl_Polyhedron_relation_with_congruence_2(Term1, Term2, Term3, 1).

:- true pred ppl_Polyhedron_get_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_constraints)).

ppl_Polyhedron_get_constraints(Term1, Term2) :-
   ppl_Polyhedron_get_constraints_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_get_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_congruences)).

ppl_Polyhedron_get_congruences(Term1, Term2) :-
   ppl_Polyhedron_get_congruences_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_get_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_generators)).

ppl_Polyhedron_get_generators(Term1, Term2) :-
   ppl_Polyhedron_get_generators_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_get_minimized_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_minimized_constraints)).

ppl_Polyhedron_get_minimized_constraints(Term1, Term2) :-
   ppl_Polyhedron_get_minimized_constraints_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_get_minimized_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_minimized_congruences)).

ppl_Polyhedron_get_minimized_congruences(Term1, Term2) :-
   ppl_Polyhedron_get_minimized_congruences_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_get_minimized_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_minimized_generators)).

ppl_Polyhedron_get_minimized_generators(Term1, Term2) :-
   ppl_Polyhedron_get_minimized_generators_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_is_empty_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_empty)).

ppl_Polyhedron_is_empty(Term1) :-
   ppl_Polyhedron_is_empty_2(Term1, 1).

:- true pred ppl_Polyhedron_is_universe_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_universe)).

ppl_Polyhedron_is_universe(Term1) :-
   ppl_Polyhedron_is_universe_2(Term1, 1).

:- true pred ppl_Polyhedron_is_bounded_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_bounded)).

ppl_Polyhedron_is_bounded(Term1) :-
   ppl_Polyhedron_is_bounded_2(Term1, 1).

:- true pred ppl_Polyhedron_contains_integer_point_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_contains_integer_point)).

ppl_Polyhedron_contains_integer_point(Term1) :-
   ppl_Polyhedron_contains_integer_point_2(Term1, 1).

:- true pred ppl_Polyhedron_is_topologically_closed_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_topologically_closed)).

ppl_Polyhedron_is_topologically_closed(Term1) :-
   ppl_Polyhedron_is_topologically_closed_2(Term1, 1).

:- true pred ppl_Polyhedron_is_discrete_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_discrete)).

ppl_Polyhedron_is_discrete(Term1) :-
   ppl_Polyhedron_is_discrete_2(Term1, 1).

:- true pred ppl_Polyhedron_topological_closure_assign(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_Polyhedron_bounds_from_above_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_bounds_from_above)).

ppl_Polyhedron_bounds_from_above(Term1, Term2) :-
   ppl_Polyhedron_bounds_from_above_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_bounds_from_below_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_bounds_from_below)).

ppl_Polyhedron_bounds_from_below(Term1, Term2) :-
   ppl_Polyhedron_bounds_from_below_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_maximize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_maximize)).

ppl_Polyhedron_maximize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Polyhedron_maximize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Polyhedron_minimize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_minimize)).

ppl_Polyhedron_minimize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Polyhedron_minimize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Polyhedron_maximize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_maximize_with_point)).

ppl_Polyhedron_maximize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Polyhedron_maximize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Polyhedron_minimize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_minimize_with_point)).

ppl_Polyhedron_minimize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Polyhedron_minimize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Polyhedron_contains_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_contains_Polyhedron)).

ppl_Polyhedron_contains_Polyhedron(Term1, Term2) :-
   ppl_Polyhedron_contains_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_strictly_contains_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_strictly_contains_Polyhedron)).

ppl_Polyhedron_strictly_contains_Polyhedron(Term1, Term2) :-
   ppl_Polyhedron_strictly_contains_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_is_disjoint_from_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_disjoint_from_Polyhedron)).

ppl_Polyhedron_is_disjoint_from_Polyhedron(Term1, Term2) :-
   ppl_Polyhedron_is_disjoint_from_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_equals_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_equals_Polyhedron)).

ppl_Polyhedron_equals_Polyhedron(Term1, Term2) :-
   ppl_Polyhedron_equals_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_OK)).

ppl_Polyhedron_OK(Term1) :-
   ppl_Polyhedron_OK_2(Term1, 1).

:- true pred ppl_Polyhedron_add_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_add_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_add_generator(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_add_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_add_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_add_generators(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_refine_with_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_refine_with_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_refine_with_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_refine_with_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_intersection_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_upper_bound_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_difference_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_concatenate_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_time_elapse_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_poly_hull_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_poly_difference_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_upper_bound_assign_if_exact_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_upper_bound_assign_if_exact)).

ppl_Polyhedron_upper_bound_assign_if_exact(Term1, Term2) :-
   ppl_Polyhedron_upper_bound_assign_if_exact_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_poly_hull_assign_if_exact_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_poly_hull_assign_if_exact)).

ppl_Polyhedron_poly_hull_assign_if_exact(Term1, Term2) :-
   ppl_Polyhedron_poly_hull_assign_if_exact_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_simplify_using_context_assign_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_simplify_using_context_assign)).

ppl_Polyhedron_simplify_using_context_assign(Term1, Term2, Term3) :-
   ppl_Polyhedron_simplify_using_context_assign_2(Term1, Term2, Term3, 1).

:- true pred ppl_Polyhedron_constrains_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_constrains)).

ppl_Polyhedron_constrains(Term1, Term2) :-
   ppl_Polyhedron_constrains_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_unconstrain_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_unconstrain_space_dimension)).

ppl_Polyhedron_unconstrain_space_dimension(Term1, Term2) :-
   ppl_Polyhedron_unconstrain_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_unconstrain_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_unconstrain_space_dimensions)).

ppl_Polyhedron_unconstrain_space_dimensions(Term1, Term2) :-
   ppl_Polyhedron_unconstrain_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_affine_image(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_bounded_affine_image(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_bounded_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_generalized_affine_image_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_generalized_affine_image)).

ppl_Polyhedron_generalized_affine_image(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Polyhedron_generalized_affine_image_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Polyhedron_generalized_affine_preimage_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_generalized_affine_preimage)).

ppl_Polyhedron_generalized_affine_preimage(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Polyhedron_generalized_affine_preimage_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Polyhedron_generalized_affine_image_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_generalized_affine_image_lhs_rhs)).

ppl_Polyhedron_generalized_affine_image_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Polyhedron_generalized_affine_image_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Polyhedron_generalized_affine_preimage_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_generalized_affine_preimage_lhs_rhs)).

ppl_Polyhedron_generalized_affine_preimage_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Polyhedron_generalized_affine_preimage_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Polyhedron_add_space_dimensions_and_embed(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_add_space_dimensions_and_project(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_remove_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_remove_space_dimensions)).

ppl_Polyhedron_remove_space_dimensions(Term1, Term2) :-
   ppl_Polyhedron_remove_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_remove_higher_space_dimensions(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_expand_space_dimension(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_fold_space_dimensions_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_fold_space_dimensions)).

ppl_Polyhedron_fold_space_dimensions(Term1, Term2, Term3) :-
   ppl_Polyhedron_fold_space_dimensions_2(Term1, Term2, Term3, 1).

:- true pred ppl_Polyhedron_map_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_map_space_dimensions)).

ppl_Polyhedron_map_space_dimensions(Term1, Term2) :-
   ppl_Polyhedron_map_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_ascii_dump_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_ascii_dump)).

ppl_Polyhedron_ascii_dump(Term1) :-
   ppl_Polyhedron_ascii_dump_2(Term1, 1).

:- true pred ppl_Polyhedron_external_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_external_memory_in_bytes)).

ppl_Polyhedron_external_memory_in_bytes(Term1, Term2) :-
   ppl_Polyhedron_external_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_total_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_total_memory_in_bytes)).

ppl_Polyhedron_total_memory_in_bytes(Term1, Term2) :-
   ppl_Polyhedron_total_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_BHRZ03_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_BHRZ03_widening_assign_with_tokens)).

ppl_Polyhedron_BHRZ03_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Polyhedron_BHRZ03_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Polyhedron_H79_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_H79_widening_assign_with_tokens)).

ppl_Polyhedron_H79_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Polyhedron_H79_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Polyhedron_BHRZ03_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_H79_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_widening_assign_with_tokens)).

ppl_Polyhedron_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Polyhedron_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Polyhedron_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens)).

ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens)).

ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens)).

ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens)).

ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Polyhedron_limited_BHRZ03_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_limited_H79_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_bounded_H79_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Polyhedron_linear_partition_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_linear_partition)).

ppl_Polyhedron_linear_partition(Term1, Term2, Term3, Term4) :-
   ppl_Polyhedron_linear_partition_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Polyhedron_intersection_assign_and_minimize_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_intersection_assign_and_minimize)).

ppl_Polyhedron_intersection_assign_and_minimize(Term1, Term2) :-
   ppl_Polyhedron_intersection_assign_and_minimize_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_poly_hull_assign_and_minimize_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_poly_hull_assign_and_minimize)).

ppl_Polyhedron_poly_hull_assign_and_minimize(Term1, Term2) :-
   ppl_Polyhedron_poly_hull_assign_and_minimize_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_add_constraint_and_minimize_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_constraint_and_minimize)).

ppl_Polyhedron_add_constraint_and_minimize(Term1, Term2) :-
   ppl_Polyhedron_add_constraint_and_minimize_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_add_congruence_and_minimize_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_congruence_and_minimize)).

ppl_Polyhedron_add_congruence_and_minimize(Term1, Term2) :-
   ppl_Polyhedron_add_congruence_and_minimize_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_add_generator_and_minimize_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_generator_and_minimize)).

ppl_Polyhedron_add_generator_and_minimize(Term1, Term2) :-
   ppl_Polyhedron_add_generator_and_minimize_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_add_constraints_and_minimize_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_constraints_and_minimize)).

ppl_Polyhedron_add_constraints_and_minimize(Term1, Term2) :-
   ppl_Polyhedron_add_constraints_and_minimize_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_add_congruences_and_minimize_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_congruences_and_minimize)).

ppl_Polyhedron_add_congruences_and_minimize(Term1, Term2) :-
   ppl_Polyhedron_add_congruences_and_minimize_2(Term1, Term2, 1).

:- true pred ppl_Polyhedron_add_generators_and_minimize_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_generators_and_minimize)).

ppl_Polyhedron_add_generators_and_minimize(Term1, Term2) :-
   ppl_Polyhedron_add_generators_and_minimize_2(Term1, Term2, 1).


:- true pred ppl_delete_Grid(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_new_Grid_from_space_dimension_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_space_dimension)).

ppl_new_Grid_from_space_dimension(Term1, Term2, Term3) :-
   ppl_new_Grid_from_space_dimension_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Grid_from_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_C_Polyhedron)).

ppl_new_Grid_from_C_Polyhedron(Term1, Term2) :-
   ppl_new_Grid_from_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Grid_from_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_NNC_Polyhedron)).

ppl_new_Grid_from_NNC_Polyhedron(Term1, Term2) :-
   ppl_new_Grid_from_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Grid_from_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_Grid)).

ppl_new_Grid_from_Grid(Term1, Term2) :-
   ppl_new_Grid_from_Grid_2(Term1, Term2, 1).

:- true pred ppl_new_Grid_from_Rational_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_Rational_Box)).

ppl_new_Grid_from_Rational_Box(Term1, Term2) :-
   ppl_new_Grid_from_Rational_Box_2(Term1, Term2, 1).

:- true pred ppl_new_Grid_from_BD_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_BD_Shape_mpz_class)).

ppl_new_Grid_from_BD_Shape_mpz_class(Term1, Term2) :-
   ppl_new_Grid_from_BD_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_Grid_from_BD_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_BD_Shape_mpq_class)).

ppl_new_Grid_from_BD_Shape_mpq_class(Term1, Term2) :-
   ppl_new_Grid_from_BD_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_Grid_from_Octagonal_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_Octagonal_Shape_mpz_class)).

ppl_new_Grid_from_Octagonal_Shape_mpz_class(Term1, Term2) :-
   ppl_new_Grid_from_Octagonal_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_Grid_from_Octagonal_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_Octagonal_Shape_mpq_class)).

ppl_new_Grid_from_Octagonal_Shape_mpq_class(Term1, Term2) :-
   ppl_new_Grid_from_Octagonal_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_Grid_from_Double_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_Double_Box)).

ppl_new_Grid_from_Double_Box(Term1, Term2) :-
   ppl_new_Grid_from_Double_Box_2(Term1, Term2, 1).

:- true pred ppl_new_Grid_from_BD_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_BD_Shape_double)).

ppl_new_Grid_from_BD_Shape_double(Term1, Term2) :-
   ppl_new_Grid_from_BD_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_Grid_from_Octagonal_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_Octagonal_Shape_double)).

ppl_new_Grid_from_Octagonal_Shape_double(Term1, Term2) :-
   ppl_new_Grid_from_Octagonal_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_Grid_from_C_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_C_Polyhedron_with_complexity)).

ppl_new_Grid_from_C_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Grid_from_C_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Grid_from_NNC_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_NNC_Polyhedron_with_complexity)).

ppl_new_Grid_from_NNC_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Grid_from_NNC_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Grid_from_Grid_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_Grid_with_complexity)).

ppl_new_Grid_from_Grid_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Grid_from_Grid_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Grid_from_Rational_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_Rational_Box_with_complexity)).

ppl_new_Grid_from_Rational_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Grid_from_Rational_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Grid_from_BD_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_BD_Shape_mpz_class_with_complexity)).

ppl_new_Grid_from_BD_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Grid_from_BD_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Grid_from_BD_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_BD_Shape_mpq_class_with_complexity)).

ppl_new_Grid_from_BD_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Grid_from_BD_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Grid_from_Octagonal_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_Octagonal_Shape_mpz_class_with_complexity)).

ppl_new_Grid_from_Octagonal_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Grid_from_Octagonal_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Grid_from_Octagonal_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_Octagonal_Shape_mpq_class_with_complexity)).

ppl_new_Grid_from_Octagonal_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Grid_from_Octagonal_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Grid_from_Double_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_Double_Box_with_complexity)).

ppl_new_Grid_from_Double_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Grid_from_Double_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Grid_from_BD_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_BD_Shape_double_with_complexity)).

ppl_new_Grid_from_BD_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Grid_from_BD_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Grid_from_Octagonal_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_Octagonal_Shape_double_with_complexity)).

ppl_new_Grid_from_Octagonal_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Grid_from_Octagonal_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Grid_from_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_constraints)).

ppl_new_Grid_from_constraints(Term1, Term2) :-
   ppl_new_Grid_from_constraints_2(Term1, Term2, 1).

:- true pred ppl_new_Grid_from_grid_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_grid_generators)).

ppl_new_Grid_from_grid_generators(Term1, Term2) :-
   ppl_new_Grid_from_grid_generators_2(Term1, Term2, 1).

:- true pred ppl_new_Grid_from_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Grid_from_congruences)).

ppl_new_Grid_from_congruences(Term1, Term2) :-
   ppl_new_Grid_from_congruences_2(Term1, Term2, 1).

:- true pred ppl_Grid_swap(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_space_dimension)).

ppl_Grid_space_dimension(Term1, Term2) :-
   ppl_Grid_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Grid_affine_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_affine_dimension)).

ppl_Grid_affine_dimension(Term1, Term2) :-
   ppl_Grid_affine_dimension_2(Term1, Term2, 1).

:- true pred ppl_Grid_relation_with_constraint_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_relation_with_constraint)).

ppl_Grid_relation_with_constraint(Term1, Term2, Term3) :-
   ppl_Grid_relation_with_constraint_2(Term1, Term2, Term3, 1).

:- true pred ppl_Grid_relation_with_generator_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_relation_with_generator)).

ppl_Grid_relation_with_generator(Term1, Term2, Term3) :-
   ppl_Grid_relation_with_generator_2(Term1, Term2, Term3, 1).

:- true pred ppl_Grid_relation_with_congruence_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_relation_with_congruence)).

ppl_Grid_relation_with_congruence(Term1, Term2, Term3) :-
   ppl_Grid_relation_with_congruence_2(Term1, Term2, Term3, 1).

:- true pred ppl_Grid_relation_with_grid_generator_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_relation_with_grid_generator)).

ppl_Grid_relation_with_grid_generator(Term1, Term2, Term3) :-
   ppl_Grid_relation_with_grid_generator_2(Term1, Term2, Term3, 1).

:- true pred ppl_Grid_get_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_get_constraints)).

ppl_Grid_get_constraints(Term1, Term2) :-
   ppl_Grid_get_constraints_2(Term1, Term2, 1).

:- true pred ppl_Grid_get_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_get_congruences)).

ppl_Grid_get_congruences(Term1, Term2) :-
   ppl_Grid_get_congruences_2(Term1, Term2, 1).

:- true pred ppl_Grid_get_grid_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_get_grid_generators)).

ppl_Grid_get_grid_generators(Term1, Term2) :-
   ppl_Grid_get_grid_generators_2(Term1, Term2, 1).

:- true pred ppl_Grid_get_minimized_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_get_minimized_constraints)).

ppl_Grid_get_minimized_constraints(Term1, Term2) :-
   ppl_Grid_get_minimized_constraints_2(Term1, Term2, 1).

:- true pred ppl_Grid_get_minimized_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_get_minimized_congruences)).

ppl_Grid_get_minimized_congruences(Term1, Term2) :-
   ppl_Grid_get_minimized_congruences_2(Term1, Term2, 1).

:- true pred ppl_Grid_get_minimized_grid_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_get_minimized_grid_generators)).

ppl_Grid_get_minimized_grid_generators(Term1, Term2) :-
   ppl_Grid_get_minimized_grid_generators_2(Term1, Term2, 1).

:- true pred ppl_Grid_is_empty_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Grid_is_empty)).

ppl_Grid_is_empty(Term1) :-
   ppl_Grid_is_empty_2(Term1, 1).

:- true pred ppl_Grid_is_universe_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Grid_is_universe)).

ppl_Grid_is_universe(Term1) :-
   ppl_Grid_is_universe_2(Term1, 1).

:- true pred ppl_Grid_is_bounded_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Grid_is_bounded)).

ppl_Grid_is_bounded(Term1) :-
   ppl_Grid_is_bounded_2(Term1, 1).

:- true pred ppl_Grid_contains_integer_point_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Grid_contains_integer_point)).

ppl_Grid_contains_integer_point(Term1) :-
   ppl_Grid_contains_integer_point_2(Term1, 1).

:- true pred ppl_Grid_is_topologically_closed_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Grid_is_topologically_closed)).

ppl_Grid_is_topologically_closed(Term1) :-
   ppl_Grid_is_topologically_closed_2(Term1, 1).

:- true pred ppl_Grid_is_discrete_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Grid_is_discrete)).

ppl_Grid_is_discrete(Term1) :-
   ppl_Grid_is_discrete_2(Term1, 1).

:- true pred ppl_Grid_topological_closure_assign(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_Grid_bounds_from_above_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_bounds_from_above)).

ppl_Grid_bounds_from_above(Term1, Term2) :-
   ppl_Grid_bounds_from_above_2(Term1, Term2, 1).

:- true pred ppl_Grid_bounds_from_below_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_bounds_from_below)).

ppl_Grid_bounds_from_below(Term1, Term2) :-
   ppl_Grid_bounds_from_below_2(Term1, Term2, 1).

:- true pred ppl_Grid_maximize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_maximize)).

ppl_Grid_maximize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Grid_maximize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Grid_minimize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_minimize)).

ppl_Grid_minimize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Grid_minimize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Grid_maximize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_maximize_with_point)).

ppl_Grid_maximize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Grid_maximize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Grid_minimize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_minimize_with_point)).

ppl_Grid_minimize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Grid_minimize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Grid_contains_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_contains_Grid)).

ppl_Grid_contains_Grid(Term1, Term2) :-
   ppl_Grid_contains_Grid_2(Term1, Term2, 1).

:- true pred ppl_Grid_strictly_contains_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_strictly_contains_Grid)).

ppl_Grid_strictly_contains_Grid(Term1, Term2) :-
   ppl_Grid_strictly_contains_Grid_2(Term1, Term2, 1).

:- true pred ppl_Grid_is_disjoint_from_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_is_disjoint_from_Grid)).

ppl_Grid_is_disjoint_from_Grid(Term1, Term2) :-
   ppl_Grid_is_disjoint_from_Grid_2(Term1, Term2, 1).

:- true pred ppl_Grid_equals_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_equals_Grid)).

ppl_Grid_equals_Grid(Term1, Term2) :-
   ppl_Grid_equals_Grid_2(Term1, Term2, 1).

:- true pred ppl_Grid_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Grid_OK)).

ppl_Grid_OK(Term1) :-
   ppl_Grid_OK_2(Term1, 1).

:- true pred ppl_Grid_add_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_add_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_add_grid_generator(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_add_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_add_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_add_grid_generators(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_refine_with_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_refine_with_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_refine_with_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_refine_with_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_intersection_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_upper_bound_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_difference_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_concatenate_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_time_elapse_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_upper_bound_assign_if_exact_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_upper_bound_assign_if_exact)).

ppl_Grid_upper_bound_assign_if_exact(Term1, Term2) :-
   ppl_Grid_upper_bound_assign_if_exact_2(Term1, Term2, 1).

:- true pred ppl_Grid_simplify_using_context_assign_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_simplify_using_context_assign)).

ppl_Grid_simplify_using_context_assign(Term1, Term2, Term3) :-
   ppl_Grid_simplify_using_context_assign_2(Term1, Term2, Term3, 1).

:- true pred ppl_Grid_constrains_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_constrains)).

ppl_Grid_constrains(Term1, Term2) :-
   ppl_Grid_constrains_2(Term1, Term2, 1).

:- true pred ppl_Grid_unconstrain_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_unconstrain_space_dimension)).

ppl_Grid_unconstrain_space_dimension(Term1, Term2) :-
   ppl_Grid_unconstrain_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Grid_unconstrain_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_unconstrain_space_dimensions)).

ppl_Grid_unconstrain_space_dimensions(Term1, Term2) :-
   ppl_Grid_unconstrain_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Grid_affine_image(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Grid_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Grid_bounded_affine_image(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Grid_bounded_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Grid_generalized_affine_image_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_generalized_affine_image)).

ppl_Grid_generalized_affine_image(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Grid_generalized_affine_image_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Grid_generalized_affine_preimage_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_generalized_affine_preimage)).

ppl_Grid_generalized_affine_preimage(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Grid_generalized_affine_preimage_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Grid_generalized_affine_image_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_generalized_affine_image_lhs_rhs)).

ppl_Grid_generalized_affine_image_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Grid_generalized_affine_image_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Grid_generalized_affine_preimage_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_generalized_affine_preimage_lhs_rhs)).

ppl_Grid_generalized_affine_preimage_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Grid_generalized_affine_preimage_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Grid_generalized_affine_image_with_congruence_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_generalized_affine_image_with_congruence)).

ppl_Grid_generalized_affine_image_with_congruence(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Grid_generalized_affine_image_with_congruence_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Grid_generalized_affine_preimage_with_congruence_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_generalized_affine_preimage_with_congruence)).

ppl_Grid_generalized_affine_preimage_with_congruence(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Grid_generalized_affine_preimage_with_congruence_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Grid_generalized_affine_image_lhs_rhs_with_congruence_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_generalized_affine_image_lhs_rhs_with_congruence)).

ppl_Grid_generalized_affine_image_lhs_rhs_with_congruence(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Grid_generalized_affine_image_lhs_rhs_with_congruence_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Grid_generalized_affine_preimage_lhs_rhs_with_congruence_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_generalized_affine_preimage_lhs_rhs_with_congruence)).

ppl_Grid_generalized_affine_preimage_lhs_rhs_with_congruence(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Grid_generalized_affine_preimage_lhs_rhs_with_congruence_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Grid_add_space_dimensions_and_embed(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_add_space_dimensions_and_project(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_remove_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_remove_space_dimensions)).

ppl_Grid_remove_space_dimensions(Term1, Term2) :-
   ppl_Grid_remove_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Grid_remove_higher_space_dimensions(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_expand_space_dimension(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Grid_fold_space_dimensions_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_fold_space_dimensions)).

ppl_Grid_fold_space_dimensions(Term1, Term2, Term3) :-
   ppl_Grid_fold_space_dimensions_2(Term1, Term2, Term3, 1).

:- true pred ppl_Grid_map_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_map_space_dimensions)).

ppl_Grid_map_space_dimensions(Term1, Term2) :-
   ppl_Grid_map_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Grid_ascii_dump_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Grid_ascii_dump)).

ppl_Grid_ascii_dump(Term1) :-
   ppl_Grid_ascii_dump_2(Term1, 1).

:- true pred ppl_Grid_external_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_external_memory_in_bytes)).

ppl_Grid_external_memory_in_bytes(Term1, Term2) :-
   ppl_Grid_external_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Grid_total_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_total_memory_in_bytes)).

ppl_Grid_total_memory_in_bytes(Term1, Term2) :-
   ppl_Grid_total_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Grid_congruence_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_congruence_widening_assign_with_tokens)).

ppl_Grid_congruence_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Grid_congruence_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Grid_generator_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_generator_widening_assign_with_tokens)).

ppl_Grid_generator_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Grid_generator_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Grid_congruence_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_generator_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_widening_assign_with_tokens)).

ppl_Grid_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Grid_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Grid_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Grid_limited_congruence_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_limited_congruence_extrapolation_assign_with_tokens)).

ppl_Grid_limited_congruence_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Grid_limited_congruence_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Grid_limited_generator_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Grid_limited_generator_extrapolation_assign_with_tokens)).

ppl_Grid_limited_generator_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Grid_limited_generator_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Grid_limited_congruence_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Grid_limited_generator_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_delete_Rational_Box(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_new_Rational_Box_from_space_dimension_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_space_dimension)).

ppl_new_Rational_Box_from_space_dimension(Term1, Term2, Term3) :-
   ppl_new_Rational_Box_from_space_dimension_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Rational_Box_from_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_C_Polyhedron)).

ppl_new_Rational_Box_from_C_Polyhedron(Term1, Term2) :-
   ppl_new_Rational_Box_from_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Rational_Box_from_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_NNC_Polyhedron)).

ppl_new_Rational_Box_from_NNC_Polyhedron(Term1, Term2) :-
   ppl_new_Rational_Box_from_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Rational_Box_from_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_Grid)).

ppl_new_Rational_Box_from_Grid(Term1, Term2) :-
   ppl_new_Rational_Box_from_Grid_2(Term1, Term2, 1).

:- true pred ppl_new_Rational_Box_from_Rational_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_Rational_Box)).

ppl_new_Rational_Box_from_Rational_Box(Term1, Term2) :-
   ppl_new_Rational_Box_from_Rational_Box_2(Term1, Term2, 1).

:- true pred ppl_new_Rational_Box_from_BD_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_BD_Shape_mpz_class)).

ppl_new_Rational_Box_from_BD_Shape_mpz_class(Term1, Term2) :-
   ppl_new_Rational_Box_from_BD_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_Rational_Box_from_BD_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_BD_Shape_mpq_class)).

ppl_new_Rational_Box_from_BD_Shape_mpq_class(Term1, Term2) :-
   ppl_new_Rational_Box_from_BD_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_Rational_Box_from_Octagonal_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_Octagonal_Shape_mpz_class)).

ppl_new_Rational_Box_from_Octagonal_Shape_mpz_class(Term1, Term2) :-
   ppl_new_Rational_Box_from_Octagonal_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_Rational_Box_from_Octagonal_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_Octagonal_Shape_mpq_class)).

ppl_new_Rational_Box_from_Octagonal_Shape_mpq_class(Term1, Term2) :-
   ppl_new_Rational_Box_from_Octagonal_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_Rational_Box_from_Double_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_Double_Box)).

ppl_new_Rational_Box_from_Double_Box(Term1, Term2) :-
   ppl_new_Rational_Box_from_Double_Box_2(Term1, Term2, 1).

:- true pred ppl_new_Rational_Box_from_BD_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_BD_Shape_double)).

ppl_new_Rational_Box_from_BD_Shape_double(Term1, Term2) :-
   ppl_new_Rational_Box_from_BD_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_Rational_Box_from_Octagonal_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_Octagonal_Shape_double)).

ppl_new_Rational_Box_from_Octagonal_Shape_double(Term1, Term2) :-
   ppl_new_Rational_Box_from_Octagonal_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_Rational_Box_from_C_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_C_Polyhedron_with_complexity)).

ppl_new_Rational_Box_from_C_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Rational_Box_from_C_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Rational_Box_from_NNC_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_NNC_Polyhedron_with_complexity)).

ppl_new_Rational_Box_from_NNC_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Rational_Box_from_NNC_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Rational_Box_from_Grid_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_Grid_with_complexity)).

ppl_new_Rational_Box_from_Grid_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Rational_Box_from_Grid_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Rational_Box_from_Rational_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_Rational_Box_with_complexity)).

ppl_new_Rational_Box_from_Rational_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Rational_Box_from_Rational_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Rational_Box_from_BD_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_BD_Shape_mpz_class_with_complexity)).

ppl_new_Rational_Box_from_BD_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Rational_Box_from_BD_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Rational_Box_from_BD_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_BD_Shape_mpq_class_with_complexity)).

ppl_new_Rational_Box_from_BD_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Rational_Box_from_BD_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Rational_Box_from_Octagonal_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_Octagonal_Shape_mpz_class_with_complexity)).

ppl_new_Rational_Box_from_Octagonal_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Rational_Box_from_Octagonal_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Rational_Box_from_Octagonal_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_Octagonal_Shape_mpq_class_with_complexity)).

ppl_new_Rational_Box_from_Octagonal_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Rational_Box_from_Octagonal_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Rational_Box_from_Double_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_Double_Box_with_complexity)).

ppl_new_Rational_Box_from_Double_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Rational_Box_from_Double_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Rational_Box_from_BD_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_BD_Shape_double_with_complexity)).

ppl_new_Rational_Box_from_BD_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Rational_Box_from_BD_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Rational_Box_from_Octagonal_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_Octagonal_Shape_double_with_complexity)).

ppl_new_Rational_Box_from_Octagonal_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Rational_Box_from_Octagonal_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Rational_Box_from_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_constraints)).

ppl_new_Rational_Box_from_constraints(Term1, Term2) :-
   ppl_new_Rational_Box_from_constraints_2(Term1, Term2, 1).

:- true pred ppl_new_Rational_Box_from_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_congruences)).

ppl_new_Rational_Box_from_congruences(Term1, Term2) :-
   ppl_new_Rational_Box_from_congruences_2(Term1, Term2, 1).

:- true pred ppl_new_Rational_Box_from_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Rational_Box_from_generators)).

ppl_new_Rational_Box_from_generators(Term1, Term2) :-
   ppl_new_Rational_Box_from_generators_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_swap(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_space_dimension)).

ppl_Rational_Box_space_dimension(Term1, Term2) :-
   ppl_Rational_Box_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_affine_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_affine_dimension)).

ppl_Rational_Box_affine_dimension(Term1, Term2) :-
   ppl_Rational_Box_affine_dimension_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_relation_with_constraint_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_relation_with_constraint)).

ppl_Rational_Box_relation_with_constraint(Term1, Term2, Term3) :-
   ppl_Rational_Box_relation_with_constraint_2(Term1, Term2, Term3, 1).

:- true pred ppl_Rational_Box_relation_with_generator_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_relation_with_generator)).

ppl_Rational_Box_relation_with_generator(Term1, Term2, Term3) :-
   ppl_Rational_Box_relation_with_generator_2(Term1, Term2, Term3, 1).

:- true pred ppl_Rational_Box_relation_with_congruence_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_relation_with_congruence)).

ppl_Rational_Box_relation_with_congruence(Term1, Term2, Term3) :-
   ppl_Rational_Box_relation_with_congruence_2(Term1, Term2, Term3, 1).

:- true pred ppl_Rational_Box_get_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_get_constraints)).

ppl_Rational_Box_get_constraints(Term1, Term2) :-
   ppl_Rational_Box_get_constraints_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_get_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_get_congruences)).

ppl_Rational_Box_get_congruences(Term1, Term2) :-
   ppl_Rational_Box_get_congruences_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_get_minimized_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_get_minimized_constraints)).

ppl_Rational_Box_get_minimized_constraints(Term1, Term2) :-
   ppl_Rational_Box_get_minimized_constraints_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_get_minimized_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_get_minimized_congruences)).

ppl_Rational_Box_get_minimized_congruences(Term1, Term2) :-
   ppl_Rational_Box_get_minimized_congruences_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_is_empty_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_is_empty)).

ppl_Rational_Box_is_empty(Term1) :-
   ppl_Rational_Box_is_empty_2(Term1, 1).

:- true pred ppl_Rational_Box_is_universe_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_is_universe)).

ppl_Rational_Box_is_universe(Term1) :-
   ppl_Rational_Box_is_universe_2(Term1, 1).

:- true pred ppl_Rational_Box_is_bounded_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_is_bounded)).

ppl_Rational_Box_is_bounded(Term1) :-
   ppl_Rational_Box_is_bounded_2(Term1, 1).

:- true pred ppl_Rational_Box_contains_integer_point_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_contains_integer_point)).

ppl_Rational_Box_contains_integer_point(Term1) :-
   ppl_Rational_Box_contains_integer_point_2(Term1, 1).

:- true pred ppl_Rational_Box_is_topologically_closed_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_is_topologically_closed)).

ppl_Rational_Box_is_topologically_closed(Term1) :-
   ppl_Rational_Box_is_topologically_closed_2(Term1, 1).

:- true pred ppl_Rational_Box_is_discrete_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_is_discrete)).

ppl_Rational_Box_is_discrete(Term1) :-
   ppl_Rational_Box_is_discrete_2(Term1, 1).

:- true pred ppl_Rational_Box_topological_closure_assign(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_Rational_Box_bounds_from_above_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_bounds_from_above)).

ppl_Rational_Box_bounds_from_above(Term1, Term2) :-
   ppl_Rational_Box_bounds_from_above_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_bounds_from_below_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_bounds_from_below)).

ppl_Rational_Box_bounds_from_below(Term1, Term2) :-
   ppl_Rational_Box_bounds_from_below_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_maximize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_maximize)).

ppl_Rational_Box_maximize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Rational_Box_maximize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Rational_Box_minimize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_minimize)).

ppl_Rational_Box_minimize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Rational_Box_minimize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Rational_Box_maximize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_maximize_with_point)).

ppl_Rational_Box_maximize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Rational_Box_maximize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Rational_Box_minimize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_minimize_with_point)).

ppl_Rational_Box_minimize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Rational_Box_minimize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Rational_Box_contains_Rational_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_contains_Rational_Box)).

ppl_Rational_Box_contains_Rational_Box(Term1, Term2) :-
   ppl_Rational_Box_contains_Rational_Box_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_strictly_contains_Rational_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_strictly_contains_Rational_Box)).

ppl_Rational_Box_strictly_contains_Rational_Box(Term1, Term2) :-
   ppl_Rational_Box_strictly_contains_Rational_Box_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_is_disjoint_from_Rational_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_is_disjoint_from_Rational_Box)).

ppl_Rational_Box_is_disjoint_from_Rational_Box(Term1, Term2) :-
   ppl_Rational_Box_is_disjoint_from_Rational_Box_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_equals_Rational_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_equals_Rational_Box)).

ppl_Rational_Box_equals_Rational_Box(Term1, Term2) :-
   ppl_Rational_Box_equals_Rational_Box_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_OK)).

ppl_Rational_Box_OK(Term1) :-
   ppl_Rational_Box_OK_2(Term1, 1).

:- true pred ppl_Rational_Box_add_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_add_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_add_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_add_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_refine_with_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_refine_with_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_refine_with_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_refine_with_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_intersection_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_upper_bound_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_difference_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_concatenate_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_time_elapse_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_upper_bound_assign_if_exact_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_upper_bound_assign_if_exact)).

ppl_Rational_Box_upper_bound_assign_if_exact(Term1, Term2) :-
   ppl_Rational_Box_upper_bound_assign_if_exact_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_simplify_using_context_assign_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_simplify_using_context_assign)).

ppl_Rational_Box_simplify_using_context_assign(Term1, Term2, Term3) :-
   ppl_Rational_Box_simplify_using_context_assign_2(Term1, Term2, Term3, 1).

:- true pred ppl_Rational_Box_constrains_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_constrains)).

ppl_Rational_Box_constrains(Term1, Term2) :-
   ppl_Rational_Box_constrains_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_unconstrain_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_unconstrain_space_dimension)).

ppl_Rational_Box_unconstrain_space_dimension(Term1, Term2) :-
   ppl_Rational_Box_unconstrain_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_unconstrain_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_unconstrain_space_dimensions)).

ppl_Rational_Box_unconstrain_space_dimensions(Term1, Term2) :-
   ppl_Rational_Box_unconstrain_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_affine_image(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_bounded_affine_image(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_bounded_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_generalized_affine_image_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_generalized_affine_image)).

ppl_Rational_Box_generalized_affine_image(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Rational_Box_generalized_affine_image_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Rational_Box_generalized_affine_preimage_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_generalized_affine_preimage)).

ppl_Rational_Box_generalized_affine_preimage(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Rational_Box_generalized_affine_preimage_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Rational_Box_generalized_affine_image_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_generalized_affine_image_lhs_rhs)).

ppl_Rational_Box_generalized_affine_image_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Rational_Box_generalized_affine_image_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Rational_Box_generalized_affine_preimage_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_generalized_affine_preimage_lhs_rhs)).

ppl_Rational_Box_generalized_affine_preimage_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Rational_Box_generalized_affine_preimage_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Rational_Box_add_space_dimensions_and_embed(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_add_space_dimensions_and_project(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_remove_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_remove_space_dimensions)).

ppl_Rational_Box_remove_space_dimensions(Term1, Term2) :-
   ppl_Rational_Box_remove_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_remove_higher_space_dimensions(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_expand_space_dimension(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_fold_space_dimensions_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_fold_space_dimensions)).

ppl_Rational_Box_fold_space_dimensions(Term1, Term2, Term3) :-
   ppl_Rational_Box_fold_space_dimensions_2(Term1, Term2, Term3, 1).

:- true pred ppl_Rational_Box_map_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_map_space_dimensions)).

ppl_Rational_Box_map_space_dimensions(Term1, Term2) :-
   ppl_Rational_Box_map_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_ascii_dump_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_ascii_dump)).

ppl_Rational_Box_ascii_dump(Term1) :-
   ppl_Rational_Box_ascii_dump_2(Term1, 1).

:- true pred ppl_Rational_Box_external_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_external_memory_in_bytes)).

ppl_Rational_Box_external_memory_in_bytes(Term1, Term2) :-
   ppl_Rational_Box_external_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_total_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_total_memory_in_bytes)).

ppl_Rational_Box_total_memory_in_bytes(Term1, Term2) :-
   ppl_Rational_Box_total_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Rational_Box_CC76_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_CC76_widening_assign_with_tokens)).

ppl_Rational_Box_CC76_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Rational_Box_CC76_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Rational_Box_CC76_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_widening_assign_with_tokens)).

ppl_Rational_Box_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Rational_Box_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Rational_Box_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_limited_CC76_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_limited_CC76_extrapolation_assign_with_tokens)).

ppl_Rational_Box_limited_CC76_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Rational_Box_limited_CC76_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Rational_Box_limited_CC76_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Rational_Box_linear_partition_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Rational_Box_linear_partition)).

ppl_Rational_Box_linear_partition(Term1, Term2, Term3, Term4) :-
   ppl_Rational_Box_linear_partition_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_delete_BD_Shape_mpz_class(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_new_BD_Shape_mpz_class_from_space_dimension_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_space_dimension)).

ppl_new_BD_Shape_mpz_class_from_space_dimension(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpz_class_from_space_dimension_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_C_Polyhedron)).

ppl_new_BD_Shape_mpz_class_from_C_Polyhedron(Term1, Term2) :-
   ppl_new_BD_Shape_mpz_class_from_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_NNC_Polyhedron)).

ppl_new_BD_Shape_mpz_class_from_NNC_Polyhedron(Term1, Term2) :-
   ppl_new_BD_Shape_mpz_class_from_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_Grid)).

ppl_new_BD_Shape_mpz_class_from_Grid(Term1, Term2) :-
   ppl_new_BD_Shape_mpz_class_from_Grid_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_Rational_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_Rational_Box)).

ppl_new_BD_Shape_mpz_class_from_Rational_Box(Term1, Term2) :-
   ppl_new_BD_Shape_mpz_class_from_Rational_Box_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpz_class)).

ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpz_class(Term1, Term2) :-
   ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpq_class)).

ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpq_class(Term1, Term2) :-
   ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpz_class)).

ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpz_class(Term1, Term2) :-
   ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpq_class)).

ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpq_class(Term1, Term2) :-
   ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_Double_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_Double_Box)).

ppl_new_BD_Shape_mpz_class_from_Double_Box(Term1, Term2) :-
   ppl_new_BD_Shape_mpz_class_from_Double_Box_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_BD_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_BD_Shape_double)).

ppl_new_BD_Shape_mpz_class_from_BD_Shape_double(Term1, Term2) :-
   ppl_new_BD_Shape_mpz_class_from_BD_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_double)).

ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_double(Term1, Term2) :-
   ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_C_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_C_Polyhedron_with_complexity)).

ppl_new_BD_Shape_mpz_class_from_C_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpz_class_from_C_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_NNC_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_NNC_Polyhedron_with_complexity)).

ppl_new_BD_Shape_mpz_class_from_NNC_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpz_class_from_NNC_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_Grid_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_Grid_with_complexity)).

ppl_new_BD_Shape_mpz_class_from_Grid_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpz_class_from_Grid_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_Rational_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_Rational_Box_with_complexity)).

ppl_new_BD_Shape_mpz_class_from_Rational_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpz_class_from_Rational_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpz_class_with_complexity)).

ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpq_class_with_complexity)).

ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpz_class_with_complexity)).

ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpq_class_with_complexity)).

ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_Double_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_Double_Box_with_complexity)).

ppl_new_BD_Shape_mpz_class_from_Double_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpz_class_from_Double_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_BD_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_BD_Shape_double_with_complexity)).

ppl_new_BD_Shape_mpz_class_from_BD_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpz_class_from_BD_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_double_with_complexity)).

ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_constraints)).

ppl_new_BD_Shape_mpz_class_from_constraints(Term1, Term2) :-
   ppl_new_BD_Shape_mpz_class_from_constraints_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_congruences)).

ppl_new_BD_Shape_mpz_class_from_congruences(Term1, Term2) :-
   ppl_new_BD_Shape_mpz_class_from_congruences_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpz_class_from_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpz_class_from_generators)).

ppl_new_BD_Shape_mpz_class_from_generators(Term1, Term2) :-
   ppl_new_BD_Shape_mpz_class_from_generators_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_swap(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_space_dimension)).

ppl_BD_Shape_mpz_class_space_dimension(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_affine_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_affine_dimension)).

ppl_BD_Shape_mpz_class_affine_dimension(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_affine_dimension_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_relation_with_constraint_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_relation_with_constraint)).

ppl_BD_Shape_mpz_class_relation_with_constraint(Term1, Term2, Term3) :-
   ppl_BD_Shape_mpz_class_relation_with_constraint_2(Term1, Term2, Term3, 1).

:- true pred ppl_BD_Shape_mpz_class_relation_with_generator_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_relation_with_generator)).

ppl_BD_Shape_mpz_class_relation_with_generator(Term1, Term2, Term3) :-
   ppl_BD_Shape_mpz_class_relation_with_generator_2(Term1, Term2, Term3, 1).

:- true pred ppl_BD_Shape_mpz_class_relation_with_congruence_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_relation_with_congruence)).

ppl_BD_Shape_mpz_class_relation_with_congruence(Term1, Term2, Term3) :-
   ppl_BD_Shape_mpz_class_relation_with_congruence_2(Term1, Term2, Term3, 1).

:- true pred ppl_BD_Shape_mpz_class_get_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_get_constraints)).

ppl_BD_Shape_mpz_class_get_constraints(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_get_constraints_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_get_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_get_congruences)).

ppl_BD_Shape_mpz_class_get_congruences(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_get_congruences_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_get_minimized_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_get_minimized_constraints)).

ppl_BD_Shape_mpz_class_get_minimized_constraints(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_get_minimized_constraints_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_get_minimized_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_get_minimized_congruences)).

ppl_BD_Shape_mpz_class_get_minimized_congruences(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_get_minimized_congruences_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_is_empty_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_is_empty)).

ppl_BD_Shape_mpz_class_is_empty(Term1) :-
   ppl_BD_Shape_mpz_class_is_empty_2(Term1, 1).

:- true pred ppl_BD_Shape_mpz_class_is_universe_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_is_universe)).

ppl_BD_Shape_mpz_class_is_universe(Term1) :-
   ppl_BD_Shape_mpz_class_is_universe_2(Term1, 1).

:- true pred ppl_BD_Shape_mpz_class_is_bounded_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_is_bounded)).

ppl_BD_Shape_mpz_class_is_bounded(Term1) :-
   ppl_BD_Shape_mpz_class_is_bounded_2(Term1, 1).

:- true pred ppl_BD_Shape_mpz_class_contains_integer_point_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_contains_integer_point)).

ppl_BD_Shape_mpz_class_contains_integer_point(Term1) :-
   ppl_BD_Shape_mpz_class_contains_integer_point_2(Term1, 1).

:- true pred ppl_BD_Shape_mpz_class_is_topologically_closed_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_is_topologically_closed)).

ppl_BD_Shape_mpz_class_is_topologically_closed(Term1) :-
   ppl_BD_Shape_mpz_class_is_topologically_closed_2(Term1, 1).

:- true pred ppl_BD_Shape_mpz_class_is_discrete_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_is_discrete)).

ppl_BD_Shape_mpz_class_is_discrete(Term1) :-
   ppl_BD_Shape_mpz_class_is_discrete_2(Term1, 1).

:- true pred ppl_BD_Shape_mpz_class_topological_closure_assign(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_bounds_from_above_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_bounds_from_above)).

ppl_BD_Shape_mpz_class_bounds_from_above(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_bounds_from_above_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_bounds_from_below_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_bounds_from_below)).

ppl_BD_Shape_mpz_class_bounds_from_below(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_bounds_from_below_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_maximize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_maximize)).

ppl_BD_Shape_mpz_class_maximize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_mpz_class_maximize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_mpz_class_minimize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_minimize)).

ppl_BD_Shape_mpz_class_minimize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_mpz_class_minimize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_mpz_class_maximize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_maximize_with_point)).

ppl_BD_Shape_mpz_class_maximize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_BD_Shape_mpz_class_maximize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_BD_Shape_mpz_class_minimize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_minimize_with_point)).

ppl_BD_Shape_mpz_class_minimize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_BD_Shape_mpz_class_minimize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_BD_Shape_mpz_class_contains_BD_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_contains_BD_Shape_mpz_class)).

ppl_BD_Shape_mpz_class_contains_BD_Shape_mpz_class(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_contains_BD_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_strictly_contains_BD_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_strictly_contains_BD_Shape_mpz_class)).

ppl_BD_Shape_mpz_class_strictly_contains_BD_Shape_mpz_class(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_strictly_contains_BD_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_is_disjoint_from_BD_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_is_disjoint_from_BD_Shape_mpz_class)).

ppl_BD_Shape_mpz_class_is_disjoint_from_BD_Shape_mpz_class(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_is_disjoint_from_BD_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_equals_BD_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_equals_BD_Shape_mpz_class)).

ppl_BD_Shape_mpz_class_equals_BD_Shape_mpz_class(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_equals_BD_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_OK)).

ppl_BD_Shape_mpz_class_OK(Term1) :-
   ppl_BD_Shape_mpz_class_OK_2(Term1, 1).

:- true pred ppl_BD_Shape_mpz_class_add_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_add_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_add_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_add_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_refine_with_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_refine_with_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_refine_with_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_refine_with_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_intersection_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_upper_bound_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_difference_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_concatenate_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_time_elapse_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_upper_bound_assign_if_exact_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_upper_bound_assign_if_exact)).

ppl_BD_Shape_mpz_class_upper_bound_assign_if_exact(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_upper_bound_assign_if_exact_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_simplify_using_context_assign_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_simplify_using_context_assign)).

ppl_BD_Shape_mpz_class_simplify_using_context_assign(Term1, Term2, Term3) :-
   ppl_BD_Shape_mpz_class_simplify_using_context_assign_2(Term1, Term2, Term3, 1).

:- true pred ppl_BD_Shape_mpz_class_constrains_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_constrains)).

ppl_BD_Shape_mpz_class_constrains(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_constrains_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_unconstrain_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_unconstrain_space_dimension)).

ppl_BD_Shape_mpz_class_unconstrain_space_dimension(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_unconstrain_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_unconstrain_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_unconstrain_space_dimensions)).

ppl_BD_Shape_mpz_class_unconstrain_space_dimensions(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_unconstrain_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_affine_image(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_bounded_affine_image(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_bounded_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_generalized_affine_image_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_generalized_affine_image)).

ppl_BD_Shape_mpz_class_generalized_affine_image(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_mpz_class_generalized_affine_image_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_mpz_class_generalized_affine_preimage_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_generalized_affine_preimage)).

ppl_BD_Shape_mpz_class_generalized_affine_preimage(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_mpz_class_generalized_affine_preimage_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_mpz_class_generalized_affine_image_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_generalized_affine_image_lhs_rhs)).

ppl_BD_Shape_mpz_class_generalized_affine_image_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_mpz_class_generalized_affine_image_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_mpz_class_generalized_affine_preimage_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_generalized_affine_preimage_lhs_rhs)).

ppl_BD_Shape_mpz_class_generalized_affine_preimage_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_mpz_class_generalized_affine_preimage_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_mpz_class_add_space_dimensions_and_embed(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_add_space_dimensions_and_project(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_remove_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_remove_space_dimensions)).

ppl_BD_Shape_mpz_class_remove_space_dimensions(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_remove_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_remove_higher_space_dimensions(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_expand_space_dimension(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_fold_space_dimensions_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_fold_space_dimensions)).

ppl_BD_Shape_mpz_class_fold_space_dimensions(Term1, Term2, Term3) :-
   ppl_BD_Shape_mpz_class_fold_space_dimensions_2(Term1, Term2, Term3, 1).

:- true pred ppl_BD_Shape_mpz_class_map_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_map_space_dimensions)).

ppl_BD_Shape_mpz_class_map_space_dimensions(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_map_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_ascii_dump_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_ascii_dump)).

ppl_BD_Shape_mpz_class_ascii_dump(Term1) :-
   ppl_BD_Shape_mpz_class_ascii_dump_2(Term1, 1).

:- true pred ppl_BD_Shape_mpz_class_external_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_external_memory_in_bytes)).

ppl_BD_Shape_mpz_class_external_memory_in_bytes(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_external_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_total_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_total_memory_in_bytes)).

ppl_BD_Shape_mpz_class_total_memory_in_bytes(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_total_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_BHMZ05_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_BHMZ05_widening_assign_with_tokens)).

ppl_BD_Shape_mpz_class_BHMZ05_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_mpz_class_BHMZ05_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_mpz_class_H79_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_H79_widening_assign_with_tokens)).

ppl_BD_Shape_mpz_class_H79_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_mpz_class_H79_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_mpz_class_BHMZ05_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_H79_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_widening_assign_with_tokens)).

ppl_BD_Shape_mpz_class_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_mpz_class_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_mpz_class_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_limited_BHMZ05_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_limited_BHMZ05_extrapolation_assign_with_tokens)).

ppl_BD_Shape_mpz_class_limited_BHMZ05_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_mpz_class_limited_BHMZ05_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_mpz_class_limited_H79_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_limited_H79_extrapolation_assign_with_tokens)).

ppl_BD_Shape_mpz_class_limited_H79_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_mpz_class_limited_H79_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_mpz_class_limited_CC76_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_limited_CC76_extrapolation_assign_with_tokens)).

ppl_BD_Shape_mpz_class_limited_CC76_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_mpz_class_limited_CC76_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_mpz_class_limited_BHMZ05_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_limited_H79_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_limited_CC76_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_CC76_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_CC76_extrapolation_assign_with_tokens)).

ppl_BD_Shape_mpz_class_CC76_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_mpz_class_CC76_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_mpz_class_CC76_extrapolation_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpz_class_CC76_narrowing_assign_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_CC76_narrowing_assign)).

ppl_BD_Shape_mpz_class_CC76_narrowing_assign(Term1, Term2) :-
   ppl_BD_Shape_mpz_class_CC76_narrowing_assign_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpz_class_linear_partition_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpz_class_linear_partition)).

ppl_BD_Shape_mpz_class_linear_partition(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_mpz_class_linear_partition_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_delete_BD_Shape_mpq_class(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_new_BD_Shape_mpq_class_from_space_dimension_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_space_dimension)).

ppl_new_BD_Shape_mpq_class_from_space_dimension(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpq_class_from_space_dimension_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_C_Polyhedron)).

ppl_new_BD_Shape_mpq_class_from_C_Polyhedron(Term1, Term2) :-
   ppl_new_BD_Shape_mpq_class_from_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron)).

ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron(Term1, Term2) :-
   ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_Grid)).

ppl_new_BD_Shape_mpq_class_from_Grid(Term1, Term2) :-
   ppl_new_BD_Shape_mpq_class_from_Grid_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_Rational_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_Rational_Box)).

ppl_new_BD_Shape_mpq_class_from_Rational_Box(Term1, Term2) :-
   ppl_new_BD_Shape_mpq_class_from_Rational_Box_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class)).

ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class(Term1, Term2) :-
   ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class)).

ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class(Term1, Term2) :-
   ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class)).

ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class(Term1, Term2) :-
   ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class)).

ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class(Term1, Term2) :-
   ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_Double_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_Double_Box)).

ppl_new_BD_Shape_mpq_class_from_Double_Box(Term1, Term2) :-
   ppl_new_BD_Shape_mpq_class_from_Double_Box_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_BD_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_BD_Shape_double)).

ppl_new_BD_Shape_mpq_class_from_BD_Shape_double(Term1, Term2) :-
   ppl_new_BD_Shape_mpq_class_from_BD_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_double)).

ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_double(Term1, Term2) :-
   ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_C_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_C_Polyhedron_with_complexity)).

ppl_new_BD_Shape_mpq_class_from_C_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpq_class_from_C_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron_with_complexity)).

ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_Grid_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_Grid_with_complexity)).

ppl_new_BD_Shape_mpq_class_from_Grid_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpq_class_from_Grid_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_Rational_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_Rational_Box_with_complexity)).

ppl_new_BD_Shape_mpq_class_from_Rational_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpq_class_from_Rational_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity)).

ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity)).

ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity)).

ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity)).

ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_Double_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_Double_Box_with_complexity)).

ppl_new_BD_Shape_mpq_class_from_Double_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpq_class_from_Double_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_BD_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_BD_Shape_double_with_complexity)).

ppl_new_BD_Shape_mpq_class_from_BD_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpq_class_from_BD_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_double_with_complexity)).

ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_constraints)).

ppl_new_BD_Shape_mpq_class_from_constraints(Term1, Term2) :-
   ppl_new_BD_Shape_mpq_class_from_constraints_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_congruences)).

ppl_new_BD_Shape_mpq_class_from_congruences(Term1, Term2) :-
   ppl_new_BD_Shape_mpq_class_from_congruences_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_mpq_class_from_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_mpq_class_from_generators)).

ppl_new_BD_Shape_mpq_class_from_generators(Term1, Term2) :-
   ppl_new_BD_Shape_mpq_class_from_generators_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_swap(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_space_dimension)).

ppl_BD_Shape_mpq_class_space_dimension(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_affine_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_affine_dimension)).

ppl_BD_Shape_mpq_class_affine_dimension(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_affine_dimension_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_relation_with_constraint_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_relation_with_constraint)).

ppl_BD_Shape_mpq_class_relation_with_constraint(Term1, Term2, Term3) :-
   ppl_BD_Shape_mpq_class_relation_with_constraint_2(Term1, Term2, Term3, 1).

:- true pred ppl_BD_Shape_mpq_class_relation_with_generator_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_relation_with_generator)).

ppl_BD_Shape_mpq_class_relation_with_generator(Term1, Term2, Term3) :-
   ppl_BD_Shape_mpq_class_relation_with_generator_2(Term1, Term2, Term3, 1).

:- true pred ppl_BD_Shape_mpq_class_relation_with_congruence_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_relation_with_congruence)).

ppl_BD_Shape_mpq_class_relation_with_congruence(Term1, Term2, Term3) :-
   ppl_BD_Shape_mpq_class_relation_with_congruence_2(Term1, Term2, Term3, 1).

:- true pred ppl_BD_Shape_mpq_class_get_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_get_constraints)).

ppl_BD_Shape_mpq_class_get_constraints(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_get_constraints_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_get_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_get_congruences)).

ppl_BD_Shape_mpq_class_get_congruences(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_get_congruences_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_get_minimized_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_get_minimized_constraints)).

ppl_BD_Shape_mpq_class_get_minimized_constraints(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_get_minimized_constraints_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_get_minimized_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_get_minimized_congruences)).

ppl_BD_Shape_mpq_class_get_minimized_congruences(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_get_minimized_congruences_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_is_empty_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_is_empty)).

ppl_BD_Shape_mpq_class_is_empty(Term1) :-
   ppl_BD_Shape_mpq_class_is_empty_2(Term1, 1).

:- true pred ppl_BD_Shape_mpq_class_is_universe_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_is_universe)).

ppl_BD_Shape_mpq_class_is_universe(Term1) :-
   ppl_BD_Shape_mpq_class_is_universe_2(Term1, 1).

:- true pred ppl_BD_Shape_mpq_class_is_bounded_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_is_bounded)).

ppl_BD_Shape_mpq_class_is_bounded(Term1) :-
   ppl_BD_Shape_mpq_class_is_bounded_2(Term1, 1).

:- true pred ppl_BD_Shape_mpq_class_contains_integer_point_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_contains_integer_point)).

ppl_BD_Shape_mpq_class_contains_integer_point(Term1) :-
   ppl_BD_Shape_mpq_class_contains_integer_point_2(Term1, 1).

:- true pred ppl_BD_Shape_mpq_class_is_topologically_closed_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_is_topologically_closed)).

ppl_BD_Shape_mpq_class_is_topologically_closed(Term1) :-
   ppl_BD_Shape_mpq_class_is_topologically_closed_2(Term1, 1).

:- true pred ppl_BD_Shape_mpq_class_is_discrete_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_is_discrete)).

ppl_BD_Shape_mpq_class_is_discrete(Term1) :-
   ppl_BD_Shape_mpq_class_is_discrete_2(Term1, 1).

:- true pred ppl_BD_Shape_mpq_class_topological_closure_assign(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_bounds_from_above_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_bounds_from_above)).

ppl_BD_Shape_mpq_class_bounds_from_above(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_bounds_from_above_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_bounds_from_below_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_bounds_from_below)).

ppl_BD_Shape_mpq_class_bounds_from_below(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_bounds_from_below_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_maximize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_maximize)).

ppl_BD_Shape_mpq_class_maximize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_mpq_class_maximize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_mpq_class_minimize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_minimize)).

ppl_BD_Shape_mpq_class_minimize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_mpq_class_minimize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_mpq_class_maximize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_maximize_with_point)).

ppl_BD_Shape_mpq_class_maximize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_BD_Shape_mpq_class_maximize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_BD_Shape_mpq_class_minimize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_minimize_with_point)).

ppl_BD_Shape_mpq_class_minimize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_BD_Shape_mpq_class_minimize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_BD_Shape_mpq_class_contains_BD_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_contains_BD_Shape_mpq_class)).

ppl_BD_Shape_mpq_class_contains_BD_Shape_mpq_class(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_contains_BD_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_strictly_contains_BD_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_strictly_contains_BD_Shape_mpq_class)).

ppl_BD_Shape_mpq_class_strictly_contains_BD_Shape_mpq_class(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_strictly_contains_BD_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_is_disjoint_from_BD_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_is_disjoint_from_BD_Shape_mpq_class)).

ppl_BD_Shape_mpq_class_is_disjoint_from_BD_Shape_mpq_class(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_is_disjoint_from_BD_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_equals_BD_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_equals_BD_Shape_mpq_class)).

ppl_BD_Shape_mpq_class_equals_BD_Shape_mpq_class(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_equals_BD_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_OK)).

ppl_BD_Shape_mpq_class_OK(Term1) :-
   ppl_BD_Shape_mpq_class_OK_2(Term1, 1).

:- true pred ppl_BD_Shape_mpq_class_add_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_add_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_add_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_add_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_refine_with_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_refine_with_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_refine_with_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_refine_with_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_intersection_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_upper_bound_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_difference_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_concatenate_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_time_elapse_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_upper_bound_assign_if_exact_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_upper_bound_assign_if_exact)).

ppl_BD_Shape_mpq_class_upper_bound_assign_if_exact(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_upper_bound_assign_if_exact_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_simplify_using_context_assign_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_simplify_using_context_assign)).

ppl_BD_Shape_mpq_class_simplify_using_context_assign(Term1, Term2, Term3) :-
   ppl_BD_Shape_mpq_class_simplify_using_context_assign_2(Term1, Term2, Term3, 1).

:- true pred ppl_BD_Shape_mpq_class_constrains_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_constrains)).

ppl_BD_Shape_mpq_class_constrains(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_constrains_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_unconstrain_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_unconstrain_space_dimension)).

ppl_BD_Shape_mpq_class_unconstrain_space_dimension(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_unconstrain_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_unconstrain_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_unconstrain_space_dimensions)).

ppl_BD_Shape_mpq_class_unconstrain_space_dimensions(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_unconstrain_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_affine_image(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_bounded_affine_image(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_bounded_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_generalized_affine_image_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_generalized_affine_image)).

ppl_BD_Shape_mpq_class_generalized_affine_image(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_mpq_class_generalized_affine_image_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_mpq_class_generalized_affine_preimage_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_generalized_affine_preimage)).

ppl_BD_Shape_mpq_class_generalized_affine_preimage(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_mpq_class_generalized_affine_preimage_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_mpq_class_generalized_affine_image_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_generalized_affine_image_lhs_rhs)).

ppl_BD_Shape_mpq_class_generalized_affine_image_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_mpq_class_generalized_affine_image_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_mpq_class_generalized_affine_preimage_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_generalized_affine_preimage_lhs_rhs)).

ppl_BD_Shape_mpq_class_generalized_affine_preimage_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_mpq_class_generalized_affine_preimage_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_mpq_class_add_space_dimensions_and_embed(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_add_space_dimensions_and_project(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_remove_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_remove_space_dimensions)).

ppl_BD_Shape_mpq_class_remove_space_dimensions(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_remove_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_remove_higher_space_dimensions(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_expand_space_dimension(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_fold_space_dimensions_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_fold_space_dimensions)).

ppl_BD_Shape_mpq_class_fold_space_dimensions(Term1, Term2, Term3) :-
   ppl_BD_Shape_mpq_class_fold_space_dimensions_2(Term1, Term2, Term3, 1).

:- true pred ppl_BD_Shape_mpq_class_map_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_map_space_dimensions)).

ppl_BD_Shape_mpq_class_map_space_dimensions(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_map_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_ascii_dump_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_ascii_dump)).

ppl_BD_Shape_mpq_class_ascii_dump(Term1) :-
   ppl_BD_Shape_mpq_class_ascii_dump_2(Term1, 1).

:- true pred ppl_BD_Shape_mpq_class_external_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_external_memory_in_bytes)).

ppl_BD_Shape_mpq_class_external_memory_in_bytes(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_external_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_total_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_total_memory_in_bytes)).

ppl_BD_Shape_mpq_class_total_memory_in_bytes(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_total_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_BHMZ05_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_BHMZ05_widening_assign_with_tokens)).

ppl_BD_Shape_mpq_class_BHMZ05_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_mpq_class_BHMZ05_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_mpq_class_H79_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_H79_widening_assign_with_tokens)).

ppl_BD_Shape_mpq_class_H79_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_mpq_class_H79_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_mpq_class_BHMZ05_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_H79_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_widening_assign_with_tokens)).

ppl_BD_Shape_mpq_class_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_mpq_class_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_mpq_class_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens)).

ppl_BD_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_mpq_class_limited_H79_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_limited_H79_extrapolation_assign_with_tokens)).

ppl_BD_Shape_mpq_class_limited_H79_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_mpq_class_limited_H79_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens)).

ppl_BD_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_mpq_class_limited_BHMZ05_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_limited_H79_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_limited_CC76_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_CC76_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_CC76_extrapolation_assign_with_tokens)).

ppl_BD_Shape_mpq_class_CC76_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_mpq_class_CC76_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_mpq_class_CC76_extrapolation_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_mpq_class_CC76_narrowing_assign_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_CC76_narrowing_assign)).

ppl_BD_Shape_mpq_class_CC76_narrowing_assign(Term1, Term2) :-
   ppl_BD_Shape_mpq_class_CC76_narrowing_assign_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_mpq_class_linear_partition_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_mpq_class_linear_partition)).

ppl_BD_Shape_mpq_class_linear_partition(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_mpq_class_linear_partition_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_delete_Octagonal_Shape_mpz_class(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_space_dimension_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_space_dimension)).

ppl_new_Octagonal_Shape_mpz_class_from_space_dimension(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpz_class_from_space_dimension_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_C_Polyhedron)).

ppl_new_Octagonal_Shape_mpz_class_from_C_Polyhedron(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpz_class_from_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron)).

ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_Grid)).

ppl_new_Octagonal_Shape_mpz_class_from_Grid(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpz_class_from_Grid_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_Rational_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_Rational_Box)).

ppl_new_Octagonal_Shape_mpz_class_from_Rational_Box(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpz_class_from_Rational_Box_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpz_class)).

ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpz_class(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpq_class)).

ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpq_class(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class)).

ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpq_class)).

ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpq_class(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_Double_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_Double_Box)).

ppl_new_Octagonal_Shape_mpz_class_from_Double_Box(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpz_class_from_Double_Box_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_double)).

ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_double(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_double)).

ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_double(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_C_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_C_Polyhedron_with_complexity)).

ppl_new_Octagonal_Shape_mpz_class_from_C_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpz_class_from_C_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron_with_complexity)).

ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_Grid_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_Grid_with_complexity)).

ppl_new_Octagonal_Shape_mpz_class_from_Grid_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpz_class_from_Grid_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_Rational_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_Rational_Box_with_complexity)).

ppl_new_Octagonal_Shape_mpz_class_from_Rational_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpz_class_from_Rational_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpz_class_with_complexity)).

ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpq_class_with_complexity)).

ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class_with_complexity)).

ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpq_class_with_complexity)).

ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_Double_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_Double_Box_with_complexity)).

ppl_new_Octagonal_Shape_mpz_class_from_Double_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpz_class_from_Double_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_double_with_complexity)).

ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_double_with_complexity)).

ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_constraints)).

ppl_new_Octagonal_Shape_mpz_class_from_constraints(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpz_class_from_constraints_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_congruences)).

ppl_new_Octagonal_Shape_mpz_class_from_congruences(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpz_class_from_congruences_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpz_class_from_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpz_class_from_generators)).

ppl_new_Octagonal_Shape_mpz_class_from_generators(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpz_class_from_generators_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_swap(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_space_dimension)).

ppl_Octagonal_Shape_mpz_class_space_dimension(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_affine_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_affine_dimension)).

ppl_Octagonal_Shape_mpz_class_affine_dimension(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_affine_dimension_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_relation_with_constraint_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_relation_with_constraint)).

ppl_Octagonal_Shape_mpz_class_relation_with_constraint(Term1, Term2, Term3) :-
   ppl_Octagonal_Shape_mpz_class_relation_with_constraint_2(Term1, Term2, Term3, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_relation_with_generator_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_relation_with_generator)).

ppl_Octagonal_Shape_mpz_class_relation_with_generator(Term1, Term2, Term3) :-
   ppl_Octagonal_Shape_mpz_class_relation_with_generator_2(Term1, Term2, Term3, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_relation_with_congruence_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_relation_with_congruence)).

ppl_Octagonal_Shape_mpz_class_relation_with_congruence(Term1, Term2, Term3) :-
   ppl_Octagonal_Shape_mpz_class_relation_with_congruence_2(Term1, Term2, Term3, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_get_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_get_constraints)).

ppl_Octagonal_Shape_mpz_class_get_constraints(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_get_constraints_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_get_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_get_congruences)).

ppl_Octagonal_Shape_mpz_class_get_congruences(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_get_congruences_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_get_minimized_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_get_minimized_constraints)).

ppl_Octagonal_Shape_mpz_class_get_minimized_constraints(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_get_minimized_constraints_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_get_minimized_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_get_minimized_congruences)).

ppl_Octagonal_Shape_mpz_class_get_minimized_congruences(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_get_minimized_congruences_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_is_empty_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_is_empty)).

ppl_Octagonal_Shape_mpz_class_is_empty(Term1) :-
   ppl_Octagonal_Shape_mpz_class_is_empty_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_is_universe_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_is_universe)).

ppl_Octagonal_Shape_mpz_class_is_universe(Term1) :-
   ppl_Octagonal_Shape_mpz_class_is_universe_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_is_bounded_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_is_bounded)).

ppl_Octagonal_Shape_mpz_class_is_bounded(Term1) :-
   ppl_Octagonal_Shape_mpz_class_is_bounded_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_contains_integer_point_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_contains_integer_point)).

ppl_Octagonal_Shape_mpz_class_contains_integer_point(Term1) :-
   ppl_Octagonal_Shape_mpz_class_contains_integer_point_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_is_topologically_closed_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_is_topologically_closed)).

ppl_Octagonal_Shape_mpz_class_is_topologically_closed(Term1) :-
   ppl_Octagonal_Shape_mpz_class_is_topologically_closed_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_is_discrete_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_is_discrete)).

ppl_Octagonal_Shape_mpz_class_is_discrete(Term1) :-
   ppl_Octagonal_Shape_mpz_class_is_discrete_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_topological_closure_assign(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_bounds_from_above_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_bounds_from_above)).

ppl_Octagonal_Shape_mpz_class_bounds_from_above(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_bounds_from_above_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_bounds_from_below_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_bounds_from_below)).

ppl_Octagonal_Shape_mpz_class_bounds_from_below(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_bounds_from_below_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_maximize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_maximize)).

ppl_Octagonal_Shape_mpz_class_maximize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_mpz_class_maximize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_minimize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_minimize)).

ppl_Octagonal_Shape_mpz_class_minimize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_mpz_class_minimize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_maximize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_maximize_with_point)).

ppl_Octagonal_Shape_mpz_class_maximize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Octagonal_Shape_mpz_class_maximize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_minimize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_minimize_with_point)).

ppl_Octagonal_Shape_mpz_class_minimize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Octagonal_Shape_mpz_class_minimize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_contains_Octagonal_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_contains_Octagonal_Shape_mpz_class)).

ppl_Octagonal_Shape_mpz_class_contains_Octagonal_Shape_mpz_class(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_contains_Octagonal_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_strictly_contains_Octagonal_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_strictly_contains_Octagonal_Shape_mpz_class)).

ppl_Octagonal_Shape_mpz_class_strictly_contains_Octagonal_Shape_mpz_class(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_strictly_contains_Octagonal_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_is_disjoint_from_Octagonal_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_is_disjoint_from_Octagonal_Shape_mpz_class)).

ppl_Octagonal_Shape_mpz_class_is_disjoint_from_Octagonal_Shape_mpz_class(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_is_disjoint_from_Octagonal_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_equals_Octagonal_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_equals_Octagonal_Shape_mpz_class)).

ppl_Octagonal_Shape_mpz_class_equals_Octagonal_Shape_mpz_class(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_equals_Octagonal_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_OK)).

ppl_Octagonal_Shape_mpz_class_OK(Term1) :-
   ppl_Octagonal_Shape_mpz_class_OK_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_add_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_add_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_add_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_add_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_refine_with_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_refine_with_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_refine_with_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_refine_with_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_intersection_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_upper_bound_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_difference_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_concatenate_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_time_elapse_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_upper_bound_assign_if_exact_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_upper_bound_assign_if_exact)).

ppl_Octagonal_Shape_mpz_class_upper_bound_assign_if_exact(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_upper_bound_assign_if_exact_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_simplify_using_context_assign_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_simplify_using_context_assign)).

ppl_Octagonal_Shape_mpz_class_simplify_using_context_assign(Term1, Term2, Term3) :-
   ppl_Octagonal_Shape_mpz_class_simplify_using_context_assign_2(Term1, Term2, Term3, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_constrains_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_constrains)).

ppl_Octagonal_Shape_mpz_class_constrains(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_constrains_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_unconstrain_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_unconstrain_space_dimension)).

ppl_Octagonal_Shape_mpz_class_unconstrain_space_dimension(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_unconstrain_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_unconstrain_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_unconstrain_space_dimensions)).

ppl_Octagonal_Shape_mpz_class_unconstrain_space_dimensions(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_unconstrain_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_affine_image(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_bounded_affine_image(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_bounded_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_generalized_affine_image_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_generalized_affine_image)).

ppl_Octagonal_Shape_mpz_class_generalized_affine_image(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_mpz_class_generalized_affine_image_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_generalized_affine_preimage_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_generalized_affine_preimage)).

ppl_Octagonal_Shape_mpz_class_generalized_affine_preimage(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_mpz_class_generalized_affine_preimage_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_generalized_affine_image_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_generalized_affine_image_lhs_rhs)).

ppl_Octagonal_Shape_mpz_class_generalized_affine_image_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_mpz_class_generalized_affine_image_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_generalized_affine_preimage_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_generalized_affine_preimage_lhs_rhs)).

ppl_Octagonal_Shape_mpz_class_generalized_affine_preimage_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_mpz_class_generalized_affine_preimage_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_add_space_dimensions_and_embed(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_add_space_dimensions_and_project(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_remove_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_remove_space_dimensions)).

ppl_Octagonal_Shape_mpz_class_remove_space_dimensions(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_remove_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_remove_higher_space_dimensions(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_expand_space_dimension(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_fold_space_dimensions_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_fold_space_dimensions)).

ppl_Octagonal_Shape_mpz_class_fold_space_dimensions(Term1, Term2, Term3) :-
   ppl_Octagonal_Shape_mpz_class_fold_space_dimensions_2(Term1, Term2, Term3, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_map_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_map_space_dimensions)).

ppl_Octagonal_Shape_mpz_class_map_space_dimensions(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_map_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_ascii_dump_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_ascii_dump)).

ppl_Octagonal_Shape_mpz_class_ascii_dump(Term1) :-
   ppl_Octagonal_Shape_mpz_class_ascii_dump_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_external_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_external_memory_in_bytes)).

ppl_Octagonal_Shape_mpz_class_external_memory_in_bytes(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_external_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_total_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_total_memory_in_bytes)).

ppl_Octagonal_Shape_mpz_class_total_memory_in_bytes(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_total_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_BHMZ05_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_BHMZ05_widening_assign_with_tokens)).

ppl_Octagonal_Shape_mpz_class_BHMZ05_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_mpz_class_BHMZ05_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_BHMZ05_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_widening_assign_with_tokens)).

ppl_Octagonal_Shape_mpz_class_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_mpz_class_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_limited_BHMZ05_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_limited_BHMZ05_extrapolation_assign_with_tokens)).

ppl_Octagonal_Shape_mpz_class_limited_BHMZ05_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_mpz_class_limited_BHMZ05_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_limited_CC76_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_limited_CC76_extrapolation_assign_with_tokens)).

ppl_Octagonal_Shape_mpz_class_limited_CC76_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_mpz_class_limited_CC76_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_limited_BHMZ05_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_limited_CC76_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_CC76_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_CC76_extrapolation_assign_with_tokens)).

ppl_Octagonal_Shape_mpz_class_CC76_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_mpz_class_CC76_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_CC76_extrapolation_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpz_class_CC76_narrowing_assign_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_CC76_narrowing_assign)).

ppl_Octagonal_Shape_mpz_class_CC76_narrowing_assign(Term1, Term2) :-
   ppl_Octagonal_Shape_mpz_class_CC76_narrowing_assign_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpz_class_linear_partition_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpz_class_linear_partition)).

ppl_Octagonal_Shape_mpz_class_linear_partition(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_mpz_class_linear_partition_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_delete_Octagonal_Shape_mpq_class(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_space_dimension_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_space_dimension)).

ppl_new_Octagonal_Shape_mpq_class_from_space_dimension(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpq_class_from_space_dimension_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_C_Polyhedron)).

ppl_new_Octagonal_Shape_mpq_class_from_C_Polyhedron(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpq_class_from_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron)).

ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_Grid)).

ppl_new_Octagonal_Shape_mpq_class_from_Grid(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpq_class_from_Grid_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box)).

ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class)).

ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class)).

ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class)).

ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class)).

ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_Double_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_Double_Box)).

ppl_new_Octagonal_Shape_mpq_class_from_Double_Box(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpq_class_from_Double_Box_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_double)).

ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_double(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_double)).

ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_double(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_C_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_C_Polyhedron_with_complexity)).

ppl_new_Octagonal_Shape_mpq_class_from_C_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpq_class_from_C_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron_with_complexity)).

ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_Grid_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_Grid_with_complexity)).

ppl_new_Octagonal_Shape_mpq_class_from_Grid_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpq_class_from_Grid_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box_with_complexity)).

ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity)).

ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity)).

ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity)).

ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity)).

ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_Double_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_Double_Box_with_complexity)).

ppl_new_Octagonal_Shape_mpq_class_from_Double_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpq_class_from_Double_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_double_with_complexity)).

ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_double_with_complexity)).

ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_constraints)).

ppl_new_Octagonal_Shape_mpq_class_from_constraints(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpq_class_from_constraints_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_congruences)).

ppl_new_Octagonal_Shape_mpq_class_from_congruences(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpq_class_from_congruences_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_mpq_class_from_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_mpq_class_from_generators)).

ppl_new_Octagonal_Shape_mpq_class_from_generators(Term1, Term2) :-
   ppl_new_Octagonal_Shape_mpq_class_from_generators_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_swap(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_space_dimension)).

ppl_Octagonal_Shape_mpq_class_space_dimension(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_affine_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_affine_dimension)).

ppl_Octagonal_Shape_mpq_class_affine_dimension(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_affine_dimension_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_relation_with_constraint_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_relation_with_constraint)).

ppl_Octagonal_Shape_mpq_class_relation_with_constraint(Term1, Term2, Term3) :-
   ppl_Octagonal_Shape_mpq_class_relation_with_constraint_2(Term1, Term2, Term3, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_relation_with_generator_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_relation_with_generator)).

ppl_Octagonal_Shape_mpq_class_relation_with_generator(Term1, Term2, Term3) :-
   ppl_Octagonal_Shape_mpq_class_relation_with_generator_2(Term1, Term2, Term3, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_relation_with_congruence_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_relation_with_congruence)).

ppl_Octagonal_Shape_mpq_class_relation_with_congruence(Term1, Term2, Term3) :-
   ppl_Octagonal_Shape_mpq_class_relation_with_congruence_2(Term1, Term2, Term3, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_get_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_get_constraints)).

ppl_Octagonal_Shape_mpq_class_get_constraints(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_get_constraints_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_get_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_get_congruences)).

ppl_Octagonal_Shape_mpq_class_get_congruences(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_get_congruences_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_get_minimized_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_get_minimized_constraints)).

ppl_Octagonal_Shape_mpq_class_get_minimized_constraints(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_get_minimized_constraints_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_get_minimized_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_get_minimized_congruences)).

ppl_Octagonal_Shape_mpq_class_get_minimized_congruences(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_get_minimized_congruences_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_is_empty_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_is_empty)).

ppl_Octagonal_Shape_mpq_class_is_empty(Term1) :-
   ppl_Octagonal_Shape_mpq_class_is_empty_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_is_universe_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_is_universe)).

ppl_Octagonal_Shape_mpq_class_is_universe(Term1) :-
   ppl_Octagonal_Shape_mpq_class_is_universe_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_is_bounded_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_is_bounded)).

ppl_Octagonal_Shape_mpq_class_is_bounded(Term1) :-
   ppl_Octagonal_Shape_mpq_class_is_bounded_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_contains_integer_point_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_contains_integer_point)).

ppl_Octagonal_Shape_mpq_class_contains_integer_point(Term1) :-
   ppl_Octagonal_Shape_mpq_class_contains_integer_point_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_is_topologically_closed_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_is_topologically_closed)).

ppl_Octagonal_Shape_mpq_class_is_topologically_closed(Term1) :-
   ppl_Octagonal_Shape_mpq_class_is_topologically_closed_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_is_discrete_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_is_discrete)).

ppl_Octagonal_Shape_mpq_class_is_discrete(Term1) :-
   ppl_Octagonal_Shape_mpq_class_is_discrete_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_topological_closure_assign(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_bounds_from_above_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_bounds_from_above)).

ppl_Octagonal_Shape_mpq_class_bounds_from_above(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_bounds_from_above_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_bounds_from_below_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_bounds_from_below)).

ppl_Octagonal_Shape_mpq_class_bounds_from_below(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_bounds_from_below_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_maximize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_maximize)).

ppl_Octagonal_Shape_mpq_class_maximize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_mpq_class_maximize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_minimize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_minimize)).

ppl_Octagonal_Shape_mpq_class_minimize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_mpq_class_minimize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_maximize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_maximize_with_point)).

ppl_Octagonal_Shape_mpq_class_maximize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Octagonal_Shape_mpq_class_maximize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_minimize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_minimize_with_point)).

ppl_Octagonal_Shape_mpq_class_minimize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Octagonal_Shape_mpq_class_minimize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_contains_Octagonal_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_contains_Octagonal_Shape_mpq_class)).

ppl_Octagonal_Shape_mpq_class_contains_Octagonal_Shape_mpq_class(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_contains_Octagonal_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_strictly_contains_Octagonal_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_strictly_contains_Octagonal_Shape_mpq_class)).

ppl_Octagonal_Shape_mpq_class_strictly_contains_Octagonal_Shape_mpq_class(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_strictly_contains_Octagonal_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_is_disjoint_from_Octagonal_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_is_disjoint_from_Octagonal_Shape_mpq_class)).

ppl_Octagonal_Shape_mpq_class_is_disjoint_from_Octagonal_Shape_mpq_class(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_is_disjoint_from_Octagonal_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_equals_Octagonal_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_equals_Octagonal_Shape_mpq_class)).

ppl_Octagonal_Shape_mpq_class_equals_Octagonal_Shape_mpq_class(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_equals_Octagonal_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_OK)).

ppl_Octagonal_Shape_mpq_class_OK(Term1) :-
   ppl_Octagonal_Shape_mpq_class_OK_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_add_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_add_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_add_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_add_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_refine_with_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_refine_with_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_refine_with_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_refine_with_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_intersection_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_upper_bound_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_difference_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_concatenate_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_time_elapse_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_upper_bound_assign_if_exact_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_upper_bound_assign_if_exact)).

ppl_Octagonal_Shape_mpq_class_upper_bound_assign_if_exact(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_upper_bound_assign_if_exact_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_simplify_using_context_assign_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_simplify_using_context_assign)).

ppl_Octagonal_Shape_mpq_class_simplify_using_context_assign(Term1, Term2, Term3) :-
   ppl_Octagonal_Shape_mpq_class_simplify_using_context_assign_2(Term1, Term2, Term3, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_constrains_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_constrains)).

ppl_Octagonal_Shape_mpq_class_constrains(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_constrains_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimension)).

ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimension(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimensions)).

ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimensions(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_affine_image(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_bounded_affine_image(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_bounded_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_generalized_affine_image_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_generalized_affine_image)).

ppl_Octagonal_Shape_mpq_class_generalized_affine_image(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_mpq_class_generalized_affine_image_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_generalized_affine_preimage_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_generalized_affine_preimage)).

ppl_Octagonal_Shape_mpq_class_generalized_affine_preimage(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_mpq_class_generalized_affine_preimage_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_generalized_affine_image_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_generalized_affine_image_lhs_rhs)).

ppl_Octagonal_Shape_mpq_class_generalized_affine_image_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_mpq_class_generalized_affine_image_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_generalized_affine_preimage_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_generalized_affine_preimage_lhs_rhs)).

ppl_Octagonal_Shape_mpq_class_generalized_affine_preimage_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_mpq_class_generalized_affine_preimage_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_add_space_dimensions_and_embed(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_add_space_dimensions_and_project(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_remove_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_remove_space_dimensions)).

ppl_Octagonal_Shape_mpq_class_remove_space_dimensions(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_remove_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_remove_higher_space_dimensions(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_expand_space_dimension(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_fold_space_dimensions_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_fold_space_dimensions)).

ppl_Octagonal_Shape_mpq_class_fold_space_dimensions(Term1, Term2, Term3) :-
   ppl_Octagonal_Shape_mpq_class_fold_space_dimensions_2(Term1, Term2, Term3, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_map_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_map_space_dimensions)).

ppl_Octagonal_Shape_mpq_class_map_space_dimensions(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_map_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_ascii_dump_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_ascii_dump)).

ppl_Octagonal_Shape_mpq_class_ascii_dump(Term1) :-
   ppl_Octagonal_Shape_mpq_class_ascii_dump_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_external_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_external_memory_in_bytes)).

ppl_Octagonal_Shape_mpq_class_external_memory_in_bytes(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_external_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_total_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_total_memory_in_bytes)).

ppl_Octagonal_Shape_mpq_class_total_memory_in_bytes(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_total_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_BHMZ05_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_BHMZ05_widening_assign_with_tokens)).

ppl_Octagonal_Shape_mpq_class_BHMZ05_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_mpq_class_BHMZ05_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_BHMZ05_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_widening_assign_with_tokens)).

ppl_Octagonal_Shape_mpq_class_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_mpq_class_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens)).

ppl_Octagonal_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens)).

ppl_Octagonal_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_limited_BHMZ05_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_limited_CC76_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_CC76_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_CC76_extrapolation_assign_with_tokens)).

ppl_Octagonal_Shape_mpq_class_CC76_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_mpq_class_CC76_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_CC76_extrapolation_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_mpq_class_CC76_narrowing_assign_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_CC76_narrowing_assign)).

ppl_Octagonal_Shape_mpq_class_CC76_narrowing_assign(Term1, Term2) :-
   ppl_Octagonal_Shape_mpq_class_CC76_narrowing_assign_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_mpq_class_linear_partition_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_mpq_class_linear_partition)).

ppl_Octagonal_Shape_mpq_class_linear_partition(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_mpq_class_linear_partition_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_delete_Constraints_Product_C_Polyhedron_Grid(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_space_dimension_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_space_dimension)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_space_dimension(Term1, Term2, Term3) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_space_dimension_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_C_Polyhedron)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_C_Polyhedron(Term1, Term2) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_NNC_Polyhedron)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_NNC_Polyhedron(Term1, Term2) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Grid)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Grid(Term1, Term2) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Grid_2(Term1, Term2, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Rational_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Rational_Box)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Rational_Box(Term1, Term2) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Rational_Box_2(Term1, Term2, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpz_class)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpz_class(Term1, Term2) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpq_class)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpq_class(Term1, Term2) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpz_class)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpz_class(Term1, Term2) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpq_class)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpq_class(Term1, Term2) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Double_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Double_Box)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Double_Box(Term1, Term2) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Double_Box_2(Term1, Term2, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_double)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_double(Term1, Term2) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_double)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_double(Term1, Term2) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Constraints_Product_C_Polyhedron_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Constraints_Product_C_Polyhedron_Grid)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Constraints_Product_C_Polyhedron_Grid(Term1, Term2) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Constraints_Product_C_Polyhedron_Grid_2(Term1, Term2, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_C_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_C_Polyhedron_with_complexity)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_C_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_C_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_NNC_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_NNC_Polyhedron_with_complexity)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_NNC_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_NNC_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Grid_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Grid_with_complexity)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Grid_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Grid_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Rational_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Rational_Box_with_complexity)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Rational_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Rational_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpz_class_with_complexity)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpq_class_with_complexity)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpz_class_with_complexity)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpq_class_with_complexity)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Double_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Double_Box_with_complexity)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Double_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Double_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_double_with_complexity)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_double_with_complexity)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Constraints_Product_C_Polyhedron_Grid_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Constraints_Product_C_Polyhedron_Grid_with_complexity)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Constraints_Product_C_Polyhedron_Grid_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Constraints_Product_C_Polyhedron_Grid_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_constraints)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_constraints(Term1, Term2) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_constraints_2(Term1, Term2, 1).

:- true pred ppl_new_Constraints_Product_C_Polyhedron_Grid_from_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Constraints_Product_C_Polyhedron_Grid_from_congruences)).

ppl_new_Constraints_Product_C_Polyhedron_Grid_from_congruences(Term1, Term2) :-
   ppl_new_Constraints_Product_C_Polyhedron_Grid_from_congruences_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_swap(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_space_dimension)).

ppl_Constraints_Product_C_Polyhedron_Grid_space_dimension(Term1, Term2) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_affine_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_affine_dimension)).

ppl_Constraints_Product_C_Polyhedron_Grid_affine_dimension(Term1, Term2) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_affine_dimension_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_constraint_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_constraint)).

ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_constraint(Term1, Term2, Term3) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_constraint_2(Term1, Term2, Term3, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_generator_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_generator)).

ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_generator(Term1, Term2, Term3) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_generator_2(Term1, Term2, Term3, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_congruence_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_congruence)).

ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_congruence(Term1, Term2, Term3) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_congruence_2(Term1, Term2, Term3, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_is_empty_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_is_empty)).

ppl_Constraints_Product_C_Polyhedron_Grid_is_empty(Term1) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_is_empty_2(Term1, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_is_universe_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_is_universe)).

ppl_Constraints_Product_C_Polyhedron_Grid_is_universe(Term1) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_is_universe_2(Term1, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_is_bounded_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_is_bounded)).

ppl_Constraints_Product_C_Polyhedron_Grid_is_bounded(Term1) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_is_bounded_2(Term1, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_is_topologically_closed_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_is_topologically_closed)).

ppl_Constraints_Product_C_Polyhedron_Grid_is_topologically_closed(Term1) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_is_topologically_closed_2(Term1, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_is_discrete_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_is_discrete)).

ppl_Constraints_Product_C_Polyhedron_Grid_is_discrete(Term1) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_is_discrete_2(Term1, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_topological_closure_assign(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_bounds_from_above_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_bounds_from_above)).

ppl_Constraints_Product_C_Polyhedron_Grid_bounds_from_above(Term1, Term2) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_bounds_from_above_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_bounds_from_below_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_bounds_from_below)).

ppl_Constraints_Product_C_Polyhedron_Grid_bounds_from_below(Term1, Term2) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_bounds_from_below_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_maximize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_maximize)).

ppl_Constraints_Product_C_Polyhedron_Grid_maximize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_maximize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_minimize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_minimize)).

ppl_Constraints_Product_C_Polyhedron_Grid_minimize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_minimize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_maximize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_maximize_with_point)).

ppl_Constraints_Product_C_Polyhedron_Grid_maximize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_maximize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_minimize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_minimize_with_point)).

ppl_Constraints_Product_C_Polyhedron_Grid_minimize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_minimize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_contains_Constraints_Product_C_Polyhedron_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_contains_Constraints_Product_C_Polyhedron_Grid)).

ppl_Constraints_Product_C_Polyhedron_Grid_contains_Constraints_Product_C_Polyhedron_Grid(Term1, Term2) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_contains_Constraints_Product_C_Polyhedron_Grid_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_strictly_contains_Constraints_Product_C_Polyhedron_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_strictly_contains_Constraints_Product_C_Polyhedron_Grid)).

ppl_Constraints_Product_C_Polyhedron_Grid_strictly_contains_Constraints_Product_C_Polyhedron_Grid(Term1, Term2) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_strictly_contains_Constraints_Product_C_Polyhedron_Grid_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_is_disjoint_from_Constraints_Product_C_Polyhedron_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_is_disjoint_from_Constraints_Product_C_Polyhedron_Grid)).

ppl_Constraints_Product_C_Polyhedron_Grid_is_disjoint_from_Constraints_Product_C_Polyhedron_Grid(Term1, Term2) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_is_disjoint_from_Constraints_Product_C_Polyhedron_Grid_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_equals_Constraints_Product_C_Polyhedron_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_equals_Constraints_Product_C_Polyhedron_Grid)).

ppl_Constraints_Product_C_Polyhedron_Grid_equals_Constraints_Product_C_Polyhedron_Grid(Term1, Term2) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_equals_Constraints_Product_C_Polyhedron_Grid_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_OK)).

ppl_Constraints_Product_C_Polyhedron_Grid_OK(Term1) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_OK_2(Term1, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_add_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_add_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_add_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_add_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_refine_with_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_refine_with_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_refine_with_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_refine_with_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_intersection_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_upper_bound_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_difference_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_concatenate_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_time_elapse_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_upper_bound_assign_if_exact_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_upper_bound_assign_if_exact)).

ppl_Constraints_Product_C_Polyhedron_Grid_upper_bound_assign_if_exact(Term1, Term2) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_upper_bound_assign_if_exact_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_constrains_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_constrains)).

ppl_Constraints_Product_C_Polyhedron_Grid_constrains(Term1, Term2) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_constrains_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_unconstrain_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_unconstrain_space_dimension)).

ppl_Constraints_Product_C_Polyhedron_Grid_unconstrain_space_dimension(Term1, Term2) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_unconstrain_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_unconstrain_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_unconstrain_space_dimensions)).

ppl_Constraints_Product_C_Polyhedron_Grid_unconstrain_space_dimensions(Term1, Term2) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_unconstrain_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_affine_image(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_bounded_affine_image(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_bounded_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_image_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_image)).

ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_image(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_image_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_preimage_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_preimage)).

ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_preimage(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_preimage_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_image_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_image_lhs_rhs)).

ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_image_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_image_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_preimage_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_preimage_lhs_rhs)).

ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_preimage_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_preimage_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_add_space_dimensions_and_embed(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_add_space_dimensions_and_project(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_remove_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_remove_space_dimensions)).

ppl_Constraints_Product_C_Polyhedron_Grid_remove_space_dimensions(Term1, Term2) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_remove_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_remove_higher_space_dimensions(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_expand_space_dimension(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_fold_space_dimensions_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_fold_space_dimensions)).

ppl_Constraints_Product_C_Polyhedron_Grid_fold_space_dimensions(Term1, Term2, Term3) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_fold_space_dimensions_2(Term1, Term2, Term3, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_map_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_map_space_dimensions)).

ppl_Constraints_Product_C_Polyhedron_Grid_map_space_dimensions(Term1, Term2) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_map_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_ascii_dump_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_ascii_dump)).

ppl_Constraints_Product_C_Polyhedron_Grid_ascii_dump(Term1) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_ascii_dump_2(Term1, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_external_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_external_memory_in_bytes)).

ppl_Constraints_Product_C_Polyhedron_Grid_external_memory_in_bytes(Term1, Term2) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_external_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_total_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_total_memory_in_bytes)).

ppl_Constraints_Product_C_Polyhedron_Grid_total_memory_in_bytes(Term1, Term2) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_total_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Constraints_Product_C_Polyhedron_Grid_widening_assign_with_tokens)).

ppl_Constraints_Product_C_Polyhedron_Grid_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Constraints_Product_C_Polyhedron_Grid_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Constraints_Product_C_Polyhedron_Grid_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_delete_Pointset_Powerset_C_Polyhedron(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_new_Pointset_Powerset_C_Polyhedron_from_space_dimension_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Pointset_Powerset_C_Polyhedron_from_space_dimension)).

ppl_new_Pointset_Powerset_C_Polyhedron_from_space_dimension(Term1, Term2, Term3) :-
   ppl_new_Pointset_Powerset_C_Polyhedron_from_space_dimension_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron)).

ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron(Term1, Term2) :-
   ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron)).

ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron(Term1, Term2) :-
   ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron_with_complexity)).

ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron_with_complexity)).

ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Pointset_Powerset_C_Polyhedron_from_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Pointset_Powerset_C_Polyhedron_from_constraints)).

ppl_new_Pointset_Powerset_C_Polyhedron_from_constraints(Term1, Term2) :-
   ppl_new_Pointset_Powerset_C_Polyhedron_from_constraints_2(Term1, Term2, 1).

:- true pred ppl_new_Pointset_Powerset_C_Polyhedron_from_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Pointset_Powerset_C_Polyhedron_from_congruences)).

ppl_new_Pointset_Powerset_C_Polyhedron_from_congruences(Term1, Term2) :-
   ppl_new_Pointset_Powerset_C_Polyhedron_from_congruences_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_swap(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_space_dimension)).

ppl_Pointset_Powerset_C_Polyhedron_space_dimension(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_affine_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_affine_dimension)).

ppl_Pointset_Powerset_C_Polyhedron_affine_dimension(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_affine_dimension_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_relation_with_constraint_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_relation_with_constraint)).

ppl_Pointset_Powerset_C_Polyhedron_relation_with_constraint(Term1, Term2, Term3) :-
   ppl_Pointset_Powerset_C_Polyhedron_relation_with_constraint_2(Term1, Term2, Term3, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_relation_with_generator_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_relation_with_generator)).

ppl_Pointset_Powerset_C_Polyhedron_relation_with_generator(Term1, Term2, Term3) :-
   ppl_Pointset_Powerset_C_Polyhedron_relation_with_generator_2(Term1, Term2, Term3, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_relation_with_congruence_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_relation_with_congruence)).

ppl_Pointset_Powerset_C_Polyhedron_relation_with_congruence(Term1, Term2, Term3) :-
   ppl_Pointset_Powerset_C_Polyhedron_relation_with_congruence_2(Term1, Term2, Term3, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_is_empty_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_is_empty)).

ppl_Pointset_Powerset_C_Polyhedron_is_empty(Term1) :-
   ppl_Pointset_Powerset_C_Polyhedron_is_empty_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_is_universe_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_is_universe)).

ppl_Pointset_Powerset_C_Polyhedron_is_universe(Term1) :-
   ppl_Pointset_Powerset_C_Polyhedron_is_universe_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_is_bounded_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_is_bounded)).

ppl_Pointset_Powerset_C_Polyhedron_is_bounded(Term1) :-
   ppl_Pointset_Powerset_C_Polyhedron_is_bounded_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_contains_integer_point_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_contains_integer_point)).

ppl_Pointset_Powerset_C_Polyhedron_contains_integer_point(Term1) :-
   ppl_Pointset_Powerset_C_Polyhedron_contains_integer_point_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_is_topologically_closed_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_is_topologically_closed)).

ppl_Pointset_Powerset_C_Polyhedron_is_topologically_closed(Term1) :-
   ppl_Pointset_Powerset_C_Polyhedron_is_topologically_closed_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_is_discrete_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_is_discrete)).

ppl_Pointset_Powerset_C_Polyhedron_is_discrete(Term1) :-
   ppl_Pointset_Powerset_C_Polyhedron_is_discrete_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_pairwise_reduce(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_omega_reduce(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_topological_closure_assign(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_bounds_from_above_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_bounds_from_above)).

ppl_Pointset_Powerset_C_Polyhedron_bounds_from_above(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_bounds_from_above_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_bounds_from_below_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_bounds_from_below)).

ppl_Pointset_Powerset_C_Polyhedron_bounds_from_below(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_bounds_from_below_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_maximize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_maximize)).

ppl_Pointset_Powerset_C_Polyhedron_maximize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Pointset_Powerset_C_Polyhedron_maximize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_minimize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_minimize)).

ppl_Pointset_Powerset_C_Polyhedron_minimize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Pointset_Powerset_C_Polyhedron_minimize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_maximize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_maximize_with_point)).

ppl_Pointset_Powerset_C_Polyhedron_maximize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Pointset_Powerset_C_Polyhedron_maximize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_minimize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_minimize_with_point)).

ppl_Pointset_Powerset_C_Polyhedron_minimize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Pointset_Powerset_C_Polyhedron_minimize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_contains_Pointset_Powerset_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_contains_Pointset_Powerset_C_Polyhedron)).

ppl_Pointset_Powerset_C_Polyhedron_contains_Pointset_Powerset_C_Polyhedron(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_contains_Pointset_Powerset_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_strictly_contains_Pointset_Powerset_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_strictly_contains_Pointset_Powerset_C_Polyhedron)).

ppl_Pointset_Powerset_C_Polyhedron_strictly_contains_Pointset_Powerset_C_Polyhedron(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_strictly_contains_Pointset_Powerset_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_is_disjoint_from_Pointset_Powerset_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_is_disjoint_from_Pointset_Powerset_C_Polyhedron)).

ppl_Pointset_Powerset_C_Polyhedron_is_disjoint_from_Pointset_Powerset_C_Polyhedron(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_is_disjoint_from_Pointset_Powerset_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_geometrically_covers_Pointset_Powerset_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_geometrically_covers_Pointset_Powerset_C_Polyhedron)).

ppl_Pointset_Powerset_C_Polyhedron_geometrically_covers_Pointset_Powerset_C_Polyhedron(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_geometrically_covers_Pointset_Powerset_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_geometrically_equals_Pointset_Powerset_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_geometrically_equals_Pointset_Powerset_C_Polyhedron)).

ppl_Pointset_Powerset_C_Polyhedron_geometrically_equals_Pointset_Powerset_C_Polyhedron(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_geometrically_equals_Pointset_Powerset_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_equals_Pointset_Powerset_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_equals_Pointset_Powerset_C_Polyhedron)).

ppl_Pointset_Powerset_C_Polyhedron_equals_Pointset_Powerset_C_Polyhedron(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_equals_Pointset_Powerset_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_OK)).

ppl_Pointset_Powerset_C_Polyhedron_OK(Term1) :-
   ppl_Pointset_Powerset_C_Polyhedron_OK_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_add_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_add_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_add_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_add_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_refine_with_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_refine_with_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_refine_with_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_refine_with_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_intersection_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_upper_bound_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_difference_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_concatenate_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_time_elapse_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_upper_bound_assign_if_exact_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_upper_bound_assign_if_exact)).

ppl_Pointset_Powerset_C_Polyhedron_upper_bound_assign_if_exact(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_upper_bound_assign_if_exact_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_simplify_using_context_assign_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_simplify_using_context_assign)).

ppl_Pointset_Powerset_C_Polyhedron_simplify_using_context_assign(Term1, Term2, Term3) :-
   ppl_Pointset_Powerset_C_Polyhedron_simplify_using_context_assign_2(Term1, Term2, Term3, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_constrains_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_constrains)).

ppl_Pointset_Powerset_C_Polyhedron_constrains(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_constrains_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_unconstrain_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_unconstrain_space_dimension)).

ppl_Pointset_Powerset_C_Polyhedron_unconstrain_space_dimension(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_unconstrain_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_unconstrain_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_unconstrain_space_dimensions)).

ppl_Pointset_Powerset_C_Polyhedron_unconstrain_space_dimensions(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_unconstrain_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_affine_image(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_bounded_affine_image(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_bounded_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_image_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_image)).

ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_image(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_image_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_preimage_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_preimage)).

ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_preimage(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_preimage_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_image_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_image_lhs_rhs)).

ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_image_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_image_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_preimage_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_preimage_lhs_rhs)).

ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_preimage_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_preimage_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_add_space_dimensions_and_embed(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_add_space_dimensions_and_project(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_remove_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_remove_space_dimensions)).

ppl_Pointset_Powerset_C_Polyhedron_remove_space_dimensions(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_remove_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_remove_higher_space_dimensions(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_expand_space_dimension(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_fold_space_dimensions_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_fold_space_dimensions)).

ppl_Pointset_Powerset_C_Polyhedron_fold_space_dimensions(Term1, Term2, Term3) :-
   ppl_Pointset_Powerset_C_Polyhedron_fold_space_dimensions_2(Term1, Term2, Term3, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_map_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_map_space_dimensions)).

ppl_Pointset_Powerset_C_Polyhedron_map_space_dimensions(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_map_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_ascii_dump_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_ascii_dump)).

ppl_Pointset_Powerset_C_Polyhedron_ascii_dump(Term1) :-
   ppl_Pointset_Powerset_C_Polyhedron_ascii_dump_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_external_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_external_memory_in_bytes)).

ppl_Pointset_Powerset_C_Polyhedron_external_memory_in_bytes(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_external_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_total_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_total_memory_in_bytes)).

ppl_Pointset_Powerset_C_Polyhedron_total_memory_in_bytes(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_total_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_size_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_size)).

ppl_Pointset_Powerset_C_Polyhedron_size(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_size_2(Term1, Term2, 1).

:- true pred ppl_new_Pointset_Powerset_C_Polyhedron_iterator_from_iterator_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Pointset_Powerset_C_Polyhedron_iterator_from_iterator)).

ppl_new_Pointset_Powerset_C_Polyhedron_iterator_from_iterator(Term1, Term2) :-
   ppl_new_Pointset_Powerset_C_Polyhedron_iterator_from_iterator_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_begin_iterator_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_begin_iterator)).

ppl_Pointset_Powerset_C_Polyhedron_begin_iterator(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_begin_iterator_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_end_iterator_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_end_iterator)).

ppl_Pointset_Powerset_C_Polyhedron_end_iterator(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_end_iterator_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_iterator_equals_iterator_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_iterator_equals_iterator)).

ppl_Pointset_Powerset_C_Polyhedron_iterator_equals_iterator(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_iterator_equals_iterator_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_increment_iterator_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_increment_iterator)).

ppl_Pointset_Powerset_C_Polyhedron_increment_iterator(Term1) :-
   ppl_Pointset_Powerset_C_Polyhedron_increment_iterator_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_decrement_iterator_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_decrement_iterator)).

ppl_Pointset_Powerset_C_Polyhedron_decrement_iterator(Term1) :-
   ppl_Pointset_Powerset_C_Polyhedron_decrement_iterator_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_get_disjunct_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_get_disjunct)).

ppl_Pointset_Powerset_C_Polyhedron_get_disjunct(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_get_disjunct_2(Term1, Term2, 1).

:- true pred ppl_delete_Pointset_Powerset_C_Polyhedron_iterator_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_delete_Pointset_Powerset_C_Polyhedron_iterator)).

ppl_delete_Pointset_Powerset_C_Polyhedron_iterator(Term1) :-
   ppl_delete_Pointset_Powerset_C_Polyhedron_iterator_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_add_disjunct(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_C_Polyhedron_drop_disjunct_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_drop_disjunct)).

ppl_Pointset_Powerset_C_Polyhedron_drop_disjunct(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_drop_disjunct_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_drop_disjuncts_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_drop_disjuncts)).

ppl_Pointset_Powerset_C_Polyhedron_drop_disjuncts(Term1, Term2, Term3) :-
   ppl_Pointset_Powerset_C_Polyhedron_drop_disjuncts_2(Term1, Term2, Term3, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_BHZ03_BHRZ03_BHRZ03_widening_assign_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_BHZ03_BHRZ03_BHRZ03_widening_assign)).

ppl_Pointset_Powerset_C_Polyhedron_BHZ03_BHRZ03_BHRZ03_widening_assign(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_BHZ03_BHRZ03_BHRZ03_widening_assign_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_BHZ03_H79_H79_widening_assign_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_BHZ03_H79_H79_widening_assign)).

ppl_Pointset_Powerset_C_Polyhedron_BHZ03_H79_H79_widening_assign(Term1, Term2) :-
   ppl_Pointset_Powerset_C_Polyhedron_BHZ03_H79_H79_widening_assign_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_BGP99_BHRZ03_extrapolation_assign_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_BGP99_BHRZ03_extrapolation_assign)).

ppl_Pointset_Powerset_C_Polyhedron_BGP99_BHRZ03_extrapolation_assign(Term1, Term2, Term3) :-
   ppl_Pointset_Powerset_C_Polyhedron_BGP99_BHRZ03_extrapolation_assign_2(Term1, Term2, Term3, 1).

:- true pred ppl_Pointset_Powerset_C_Polyhedron_BGP99_H79_extrapolation_assign_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_C_Polyhedron_BGP99_H79_extrapolation_assign)).

ppl_Pointset_Powerset_C_Polyhedron_BGP99_H79_extrapolation_assign(Term1, Term2, Term3) :-
   ppl_Pointset_Powerset_C_Polyhedron_BGP99_H79_extrapolation_assign_2(Term1, Term2, Term3, 1).

:- true pred ppl_delete_Pointset_Powerset_NNC_Polyhedron(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_new_Pointset_Powerset_NNC_Polyhedron_from_space_dimension_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Pointset_Powerset_NNC_Polyhedron_from_space_dimension)).

ppl_new_Pointset_Powerset_NNC_Polyhedron_from_space_dimension(Term1, Term2, Term3) :-
   ppl_new_Pointset_Powerset_NNC_Polyhedron_from_space_dimension_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron)).

ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron(Term1, Term2) :-
   ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron)).

ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron(Term1, Term2) :-
   ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron_with_complexity)).

ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity)).

ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Pointset_Powerset_NNC_Polyhedron_from_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Pointset_Powerset_NNC_Polyhedron_from_constraints)).

ppl_new_Pointset_Powerset_NNC_Polyhedron_from_constraints(Term1, Term2) :-
   ppl_new_Pointset_Powerset_NNC_Polyhedron_from_constraints_2(Term1, Term2, 1).

:- true pred ppl_new_Pointset_Powerset_NNC_Polyhedron_from_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Pointset_Powerset_NNC_Polyhedron_from_congruences)).

ppl_new_Pointset_Powerset_NNC_Polyhedron_from_congruences(Term1, Term2) :-
   ppl_new_Pointset_Powerset_NNC_Polyhedron_from_congruences_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_swap(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_space_dimension)).

ppl_Pointset_Powerset_NNC_Polyhedron_space_dimension(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_affine_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_affine_dimension)).

ppl_Pointset_Powerset_NNC_Polyhedron_affine_dimension(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_affine_dimension_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_constraint_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_constraint)).

ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_constraint(Term1, Term2, Term3) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_constraint_2(Term1, Term2, Term3, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_generator_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_generator)).

ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_generator(Term1, Term2, Term3) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_generator_2(Term1, Term2, Term3, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_congruence_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_congruence)).

ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_congruence(Term1, Term2, Term3) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_congruence_2(Term1, Term2, Term3, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_is_empty_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_is_empty)).

ppl_Pointset_Powerset_NNC_Polyhedron_is_empty(Term1) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_is_empty_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_is_universe_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_is_universe)).

ppl_Pointset_Powerset_NNC_Polyhedron_is_universe(Term1) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_is_universe_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_is_bounded_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_is_bounded)).

ppl_Pointset_Powerset_NNC_Polyhedron_is_bounded(Term1) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_is_bounded_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_contains_integer_point_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_contains_integer_point)).

ppl_Pointset_Powerset_NNC_Polyhedron_contains_integer_point(Term1) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_contains_integer_point_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_is_topologically_closed_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_is_topologically_closed)).

ppl_Pointset_Powerset_NNC_Polyhedron_is_topologically_closed(Term1) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_is_topologically_closed_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_is_discrete_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_is_discrete)).

ppl_Pointset_Powerset_NNC_Polyhedron_is_discrete(Term1) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_is_discrete_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_pairwise_reduce(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_omega_reduce(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_topological_closure_assign(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_bounds_from_above_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_bounds_from_above)).

ppl_Pointset_Powerset_NNC_Polyhedron_bounds_from_above(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_bounds_from_above_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_bounds_from_below_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_bounds_from_below)).

ppl_Pointset_Powerset_NNC_Polyhedron_bounds_from_below(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_bounds_from_below_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_maximize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_maximize)).

ppl_Pointset_Powerset_NNC_Polyhedron_maximize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_maximize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_minimize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_minimize)).

ppl_Pointset_Powerset_NNC_Polyhedron_minimize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_minimize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_maximize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_maximize_with_point)).

ppl_Pointset_Powerset_NNC_Polyhedron_maximize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_maximize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_minimize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_minimize_with_point)).

ppl_Pointset_Powerset_NNC_Polyhedron_minimize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_minimize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_contains_Pointset_Powerset_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_contains_Pointset_Powerset_NNC_Polyhedron)).

ppl_Pointset_Powerset_NNC_Polyhedron_contains_Pointset_Powerset_NNC_Polyhedron(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_contains_Pointset_Powerset_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_strictly_contains_Pointset_Powerset_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_strictly_contains_Pointset_Powerset_NNC_Polyhedron)).

ppl_Pointset_Powerset_NNC_Polyhedron_strictly_contains_Pointset_Powerset_NNC_Polyhedron(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_strictly_contains_Pointset_Powerset_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_is_disjoint_from_Pointset_Powerset_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_is_disjoint_from_Pointset_Powerset_NNC_Polyhedron)).

ppl_Pointset_Powerset_NNC_Polyhedron_is_disjoint_from_Pointset_Powerset_NNC_Polyhedron(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_is_disjoint_from_Pointset_Powerset_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_covers_Pointset_Powerset_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_covers_Pointset_Powerset_NNC_Polyhedron)).

ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_covers_Pointset_Powerset_NNC_Polyhedron(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_covers_Pointset_Powerset_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_equals_Pointset_Powerset_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_equals_Pointset_Powerset_NNC_Polyhedron)).

ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_equals_Pointset_Powerset_NNC_Polyhedron(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_equals_Pointset_Powerset_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_equals_Pointset_Powerset_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_equals_Pointset_Powerset_NNC_Polyhedron)).

ppl_Pointset_Powerset_NNC_Polyhedron_equals_Pointset_Powerset_NNC_Polyhedron(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_equals_Pointset_Powerset_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_OK)).

ppl_Pointset_Powerset_NNC_Polyhedron_OK(Term1) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_OK_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_add_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_add_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_add_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_add_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_refine_with_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_refine_with_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_refine_with_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_refine_with_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_intersection_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_upper_bound_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_difference_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_concatenate_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_time_elapse_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_upper_bound_assign_if_exact_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_upper_bound_assign_if_exact)).

ppl_Pointset_Powerset_NNC_Polyhedron_upper_bound_assign_if_exact(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_upper_bound_assign_if_exact_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_simplify_using_context_assign_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_simplify_using_context_assign)).

ppl_Pointset_Powerset_NNC_Polyhedron_simplify_using_context_assign(Term1, Term2, Term3) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_simplify_using_context_assign_2(Term1, Term2, Term3, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_constrains_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_constrains)).

ppl_Pointset_Powerset_NNC_Polyhedron_constrains(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_constrains_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimension)).

ppl_Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimension(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimensions)).

ppl_Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimensions(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_affine_image(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_bounded_affine_image(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_bounded_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_image_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_image)).

ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_image(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_image_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_preimage_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_preimage)).

ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_preimage(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_preimage_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_image_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_image_lhs_rhs)).

ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_image_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_image_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_preimage_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_preimage_lhs_rhs)).

ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_preimage_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_preimage_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_add_space_dimensions_and_embed(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_add_space_dimensions_and_project(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_remove_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_remove_space_dimensions)).

ppl_Pointset_Powerset_NNC_Polyhedron_remove_space_dimensions(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_remove_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_remove_higher_space_dimensions(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_expand_space_dimension(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_fold_space_dimensions_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_fold_space_dimensions)).

ppl_Pointset_Powerset_NNC_Polyhedron_fold_space_dimensions(Term1, Term2, Term3) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_fold_space_dimensions_2(Term1, Term2, Term3, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_map_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_map_space_dimensions)).

ppl_Pointset_Powerset_NNC_Polyhedron_map_space_dimensions(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_map_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_ascii_dump_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_ascii_dump)).

ppl_Pointset_Powerset_NNC_Polyhedron_ascii_dump(Term1) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_ascii_dump_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_external_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_external_memory_in_bytes)).

ppl_Pointset_Powerset_NNC_Polyhedron_external_memory_in_bytes(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_external_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_total_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_total_memory_in_bytes)).

ppl_Pointset_Powerset_NNC_Polyhedron_total_memory_in_bytes(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_total_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_size_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_size)).

ppl_Pointset_Powerset_NNC_Polyhedron_size(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_size_2(Term1, Term2, 1).

:- true pred ppl_new_Pointset_Powerset_NNC_Polyhedron_iterator_from_iterator_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Pointset_Powerset_NNC_Polyhedron_iterator_from_iterator)).

ppl_new_Pointset_Powerset_NNC_Polyhedron_iterator_from_iterator(Term1, Term2) :-
   ppl_new_Pointset_Powerset_NNC_Polyhedron_iterator_from_iterator_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_begin_iterator_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_begin_iterator)).

ppl_Pointset_Powerset_NNC_Polyhedron_begin_iterator(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_begin_iterator_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_end_iterator_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_end_iterator)).

ppl_Pointset_Powerset_NNC_Polyhedron_end_iterator(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_end_iterator_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_iterator_equals_iterator_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_iterator_equals_iterator)).

ppl_Pointset_Powerset_NNC_Polyhedron_iterator_equals_iterator(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_iterator_equals_iterator_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_increment_iterator_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_increment_iterator)).

ppl_Pointset_Powerset_NNC_Polyhedron_increment_iterator(Term1) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_increment_iterator_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_decrement_iterator_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_decrement_iterator)).

ppl_Pointset_Powerset_NNC_Polyhedron_decrement_iterator(Term1) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_decrement_iterator_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_get_disjunct_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_get_disjunct)).

ppl_Pointset_Powerset_NNC_Polyhedron_get_disjunct(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_get_disjunct_2(Term1, Term2, 1).

:- true pred ppl_delete_Pointset_Powerset_NNC_Polyhedron_iterator_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_delete_Pointset_Powerset_NNC_Polyhedron_iterator)).

ppl_delete_Pointset_Powerset_NNC_Polyhedron_iterator(Term1) :-
   ppl_delete_Pointset_Powerset_NNC_Polyhedron_iterator_2(Term1, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_add_disjunct(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_drop_disjunct_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_drop_disjunct)).

ppl_Pointset_Powerset_NNC_Polyhedron_drop_disjunct(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_drop_disjunct_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_drop_disjuncts_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_drop_disjuncts)).

ppl_Pointset_Powerset_NNC_Polyhedron_drop_disjuncts(Term1, Term2, Term3) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_drop_disjuncts_2(Term1, Term2, Term3, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_BHZ03_BHRZ03_BHRZ03_widening_assign_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_BHZ03_BHRZ03_BHRZ03_widening_assign)).

ppl_Pointset_Powerset_NNC_Polyhedron_BHZ03_BHRZ03_BHRZ03_widening_assign(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_BHZ03_BHRZ03_BHRZ03_widening_assign_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_BHZ03_H79_H79_widening_assign_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_BHZ03_H79_H79_widening_assign)).

ppl_Pointset_Powerset_NNC_Polyhedron_BHZ03_H79_H79_widening_assign(Term1, Term2) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_BHZ03_H79_H79_widening_assign_2(Term1, Term2, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_BGP99_BHRZ03_extrapolation_assign_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_BGP99_BHRZ03_extrapolation_assign)).

ppl_Pointset_Powerset_NNC_Polyhedron_BGP99_BHRZ03_extrapolation_assign(Term1, Term2, Term3) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_BGP99_BHRZ03_extrapolation_assign_2(Term1, Term2, Term3, 1).

:- true pred ppl_Pointset_Powerset_NNC_Polyhedron_BGP99_H79_extrapolation_assign_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Pointset_Powerset_NNC_Polyhedron_BGP99_H79_extrapolation_assign)).

ppl_Pointset_Powerset_NNC_Polyhedron_BGP99_H79_extrapolation_assign(Term1, Term2, Term3) :-
   ppl_Pointset_Powerset_NNC_Polyhedron_BGP99_H79_extrapolation_assign_2(Term1, Term2, Term3, 1).

:- true pred ppl_delete_Double_Box(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_new_Double_Box_from_space_dimension_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_space_dimension)).

ppl_new_Double_Box_from_space_dimension(Term1, Term2, Term3) :-
   ppl_new_Double_Box_from_space_dimension_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Double_Box_from_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_C_Polyhedron)).

ppl_new_Double_Box_from_C_Polyhedron(Term1, Term2) :-
   ppl_new_Double_Box_from_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Double_Box_from_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_NNC_Polyhedron)).

ppl_new_Double_Box_from_NNC_Polyhedron(Term1, Term2) :-
   ppl_new_Double_Box_from_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Double_Box_from_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_Grid)).

ppl_new_Double_Box_from_Grid(Term1, Term2) :-
   ppl_new_Double_Box_from_Grid_2(Term1, Term2, 1).

:- true pred ppl_new_Double_Box_from_Rational_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_Rational_Box)).

ppl_new_Double_Box_from_Rational_Box(Term1, Term2) :-
   ppl_new_Double_Box_from_Rational_Box_2(Term1, Term2, 1).

:- true pred ppl_new_Double_Box_from_BD_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_BD_Shape_mpz_class)).

ppl_new_Double_Box_from_BD_Shape_mpz_class(Term1, Term2) :-
   ppl_new_Double_Box_from_BD_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_Double_Box_from_BD_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_BD_Shape_mpq_class)).

ppl_new_Double_Box_from_BD_Shape_mpq_class(Term1, Term2) :-
   ppl_new_Double_Box_from_BD_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_Double_Box_from_Octagonal_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_Octagonal_Shape_mpz_class)).

ppl_new_Double_Box_from_Octagonal_Shape_mpz_class(Term1, Term2) :-
   ppl_new_Double_Box_from_Octagonal_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_Double_Box_from_Octagonal_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_Octagonal_Shape_mpq_class)).

ppl_new_Double_Box_from_Octagonal_Shape_mpq_class(Term1, Term2) :-
   ppl_new_Double_Box_from_Octagonal_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_Double_Box_from_Double_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_Double_Box)).

ppl_new_Double_Box_from_Double_Box(Term1, Term2) :-
   ppl_new_Double_Box_from_Double_Box_2(Term1, Term2, 1).

:- true pred ppl_new_Double_Box_from_BD_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_BD_Shape_double)).

ppl_new_Double_Box_from_BD_Shape_double(Term1, Term2) :-
   ppl_new_Double_Box_from_BD_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_Double_Box_from_Octagonal_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_Octagonal_Shape_double)).

ppl_new_Double_Box_from_Octagonal_Shape_double(Term1, Term2) :-
   ppl_new_Double_Box_from_Octagonal_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_Double_Box_from_C_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_C_Polyhedron_with_complexity)).

ppl_new_Double_Box_from_C_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Double_Box_from_C_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Double_Box_from_NNC_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_NNC_Polyhedron_with_complexity)).

ppl_new_Double_Box_from_NNC_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Double_Box_from_NNC_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Double_Box_from_Grid_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_Grid_with_complexity)).

ppl_new_Double_Box_from_Grid_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Double_Box_from_Grid_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Double_Box_from_Rational_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_Rational_Box_with_complexity)).

ppl_new_Double_Box_from_Rational_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Double_Box_from_Rational_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Double_Box_from_BD_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_BD_Shape_mpz_class_with_complexity)).

ppl_new_Double_Box_from_BD_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Double_Box_from_BD_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Double_Box_from_BD_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_BD_Shape_mpq_class_with_complexity)).

ppl_new_Double_Box_from_BD_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Double_Box_from_BD_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Double_Box_from_Octagonal_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_Octagonal_Shape_mpz_class_with_complexity)).

ppl_new_Double_Box_from_Octagonal_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Double_Box_from_Octagonal_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Double_Box_from_Octagonal_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_Octagonal_Shape_mpq_class_with_complexity)).

ppl_new_Double_Box_from_Octagonal_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Double_Box_from_Octagonal_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Double_Box_from_Double_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_Double_Box_with_complexity)).

ppl_new_Double_Box_from_Double_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Double_Box_from_Double_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Double_Box_from_BD_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_BD_Shape_double_with_complexity)).

ppl_new_Double_Box_from_BD_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Double_Box_from_BD_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Double_Box_from_Octagonal_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_Octagonal_Shape_double_with_complexity)).

ppl_new_Double_Box_from_Octagonal_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Double_Box_from_Octagonal_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Double_Box_from_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_constraints)).

ppl_new_Double_Box_from_constraints(Term1, Term2) :-
   ppl_new_Double_Box_from_constraints_2(Term1, Term2, 1).

:- true pred ppl_new_Double_Box_from_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_congruences)).

ppl_new_Double_Box_from_congruences(Term1, Term2) :-
   ppl_new_Double_Box_from_congruences_2(Term1, Term2, 1).

:- true pred ppl_new_Double_Box_from_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Double_Box_from_generators)).

ppl_new_Double_Box_from_generators(Term1, Term2) :-
   ppl_new_Double_Box_from_generators_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_swap(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_space_dimension)).

ppl_Double_Box_space_dimension(Term1, Term2) :-
   ppl_Double_Box_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_affine_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_affine_dimension)).

ppl_Double_Box_affine_dimension(Term1, Term2) :-
   ppl_Double_Box_affine_dimension_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_relation_with_constraint_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_relation_with_constraint)).

ppl_Double_Box_relation_with_constraint(Term1, Term2, Term3) :-
   ppl_Double_Box_relation_with_constraint_2(Term1, Term2, Term3, 1).

:- true pred ppl_Double_Box_relation_with_generator_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_relation_with_generator)).

ppl_Double_Box_relation_with_generator(Term1, Term2, Term3) :-
   ppl_Double_Box_relation_with_generator_2(Term1, Term2, Term3, 1).

:- true pred ppl_Double_Box_relation_with_congruence_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_relation_with_congruence)).

ppl_Double_Box_relation_with_congruence(Term1, Term2, Term3) :-
   ppl_Double_Box_relation_with_congruence_2(Term1, Term2, Term3, 1).

:- true pred ppl_Double_Box_get_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_get_constraints)).

ppl_Double_Box_get_constraints(Term1, Term2) :-
   ppl_Double_Box_get_constraints_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_get_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_get_congruences)).

ppl_Double_Box_get_congruences(Term1, Term2) :-
   ppl_Double_Box_get_congruences_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_get_minimized_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_get_minimized_constraints)).

ppl_Double_Box_get_minimized_constraints(Term1, Term2) :-
   ppl_Double_Box_get_minimized_constraints_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_get_minimized_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_get_minimized_congruences)).

ppl_Double_Box_get_minimized_congruences(Term1, Term2) :-
   ppl_Double_Box_get_minimized_congruences_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_is_empty_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Double_Box_is_empty)).

ppl_Double_Box_is_empty(Term1) :-
   ppl_Double_Box_is_empty_2(Term1, 1).

:- true pred ppl_Double_Box_is_universe_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Double_Box_is_universe)).

ppl_Double_Box_is_universe(Term1) :-
   ppl_Double_Box_is_universe_2(Term1, 1).

:- true pred ppl_Double_Box_is_bounded_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Double_Box_is_bounded)).

ppl_Double_Box_is_bounded(Term1) :-
   ppl_Double_Box_is_bounded_2(Term1, 1).

:- true pred ppl_Double_Box_contains_integer_point_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Double_Box_contains_integer_point)).

ppl_Double_Box_contains_integer_point(Term1) :-
   ppl_Double_Box_contains_integer_point_2(Term1, 1).

:- true pred ppl_Double_Box_is_topologically_closed_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Double_Box_is_topologically_closed)).

ppl_Double_Box_is_topologically_closed(Term1) :-
   ppl_Double_Box_is_topologically_closed_2(Term1, 1).

:- true pred ppl_Double_Box_is_discrete_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Double_Box_is_discrete)).

ppl_Double_Box_is_discrete(Term1) :-
   ppl_Double_Box_is_discrete_2(Term1, 1).

:- true pred ppl_Double_Box_topological_closure_assign(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_Double_Box_bounds_from_above_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_bounds_from_above)).

ppl_Double_Box_bounds_from_above(Term1, Term2) :-
   ppl_Double_Box_bounds_from_above_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_bounds_from_below_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_bounds_from_below)).

ppl_Double_Box_bounds_from_below(Term1, Term2) :-
   ppl_Double_Box_bounds_from_below_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_maximize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_maximize)).

ppl_Double_Box_maximize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Double_Box_maximize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Double_Box_minimize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_minimize)).

ppl_Double_Box_minimize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Double_Box_minimize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Double_Box_maximize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_maximize_with_point)).

ppl_Double_Box_maximize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Double_Box_maximize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Double_Box_minimize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_minimize_with_point)).

ppl_Double_Box_minimize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Double_Box_minimize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Double_Box_contains_Double_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_contains_Double_Box)).

ppl_Double_Box_contains_Double_Box(Term1, Term2) :-
   ppl_Double_Box_contains_Double_Box_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_strictly_contains_Double_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_strictly_contains_Double_Box)).

ppl_Double_Box_strictly_contains_Double_Box(Term1, Term2) :-
   ppl_Double_Box_strictly_contains_Double_Box_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_is_disjoint_from_Double_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_is_disjoint_from_Double_Box)).

ppl_Double_Box_is_disjoint_from_Double_Box(Term1, Term2) :-
   ppl_Double_Box_is_disjoint_from_Double_Box_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_equals_Double_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_equals_Double_Box)).

ppl_Double_Box_equals_Double_Box(Term1, Term2) :-
   ppl_Double_Box_equals_Double_Box_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Double_Box_OK)).

ppl_Double_Box_OK(Term1) :-
   ppl_Double_Box_OK_2(Term1, 1).

:- true pred ppl_Double_Box_add_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_add_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_add_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_add_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_refine_with_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_refine_with_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_refine_with_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_refine_with_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_intersection_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_upper_bound_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_difference_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_concatenate_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_time_elapse_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_upper_bound_assign_if_exact_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_upper_bound_assign_if_exact)).

ppl_Double_Box_upper_bound_assign_if_exact(Term1, Term2) :-
   ppl_Double_Box_upper_bound_assign_if_exact_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_simplify_using_context_assign_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_simplify_using_context_assign)).

ppl_Double_Box_simplify_using_context_assign(Term1, Term2, Term3) :-
   ppl_Double_Box_simplify_using_context_assign_2(Term1, Term2, Term3, 1).

:- true pred ppl_Double_Box_constrains_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_constrains)).

ppl_Double_Box_constrains(Term1, Term2) :-
   ppl_Double_Box_constrains_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_unconstrain_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_unconstrain_space_dimension)).

ppl_Double_Box_unconstrain_space_dimension(Term1, Term2) :-
   ppl_Double_Box_unconstrain_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_unconstrain_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_unconstrain_space_dimensions)).

ppl_Double_Box_unconstrain_space_dimensions(Term1, Term2) :-
   ppl_Double_Box_unconstrain_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_affine_image(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Double_Box_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Double_Box_bounded_affine_image(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Double_Box_bounded_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Double_Box_generalized_affine_image_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_generalized_affine_image)).

ppl_Double_Box_generalized_affine_image(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Double_Box_generalized_affine_image_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Double_Box_generalized_affine_preimage_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_generalized_affine_preimage)).

ppl_Double_Box_generalized_affine_preimage(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Double_Box_generalized_affine_preimage_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Double_Box_generalized_affine_image_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_generalized_affine_image_lhs_rhs)).

ppl_Double_Box_generalized_affine_image_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Double_Box_generalized_affine_image_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Double_Box_generalized_affine_preimage_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_generalized_affine_preimage_lhs_rhs)).

ppl_Double_Box_generalized_affine_preimage_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Double_Box_generalized_affine_preimage_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Double_Box_add_space_dimensions_and_embed(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_add_space_dimensions_and_project(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_remove_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_remove_space_dimensions)).

ppl_Double_Box_remove_space_dimensions(Term1, Term2) :-
   ppl_Double_Box_remove_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_remove_higher_space_dimensions(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_expand_space_dimension(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Double_Box_fold_space_dimensions_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_fold_space_dimensions)).

ppl_Double_Box_fold_space_dimensions(Term1, Term2, Term3) :-
   ppl_Double_Box_fold_space_dimensions_2(Term1, Term2, Term3, 1).

:- true pred ppl_Double_Box_map_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_map_space_dimensions)).

ppl_Double_Box_map_space_dimensions(Term1, Term2) :-
   ppl_Double_Box_map_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_ascii_dump_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Double_Box_ascii_dump)).

ppl_Double_Box_ascii_dump(Term1) :-
   ppl_Double_Box_ascii_dump_2(Term1, 1).

:- true pred ppl_Double_Box_external_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_external_memory_in_bytes)).

ppl_Double_Box_external_memory_in_bytes(Term1, Term2) :-
   ppl_Double_Box_external_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_total_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_total_memory_in_bytes)).

ppl_Double_Box_total_memory_in_bytes(Term1, Term2) :-
   ppl_Double_Box_total_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Double_Box_CC76_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_CC76_widening_assign_with_tokens)).

ppl_Double_Box_CC76_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Double_Box_CC76_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Double_Box_CC76_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_widening_assign_with_tokens)).

ppl_Double_Box_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Double_Box_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Double_Box_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Double_Box_limited_CC76_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_limited_CC76_extrapolation_assign_with_tokens)).

ppl_Double_Box_limited_CC76_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Double_Box_limited_CC76_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Double_Box_limited_CC76_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Double_Box_linear_partition_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Double_Box_linear_partition)).

ppl_Double_Box_linear_partition(Term1, Term2, Term3, Term4) :-
   ppl_Double_Box_linear_partition_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_delete_BD_Shape_double(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_new_BD_Shape_double_from_space_dimension_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_space_dimension)).

ppl_new_BD_Shape_double_from_space_dimension(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_double_from_space_dimension_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_double_from_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_C_Polyhedron)).

ppl_new_BD_Shape_double_from_C_Polyhedron(Term1, Term2) :-
   ppl_new_BD_Shape_double_from_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_double_from_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_NNC_Polyhedron)).

ppl_new_BD_Shape_double_from_NNC_Polyhedron(Term1, Term2) :-
   ppl_new_BD_Shape_double_from_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_double_from_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_Grid)).

ppl_new_BD_Shape_double_from_Grid(Term1, Term2) :-
   ppl_new_BD_Shape_double_from_Grid_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_double_from_Rational_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_Rational_Box)).

ppl_new_BD_Shape_double_from_Rational_Box(Term1, Term2) :-
   ppl_new_BD_Shape_double_from_Rational_Box_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_double_from_BD_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_BD_Shape_mpz_class)).

ppl_new_BD_Shape_double_from_BD_Shape_mpz_class(Term1, Term2) :-
   ppl_new_BD_Shape_double_from_BD_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_double_from_BD_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_BD_Shape_mpq_class)).

ppl_new_BD_Shape_double_from_BD_Shape_mpq_class(Term1, Term2) :-
   ppl_new_BD_Shape_double_from_BD_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_double_from_Octagonal_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_Octagonal_Shape_mpz_class)).

ppl_new_BD_Shape_double_from_Octagonal_Shape_mpz_class(Term1, Term2) :-
   ppl_new_BD_Shape_double_from_Octagonal_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_double_from_Octagonal_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_Octagonal_Shape_mpq_class)).

ppl_new_BD_Shape_double_from_Octagonal_Shape_mpq_class(Term1, Term2) :-
   ppl_new_BD_Shape_double_from_Octagonal_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_double_from_Double_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_Double_Box)).

ppl_new_BD_Shape_double_from_Double_Box(Term1, Term2) :-
   ppl_new_BD_Shape_double_from_Double_Box_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_double_from_BD_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_BD_Shape_double)).

ppl_new_BD_Shape_double_from_BD_Shape_double(Term1, Term2) :-
   ppl_new_BD_Shape_double_from_BD_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_double_from_Octagonal_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_Octagonal_Shape_double)).

ppl_new_BD_Shape_double_from_Octagonal_Shape_double(Term1, Term2) :-
   ppl_new_BD_Shape_double_from_Octagonal_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_double_from_C_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_C_Polyhedron_with_complexity)).

ppl_new_BD_Shape_double_from_C_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_double_from_C_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_double_from_NNC_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_NNC_Polyhedron_with_complexity)).

ppl_new_BD_Shape_double_from_NNC_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_double_from_NNC_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_double_from_Grid_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_Grid_with_complexity)).

ppl_new_BD_Shape_double_from_Grid_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_double_from_Grid_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_double_from_Rational_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_Rational_Box_with_complexity)).

ppl_new_BD_Shape_double_from_Rational_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_double_from_Rational_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_double_from_BD_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_BD_Shape_mpz_class_with_complexity)).

ppl_new_BD_Shape_double_from_BD_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_double_from_BD_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_double_from_BD_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_BD_Shape_mpq_class_with_complexity)).

ppl_new_BD_Shape_double_from_BD_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_double_from_BD_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_double_from_Octagonal_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_Octagonal_Shape_mpz_class_with_complexity)).

ppl_new_BD_Shape_double_from_Octagonal_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_double_from_Octagonal_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_double_from_Octagonal_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_Octagonal_Shape_mpq_class_with_complexity)).

ppl_new_BD_Shape_double_from_Octagonal_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_double_from_Octagonal_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_double_from_Double_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_Double_Box_with_complexity)).

ppl_new_BD_Shape_double_from_Double_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_double_from_Double_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_double_from_BD_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_BD_Shape_double_with_complexity)).

ppl_new_BD_Shape_double_from_BD_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_double_from_BD_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_double_from_Octagonal_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_Octagonal_Shape_double_with_complexity)).

ppl_new_BD_Shape_double_from_Octagonal_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_BD_Shape_double_from_Octagonal_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_BD_Shape_double_from_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_constraints)).

ppl_new_BD_Shape_double_from_constraints(Term1, Term2) :-
   ppl_new_BD_Shape_double_from_constraints_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_double_from_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_congruences)).

ppl_new_BD_Shape_double_from_congruences(Term1, Term2) :-
   ppl_new_BD_Shape_double_from_congruences_2(Term1, Term2, 1).

:- true pred ppl_new_BD_Shape_double_from_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_BD_Shape_double_from_generators)).

ppl_new_BD_Shape_double_from_generators(Term1, Term2) :-
   ppl_new_BD_Shape_double_from_generators_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_swap(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_space_dimension)).

ppl_BD_Shape_double_space_dimension(Term1, Term2) :-
   ppl_BD_Shape_double_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_affine_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_affine_dimension)).

ppl_BD_Shape_double_affine_dimension(Term1, Term2) :-
   ppl_BD_Shape_double_affine_dimension_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_relation_with_constraint_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_relation_with_constraint)).

ppl_BD_Shape_double_relation_with_constraint(Term1, Term2, Term3) :-
   ppl_BD_Shape_double_relation_with_constraint_2(Term1, Term2, Term3, 1).

:- true pred ppl_BD_Shape_double_relation_with_generator_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_relation_with_generator)).

ppl_BD_Shape_double_relation_with_generator(Term1, Term2, Term3) :-
   ppl_BD_Shape_double_relation_with_generator_2(Term1, Term2, Term3, 1).

:- true pred ppl_BD_Shape_double_relation_with_congruence_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_relation_with_congruence)).

ppl_BD_Shape_double_relation_with_congruence(Term1, Term2, Term3) :-
   ppl_BD_Shape_double_relation_with_congruence_2(Term1, Term2, Term3, 1).

:- true pred ppl_BD_Shape_double_get_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_get_constraints)).

ppl_BD_Shape_double_get_constraints(Term1, Term2) :-
   ppl_BD_Shape_double_get_constraints_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_get_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_get_congruences)).

ppl_BD_Shape_double_get_congruences(Term1, Term2) :-
   ppl_BD_Shape_double_get_congruences_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_get_minimized_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_get_minimized_constraints)).

ppl_BD_Shape_double_get_minimized_constraints(Term1, Term2) :-
   ppl_BD_Shape_double_get_minimized_constraints_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_get_minimized_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_get_minimized_congruences)).

ppl_BD_Shape_double_get_minimized_congruences(Term1, Term2) :-
   ppl_BD_Shape_double_get_minimized_congruences_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_is_empty_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_is_empty)).

ppl_BD_Shape_double_is_empty(Term1) :-
   ppl_BD_Shape_double_is_empty_2(Term1, 1).

:- true pred ppl_BD_Shape_double_is_universe_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_is_universe)).

ppl_BD_Shape_double_is_universe(Term1) :-
   ppl_BD_Shape_double_is_universe_2(Term1, 1).

:- true pred ppl_BD_Shape_double_is_bounded_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_is_bounded)).

ppl_BD_Shape_double_is_bounded(Term1) :-
   ppl_BD_Shape_double_is_bounded_2(Term1, 1).

:- true pred ppl_BD_Shape_double_contains_integer_point_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_contains_integer_point)).

ppl_BD_Shape_double_contains_integer_point(Term1) :-
   ppl_BD_Shape_double_contains_integer_point_2(Term1, 1).

:- true pred ppl_BD_Shape_double_is_topologically_closed_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_is_topologically_closed)).

ppl_BD_Shape_double_is_topologically_closed(Term1) :-
   ppl_BD_Shape_double_is_topologically_closed_2(Term1, 1).

:- true pred ppl_BD_Shape_double_is_discrete_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_is_discrete)).

ppl_BD_Shape_double_is_discrete(Term1) :-
   ppl_BD_Shape_double_is_discrete_2(Term1, 1).

:- true pred ppl_BD_Shape_double_topological_closure_assign(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_BD_Shape_double_bounds_from_above_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_bounds_from_above)).

ppl_BD_Shape_double_bounds_from_above(Term1, Term2) :-
   ppl_BD_Shape_double_bounds_from_above_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_bounds_from_below_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_bounds_from_below)).

ppl_BD_Shape_double_bounds_from_below(Term1, Term2) :-
   ppl_BD_Shape_double_bounds_from_below_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_maximize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_maximize)).

ppl_BD_Shape_double_maximize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_double_maximize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_double_minimize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_minimize)).

ppl_BD_Shape_double_minimize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_double_minimize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_double_maximize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_maximize_with_point)).

ppl_BD_Shape_double_maximize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_BD_Shape_double_maximize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_BD_Shape_double_minimize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_minimize_with_point)).

ppl_BD_Shape_double_minimize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_BD_Shape_double_minimize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_BD_Shape_double_contains_BD_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_contains_BD_Shape_double)).

ppl_BD_Shape_double_contains_BD_Shape_double(Term1, Term2) :-
   ppl_BD_Shape_double_contains_BD_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_strictly_contains_BD_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_strictly_contains_BD_Shape_double)).

ppl_BD_Shape_double_strictly_contains_BD_Shape_double(Term1, Term2) :-
   ppl_BD_Shape_double_strictly_contains_BD_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_is_disjoint_from_BD_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_is_disjoint_from_BD_Shape_double)).

ppl_BD_Shape_double_is_disjoint_from_BD_Shape_double(Term1, Term2) :-
   ppl_BD_Shape_double_is_disjoint_from_BD_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_equals_BD_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_equals_BD_Shape_double)).

ppl_BD_Shape_double_equals_BD_Shape_double(Term1, Term2) :-
   ppl_BD_Shape_double_equals_BD_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_OK)).

ppl_BD_Shape_double_OK(Term1) :-
   ppl_BD_Shape_double_OK_2(Term1, 1).

:- true pred ppl_BD_Shape_double_add_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_add_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_add_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_add_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_refine_with_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_refine_with_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_refine_with_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_refine_with_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_intersection_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_upper_bound_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_difference_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_concatenate_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_time_elapse_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_upper_bound_assign_if_exact_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_upper_bound_assign_if_exact)).

ppl_BD_Shape_double_upper_bound_assign_if_exact(Term1, Term2) :-
   ppl_BD_Shape_double_upper_bound_assign_if_exact_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_simplify_using_context_assign_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_simplify_using_context_assign)).

ppl_BD_Shape_double_simplify_using_context_assign(Term1, Term2, Term3) :-
   ppl_BD_Shape_double_simplify_using_context_assign_2(Term1, Term2, Term3, 1).

:- true pred ppl_BD_Shape_double_constrains_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_constrains)).

ppl_BD_Shape_double_constrains(Term1, Term2) :-
   ppl_BD_Shape_double_constrains_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_unconstrain_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_unconstrain_space_dimension)).

ppl_BD_Shape_double_unconstrain_space_dimension(Term1, Term2) :-
   ppl_BD_Shape_double_unconstrain_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_unconstrain_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_unconstrain_space_dimensions)).

ppl_BD_Shape_double_unconstrain_space_dimensions(Term1, Term2) :-
   ppl_BD_Shape_double_unconstrain_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_affine_image(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_bounded_affine_image(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_bounded_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_generalized_affine_image_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_generalized_affine_image)).

ppl_BD_Shape_double_generalized_affine_image(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_double_generalized_affine_image_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_double_generalized_affine_preimage_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_generalized_affine_preimage)).

ppl_BD_Shape_double_generalized_affine_preimage(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_double_generalized_affine_preimage_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_double_generalized_affine_image_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_generalized_affine_image_lhs_rhs)).

ppl_BD_Shape_double_generalized_affine_image_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_double_generalized_affine_image_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_double_generalized_affine_preimage_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_generalized_affine_preimage_lhs_rhs)).

ppl_BD_Shape_double_generalized_affine_preimage_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_double_generalized_affine_preimage_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_double_add_space_dimensions_and_embed(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_add_space_dimensions_and_project(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_remove_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_remove_space_dimensions)).

ppl_BD_Shape_double_remove_space_dimensions(Term1, Term2) :-
   ppl_BD_Shape_double_remove_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_remove_higher_space_dimensions(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_expand_space_dimension(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_fold_space_dimensions_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_fold_space_dimensions)).

ppl_BD_Shape_double_fold_space_dimensions(Term1, Term2, Term3) :-
   ppl_BD_Shape_double_fold_space_dimensions_2(Term1, Term2, Term3, 1).

:- true pred ppl_BD_Shape_double_map_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_map_space_dimensions)).

ppl_BD_Shape_double_map_space_dimensions(Term1, Term2) :-
   ppl_BD_Shape_double_map_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_ascii_dump_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_ascii_dump)).

ppl_BD_Shape_double_ascii_dump(Term1) :-
   ppl_BD_Shape_double_ascii_dump_2(Term1, 1).

:- true pred ppl_BD_Shape_double_external_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_external_memory_in_bytes)).

ppl_BD_Shape_double_external_memory_in_bytes(Term1, Term2) :-
   ppl_BD_Shape_double_external_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_total_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_total_memory_in_bytes)).

ppl_BD_Shape_double_total_memory_in_bytes(Term1, Term2) :-
   ppl_BD_Shape_double_total_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_BHMZ05_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_BHMZ05_widening_assign_with_tokens)).

ppl_BD_Shape_double_BHMZ05_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_double_BHMZ05_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_double_H79_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_H79_widening_assign_with_tokens)).

ppl_BD_Shape_double_H79_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_double_H79_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_double_BHMZ05_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_H79_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_widening_assign_with_tokens)).

ppl_BD_Shape_double_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_double_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_double_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_limited_BHMZ05_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_limited_BHMZ05_extrapolation_assign_with_tokens)).

ppl_BD_Shape_double_limited_BHMZ05_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_double_limited_BHMZ05_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_double_limited_H79_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_limited_H79_extrapolation_assign_with_tokens)).

ppl_BD_Shape_double_limited_H79_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_double_limited_H79_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_double_limited_CC76_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_limited_CC76_extrapolation_assign_with_tokens)).

ppl_BD_Shape_double_limited_CC76_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_BD_Shape_double_limited_CC76_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_BD_Shape_double_limited_BHMZ05_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_limited_H79_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_limited_CC76_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_CC76_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_CC76_extrapolation_assign_with_tokens)).

ppl_BD_Shape_double_CC76_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_double_CC76_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_BD_Shape_double_CC76_extrapolation_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_BD_Shape_double_CC76_narrowing_assign_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_CC76_narrowing_assign)).

ppl_BD_Shape_double_CC76_narrowing_assign(Term1, Term2) :-
   ppl_BD_Shape_double_CC76_narrowing_assign_2(Term1, Term2, 1).

:- true pred ppl_BD_Shape_double_linear_partition_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_BD_Shape_double_linear_partition)).

ppl_BD_Shape_double_linear_partition(Term1, Term2, Term3, Term4) :-
   ppl_BD_Shape_double_linear_partition_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_delete_Octagonal_Shape_double(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_new_Octagonal_Shape_double_from_space_dimension_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_space_dimension)).

ppl_new_Octagonal_Shape_double_from_space_dimension(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_double_from_space_dimension_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_C_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_C_Polyhedron)).

ppl_new_Octagonal_Shape_double_from_C_Polyhedron(Term1, Term2) :-
   ppl_new_Octagonal_Shape_double_from_C_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_NNC_Polyhedron_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_NNC_Polyhedron)).

ppl_new_Octagonal_Shape_double_from_NNC_Polyhedron(Term1, Term2) :-
   ppl_new_Octagonal_Shape_double_from_NNC_Polyhedron_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_Grid_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_Grid)).

ppl_new_Octagonal_Shape_double_from_Grid(Term1, Term2) :-
   ppl_new_Octagonal_Shape_double_from_Grid_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_Rational_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_Rational_Box)).

ppl_new_Octagonal_Shape_double_from_Rational_Box(Term1, Term2) :-
   ppl_new_Octagonal_Shape_double_from_Rational_Box_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_BD_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_BD_Shape_mpz_class)).

ppl_new_Octagonal_Shape_double_from_BD_Shape_mpz_class(Term1, Term2) :-
   ppl_new_Octagonal_Shape_double_from_BD_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_BD_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_BD_Shape_mpq_class)).

ppl_new_Octagonal_Shape_double_from_BD_Shape_mpq_class(Term1, Term2) :-
   ppl_new_Octagonal_Shape_double_from_BD_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpz_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpz_class)).

ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpz_class(Term1, Term2) :-
   ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpz_class_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpq_class_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpq_class)).

ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpq_class(Term1, Term2) :-
   ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpq_class_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_Double_Box_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_Double_Box)).

ppl_new_Octagonal_Shape_double_from_Double_Box(Term1, Term2) :-
   ppl_new_Octagonal_Shape_double_from_Double_Box_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_BD_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_BD_Shape_double)).

ppl_new_Octagonal_Shape_double_from_BD_Shape_double(Term1, Term2) :-
   ppl_new_Octagonal_Shape_double_from_BD_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_double)).

ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_double(Term1, Term2) :-
   ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_C_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_C_Polyhedron_with_complexity)).

ppl_new_Octagonal_Shape_double_from_C_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_double_from_C_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_NNC_Polyhedron_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_NNC_Polyhedron_with_complexity)).

ppl_new_Octagonal_Shape_double_from_NNC_Polyhedron_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_double_from_NNC_Polyhedron_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_Grid_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_Grid_with_complexity)).

ppl_new_Octagonal_Shape_double_from_Grid_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_double_from_Grid_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_Rational_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_Rational_Box_with_complexity)).

ppl_new_Octagonal_Shape_double_from_Rational_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_double_from_Rational_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_BD_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_BD_Shape_mpz_class_with_complexity)).

ppl_new_Octagonal_Shape_double_from_BD_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_double_from_BD_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_BD_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_BD_Shape_mpq_class_with_complexity)).

ppl_new_Octagonal_Shape_double_from_BD_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_double_from_BD_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpz_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpz_class_with_complexity)).

ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpz_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpz_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpq_class_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpq_class_with_complexity)).

ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpq_class_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpq_class_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_Double_Box_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_Double_Box_with_complexity)).

ppl_new_Octagonal_Shape_double_from_Double_Box_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_double_from_Double_Box_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_BD_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_BD_Shape_double_with_complexity)).

ppl_new_Octagonal_Shape_double_from_BD_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_double_from_BD_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_double_with_complexity_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_double_with_complexity)).

ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_double_with_complexity(Term1, Term2, Term3) :-
   ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_double_with_complexity_2(Term1, Term2, Term3, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_constraints)).

ppl_new_Octagonal_Shape_double_from_constraints(Term1, Term2) :-
   ppl_new_Octagonal_Shape_double_from_constraints_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_congruences)).

ppl_new_Octagonal_Shape_double_from_congruences(Term1, Term2) :-
   ppl_new_Octagonal_Shape_double_from_congruences_2(Term1, Term2, 1).

:- true pred ppl_new_Octagonal_Shape_double_from_generators_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_Octagonal_Shape_double_from_generators)).

ppl_new_Octagonal_Shape_double_from_generators(Term1, Term2) :-
   ppl_new_Octagonal_Shape_double_from_generators_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_swap(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_space_dimension)).

ppl_Octagonal_Shape_double_space_dimension(Term1, Term2) :-
   ppl_Octagonal_Shape_double_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_affine_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_affine_dimension)).

ppl_Octagonal_Shape_double_affine_dimension(Term1, Term2) :-
   ppl_Octagonal_Shape_double_affine_dimension_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_relation_with_constraint_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_relation_with_constraint)).

ppl_Octagonal_Shape_double_relation_with_constraint(Term1, Term2, Term3) :-
   ppl_Octagonal_Shape_double_relation_with_constraint_2(Term1, Term2, Term3, 1).

:- true pred ppl_Octagonal_Shape_double_relation_with_generator_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_relation_with_generator)).

ppl_Octagonal_Shape_double_relation_with_generator(Term1, Term2, Term3) :-
   ppl_Octagonal_Shape_double_relation_with_generator_2(Term1, Term2, Term3, 1).

:- true pred ppl_Octagonal_Shape_double_relation_with_congruence_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_relation_with_congruence)).

ppl_Octagonal_Shape_double_relation_with_congruence(Term1, Term2, Term3) :-
   ppl_Octagonal_Shape_double_relation_with_congruence_2(Term1, Term2, Term3, 1).

:- true pred ppl_Octagonal_Shape_double_get_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_get_constraints)).

ppl_Octagonal_Shape_double_get_constraints(Term1, Term2) :-
   ppl_Octagonal_Shape_double_get_constraints_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_get_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_get_congruences)).

ppl_Octagonal_Shape_double_get_congruences(Term1, Term2) :-
   ppl_Octagonal_Shape_double_get_congruences_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_get_minimized_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_get_minimized_constraints)).

ppl_Octagonal_Shape_double_get_minimized_constraints(Term1, Term2) :-
   ppl_Octagonal_Shape_double_get_minimized_constraints_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_get_minimized_congruences_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_get_minimized_congruences)).

ppl_Octagonal_Shape_double_get_minimized_congruences(Term1, Term2) :-
   ppl_Octagonal_Shape_double_get_minimized_congruences_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_is_empty_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_is_empty)).

ppl_Octagonal_Shape_double_is_empty(Term1) :-
   ppl_Octagonal_Shape_double_is_empty_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_double_is_universe_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_is_universe)).

ppl_Octagonal_Shape_double_is_universe(Term1) :-
   ppl_Octagonal_Shape_double_is_universe_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_double_is_bounded_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_is_bounded)).

ppl_Octagonal_Shape_double_is_bounded(Term1) :-
   ppl_Octagonal_Shape_double_is_bounded_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_double_contains_integer_point_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_contains_integer_point)).

ppl_Octagonal_Shape_double_contains_integer_point(Term1) :-
   ppl_Octagonal_Shape_double_contains_integer_point_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_double_is_topologically_closed_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_is_topologically_closed)).

ppl_Octagonal_Shape_double_is_topologically_closed(Term1) :-
   ppl_Octagonal_Shape_double_is_topologically_closed_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_double_is_discrete_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_is_discrete)).

ppl_Octagonal_Shape_double_is_discrete(Term1) :-
   ppl_Octagonal_Shape_double_is_discrete_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_double_topological_closure_assign(in(Term1))
          :: any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_bounds_from_above_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_bounds_from_above)).

ppl_Octagonal_Shape_double_bounds_from_above(Term1, Term2) :-
   ppl_Octagonal_Shape_double_bounds_from_above_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_bounds_from_below_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_bounds_from_below)).

ppl_Octagonal_Shape_double_bounds_from_below(Term1, Term2) :-
   ppl_Octagonal_Shape_double_bounds_from_below_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_maximize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_maximize)).

ppl_Octagonal_Shape_double_maximize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_double_maximize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_double_minimize_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_minimize)).

ppl_Octagonal_Shape_double_minimize(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_double_minimize_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_double_maximize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_maximize_with_point)).

ppl_Octagonal_Shape_double_maximize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Octagonal_Shape_double_maximize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Octagonal_Shape_double_minimize_with_point_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), in(Term6), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_minimize_with_point)).

ppl_Octagonal_Shape_double_minimize_with_point(Term1, Term2, Term3, Term4, Term5, Term6) :-
   ppl_Octagonal_Shape_double_minimize_with_point_2(Term1, Term2, Term3, Term4, Term5, Term6, 1).

:- true pred ppl_Octagonal_Shape_double_contains_Octagonal_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_contains_Octagonal_Shape_double)).

ppl_Octagonal_Shape_double_contains_Octagonal_Shape_double(Term1, Term2) :-
   ppl_Octagonal_Shape_double_contains_Octagonal_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_strictly_contains_Octagonal_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_strictly_contains_Octagonal_Shape_double)).

ppl_Octagonal_Shape_double_strictly_contains_Octagonal_Shape_double(Term1, Term2) :-
   ppl_Octagonal_Shape_double_strictly_contains_Octagonal_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_is_disjoint_from_Octagonal_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_is_disjoint_from_Octagonal_Shape_double)).

ppl_Octagonal_Shape_double_is_disjoint_from_Octagonal_Shape_double(Term1, Term2) :-
   ppl_Octagonal_Shape_double_is_disjoint_from_Octagonal_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_equals_Octagonal_Shape_double_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_equals_Octagonal_Shape_double)).

ppl_Octagonal_Shape_double_equals_Octagonal_Shape_double(Term1, Term2) :-
   ppl_Octagonal_Shape_double_equals_Octagonal_Shape_double_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_OK)).

ppl_Octagonal_Shape_double_OK(Term1) :-
   ppl_Octagonal_Shape_double_OK_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_double_add_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_add_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_add_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_add_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_refine_with_constraint(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_refine_with_congruence(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_refine_with_constraints(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_refine_with_congruences(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_intersection_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_upper_bound_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_difference_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_concatenate_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_time_elapse_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_upper_bound_assign_if_exact_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_upper_bound_assign_if_exact)).

ppl_Octagonal_Shape_double_upper_bound_assign_if_exact(Term1, Term2) :-
   ppl_Octagonal_Shape_double_upper_bound_assign_if_exact_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_simplify_using_context_assign_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_simplify_using_context_assign)).

ppl_Octagonal_Shape_double_simplify_using_context_assign(Term1, Term2, Term3) :-
   ppl_Octagonal_Shape_double_simplify_using_context_assign_2(Term1, Term2, Term3, 1).

:- true pred ppl_Octagonal_Shape_double_constrains_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_constrains)).

ppl_Octagonal_Shape_double_constrains(Term1, Term2) :-
   ppl_Octagonal_Shape_double_constrains_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_unconstrain_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_unconstrain_space_dimension)).

ppl_Octagonal_Shape_double_unconstrain_space_dimension(Term1, Term2) :-
   ppl_Octagonal_Shape_double_unconstrain_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_unconstrain_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_unconstrain_space_dimensions)).

ppl_Octagonal_Shape_double_unconstrain_space_dimensions(Term1, Term2) :-
   ppl_Octagonal_Shape_double_unconstrain_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_affine_image(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4))
          :: any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_bounded_affine_image(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_bounded_affine_preimage(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5))
          :: any_term * any_term * any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_generalized_affine_image_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_generalized_affine_image)).

ppl_Octagonal_Shape_double_generalized_affine_image(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_double_generalized_affine_image_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_double_generalized_affine_preimage_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_generalized_affine_preimage)).

ppl_Octagonal_Shape_double_generalized_affine_preimage(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_double_generalized_affine_preimage_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_double_generalized_affine_image_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_generalized_affine_image_lhs_rhs)).

ppl_Octagonal_Shape_double_generalized_affine_image_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_double_generalized_affine_image_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Octagonal_Shape_double_generalized_affine_preimage_lhs_rhs_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_generalized_affine_preimage_lhs_rhs)).

ppl_Octagonal_Shape_double_generalized_affine_preimage_lhs_rhs(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_double_generalized_affine_preimage_lhs_rhs_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Octagonal_Shape_double_add_space_dimensions_and_embed(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_add_space_dimensions_and_project(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_remove_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_remove_space_dimensions)).

ppl_Octagonal_Shape_double_remove_space_dimensions(Term1, Term2) :-
   ppl_Octagonal_Shape_double_remove_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_remove_higher_space_dimensions(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_expand_space_dimension(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_fold_space_dimensions_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_fold_space_dimensions)).

ppl_Octagonal_Shape_double_fold_space_dimensions(Term1, Term2, Term3) :-
   ppl_Octagonal_Shape_double_fold_space_dimensions_2(Term1, Term2, Term3, 1).

:- true pred ppl_Octagonal_Shape_double_map_space_dimensions_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_map_space_dimensions)).

ppl_Octagonal_Shape_double_map_space_dimensions(Term1, Term2) :-
   ppl_Octagonal_Shape_double_map_space_dimensions_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_ascii_dump_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_ascii_dump)).

ppl_Octagonal_Shape_double_ascii_dump(Term1) :-
   ppl_Octagonal_Shape_double_ascii_dump_2(Term1, 1).

:- true pred ppl_Octagonal_Shape_double_external_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_external_memory_in_bytes)).

ppl_Octagonal_Shape_double_external_memory_in_bytes(Term1, Term2) :-
   ppl_Octagonal_Shape_double_external_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_total_memory_in_bytes_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_total_memory_in_bytes)).

ppl_Octagonal_Shape_double_total_memory_in_bytes(Term1, Term2) :-
   ppl_Octagonal_Shape_double_total_memory_in_bytes_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_BHMZ05_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_BHMZ05_widening_assign_with_tokens)).

ppl_Octagonal_Shape_double_BHMZ05_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_double_BHMZ05_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Octagonal_Shape_double_BHMZ05_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_widening_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_widening_assign_with_tokens)).

ppl_Octagonal_Shape_double_widening_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_double_widening_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Octagonal_Shape_double_widening_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_limited_BHMZ05_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_limited_BHMZ05_extrapolation_assign_with_tokens)).

ppl_Octagonal_Shape_double_limited_BHMZ05_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_double_limited_BHMZ05_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_double_limited_CC76_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), in(Term5), go(Success))
          :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_limited_CC76_extrapolation_assign_with_tokens)).

ppl_Octagonal_Shape_double_limited_CC76_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4, Term5) :-
   ppl_Octagonal_Shape_double_limited_CC76_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, Term5, 1).

:- true pred ppl_Octagonal_Shape_double_limited_BHMZ05_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_limited_CC76_extrapolation_assign(in(Term1), in(Term2), in(Term3))
          :: any_term * any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_CC76_extrapolation_assign_with_tokens_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_CC76_extrapolation_assign_with_tokens)).

ppl_Octagonal_Shape_double_CC76_extrapolation_assign_with_tokens(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_double_CC76_extrapolation_assign_with_tokens_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_Octagonal_Shape_double_CC76_extrapolation_assign(in(Term1), in(Term2))
          :: any_term * any_term +  foreign.

:- true pred ppl_Octagonal_Shape_double_CC76_narrowing_assign_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_CC76_narrowing_assign)).

ppl_Octagonal_Shape_double_CC76_narrowing_assign(Term1, Term2) :-
   ppl_Octagonal_Shape_double_CC76_narrowing_assign_2(Term1, Term2, 1).

:- true pred ppl_Octagonal_Shape_double_linear_partition_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Octagonal_Shape_double_linear_partition)).

ppl_Octagonal_Shape_double_linear_partition(Term1, Term2, Term3, Term4) :-
   ppl_Octagonal_Shape_double_linear_partition_2(Term1, Term2, Term3, Term4, 1).

:- include(library(ppl/ppl_decl_auto)).
:- use_foreign_source(['0_10/ppl_prolog_Octagonal_Shape_mpq_class.cc',
		       '0_10/ppl_prolog_BD_Shape_double.cc',
		       '0_10/ppl_prolog_Octagonal_Shape_mpz_class.cc',
		       '0_10/ppl_prolog_BD_Shape_mpq_class.cc',
		       '0_10/ppl_prolog_Pointset_Powerset_C_Polyhedron.cc',
		       '0_10/ppl_prolog_BD_Shape_mpz_class.cc',
		       '0_10/ppl_prolog_Pointset_Powerset_NNC_Polyhedron.cc', 
		       '0_10/ppl_prolog_Constraints_Product_C_Polyhedron_Grid.cc',	
		       '0_10/ppl_prolog_Polyhedron.cc',
		       '0_10/ppl_prolog_Double_Box.cc',
		       '0_10/ppl_prolog_Rational_Box.cc',
		       '0_10/ppl_prolog_Grid.cc',
		       '0_10/ppl_prolog_common.cc',
		       '0_10/ppl_prolog_Octagonal_Shape_double.cc',
		       '0_10/ciao_efli.cc']).
:- use_foreign_library(['pwl']).


:- impl_defined(
[
	ppl_version_major_2/2,
	ppl_version_minor_2/2,
	ppl_version_revision_2/2,
	ppl_version_beta_2/2,
	ppl_version_2/2,
	ppl_banner_2/2,
	ppl_max_space_dimension_2/2,
	ppl_Coefficient_is_bounded_2/1,
	ppl_Coefficient_max_2/2,
	ppl_Coefficient_min_2/2,
	ppl_timeout_exception_atom_2/2,
	ppl_new_MIP_Problem_from_space_dimension_2/3,
	ppl_new_MIP_Problem_2/6,
	ppl_new_MIP_Problem_from_MIP_Problem_2/3,
	ppl_MIP_Problem_space_dimension_2/3,
	ppl_MIP_Problem_integer_space_dimensions_2/3,
	ppl_MIP_Problem_constraints_2/3,
	ppl_MIP_Problem_objective_function_2/3,
	ppl_MIP_Problem_optimization_mode_2/3,
	ppl_MIP_Problem_clear_2/2,
	ppl_MIP_Problem_add_space_dimensions_and_embed_2/3,
	ppl_MIP_Problem_add_to_integer_space_dimensions_2/3,
	ppl_MIP_Problem_add_constraint_2/3,
	ppl_MIP_Problem_add_constraints_2/3,
	ppl_MIP_Problem_set_objective_function_2/3,
	ppl_MIP_Problem_set_optimization_mode_2/3,
	ppl_MIP_Problem_get_control_parameter_2/4,
	ppl_MIP_Problem_is_satisfiable_2/2,
	ppl_MIP_Problem_solve_2/3,
	ppl_MIP_Problem_feasible_point_2/3,
	ppl_MIP_Problem_optimizing_point_2/3,
	ppl_MIP_Problem_optimal_value_2/4,
	ppl_MIP_Problem_evaluate_objective_function_2/5,
	ppl_MIP_Problem_OK_2/2,
	ppl_new_C_Polyhedron_from_space_dimension_2/4,
	ppl_new_NNC_Polyhedron_from_space_dimension_2/4,
	ppl_new_C_Polyhedron_from_C_Polyhedron_2/3,
	ppl_new_C_Polyhedron_from_NNC_Polyhedron_2/3,
	ppl_new_C_Polyhedron_from_Grid_2/3,
	ppl_new_C_Polyhedron_from_Rational_Box_2/3,
	ppl_new_C_Polyhedron_from_BD_Shape_mpz_class_2/3,
	ppl_new_C_Polyhedron_from_BD_Shape_mpq_class_2/3,
	ppl_new_C_Polyhedron_from_Octagonal_Shape_mpz_class_2/3,
	ppl_new_C_Polyhedron_from_Octagonal_Shape_mpq_class_2/3,
	ppl_new_C_Polyhedron_from_Double_Box_2/3,
	ppl_new_C_Polyhedron_from_BD_Shape_double_2/3,
	ppl_new_C_Polyhedron_from_Octagonal_Shape_double_2/3,
	ppl_new_NNC_Polyhedron_from_C_Polyhedron_2/3,
	ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_2/3,
	ppl_new_NNC_Polyhedron_from_Grid_2/3,
	ppl_new_NNC_Polyhedron_from_Rational_Box_2/3,
	ppl_new_NNC_Polyhedron_from_BD_Shape_mpz_class_2/3,
	ppl_new_NNC_Polyhedron_from_BD_Shape_mpq_class_2/3,
	ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class_2/3,
	ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class_2/3,
	ppl_new_NNC_Polyhedron_from_Double_Box_2/3,
	ppl_new_NNC_Polyhedron_from_BD_Shape_double_2/3,
	ppl_new_NNC_Polyhedron_from_Octagonal_Shape_double_2/3,
	ppl_new_C_Polyhedron_from_C_Polyhedron_with_complexity_2/4,
	ppl_new_C_Polyhedron_from_NNC_Polyhedron_with_complexity_2/4,
	ppl_new_C_Polyhedron_from_Grid_with_complexity_2/4,
	ppl_new_C_Polyhedron_from_Rational_Box_with_complexity_2/4,
	ppl_new_C_Polyhedron_from_BD_Shape_mpz_class_with_complexity_2/4,
	ppl_new_C_Polyhedron_from_BD_Shape_mpq_class_with_complexity_2/4,
	ppl_new_C_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity_2/4,
	ppl_new_C_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity_2/4,
	ppl_new_C_Polyhedron_from_Double_Box_with_complexity_2/4,
	ppl_new_C_Polyhedron_from_BD_Shape_double_with_complexity_2/4,
	ppl_new_C_Polyhedron_from_Octagonal_Shape_double_with_complexity_2/4,
	ppl_new_NNC_Polyhedron_from_C_Polyhedron_with_complexity_2/4,
	ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity_2/4,
	ppl_new_NNC_Polyhedron_from_Grid_with_complexity_2/4,
	ppl_new_NNC_Polyhedron_from_Rational_Box_with_complexity_2/4,
	ppl_new_NNC_Polyhedron_from_BD_Shape_mpz_class_with_complexity_2/4,
	ppl_new_NNC_Polyhedron_from_BD_Shape_mpq_class_with_complexity_2/4,
	ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpz_class_with_complexity_2/4,
	ppl_new_NNC_Polyhedron_from_Octagonal_Shape_mpq_class_with_complexity_2/4,
	ppl_new_NNC_Polyhedron_from_Double_Box_with_complexity_2/4,
	ppl_new_NNC_Polyhedron_from_BD_Shape_double_with_complexity_2/4,
	ppl_new_NNC_Polyhedron_from_Octagonal_Shape_double_with_complexity_2/4,
	ppl_new_C_Polyhedron_from_constraints_2/3,
	ppl_new_NNC_Polyhedron_from_constraints_2/3,
	ppl_new_C_Polyhedron_from_congruences_2/3,
	ppl_new_NNC_Polyhedron_from_congruences_2/3,
	ppl_new_C_Polyhedron_from_generators_2/3,
	ppl_new_NNC_Polyhedron_from_generators_2/3,
	ppl_Polyhedron_space_dimension_2/3,
	ppl_Polyhedron_affine_dimension_2/3,
	ppl_Polyhedron_relation_with_constraint_2/4,
	ppl_Polyhedron_relation_with_generator_2/4,
	ppl_Polyhedron_relation_with_congruence_2/4,
	ppl_Polyhedron_get_constraints_2/3,
	ppl_Polyhedron_get_congruences_2/3,
	ppl_Polyhedron_get_generators_2/3,
	ppl_Polyhedron_get_minimized_constraints_2/3,
	ppl_Polyhedron_get_minimized_congruences_2/3,
	ppl_Polyhedron_get_minimized_generators_2/3,
	ppl_Polyhedron_is_empty_2/2,
	ppl_Polyhedron_is_universe_2/2,
	ppl_Polyhedron_is_bounded_2/2,
	ppl_Polyhedron_contains_integer_point_2/2,
	ppl_Polyhedron_is_topologically_closed_2/2,
	ppl_Polyhedron_is_discrete_2/2,
	ppl_Polyhedron_bounds_from_above_2/3,
	ppl_Polyhedron_bounds_from_below_2/3,
	ppl_Polyhedron_maximize_2/6,
	ppl_Polyhedron_minimize_2/6,
	ppl_Polyhedron_maximize_with_point_2/7,
	ppl_Polyhedron_minimize_with_point_2/7,
	ppl_Polyhedron_contains_Polyhedron_2/3,
	ppl_Polyhedron_strictly_contains_Polyhedron_2/3,
	ppl_Polyhedron_is_disjoint_from_Polyhedron_2/3,
	ppl_Polyhedron_equals_Polyhedron_2/3,
	ppl_Polyhedron_OK_2/2,
	ppl_Polyhedron_upper_bound_assign_if_exact_2/3,
	ppl_Polyhedron_poly_hull_assign_if_exact_2/3,
	ppl_Polyhedron_simplify_using_context_assign_2/4,
	ppl_Polyhedron_constrains_2/3,
	ppl_Polyhedron_unconstrain_space_dimension_2/3,
	ppl_Polyhedron_unconstrain_space_dimensions_2/3,
	ppl_Polyhedron_generalized_affine_image_2/6,
	ppl_Polyhedron_generalized_affine_preimage_2/6,
	ppl_Polyhedron_generalized_affine_image_lhs_rhs_2/5,
	ppl_Polyhedron_generalized_affine_preimage_lhs_rhs_2/5,
	ppl_Polyhedron_remove_space_dimensions_2/3,
	ppl_Polyhedron_fold_space_dimensions_2/4,
	ppl_Polyhedron_map_space_dimensions_2/3,
	ppl_Polyhedron_ascii_dump_2/2,
	ppl_Polyhedron_external_memory_in_bytes_2/3,
	ppl_Polyhedron_total_memory_in_bytes_2/3,
	ppl_Polyhedron_BHRZ03_widening_assign_with_tokens_2/5,
	ppl_Polyhedron_H79_widening_assign_with_tokens_2/5,
	ppl_Polyhedron_widening_assign_with_tokens_2/5,
	ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens_2/6,
	ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens_2/6,
	ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens_2/6,
	ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens_2/6,
	ppl_Polyhedron_linear_partition_2/5,
	ppl_Polyhedron_intersection_assign_and_minimize_2/3,
	ppl_Polyhedron_poly_hull_assign_and_minimize_2/3,
	ppl_Polyhedron_add_constraint_and_minimize_2/3,
	ppl_Polyhedron_add_congruence_and_minimize_2/3,
	ppl_Polyhedron_add_generator_and_minimize_2/3,
	ppl_Polyhedron_add_constraints_and_minimize_2/3,
	ppl_Polyhedron_add_congruences_and_minimize_2/3,
	ppl_Polyhedron_add_generators_and_minimize_2/3
,
	ppl_new_Grid_from_space_dimension_2/4,
	ppl_new_Grid_from_C_Polyhedron_2/3,
	ppl_new_Grid_from_NNC_Polyhedron_2/3,
	ppl_new_Grid_from_Grid_2/3,
	ppl_new_Grid_from_Rational_Box_2/3,
	ppl_new_Grid_from_BD_Shape_mpz_class_2/3,
	ppl_new_Grid_from_BD_Shape_mpq_class_2/3,
	ppl_new_Grid_from_Octagonal_Shape_mpz_class_2/3,
	ppl_new_Grid_from_Octagonal_Shape_mpq_class_2/3,
	ppl_new_Grid_from_Double_Box_2/3,
	ppl_new_Grid_from_BD_Shape_double_2/3,
	ppl_new_Grid_from_Octagonal_Shape_double_2/3,
	ppl_new_Grid_from_C_Polyhedron_with_complexity_2/4,
	ppl_new_Grid_from_NNC_Polyhedron_with_complexity_2/4,
	ppl_new_Grid_from_Grid_with_complexity_2/4,
	ppl_new_Grid_from_Rational_Box_with_complexity_2/4,
	ppl_new_Grid_from_BD_Shape_mpz_class_with_complexity_2/4,
	ppl_new_Grid_from_BD_Shape_mpq_class_with_complexity_2/4,
	ppl_new_Grid_from_Octagonal_Shape_mpz_class_with_complexity_2/4,
	ppl_new_Grid_from_Octagonal_Shape_mpq_class_with_complexity_2/4,
	ppl_new_Grid_from_Double_Box_with_complexity_2/4,
	ppl_new_Grid_from_BD_Shape_double_with_complexity_2/4,
	ppl_new_Grid_from_Octagonal_Shape_double_with_complexity_2/4,
	ppl_new_Grid_from_constraints_2/3,
	ppl_new_Grid_from_grid_generators_2/3,
	ppl_new_Grid_from_congruences_2/3,
	ppl_Grid_space_dimension_2/3,
	ppl_Grid_affine_dimension_2/3,
	ppl_Grid_relation_with_constraint_2/4,
	ppl_Grid_relation_with_generator_2/4,
	ppl_Grid_relation_with_congruence_2/4,
	ppl_Grid_relation_with_grid_generator_2/4,
	ppl_Grid_get_constraints_2/3,
	ppl_Grid_get_congruences_2/3,
	ppl_Grid_get_grid_generators_2/3,
	ppl_Grid_get_minimized_constraints_2/3,
	ppl_Grid_get_minimized_congruences_2/3,
	ppl_Grid_get_minimized_grid_generators_2/3,
	ppl_Grid_is_empty_2/2,
	ppl_Grid_is_universe_2/2,
	ppl_Grid_is_bounded_2/2,
	ppl_Grid_contains_integer_point_2/2,
	ppl_Grid_is_topologically_closed_2/2,
	ppl_Grid_is_discrete_2/2,
	ppl_Grid_bounds_from_above_2/3,
	ppl_Grid_bounds_from_below_2/3,
	ppl_Grid_maximize_2/6,
	ppl_Grid_minimize_2/6,
	ppl_Grid_maximize_with_point_2/7,
	ppl_Grid_minimize_with_point_2/7,
	ppl_Grid_contains_Grid_2/3,
	ppl_Grid_strictly_contains_Grid_2/3,
	ppl_Grid_is_disjoint_from_Grid_2/3,
	ppl_Grid_equals_Grid_2/3,
	ppl_Grid_OK_2/2,
	ppl_Grid_upper_bound_assign_if_exact_2/3,
	ppl_Grid_simplify_using_context_assign_2/4,
	ppl_Grid_constrains_2/3,
	ppl_Grid_unconstrain_space_dimension_2/3,
	ppl_Grid_unconstrain_space_dimensions_2/3,
	ppl_Grid_generalized_affine_image_2/6,
	ppl_Grid_generalized_affine_preimage_2/6,
	ppl_Grid_generalized_affine_image_lhs_rhs_2/5,
	ppl_Grid_generalized_affine_preimage_lhs_rhs_2/5,
	ppl_Grid_generalized_affine_image_with_congruence_2/7,
	ppl_Grid_generalized_affine_preimage_with_congruence_2/7,
	ppl_Grid_generalized_affine_image_lhs_rhs_with_congruence_2/6,
	ppl_Grid_generalized_affine_preimage_lhs_rhs_with_congruence_2/6,
	ppl_Grid_remove_space_dimensions_2/3,
	ppl_Grid_fold_space_dimensions_2/4,
	ppl_Grid_map_space_dimensions_2/3,
	ppl_Grid_ascii_dump_2/2,
	ppl_Grid_external_memory_in_bytes_2/3,
	ppl_Grid_total_memory_in_bytes_2/3,
	ppl_Grid_congruence_widening_assign_with_tokens_2/5,
	ppl_Grid_generator_widening_assign_with_tokens_2/5,
	ppl_Grid_widening_assign_with_tokens_2/5,
	ppl_Grid_limited_congruence_extrapolation_assign_with_tokens_2/6,
	ppl_Grid_limited_generator_extrapolation_assign_with_tokens_2/6,
	ppl_new_Rational_Box_from_space_dimension_2/4,
	ppl_new_Rational_Box_from_C_Polyhedron_2/3,
	ppl_new_Rational_Box_from_NNC_Polyhedron_2/3,
	ppl_new_Rational_Box_from_Grid_2/3,
	ppl_new_Rational_Box_from_Rational_Box_2/3,
	ppl_new_Rational_Box_from_BD_Shape_mpz_class_2/3,
	ppl_new_Rational_Box_from_BD_Shape_mpq_class_2/3,
	ppl_new_Rational_Box_from_Octagonal_Shape_mpz_class_2/3,
	ppl_new_Rational_Box_from_Octagonal_Shape_mpq_class_2/3,
	ppl_new_Rational_Box_from_Double_Box_2/3,
	ppl_new_Rational_Box_from_BD_Shape_double_2/3,
	ppl_new_Rational_Box_from_Octagonal_Shape_double_2/3,
	ppl_new_Rational_Box_from_C_Polyhedron_with_complexity_2/4,
	ppl_new_Rational_Box_from_NNC_Polyhedron_with_complexity_2/4,
	ppl_new_Rational_Box_from_Grid_with_complexity_2/4,
	ppl_new_Rational_Box_from_Rational_Box_with_complexity_2/4,
	ppl_new_Rational_Box_from_BD_Shape_mpz_class_with_complexity_2/4,
	ppl_new_Rational_Box_from_BD_Shape_mpq_class_with_complexity_2/4,
	ppl_new_Rational_Box_from_Octagonal_Shape_mpz_class_with_complexity_2/4,
	ppl_new_Rational_Box_from_Octagonal_Shape_mpq_class_with_complexity_2/4,
	ppl_new_Rational_Box_from_Double_Box_with_complexity_2/4,
	ppl_new_Rational_Box_from_BD_Shape_double_with_complexity_2/4,
	ppl_new_Rational_Box_from_Octagonal_Shape_double_with_complexity_2/4,
	ppl_new_Rational_Box_from_constraints_2/3,
	ppl_new_Rational_Box_from_congruences_2/3,
	ppl_new_Rational_Box_from_generators_2/3,
	ppl_Rational_Box_space_dimension_2/3,
	ppl_Rational_Box_affine_dimension_2/3,
	ppl_Rational_Box_relation_with_constraint_2/4,
	ppl_Rational_Box_relation_with_generator_2/4,
	ppl_Rational_Box_relation_with_congruence_2/4,
	ppl_Rational_Box_get_constraints_2/3,
	ppl_Rational_Box_get_congruences_2/3,
	ppl_Rational_Box_get_minimized_constraints_2/3,
	ppl_Rational_Box_get_minimized_congruences_2/3,
	ppl_Rational_Box_is_empty_2/2,
	ppl_Rational_Box_is_universe_2/2,
	ppl_Rational_Box_is_bounded_2/2,
	ppl_Rational_Box_contains_integer_point_2/2,
	ppl_Rational_Box_is_topologically_closed_2/2,
	ppl_Rational_Box_is_discrete_2/2,
	ppl_Rational_Box_bounds_from_above_2/3,
	ppl_Rational_Box_bounds_from_below_2/3,
	ppl_Rational_Box_maximize_2/6,
	ppl_Rational_Box_minimize_2/6,
	ppl_Rational_Box_maximize_with_point_2/7,
	ppl_Rational_Box_minimize_with_point_2/7,
	ppl_Rational_Box_contains_Rational_Box_2/3,
	ppl_Rational_Box_strictly_contains_Rational_Box_2/3,
	ppl_Rational_Box_is_disjoint_from_Rational_Box_2/3,
	ppl_Rational_Box_equals_Rational_Box_2/3,
	ppl_Rational_Box_OK_2/2,
	ppl_Rational_Box_upper_bound_assign_if_exact_2/3,
	ppl_Rational_Box_simplify_using_context_assign_2/4,
	ppl_Rational_Box_constrains_2/3,
	ppl_Rational_Box_unconstrain_space_dimension_2/3,
	ppl_Rational_Box_unconstrain_space_dimensions_2/3,
	ppl_Rational_Box_generalized_affine_image_2/6,
	ppl_Rational_Box_generalized_affine_preimage_2/6,
	ppl_Rational_Box_generalized_affine_image_lhs_rhs_2/5,
	ppl_Rational_Box_generalized_affine_preimage_lhs_rhs_2/5,
	ppl_Rational_Box_remove_space_dimensions_2/3,
	ppl_Rational_Box_fold_space_dimensions_2/4,
	ppl_Rational_Box_map_space_dimensions_2/3,
	ppl_Rational_Box_ascii_dump_2/2,
	ppl_Rational_Box_external_memory_in_bytes_2/3,
	ppl_Rational_Box_total_memory_in_bytes_2/3,
	ppl_Rational_Box_CC76_widening_assign_with_tokens_2/5,
	ppl_Rational_Box_widening_assign_with_tokens_2/5,
	ppl_Rational_Box_limited_CC76_extrapolation_assign_with_tokens_2/6,
	ppl_Rational_Box_linear_partition_2/5,
	ppl_new_BD_Shape_mpz_class_from_space_dimension_2/4,
	ppl_new_BD_Shape_mpz_class_from_C_Polyhedron_2/3,
	ppl_new_BD_Shape_mpz_class_from_NNC_Polyhedron_2/3,
	ppl_new_BD_Shape_mpz_class_from_Grid_2/3,
	ppl_new_BD_Shape_mpz_class_from_Rational_Box_2/3,
	ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpz_class_2/3,
	ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpq_class_2/3,
	ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpz_class_2/3,
	ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpq_class_2/3,
	ppl_new_BD_Shape_mpz_class_from_Double_Box_2/3,
	ppl_new_BD_Shape_mpz_class_from_BD_Shape_double_2/3,
	ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_double_2/3,
	ppl_new_BD_Shape_mpz_class_from_C_Polyhedron_with_complexity_2/4,
	ppl_new_BD_Shape_mpz_class_from_NNC_Polyhedron_with_complexity_2/4,
	ppl_new_BD_Shape_mpz_class_from_Grid_with_complexity_2/4,
	ppl_new_BD_Shape_mpz_class_from_Rational_Box_with_complexity_2/4,
	ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpz_class_with_complexity_2/4,
	ppl_new_BD_Shape_mpz_class_from_BD_Shape_mpq_class_with_complexity_2/4,
	ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpz_class_with_complexity_2/4,
	ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_mpq_class_with_complexity_2/4,
	ppl_new_BD_Shape_mpz_class_from_Double_Box_with_complexity_2/4,
	ppl_new_BD_Shape_mpz_class_from_BD_Shape_double_with_complexity_2/4,
	ppl_new_BD_Shape_mpz_class_from_Octagonal_Shape_double_with_complexity_2/4,
	ppl_new_BD_Shape_mpz_class_from_constraints_2/3,
	ppl_new_BD_Shape_mpz_class_from_congruences_2/3,
	ppl_new_BD_Shape_mpz_class_from_generators_2/3,
	ppl_BD_Shape_mpz_class_space_dimension_2/3,
	ppl_BD_Shape_mpz_class_affine_dimension_2/3,
	ppl_BD_Shape_mpz_class_relation_with_constraint_2/4,
	ppl_BD_Shape_mpz_class_relation_with_generator_2/4,
	ppl_BD_Shape_mpz_class_relation_with_congruence_2/4,
	ppl_BD_Shape_mpz_class_get_constraints_2/3,
	ppl_BD_Shape_mpz_class_get_congruences_2/3,
	ppl_BD_Shape_mpz_class_get_minimized_constraints_2/3,
	ppl_BD_Shape_mpz_class_get_minimized_congruences_2/3,
	ppl_BD_Shape_mpz_class_is_empty_2/2,
	ppl_BD_Shape_mpz_class_is_universe_2/2,
	ppl_BD_Shape_mpz_class_is_bounded_2/2,
	ppl_BD_Shape_mpz_class_contains_integer_point_2/2,
	ppl_BD_Shape_mpz_class_is_topologically_closed_2/2,
	ppl_BD_Shape_mpz_class_is_discrete_2/2,
	ppl_BD_Shape_mpz_class_bounds_from_above_2/3,
	ppl_BD_Shape_mpz_class_bounds_from_below_2/3,
	ppl_BD_Shape_mpz_class_maximize_2/6,
	ppl_BD_Shape_mpz_class_minimize_2/6,
	ppl_BD_Shape_mpz_class_maximize_with_point_2/7,
	ppl_BD_Shape_mpz_class_minimize_with_point_2/7,
	ppl_BD_Shape_mpz_class_contains_BD_Shape_mpz_class_2/3,
	ppl_BD_Shape_mpz_class_strictly_contains_BD_Shape_mpz_class_2/3,
	ppl_BD_Shape_mpz_class_is_disjoint_from_BD_Shape_mpz_class_2/3,
	ppl_BD_Shape_mpz_class_equals_BD_Shape_mpz_class_2/3,
	ppl_BD_Shape_mpz_class_OK_2/2,
	ppl_BD_Shape_mpz_class_upper_bound_assign_if_exact_2/3,
	ppl_BD_Shape_mpz_class_simplify_using_context_assign_2/4,
	ppl_BD_Shape_mpz_class_constrains_2/3,
	ppl_BD_Shape_mpz_class_unconstrain_space_dimension_2/3,
	ppl_BD_Shape_mpz_class_unconstrain_space_dimensions_2/3,
	ppl_BD_Shape_mpz_class_generalized_affine_image_2/6,
	ppl_BD_Shape_mpz_class_generalized_affine_preimage_2/6,
	ppl_BD_Shape_mpz_class_generalized_affine_image_lhs_rhs_2/5,
	ppl_BD_Shape_mpz_class_generalized_affine_preimage_lhs_rhs_2/5,
	ppl_BD_Shape_mpz_class_remove_space_dimensions_2/3,
	ppl_BD_Shape_mpz_class_fold_space_dimensions_2/4,
	ppl_BD_Shape_mpz_class_map_space_dimensions_2/3,
	ppl_BD_Shape_mpz_class_ascii_dump_2/2,
	ppl_BD_Shape_mpz_class_external_memory_in_bytes_2/3,
	ppl_BD_Shape_mpz_class_total_memory_in_bytes_2/3,
	ppl_BD_Shape_mpz_class_BHMZ05_widening_assign_with_tokens_2/5,
	ppl_BD_Shape_mpz_class_H79_widening_assign_with_tokens_2/5,
	ppl_BD_Shape_mpz_class_widening_assign_with_tokens_2/5,
	ppl_BD_Shape_mpz_class_limited_BHMZ05_extrapolation_assign_with_tokens_2/6,
	ppl_BD_Shape_mpz_class_limited_H79_extrapolation_assign_with_tokens_2/6,
	ppl_BD_Shape_mpz_class_limited_CC76_extrapolation_assign_with_tokens_2/6,
	ppl_BD_Shape_mpz_class_CC76_extrapolation_assign_with_tokens_2/5,
	ppl_BD_Shape_mpz_class_CC76_narrowing_assign_2/3,
	ppl_BD_Shape_mpz_class_linear_partition_2/5,
	ppl_new_BD_Shape_mpq_class_from_space_dimension_2/4,
	ppl_new_BD_Shape_mpq_class_from_C_Polyhedron_2/3,
	ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron_2/3,
	ppl_new_BD_Shape_mpq_class_from_Grid_2/3,
	ppl_new_BD_Shape_mpq_class_from_Rational_Box_2/3,
	ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class_2/3,
	ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class_2/3,
	ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class_2/3,
	ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class_2/3,
	ppl_new_BD_Shape_mpq_class_from_Double_Box_2/3,
	ppl_new_BD_Shape_mpq_class_from_BD_Shape_double_2/3,
	ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_double_2/3,
	ppl_new_BD_Shape_mpq_class_from_C_Polyhedron_with_complexity_2/4,
	ppl_new_BD_Shape_mpq_class_from_NNC_Polyhedron_with_complexity_2/4,
	ppl_new_BD_Shape_mpq_class_from_Grid_with_complexity_2/4,
	ppl_new_BD_Shape_mpq_class_from_Rational_Box_with_complexity_2/4,
	ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity_2/4,
	ppl_new_BD_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity_2/4,
	ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity_2/4,
	ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity_2/4,
	ppl_new_BD_Shape_mpq_class_from_Double_Box_with_complexity_2/4,
	ppl_new_BD_Shape_mpq_class_from_BD_Shape_double_with_complexity_2/4,
	ppl_new_BD_Shape_mpq_class_from_Octagonal_Shape_double_with_complexity_2/4,
	ppl_new_BD_Shape_mpq_class_from_constraints_2/3,
	ppl_new_BD_Shape_mpq_class_from_congruences_2/3,
	ppl_new_BD_Shape_mpq_class_from_generators_2/3,
	ppl_BD_Shape_mpq_class_space_dimension_2/3,
	ppl_BD_Shape_mpq_class_affine_dimension_2/3,
	ppl_BD_Shape_mpq_class_relation_with_constraint_2/4,
	ppl_BD_Shape_mpq_class_relation_with_generator_2/4,
	ppl_BD_Shape_mpq_class_relation_with_congruence_2/4,
	ppl_BD_Shape_mpq_class_get_constraints_2/3,
	ppl_BD_Shape_mpq_class_get_congruences_2/3,
	ppl_BD_Shape_mpq_class_get_minimized_constraints_2/3,
	ppl_BD_Shape_mpq_class_get_minimized_congruences_2/3,
	ppl_BD_Shape_mpq_class_is_empty_2/2,
	ppl_BD_Shape_mpq_class_is_universe_2/2,
	ppl_BD_Shape_mpq_class_is_bounded_2/2,
	ppl_BD_Shape_mpq_class_contains_integer_point_2/2,
	ppl_BD_Shape_mpq_class_is_topologically_closed_2/2,
	ppl_BD_Shape_mpq_class_is_discrete_2/2,
	ppl_BD_Shape_mpq_class_bounds_from_above_2/3,
	ppl_BD_Shape_mpq_class_bounds_from_below_2/3,
	ppl_BD_Shape_mpq_class_maximize_2/6,
	ppl_BD_Shape_mpq_class_minimize_2/6,
	ppl_BD_Shape_mpq_class_maximize_with_point_2/7,
	ppl_BD_Shape_mpq_class_minimize_with_point_2/7,
	ppl_BD_Shape_mpq_class_contains_BD_Shape_mpq_class_2/3,
	ppl_BD_Shape_mpq_class_strictly_contains_BD_Shape_mpq_class_2/3,
	ppl_BD_Shape_mpq_class_is_disjoint_from_BD_Shape_mpq_class_2/3,
	ppl_BD_Shape_mpq_class_equals_BD_Shape_mpq_class_2/3,
	ppl_BD_Shape_mpq_class_OK_2/2,
	ppl_BD_Shape_mpq_class_upper_bound_assign_if_exact_2/3,
	ppl_BD_Shape_mpq_class_simplify_using_context_assign_2/4,
	ppl_BD_Shape_mpq_class_constrains_2/3,
	ppl_BD_Shape_mpq_class_unconstrain_space_dimension_2/3,
	ppl_BD_Shape_mpq_class_unconstrain_space_dimensions_2/3,
	ppl_BD_Shape_mpq_class_generalized_affine_image_2/6,
	ppl_BD_Shape_mpq_class_generalized_affine_preimage_2/6,
	ppl_BD_Shape_mpq_class_generalized_affine_image_lhs_rhs_2/5,
	ppl_BD_Shape_mpq_class_generalized_affine_preimage_lhs_rhs_2/5,
	ppl_BD_Shape_mpq_class_remove_space_dimensions_2/3,
	ppl_BD_Shape_mpq_class_fold_space_dimensions_2/4,
	ppl_BD_Shape_mpq_class_map_space_dimensions_2/3,
	ppl_BD_Shape_mpq_class_ascii_dump_2/2,
	ppl_BD_Shape_mpq_class_external_memory_in_bytes_2/3,
	ppl_BD_Shape_mpq_class_total_memory_in_bytes_2/3,
	ppl_BD_Shape_mpq_class_BHMZ05_widening_assign_with_tokens_2/5,
	ppl_BD_Shape_mpq_class_H79_widening_assign_with_tokens_2/5,
	ppl_BD_Shape_mpq_class_widening_assign_with_tokens_2/5,
	ppl_BD_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens_2/6,
	ppl_BD_Shape_mpq_class_limited_H79_extrapolation_assign_with_tokens_2/6,
	ppl_BD_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens_2/6,
	ppl_BD_Shape_mpq_class_CC76_extrapolation_assign_with_tokens_2/5,
	ppl_BD_Shape_mpq_class_CC76_narrowing_assign_2/3,
	ppl_BD_Shape_mpq_class_linear_partition_2/5,
	ppl_new_Octagonal_Shape_mpz_class_from_space_dimension_2/4,
	ppl_new_Octagonal_Shape_mpz_class_from_C_Polyhedron_2/3,
	ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron_2/3,
	ppl_new_Octagonal_Shape_mpz_class_from_Grid_2/3,
	ppl_new_Octagonal_Shape_mpz_class_from_Rational_Box_2/3,
	ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpz_class_2/3,
	ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpq_class_2/3,
	ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class_2/3,
	ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpq_class_2/3,
	ppl_new_Octagonal_Shape_mpz_class_from_Double_Box_2/3,
	ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_double_2/3,
	ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_double_2/3,
	ppl_new_Octagonal_Shape_mpz_class_from_C_Polyhedron_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpz_class_from_NNC_Polyhedron_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpz_class_from_Grid_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpz_class_from_Rational_Box_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpz_class_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_mpq_class_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpz_class_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_mpq_class_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpz_class_from_Double_Box_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpz_class_from_BD_Shape_double_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpz_class_from_Octagonal_Shape_double_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpz_class_from_constraints_2/3,
	ppl_new_Octagonal_Shape_mpz_class_from_congruences_2/3,
	ppl_new_Octagonal_Shape_mpz_class_from_generators_2/3,
	ppl_Octagonal_Shape_mpz_class_space_dimension_2/3,
	ppl_Octagonal_Shape_mpz_class_affine_dimension_2/3,
	ppl_Octagonal_Shape_mpz_class_relation_with_constraint_2/4,
	ppl_Octagonal_Shape_mpz_class_relation_with_generator_2/4,
	ppl_Octagonal_Shape_mpz_class_relation_with_congruence_2/4,
	ppl_Octagonal_Shape_mpz_class_get_constraints_2/3,
	ppl_Octagonal_Shape_mpz_class_get_congruences_2/3,
	ppl_Octagonal_Shape_mpz_class_get_minimized_constraints_2/3,
	ppl_Octagonal_Shape_mpz_class_get_minimized_congruences_2/3,
	ppl_Octagonal_Shape_mpz_class_is_empty_2/2,
	ppl_Octagonal_Shape_mpz_class_is_universe_2/2,
	ppl_Octagonal_Shape_mpz_class_is_bounded_2/2,
	ppl_Octagonal_Shape_mpz_class_contains_integer_point_2/2,
	ppl_Octagonal_Shape_mpz_class_is_topologically_closed_2/2,
	ppl_Octagonal_Shape_mpz_class_is_discrete_2/2,
	ppl_Octagonal_Shape_mpz_class_bounds_from_above_2/3,
	ppl_Octagonal_Shape_mpz_class_bounds_from_below_2/3,
	ppl_Octagonal_Shape_mpz_class_maximize_2/6,
	ppl_Octagonal_Shape_mpz_class_minimize_2/6,
	ppl_Octagonal_Shape_mpz_class_maximize_with_point_2/7,
	ppl_Octagonal_Shape_mpz_class_minimize_with_point_2/7,
	ppl_Octagonal_Shape_mpz_class_contains_Octagonal_Shape_mpz_class_2/3,
	ppl_Octagonal_Shape_mpz_class_strictly_contains_Octagonal_Shape_mpz_class_2/3,
	ppl_Octagonal_Shape_mpz_class_is_disjoint_from_Octagonal_Shape_mpz_class_2/3,
	ppl_Octagonal_Shape_mpz_class_equals_Octagonal_Shape_mpz_class_2/3,
	ppl_Octagonal_Shape_mpz_class_OK_2/2,
	ppl_Octagonal_Shape_mpz_class_upper_bound_assign_if_exact_2/3,
	ppl_Octagonal_Shape_mpz_class_simplify_using_context_assign_2/4,
	ppl_Octagonal_Shape_mpz_class_constrains_2/3,
	ppl_Octagonal_Shape_mpz_class_unconstrain_space_dimension_2/3,
	ppl_Octagonal_Shape_mpz_class_unconstrain_space_dimensions_2/3,
	ppl_Octagonal_Shape_mpz_class_generalized_affine_image_2/6,
	ppl_Octagonal_Shape_mpz_class_generalized_affine_preimage_2/6,
	ppl_Octagonal_Shape_mpz_class_generalized_affine_image_lhs_rhs_2/5,
	ppl_Octagonal_Shape_mpz_class_generalized_affine_preimage_lhs_rhs_2/5,
	ppl_Octagonal_Shape_mpz_class_remove_space_dimensions_2/3,
	ppl_Octagonal_Shape_mpz_class_fold_space_dimensions_2/4,
	ppl_Octagonal_Shape_mpz_class_map_space_dimensions_2/3,
	ppl_Octagonal_Shape_mpz_class_ascii_dump_2/2,
	ppl_Octagonal_Shape_mpz_class_external_memory_in_bytes_2/3,
	ppl_Octagonal_Shape_mpz_class_total_memory_in_bytes_2/3,
	ppl_Octagonal_Shape_mpz_class_BHMZ05_widening_assign_with_tokens_2/5,
	ppl_Octagonal_Shape_mpz_class_widening_assign_with_tokens_2/5,
	ppl_Octagonal_Shape_mpz_class_limited_BHMZ05_extrapolation_assign_with_tokens_2/6,
	ppl_Octagonal_Shape_mpz_class_limited_CC76_extrapolation_assign_with_tokens_2/6,
	ppl_Octagonal_Shape_mpz_class_CC76_extrapolation_assign_with_tokens_2/5,
	ppl_Octagonal_Shape_mpz_class_CC76_narrowing_assign_2/3,
	ppl_Octagonal_Shape_mpz_class_linear_partition_2/5,
	ppl_new_Octagonal_Shape_mpq_class_from_space_dimension_2/4,
	ppl_new_Octagonal_Shape_mpq_class_from_C_Polyhedron_2/3,
	ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron_2/3,
	ppl_new_Octagonal_Shape_mpq_class_from_Grid_2/3,
	ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box_2/3,
	ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class_2/3,
	ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class_2/3,
	ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class_2/3,
	ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class_2/3,
	ppl_new_Octagonal_Shape_mpq_class_from_Double_Box_2/3,
	ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_double_2/3,
	ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_double_2/3,
	ppl_new_Octagonal_Shape_mpq_class_from_C_Polyhedron_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpq_class_from_NNC_Polyhedron_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpq_class_from_Grid_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpq_class_from_Rational_Box_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpz_class_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_mpq_class_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpz_class_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_mpq_class_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpq_class_from_Double_Box_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpq_class_from_BD_Shape_double_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpq_class_from_Octagonal_Shape_double_with_complexity_2/4,
	ppl_new_Octagonal_Shape_mpq_class_from_constraints_2/3,
	ppl_new_Octagonal_Shape_mpq_class_from_congruences_2/3,
	ppl_new_Octagonal_Shape_mpq_class_from_generators_2/3,
	ppl_Octagonal_Shape_mpq_class_space_dimension_2/3,
	ppl_Octagonal_Shape_mpq_class_affine_dimension_2/3,
	ppl_Octagonal_Shape_mpq_class_relation_with_constraint_2/4,
	ppl_Octagonal_Shape_mpq_class_relation_with_generator_2/4,
	ppl_Octagonal_Shape_mpq_class_relation_with_congruence_2/4,
	ppl_Octagonal_Shape_mpq_class_get_constraints_2/3,
	ppl_Octagonal_Shape_mpq_class_get_congruences_2/3,
	ppl_Octagonal_Shape_mpq_class_get_minimized_constraints_2/3,
	ppl_Octagonal_Shape_mpq_class_get_minimized_congruences_2/3,
	ppl_Octagonal_Shape_mpq_class_is_empty_2/2,
	ppl_Octagonal_Shape_mpq_class_is_universe_2/2,
	ppl_Octagonal_Shape_mpq_class_is_bounded_2/2,
	ppl_Octagonal_Shape_mpq_class_contains_integer_point_2/2,
	ppl_Octagonal_Shape_mpq_class_is_topologically_closed_2/2,
	ppl_Octagonal_Shape_mpq_class_is_discrete_2/2,
	ppl_Octagonal_Shape_mpq_class_bounds_from_above_2/3,
	ppl_Octagonal_Shape_mpq_class_bounds_from_below_2/3,
	ppl_Octagonal_Shape_mpq_class_maximize_2/6,
	ppl_Octagonal_Shape_mpq_class_minimize_2/6,
	ppl_Octagonal_Shape_mpq_class_maximize_with_point_2/7,
	ppl_Octagonal_Shape_mpq_class_minimize_with_point_2/7,
	ppl_Octagonal_Shape_mpq_class_contains_Octagonal_Shape_mpq_class_2/3,
	ppl_Octagonal_Shape_mpq_class_strictly_contains_Octagonal_Shape_mpq_class_2/3,
	ppl_Octagonal_Shape_mpq_class_is_disjoint_from_Octagonal_Shape_mpq_class_2/3,
	ppl_Octagonal_Shape_mpq_class_equals_Octagonal_Shape_mpq_class_2/3,
	ppl_Octagonal_Shape_mpq_class_OK_2/2,
	ppl_Octagonal_Shape_mpq_class_upper_bound_assign_if_exact_2/3,
	ppl_Octagonal_Shape_mpq_class_simplify_using_context_assign_2/4,
	ppl_Octagonal_Shape_mpq_class_constrains_2/3,
	ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimension_2/3,
	ppl_Octagonal_Shape_mpq_class_unconstrain_space_dimensions_2/3,
	ppl_Octagonal_Shape_mpq_class_generalized_affine_image_2/6,
	ppl_Octagonal_Shape_mpq_class_generalized_affine_preimage_2/6,
	ppl_Octagonal_Shape_mpq_class_generalized_affine_image_lhs_rhs_2/5,
	ppl_Octagonal_Shape_mpq_class_generalized_affine_preimage_lhs_rhs_2/5,
	ppl_Octagonal_Shape_mpq_class_remove_space_dimensions_2/3,
	ppl_Octagonal_Shape_mpq_class_fold_space_dimensions_2/4,
	ppl_Octagonal_Shape_mpq_class_map_space_dimensions_2/3,
	ppl_Octagonal_Shape_mpq_class_ascii_dump_2/2,
	ppl_Octagonal_Shape_mpq_class_external_memory_in_bytes_2/3,
	ppl_Octagonal_Shape_mpq_class_total_memory_in_bytes_2/3,
	ppl_Octagonal_Shape_mpq_class_BHMZ05_widening_assign_with_tokens_2/5,
	ppl_Octagonal_Shape_mpq_class_widening_assign_with_tokens_2/5,
	ppl_Octagonal_Shape_mpq_class_limited_BHMZ05_extrapolation_assign_with_tokens_2/6,
	ppl_Octagonal_Shape_mpq_class_limited_CC76_extrapolation_assign_with_tokens_2/6,
	ppl_Octagonal_Shape_mpq_class_CC76_extrapolation_assign_with_tokens_2/5,
	ppl_Octagonal_Shape_mpq_class_CC76_narrowing_assign_2/3,
	ppl_Octagonal_Shape_mpq_class_linear_partition_2/5,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_space_dimension_2/4,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_C_Polyhedron_2/3,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_NNC_Polyhedron_2/3,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Grid_2/3,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Rational_Box_2/3,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpz_class_2/3,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpq_class_2/3,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpz_class_2/3,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpq_class_2/3,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Double_Box_2/3,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_double_2/3,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_double_2/3,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Constraints_Product_C_Polyhedron_Grid_2/3,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_C_Polyhedron_with_complexity_2/4,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_NNC_Polyhedron_with_complexity_2/4,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Grid_with_complexity_2/4,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Rational_Box_with_complexity_2/4,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpz_class_with_complexity_2/4,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_mpq_class_with_complexity_2/4,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpz_class_with_complexity_2/4,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_mpq_class_with_complexity_2/4,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Double_Box_with_complexity_2/4,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_BD_Shape_double_with_complexity_2/4,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Octagonal_Shape_double_with_complexity_2/4,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_Constraints_Product_C_Polyhedron_Grid_with_complexity_2/4,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_constraints_2/3,
	ppl_new_Constraints_Product_C_Polyhedron_Grid_from_congruences_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_space_dimension_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_affine_dimension_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_constraint_2/4,
	ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_generator_2/4,
	ppl_Constraints_Product_C_Polyhedron_Grid_relation_with_congruence_2/4,
	ppl_Constraints_Product_C_Polyhedron_Grid_is_empty_2/2,
	ppl_Constraints_Product_C_Polyhedron_Grid_is_universe_2/2,
	ppl_Constraints_Product_C_Polyhedron_Grid_is_bounded_2/2,
	ppl_Constraints_Product_C_Polyhedron_Grid_is_topologically_closed_2/2,
	ppl_Constraints_Product_C_Polyhedron_Grid_is_discrete_2/2,
	ppl_Constraints_Product_C_Polyhedron_Grid_bounds_from_above_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_bounds_from_below_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_maximize_2/6,
	ppl_Constraints_Product_C_Polyhedron_Grid_minimize_2/6,
	ppl_Constraints_Product_C_Polyhedron_Grid_maximize_with_point_2/7,
	ppl_Constraints_Product_C_Polyhedron_Grid_minimize_with_point_2/7,
	ppl_Constraints_Product_C_Polyhedron_Grid_contains_Constraints_Product_C_Polyhedron_Grid_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_strictly_contains_Constraints_Product_C_Polyhedron_Grid_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_is_disjoint_from_Constraints_Product_C_Polyhedron_Grid_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_equals_Constraints_Product_C_Polyhedron_Grid_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_OK_2/2,
	ppl_Constraints_Product_C_Polyhedron_Grid_upper_bound_assign_if_exact_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_constrains_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_unconstrain_space_dimension_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_unconstrain_space_dimensions_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_image_2/6,
	ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_preimage_2/6,
	ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_image_lhs_rhs_2/5,
	ppl_Constraints_Product_C_Polyhedron_Grid_generalized_affine_preimage_lhs_rhs_2/5,
	ppl_Constraints_Product_C_Polyhedron_Grid_remove_space_dimensions_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_fold_space_dimensions_2/4,
	ppl_Constraints_Product_C_Polyhedron_Grid_map_space_dimensions_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_ascii_dump_2/2,
	ppl_Constraints_Product_C_Polyhedron_Grid_external_memory_in_bytes_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_total_memory_in_bytes_2/3,
	ppl_Constraints_Product_C_Polyhedron_Grid_widening_assign_with_tokens_2/5,
	ppl_new_Pointset_Powerset_C_Polyhedron_from_space_dimension_2/4,
	ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron_2/3,
	ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron_2/3,
	ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron_with_complexity_2/4,
	ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron_with_complexity_2/4,
	ppl_new_Pointset_Powerset_C_Polyhedron_from_constraints_2/3,
	ppl_new_Pointset_Powerset_C_Polyhedron_from_congruences_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_space_dimension_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_affine_dimension_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_relation_with_constraint_2/4,
	ppl_Pointset_Powerset_C_Polyhedron_relation_with_generator_2/4,
	ppl_Pointset_Powerset_C_Polyhedron_relation_with_congruence_2/4,
	ppl_Pointset_Powerset_C_Polyhedron_is_empty_2/2,
	ppl_Pointset_Powerset_C_Polyhedron_is_universe_2/2,
	ppl_Pointset_Powerset_C_Polyhedron_is_bounded_2/2,
	ppl_Pointset_Powerset_C_Polyhedron_contains_integer_point_2/2,
	ppl_Pointset_Powerset_C_Polyhedron_is_topologically_closed_2/2,
	ppl_Pointset_Powerset_C_Polyhedron_is_discrete_2/2,
	ppl_Pointset_Powerset_C_Polyhedron_bounds_from_above_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_bounds_from_below_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_maximize_2/6,
	ppl_Pointset_Powerset_C_Polyhedron_minimize_2/6,
	ppl_Pointset_Powerset_C_Polyhedron_maximize_with_point_2/7,
	ppl_Pointset_Powerset_C_Polyhedron_minimize_with_point_2/7,
	ppl_Pointset_Powerset_C_Polyhedron_contains_Pointset_Powerset_C_Polyhedron_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_strictly_contains_Pointset_Powerset_C_Polyhedron_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_is_disjoint_from_Pointset_Powerset_C_Polyhedron_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_geometrically_covers_Pointset_Powerset_C_Polyhedron_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_geometrically_equals_Pointset_Powerset_C_Polyhedron_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_equals_Pointset_Powerset_C_Polyhedron_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_OK_2/2,
	ppl_Pointset_Powerset_C_Polyhedron_upper_bound_assign_if_exact_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_simplify_using_context_assign_2/4,
	ppl_Pointset_Powerset_C_Polyhedron_constrains_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_unconstrain_space_dimension_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_unconstrain_space_dimensions_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_image_2/6,
	ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_preimage_2/6,
	ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_image_lhs_rhs_2/5,
	ppl_Pointset_Powerset_C_Polyhedron_generalized_affine_preimage_lhs_rhs_2/5,
	ppl_Pointset_Powerset_C_Polyhedron_remove_space_dimensions_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_fold_space_dimensions_2/4,
	ppl_Pointset_Powerset_C_Polyhedron_map_space_dimensions_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_ascii_dump_2/2,
	ppl_Pointset_Powerset_C_Polyhedron_external_memory_in_bytes_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_total_memory_in_bytes_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_size_2/3,
	ppl_new_Pointset_Powerset_C_Polyhedron_iterator_from_iterator_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_begin_iterator_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_end_iterator_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_iterator_equals_iterator_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_increment_iterator_2/2,
	ppl_Pointset_Powerset_C_Polyhedron_decrement_iterator_2/2,
	ppl_Pointset_Powerset_C_Polyhedron_get_disjunct_2/3,
	ppl_delete_Pointset_Powerset_C_Polyhedron_iterator_2/2,
	ppl_Pointset_Powerset_C_Polyhedron_drop_disjunct_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_drop_disjuncts_2/4,
	ppl_Pointset_Powerset_C_Polyhedron_BHZ03_BHRZ03_BHRZ03_widening_assign_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_BHZ03_H79_H79_widening_assign_2/3,
	ppl_Pointset_Powerset_C_Polyhedron_BGP99_BHRZ03_extrapolation_assign_2/4,
	ppl_Pointset_Powerset_C_Polyhedron_BGP99_H79_extrapolation_assign_2/4,
	ppl_new_Pointset_Powerset_NNC_Polyhedron_from_space_dimension_2/4,
	ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron_2/3,
	ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron_2/3,
	ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron_with_complexity_2/4,
	ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron_with_complexity_2/4,
	ppl_new_Pointset_Powerset_NNC_Polyhedron_from_constraints_2/3,
	ppl_new_Pointset_Powerset_NNC_Polyhedron_from_congruences_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_space_dimension_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_affine_dimension_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_constraint_2/4,
	ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_generator_2/4,
	ppl_Pointset_Powerset_NNC_Polyhedron_relation_with_congruence_2/4,
	ppl_Pointset_Powerset_NNC_Polyhedron_is_empty_2/2,
	ppl_Pointset_Powerset_NNC_Polyhedron_is_universe_2/2,
	ppl_Pointset_Powerset_NNC_Polyhedron_is_bounded_2/2,
	ppl_Pointset_Powerset_NNC_Polyhedron_contains_integer_point_2/2,
	ppl_Pointset_Powerset_NNC_Polyhedron_is_topologically_closed_2/2,
	ppl_Pointset_Powerset_NNC_Polyhedron_is_discrete_2/2,
	ppl_Pointset_Powerset_NNC_Polyhedron_bounds_from_above_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_bounds_from_below_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_maximize_2/6,
	ppl_Pointset_Powerset_NNC_Polyhedron_minimize_2/6,
	ppl_Pointset_Powerset_NNC_Polyhedron_maximize_with_point_2/7,
	ppl_Pointset_Powerset_NNC_Polyhedron_minimize_with_point_2/7,
	ppl_Pointset_Powerset_NNC_Polyhedron_contains_Pointset_Powerset_NNC_Polyhedron_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_strictly_contains_Pointset_Powerset_NNC_Polyhedron_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_is_disjoint_from_Pointset_Powerset_NNC_Polyhedron_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_covers_Pointset_Powerset_NNC_Polyhedron_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_geometrically_equals_Pointset_Powerset_NNC_Polyhedron_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_equals_Pointset_Powerset_NNC_Polyhedron_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_OK_2/2,
	ppl_Pointset_Powerset_NNC_Polyhedron_upper_bound_assign_if_exact_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_simplify_using_context_assign_2/4,
	ppl_Pointset_Powerset_NNC_Polyhedron_constrains_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimension_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_unconstrain_space_dimensions_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_image_2/6,
	ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_preimage_2/6,
	ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_image_lhs_rhs_2/5,
	ppl_Pointset_Powerset_NNC_Polyhedron_generalized_affine_preimage_lhs_rhs_2/5,
	ppl_Pointset_Powerset_NNC_Polyhedron_remove_space_dimensions_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_fold_space_dimensions_2/4,
	ppl_Pointset_Powerset_NNC_Polyhedron_map_space_dimensions_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_ascii_dump_2/2,
	ppl_Pointset_Powerset_NNC_Polyhedron_external_memory_in_bytes_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_total_memory_in_bytes_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_size_2/3,
	ppl_new_Pointset_Powerset_NNC_Polyhedron_iterator_from_iterator_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_begin_iterator_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_end_iterator_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_iterator_equals_iterator_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_increment_iterator_2/2,
	ppl_Pointset_Powerset_NNC_Polyhedron_decrement_iterator_2/2,
	ppl_Pointset_Powerset_NNC_Polyhedron_get_disjunct_2/3,
	ppl_delete_Pointset_Powerset_NNC_Polyhedron_iterator_2/2,
	ppl_Pointset_Powerset_NNC_Polyhedron_drop_disjunct_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_drop_disjuncts_2/4,
	ppl_Pointset_Powerset_NNC_Polyhedron_BHZ03_BHRZ03_BHRZ03_widening_assign_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_BHZ03_H79_H79_widening_assign_2/3,
	ppl_Pointset_Powerset_NNC_Polyhedron_BGP99_BHRZ03_extrapolation_assign_2/4,
	ppl_Pointset_Powerset_NNC_Polyhedron_BGP99_H79_extrapolation_assign_2/4,
	ppl_new_Double_Box_from_space_dimension_2/4,
	ppl_new_Double_Box_from_C_Polyhedron_2/3,
	ppl_new_Double_Box_from_NNC_Polyhedron_2/3,
	ppl_new_Double_Box_from_Grid_2/3,
	ppl_new_Double_Box_from_Rational_Box_2/3,
	ppl_new_Double_Box_from_BD_Shape_mpz_class_2/3,
	ppl_new_Double_Box_from_BD_Shape_mpq_class_2/3,
	ppl_new_Double_Box_from_Octagonal_Shape_mpz_class_2/3,
	ppl_new_Double_Box_from_Octagonal_Shape_mpq_class_2/3,
	ppl_new_Double_Box_from_Double_Box_2/3,
	ppl_new_Double_Box_from_BD_Shape_double_2/3,
	ppl_new_Double_Box_from_Octagonal_Shape_double_2/3,
	ppl_new_Double_Box_from_C_Polyhedron_with_complexity_2/4,
	ppl_new_Double_Box_from_NNC_Polyhedron_with_complexity_2/4,
	ppl_new_Double_Box_from_Grid_with_complexity_2/4,
	ppl_new_Double_Box_from_Rational_Box_with_complexity_2/4,
	ppl_new_Double_Box_from_BD_Shape_mpz_class_with_complexity_2/4,
	ppl_new_Double_Box_from_BD_Shape_mpq_class_with_complexity_2/4,
	ppl_new_Double_Box_from_Octagonal_Shape_mpz_class_with_complexity_2/4,
	ppl_new_Double_Box_from_Octagonal_Shape_mpq_class_with_complexity_2/4,
	ppl_new_Double_Box_from_Double_Box_with_complexity_2/4,
	ppl_new_Double_Box_from_BD_Shape_double_with_complexity_2/4,
	ppl_new_Double_Box_from_Octagonal_Shape_double_with_complexity_2/4,
	ppl_new_Double_Box_from_constraints_2/3,
	ppl_new_Double_Box_from_congruences_2/3,
	ppl_new_Double_Box_from_generators_2/3,
	ppl_Double_Box_space_dimension_2/3,
	ppl_Double_Box_affine_dimension_2/3,
	ppl_Double_Box_relation_with_constraint_2/4,
	ppl_Double_Box_relation_with_generator_2/4,
	ppl_Double_Box_relation_with_congruence_2/4,
	ppl_Double_Box_get_constraints_2/3,
	ppl_Double_Box_get_congruences_2/3,
	ppl_Double_Box_get_minimized_constraints_2/3,
	ppl_Double_Box_get_minimized_congruences_2/3,
	ppl_Double_Box_is_empty_2/2,
	ppl_Double_Box_is_universe_2/2,
	ppl_Double_Box_is_bounded_2/2,
	ppl_Double_Box_contains_integer_point_2/2,
	ppl_Double_Box_is_topologically_closed_2/2,
	ppl_Double_Box_is_discrete_2/2,
	ppl_Double_Box_bounds_from_above_2/3,
	ppl_Double_Box_bounds_from_below_2/3,
	ppl_Double_Box_maximize_2/6,
	ppl_Double_Box_minimize_2/6,
	ppl_Double_Box_maximize_with_point_2/7,
	ppl_Double_Box_minimize_with_point_2/7,
	ppl_Double_Box_contains_Double_Box_2/3,
	ppl_Double_Box_strictly_contains_Double_Box_2/3,
	ppl_Double_Box_is_disjoint_from_Double_Box_2/3,
	ppl_Double_Box_equals_Double_Box_2/3,
	ppl_Double_Box_OK_2/2,
	ppl_Double_Box_upper_bound_assign_if_exact_2/3,
	ppl_Double_Box_simplify_using_context_assign_2/4,
	ppl_Double_Box_constrains_2/3,
	ppl_Double_Box_unconstrain_space_dimension_2/3,
	ppl_Double_Box_unconstrain_space_dimensions_2/3,
	ppl_Double_Box_generalized_affine_image_2/6,
	ppl_Double_Box_generalized_affine_preimage_2/6,
	ppl_Double_Box_generalized_affine_image_lhs_rhs_2/5,
	ppl_Double_Box_generalized_affine_preimage_lhs_rhs_2/5,
	ppl_Double_Box_remove_space_dimensions_2/3,
	ppl_Double_Box_fold_space_dimensions_2/4,
	ppl_Double_Box_map_space_dimensions_2/3,
	ppl_Double_Box_ascii_dump_2/2,
	ppl_Double_Box_external_memory_in_bytes_2/3,
	ppl_Double_Box_total_memory_in_bytes_2/3,
	ppl_Double_Box_CC76_widening_assign_with_tokens_2/5,
	ppl_Double_Box_widening_assign_with_tokens_2/5,
	ppl_Double_Box_limited_CC76_extrapolation_assign_with_tokens_2/6,
	ppl_Double_Box_linear_partition_2/5,
	ppl_new_BD_Shape_double_from_space_dimension_2/4,
	ppl_new_BD_Shape_double_from_C_Polyhedron_2/3,
	ppl_new_BD_Shape_double_from_NNC_Polyhedron_2/3,
	ppl_new_BD_Shape_double_from_Grid_2/3,
	ppl_new_BD_Shape_double_from_Rational_Box_2/3,
	ppl_new_BD_Shape_double_from_BD_Shape_mpz_class_2/3,
	ppl_new_BD_Shape_double_from_BD_Shape_mpq_class_2/3,
	ppl_new_BD_Shape_double_from_Octagonal_Shape_mpz_class_2/3,
	ppl_new_BD_Shape_double_from_Octagonal_Shape_mpq_class_2/3,
	ppl_new_BD_Shape_double_from_Double_Box_2/3,
	ppl_new_BD_Shape_double_from_BD_Shape_double_2/3,
	ppl_new_BD_Shape_double_from_Octagonal_Shape_double_2/3,
	ppl_new_BD_Shape_double_from_C_Polyhedron_with_complexity_2/4,
	ppl_new_BD_Shape_double_from_NNC_Polyhedron_with_complexity_2/4,
	ppl_new_BD_Shape_double_from_Grid_with_complexity_2/4,
	ppl_new_BD_Shape_double_from_Rational_Box_with_complexity_2/4,
	ppl_new_BD_Shape_double_from_BD_Shape_mpz_class_with_complexity_2/4,
	ppl_new_BD_Shape_double_from_BD_Shape_mpq_class_with_complexity_2/4,
	ppl_new_BD_Shape_double_from_Octagonal_Shape_mpz_class_with_complexity_2/4,
	ppl_new_BD_Shape_double_from_Octagonal_Shape_mpq_class_with_complexity_2/4,
	ppl_new_BD_Shape_double_from_Double_Box_with_complexity_2/4,
	ppl_new_BD_Shape_double_from_BD_Shape_double_with_complexity_2/4,
	ppl_new_BD_Shape_double_from_Octagonal_Shape_double_with_complexity_2/4,
	ppl_new_BD_Shape_double_from_constraints_2/3,
	ppl_new_BD_Shape_double_from_congruences_2/3,
	ppl_new_BD_Shape_double_from_generators_2/3,
	ppl_BD_Shape_double_space_dimension_2/3,
	ppl_BD_Shape_double_affine_dimension_2/3,
	ppl_BD_Shape_double_relation_with_constraint_2/4,
	ppl_BD_Shape_double_relation_with_generator_2/4,
	ppl_BD_Shape_double_relation_with_congruence_2/4,
	ppl_BD_Shape_double_get_constraints_2/3,
	ppl_BD_Shape_double_get_congruences_2/3,
	ppl_BD_Shape_double_get_minimized_constraints_2/3,
	ppl_BD_Shape_double_get_minimized_congruences_2/3,
	ppl_BD_Shape_double_is_empty_2/2,
	ppl_BD_Shape_double_is_universe_2/2,
	ppl_BD_Shape_double_is_bounded_2/2,
	ppl_BD_Shape_double_contains_integer_point_2/2,
	ppl_BD_Shape_double_is_topologically_closed_2/2,
	ppl_BD_Shape_double_is_discrete_2/2,
	ppl_BD_Shape_double_bounds_from_above_2/3,
	ppl_BD_Shape_double_bounds_from_below_2/3,
	ppl_BD_Shape_double_maximize_2/6,
	ppl_BD_Shape_double_minimize_2/6,
	ppl_BD_Shape_double_maximize_with_point_2/7,
	ppl_BD_Shape_double_minimize_with_point_2/7,
	ppl_BD_Shape_double_contains_BD_Shape_double_2/3,
	ppl_BD_Shape_double_strictly_contains_BD_Shape_double_2/3,
	ppl_BD_Shape_double_is_disjoint_from_BD_Shape_double_2/3,
	ppl_BD_Shape_double_equals_BD_Shape_double_2/3,
	ppl_BD_Shape_double_OK_2/2,
	ppl_BD_Shape_double_upper_bound_assign_if_exact_2/3,
	ppl_BD_Shape_double_simplify_using_context_assign_2/4,
	ppl_BD_Shape_double_constrains_2/3,
	ppl_BD_Shape_double_unconstrain_space_dimension_2/3,
	ppl_BD_Shape_double_unconstrain_space_dimensions_2/3,
	ppl_BD_Shape_double_generalized_affine_image_2/6,
	ppl_BD_Shape_double_generalized_affine_preimage_2/6,
	ppl_BD_Shape_double_generalized_affine_image_lhs_rhs_2/5,
	ppl_BD_Shape_double_generalized_affine_preimage_lhs_rhs_2/5,
	ppl_BD_Shape_double_remove_space_dimensions_2/3,
	ppl_BD_Shape_double_fold_space_dimensions_2/4,
	ppl_BD_Shape_double_map_space_dimensions_2/3,
	ppl_BD_Shape_double_ascii_dump_2/2,
	ppl_BD_Shape_double_external_memory_in_bytes_2/3,
	ppl_BD_Shape_double_total_memory_in_bytes_2/3,
	ppl_BD_Shape_double_BHMZ05_widening_assign_with_tokens_2/5,
	ppl_BD_Shape_double_H79_widening_assign_with_tokens_2/5,
	ppl_BD_Shape_double_widening_assign_with_tokens_2/5,
	ppl_BD_Shape_double_limited_BHMZ05_extrapolation_assign_with_tokens_2/6,
	ppl_BD_Shape_double_limited_H79_extrapolation_assign_with_tokens_2/6,
	ppl_BD_Shape_double_limited_CC76_extrapolation_assign_with_tokens_2/6,
	ppl_BD_Shape_double_CC76_extrapolation_assign_with_tokens_2/5,
	ppl_BD_Shape_double_CC76_narrowing_assign_2/3,
	ppl_BD_Shape_double_linear_partition_2/5,
	ppl_new_Octagonal_Shape_double_from_space_dimension_2/4,
	ppl_new_Octagonal_Shape_double_from_C_Polyhedron_2/3,
	ppl_new_Octagonal_Shape_double_from_NNC_Polyhedron_2/3,
	ppl_new_Octagonal_Shape_double_from_Grid_2/3,
	ppl_new_Octagonal_Shape_double_from_Rational_Box_2/3,
	ppl_new_Octagonal_Shape_double_from_BD_Shape_mpz_class_2/3,
	ppl_new_Octagonal_Shape_double_from_BD_Shape_mpq_class_2/3,
	ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpz_class_2/3,
	ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpq_class_2/3,
	ppl_new_Octagonal_Shape_double_from_Double_Box_2/3,
	ppl_new_Octagonal_Shape_double_from_BD_Shape_double_2/3,
	ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_double_2/3,
	ppl_new_Octagonal_Shape_double_from_C_Polyhedron_with_complexity_2/4,
	ppl_new_Octagonal_Shape_double_from_NNC_Polyhedron_with_complexity_2/4,
	ppl_new_Octagonal_Shape_double_from_Grid_with_complexity_2/4,
	ppl_new_Octagonal_Shape_double_from_Rational_Box_with_complexity_2/4,
	ppl_new_Octagonal_Shape_double_from_BD_Shape_mpz_class_with_complexity_2/4,
	ppl_new_Octagonal_Shape_double_from_BD_Shape_mpq_class_with_complexity_2/4,
	ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpz_class_with_complexity_2/4,
	ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_mpq_class_with_complexity_2/4,
	ppl_new_Octagonal_Shape_double_from_Double_Box_with_complexity_2/4,
	ppl_new_Octagonal_Shape_double_from_BD_Shape_double_with_complexity_2/4,
	ppl_new_Octagonal_Shape_double_from_Octagonal_Shape_double_with_complexity_2/4,
	ppl_new_Octagonal_Shape_double_from_constraints_2/3,
	ppl_new_Octagonal_Shape_double_from_congruences_2/3,
	ppl_new_Octagonal_Shape_double_from_generators_2/3,
	ppl_Octagonal_Shape_double_space_dimension_2/3,
	ppl_Octagonal_Shape_double_affine_dimension_2/3,
	ppl_Octagonal_Shape_double_relation_with_constraint_2/4,
	ppl_Octagonal_Shape_double_relation_with_generator_2/4,
	ppl_Octagonal_Shape_double_relation_with_congruence_2/4,
	ppl_Octagonal_Shape_double_get_constraints_2/3,
	ppl_Octagonal_Shape_double_get_congruences_2/3,
	ppl_Octagonal_Shape_double_get_minimized_constraints_2/3,
	ppl_Octagonal_Shape_double_get_minimized_congruences_2/3,
	ppl_Octagonal_Shape_double_is_empty_2/2,
	ppl_Octagonal_Shape_double_is_universe_2/2,
	ppl_Octagonal_Shape_double_is_bounded_2/2,
	ppl_Octagonal_Shape_double_contains_integer_point_2/2,
	ppl_Octagonal_Shape_double_is_topologically_closed_2/2,
	ppl_Octagonal_Shape_double_is_discrete_2/2,
	ppl_Octagonal_Shape_double_bounds_from_above_2/3,
	ppl_Octagonal_Shape_double_bounds_from_below_2/3,
	ppl_Octagonal_Shape_double_maximize_2/6,
	ppl_Octagonal_Shape_double_minimize_2/6,
	ppl_Octagonal_Shape_double_maximize_with_point_2/7,
	ppl_Octagonal_Shape_double_minimize_with_point_2/7,
	ppl_Octagonal_Shape_double_contains_Octagonal_Shape_double_2/3,
	ppl_Octagonal_Shape_double_strictly_contains_Octagonal_Shape_double_2/3,
	ppl_Octagonal_Shape_double_is_disjoint_from_Octagonal_Shape_double_2/3,
	ppl_Octagonal_Shape_double_equals_Octagonal_Shape_double_2/3,
	ppl_Octagonal_Shape_double_OK_2/2,
	ppl_Octagonal_Shape_double_upper_bound_assign_if_exact_2/3,
	ppl_Octagonal_Shape_double_simplify_using_context_assign_2/4,
	ppl_Octagonal_Shape_double_constrains_2/3,
	ppl_Octagonal_Shape_double_unconstrain_space_dimension_2/3,
	ppl_Octagonal_Shape_double_unconstrain_space_dimensions_2/3,
	ppl_Octagonal_Shape_double_generalized_affine_image_2/6,
	ppl_Octagonal_Shape_double_generalized_affine_preimage_2/6,
	ppl_Octagonal_Shape_double_generalized_affine_image_lhs_rhs_2/5,
	ppl_Octagonal_Shape_double_generalized_affine_preimage_lhs_rhs_2/5,
	ppl_Octagonal_Shape_double_remove_space_dimensions_2/3,
	ppl_Octagonal_Shape_double_fold_space_dimensions_2/4,
	ppl_Octagonal_Shape_double_map_space_dimensions_2/3,
	ppl_Octagonal_Shape_double_ascii_dump_2/2,
	ppl_Octagonal_Shape_double_external_memory_in_bytes_2/3,
	ppl_Octagonal_Shape_double_total_memory_in_bytes_2/3,
	ppl_Octagonal_Shape_double_BHMZ05_widening_assign_with_tokens_2/5,
	ppl_Octagonal_Shape_double_widening_assign_with_tokens_2/5,
	ppl_Octagonal_Shape_double_limited_BHMZ05_extrapolation_assign_with_tokens_2/6,
	ppl_Octagonal_Shape_double_limited_CC76_extrapolation_assign_with_tokens_2/6,
	ppl_Octagonal_Shape_double_CC76_extrapolation_assign_with_tokens_2/5,
	ppl_Octagonal_Shape_double_CC76_narrowing_assign_2/3,
	ppl_Octagonal_Shape_double_linear_partition_2/5
]).

:- doc(version_maintenance,off).

