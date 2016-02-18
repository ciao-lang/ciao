/* Ciao Prolog interface: Ciao Prolog part.
   Copyright (C) 2001-2006 Roberto Bagnara <bagnara@cs.unipr.it>

This file is part of the Parma Polyhedra Library (PPL).

The PPL is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
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
        ppl_set_timeout_exception_atom/1,
        ppl_timeout_exception_atom/1,
        ppl_set_timeout/1,
        ppl_reset_timeout/0,
	ppl_new_C_Polyhedron_from_space_dimension/3,
	ppl_new_NNC_Polyhedron_from_space_dimension/3,
	ppl_new_C_Polyhedron_from_C_Polyhedron/2,
	ppl_new_C_Polyhedron_from_NNC_Polyhedron/2,
	ppl_new_NNC_Polyhedron_from_C_Polyhedron/2,
	ppl_new_NNC_Polyhedron_from_NNC_Polyhedron/2,
	ppl_new_C_Polyhedron_from_constraints/2,
	ppl_new_NNC_Polyhedron_from_constraints/2,
	ppl_new_C_Polyhedron_from_generators/2,
	ppl_new_NNC_Polyhedron_from_generators/2,
	ppl_new_C_Polyhedron_from_bounding_box/2,
	ppl_new_NNC_Polyhedron_from_bounding_box/2,
        ppl_Polyhedron_swap/2,
        ppl_delete_Polyhedron/1,
        ppl_Polyhedron_space_dimension/2,
        ppl_Polyhedron_affine_dimension/2,
        ppl_Polyhedron_get_constraints/2,
        ppl_Polyhedron_get_minimized_constraints/2,
        ppl_Polyhedron_get_generators/2,
        ppl_Polyhedron_get_minimized_generators/2,
        ppl_Polyhedron_relation_with_constraint/3,
        ppl_Polyhedron_relation_with_generator/3,
        ppl_Polyhedron_get_bounding_box/3,
        ppl_Polyhedron_is_empty/1,
        ppl_Polyhedron_is_universe/1,
        ppl_Polyhedron_is_bounded/1,
        ppl_Polyhedron_bounds_from_above/2,
        ppl_Polyhedron_bounds_from_below/2,
        ppl_Polyhedron_maximize/5,
        ppl_Polyhedron_maximize_with_point/6,
        ppl_Polyhedron_minimize/5,
        ppl_Polyhedron_minimize_with_point/6,
        ppl_Polyhedron_is_topologically_closed/1,
        ppl_Polyhedron_contains_Polyhedron/2,
        ppl_Polyhedron_strictly_contains_Polyhedron/2,
        ppl_Polyhedron_is_disjoint_from_Polyhedron/2,
        ppl_Polyhedron_equals_Polyhedron/2,
        ppl_Polyhedron_OK/1,
        ppl_Polyhedron_add_constraint/2,
        ppl_Polyhedron_add_constraint_and_minimize/2,
        ppl_Polyhedron_add_generator/2,
        ppl_Polyhedron_add_generator_and_minimize/2,
        ppl_Polyhedron_add_constraints/2,
        ppl_Polyhedron_add_constraints_and_minimize/2,
        ppl_Polyhedron_add_generators/2,
        ppl_Polyhedron_add_generators_and_minimize/2,
        ppl_Polyhedron_intersection_assign/2,
        ppl_Polyhedron_intersection_assign_and_minimize/2,
        ppl_Polyhedron_poly_hull_assign/2,
        ppl_Polyhedron_poly_hull_assign_and_minimize/2,
        ppl_Polyhedron_poly_difference_assign/2,
        ppl_Polyhedron_affine_image/4,
        ppl_Polyhedron_affine_preimage/4,
        ppl_Polyhedron_bounded_affine_image/5,
        ppl_Polyhedron_bounded_affine_preimage/5,
        ppl_Polyhedron_generalized_affine_image/5,
        ppl_Polyhedron_generalized_affine_preimage/5,
        ppl_Polyhedron_generalized_affine_image_lhs_rhs/4,
        ppl_Polyhedron_generalized_affine_preimage_lhs_rhs/4,
        ppl_Polyhedron_time_elapse_assign/2,
        ppl_Polyhedron_topological_closure_assign/1,
        ppl_Polyhedron_BHRZ03_widening_assign_with_tokens/4,
        ppl_Polyhedron_BHRZ03_widening_assign/2,
        ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens/5,
        ppl_Polyhedron_limited_BHRZ03_extrapolation_assign/3,
        ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens/5,
        ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign/3,
        ppl_Polyhedron_H79_widening_assign_with_tokens/4,
        ppl_Polyhedron_H79_widening_assign/2,
        ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens/5,
        ppl_Polyhedron_limited_H79_extrapolation_assign/3,
        ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens/5,
        ppl_Polyhedron_bounded_H79_extrapolation_assign/3,
        ppl_Polyhedron_add_space_dimensions_and_project/2,
        ppl_Polyhedron_add_space_dimensions_and_embed/2,
        ppl_Polyhedron_concatenate_assign/2,
        ppl_Polyhedron_remove_space_dimensions/2,
        ppl_Polyhedron_remove_higher_space_dimensions/2,
        ppl_Polyhedron_expand_space_dimension/3,
        ppl_Polyhedron_fold_space_dimensions/3,
        ppl_Polyhedron_map_space_dimensions/2,
	ppl_new_LP_Problem_trivial/1,
	ppl_new_LP_Problem/4,
	ppl_new_LP_Problem_from_LP_Problem/2,
	ppl_LP_Problem_swap/2,
	ppl_delete_LP_Problem/1,
	ppl_LP_Problem_space_dimension/2,
	ppl_LP_Problem_constraints/2,
	ppl_LP_Problem_objective_function/2,
	ppl_LP_Problem_optimization_mode/2,
	ppl_LP_Problem_clear/1,
	ppl_LP_Problem_add_constraint/2,
	ppl_LP_Problem_add_constraints/2,
	ppl_LP_Problem_set_objective_function/2,
	ppl_LP_Problem_set_optimization_mode/2,
	ppl_LP_Problem_is_satisfiable/1,
	ppl_LP_Problem_solve/2,
	ppl_LP_Problem_feasible_point/2,
	ppl_LP_Problem_optimizing_point/2,
	ppl_LP_Problem_optimal_value/3,
	ppl_LP_Problem_evaluate_objective_function/4,
	ppl_LP_Problem_OK/1
]).
:- use_package([
        assertions,
        basicmodes,
        regtypes,
        foreign_interface
]).


:- true pred ppl_version_major_2(in(Version),
                               go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_version_major)).

ppl_version_major(Version) :-
   ppl_version_major_2(Version, 1).

:- true pred ppl_version_minor_2(in(Version),
                               go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_version_minor)).

ppl_version_minor(Version) :-
   ppl_version_minor_2(Version, 1).

:- true pred ppl_version_revision_2(in(Version),
                               go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_version_revision)).

ppl_version_revision(Version) :-
   ppl_version_revision_2(Version, 1).

:- true pred ppl_version_beta_2(in(Version),
                               go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_version_beta)).

ppl_version_beta(Version) :-
   ppl_version_beta_2(Version, 1).

:- true pred ppl_version_2(in(Version),
                               go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_version)).

ppl_version(Version) :-
   ppl_version_2(Version, 1).


:- true pred ppl_banner_2(in(Banner),
                               go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_banner)).

ppl_banner(Banner) :-
   ppl_banner_2(Banner, 1).

:- true pred ppl_max_space_dimension_2(in(Dimension),
                                       go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_max_space_dimension)).

ppl_max_space_dimension(Dimension) :-
   ppl_max_space_dimension_2(Dimension, 1).

:- true pred ppl_Coefficient_is_bounded_1(go(Success))
          :: int
  + (returns(Success), foreign(ppl_Coefficient_is_bounded)).

ppl_Coefficient_is_bounded :-
   ppl_Coefficient_is_bounded_1(1).

:- true pred ppl_Coefficient_max_2(in(Max),
                                       go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Coefficient_max)).

ppl_Coefficient_max(Max) :-
   ppl_Coefficient_max_2(Max, 1).

:- true pred ppl_Coefficient_min_2(in(Min),
                                       go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_Coefficient_min)).

ppl_Coefficient_min(Min) :-
   ppl_Coefficient_min_2(Min, 1).

:- true pred ppl_initialize + foreign.

:- true pred ppl_finalize + foreign.

:- true pred ppl_set_timeout_exception_atom(in(Atom))
             :: any_term + foreign.

:- true pred ppl_timeout_exception_atom_2(in(Atom),
                                     go(Success))
  :: any_term * int
  + (returns(Success), foreign(ppl_timeout_exception_atom)).

ppl_timeout_exception_atom(Atom) :-
   ppl_timeout_exception_atom_2(Atom, 1).

:- true pred ppl_set_timeout(in(Time))
             :: any_term + foreign.

:- true pred ppl_reset_timeout + foreign.

:- true pred ppl_new_C_Polyhedron_from_space_dimension_2(in(Dimension),
                                                 in(Atom),
                                                 in(Handle),
                                                 go(Success))
  :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_space_dimension)).

ppl_new_C_Polyhedron_from_space_dimension(Dimension, Atom, Handle) :-
   ppl_new_C_Polyhedron_from_space_dimension_2(Dimension, Atom, Handle, 1).

:- true pred ppl_new_NNC_Polyhedron_from_space_dimension_2(in(Dimension),
                                                 in(Atom),
                                                 in(Handle),
                                                 go(Success))
  :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_space_dimension)).

ppl_new_NNC_Polyhedron_from_space_dimension(Dimension, Atom, Handle) :-
   ppl_new_NNC_Polyhedron_from_space_dimension_2(Dimension, Atom, Handle, 1).


:- true pred ppl_new_C_Polyhedron_from_C_Polyhedron_2(in(Srd_Handle),
						  in(Dst_Handle),
						  go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_C_Polyhedron)).

ppl_new_C_Polyhedron_from_C_Polyhedron(Src_Handle, Dst_Handle) :-
   ppl_new_C_Polyhedron_from_C_Polyhedron_2(
               Src_Handle, Dst_Handle, 1).

:- true pred ppl_new_C_Polyhedron_from_NNC_Polyhedron_2(in(Srd_Handle),
						  in(Dst_Handle),
						  go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_NNC_Polyhedron)).

ppl_new_C_Polyhedron_from_NNC_Polyhedron(Src_Handle, Dst_Handle) :-
   ppl_new_C_Polyhedron_from_NNC_Polyhedron_2(
               Src_Handle, Dst_Handle, 1).

:- true pred ppl_new_NNC_Polyhedron_from_C_Polyhedron_2(in(Srd_Handle),
						  in(Dst_Handle),
						  go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_C_Polyhedron)).

ppl_new_NNC_Polyhedron_from_C_Polyhedron(Src_Handle, Dst_Handle) :-
   ppl_new_NNC_Polyhedron_from_C_Polyhedron_2(
               Src_Handle, Dst_Handle, 1).

:- true pred ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_2(in(Srd_Handle),
						  in(Dst_Handle),
						  go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_NNC_Polyhedron)).

ppl_new_NNC_Polyhedron_from_NNC_Polyhedron(Src_Handle, Dst_Handle) :-
   ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_2(
               Src_Handle, Dst_Handle, 1).

:- true pred ppl_new_C_Polyhedron_from_constraints_2(in(CList),
                                                   in(Handle),
                                                   go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_constraints)).

ppl_new_C_Polyhedron_from_constraints(CList, Handle) :-
   ppl_new_C_Polyhedron_from_constraints_2(CList, Handle, 1).

:- true pred ppl_new_NNC_Polyhedron_from_constraints_2(in(CList),
                                                   in(Handle),
                                                   go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_constraints)).

ppl_new_NNC_Polyhedron_from_constraints(CList, Handle) :-
   ppl_new_NNC_Polyhedron_from_constraints_2(CList, Handle, 1).

:- true pred ppl_new_C_Polyhedron_from_generators_2(in(GList),
                                                  in(Handle),
                                                  go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_generators)).

ppl_new_C_Polyhedron_from_generators(GList, Handle) :-
   ppl_new_C_Polyhedron_from_generators_2(GList, Handle, 1).

:- true pred ppl_new_NNC_Polyhedron_from_generators_2(in(GList),
                                                  in(Handle),
                                                  go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_generators)).

ppl_new_NNC_Polyhedron_from_generators(GList, Handle) :-
   ppl_new_NNC_Polyhedron_from_generators_2(GList, Handle, 1).


:- true pred ppl_new_C_Polyhedron_from_bounding_box_2(in(BBox),
                                               in(Handle),
                                               go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_C_Polyhedron_from_bounding_box)).

ppl_new_C_Polyhedron_from_bounding_box(BBox, Handle) :-
   ppl_new_C_Polyhedron_from_bounding_box_2(BBox, Handle, 1).

:- true pred ppl_new_NNC_Polyhedron_from_bounding_box_2(in(BBox),
                                               in(Handle),
                                               go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_NNC_Polyhedron_from_bounding_box)).

ppl_new_NNC_Polyhedron_from_bounding_box(BBox, Handle) :-
   ppl_new_NNC_Polyhedron_from_bounding_box_2(BBox, Handle, 1).

:- true pred ppl_Polyhedron_swap(in(Handle1),
                                 in(Handle2))
  :: any_term * any_term + foreign.

:- true pred ppl_delete_Polyhedron(in(Handle))
  :: any_term + foreign.

:- true pred ppl_Polyhedron_space_dimension_2(in(Handle),
                                              in(Dimension),
                                              go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_space_dimension)).

ppl_Polyhedron_space_dimension(Handle, Dimension) :-
        ppl_Polyhedron_space_dimension_2(Handle, Dimension, 1).

:- true pred ppl_Polyhedron_affine_dimension_2(in(Handle),
					       in(Dimension),
					       go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_affine_dimension)).

ppl_Polyhedron_affine_dimension(Handle, Dimension) :-
        ppl_Polyhedron_affine_dimension_2(Handle, Dimension, 1).

:- true pred ppl_Polyhedron_get_constraints_2(in(Handle),
                                              in(CList),
                                              go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_constraints)).

ppl_Polyhedron_get_constraints(Handle, CList) :-
        ppl_Polyhedron_get_constraints_2(Handle, CList, 1).


:- true pred ppl_Polyhedron_get_minimized_constraints_2(in(Handle),
                                                        in(CList),
                                                        go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_minimized_constraints)).

ppl_Polyhedron_get_minimized_constraints(Handle, CList) :-
        ppl_Polyhedron_get_minimized_constraints_2(Handle, CList, 1).

:- true pred ppl_Polyhedron_get_generators_2(in(Handle),
                                             in(GList),
                                             go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_generators)).

ppl_Polyhedron_get_generators(Handle, GList) :-
        ppl_Polyhedron_get_generators_2(Handle, GList, 1).


:- true pred ppl_Polyhedron_get_minimized_generators_2(in(Handle),
                                                       in(GList),
                                                       go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_minimized_generators)).

ppl_Polyhedron_get_minimized_generators(Handle, GList) :-
        ppl_Polyhedron_get_minimized_generators_2(Handle, GList, 1).

:- true pred ppl_Polyhedron_relation_with_constraint_2(in(Handle),
                                                       in(Constraint),
                                                       in(RList),
                                                       go(Success))
  :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_relation_with_constraint)).

ppl_Polyhedron_relation_with_constraint(Handle, Constraint, RList) :-
        ppl_Polyhedron_relation_with_constraint_2(Handle, Constraint,
                                                  RList, 1).

:- true pred ppl_Polyhedron_relation_with_generator_2(in(Handle),
                                                     in(Generator),
                                                     in(RList),
                                                     go(Success))
  :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_relation_with_generator)).

ppl_Polyhedron_relation_with_generator(Handle, Generator, RList) :-
        ppl_Polyhedron_relation_with_generator_2(Handle, Generator, RList, 1).

:- true pred ppl_Polyhedron_get_bounding_box_2(in(Handle),
                                               in(Relation),
                                               in(BBox),
                                               go(Success))
  :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_get_bounding_box)).

ppl_Polyhedron_get_bounding_box(Handle, Relation, BBox) :-
        ppl_Polyhedron_get_bounding_box_2(Handle, Relation, BBox, 1).

:- true pred ppl_Polyhedron_is_empty_2(in(Handle),
                                       go(Success))
  :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_empty)).

ppl_Polyhedron_is_empty(Handle) :-
	ppl_Polyhedron_is_empty_2(Handle, 1).

:- true pred ppl_Polyhedron_is_universe_2(in(Handle),
                                          go(Success))
  :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_universe)).

ppl_Polyhedron_is_universe(Handle) :-
	ppl_Polyhedron_is_universe_2(Handle, 1).

:- true pred ppl_Polyhedron_is_bounded_2(in(Handle),
                                         go(Success))
  :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_bounded)).

ppl_Polyhedron_is_bounded(Handle) :-
	ppl_Polyhedron_is_bounded_2(Handle, 1).

:- true pred ppl_Polyhedron_bounds_from_above_2(in(Handle),
                                                in(Linear_Expression),
                                                go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_bounds_from_above)).

ppl_Polyhedron_bounds_from_above(Handle, Linear_Expression) :-
	ppl_Polyhedron_bounds_from_above_2(Handle, Linear_Expression, 1).

:- true pred ppl_Polyhedron_bounds_from_below_2(in(Handle),
                                                in(Linear_Expression),
                                                go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_bounds_from_below)).

ppl_Polyhedron_bounds_from_below(Handle, Linear_Expression) :-
	ppl_Polyhedron_bounds_from_below_2(Handle, Linear_Expression, 1).

:- true pred ppl_Polyhedron_maximize_2(in(Handle),
                                       in(Linear_Expression),
                                       in(Num),
                                       in(Den),
                                       in(Max),
                                       go(Success))
  :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_maximize)).

ppl_Polyhedron_maximize(Handle, Linear_Expression, Num, Den, Max) :-
	ppl_Polyhedron_maximize_2(Handle, Linear_Expression, Num, Den, Max, 1).

:- true pred ppl_Polyhedron_maximize_with_point_2(in(Handle),
                                       in(Linear_Expression),
                                       in(Num),
                                       in(Den),
                                       in(Max),
                                       in(Point),
                                       go(Success))
  :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_maximize_with_point)).

ppl_Polyhedron_maximize_with_point(Handle, Linear_Expression,
				   Num, Den, Max, Point) :-
	ppl_Polyhedron_maximize_with_point_2(Handle, Linear_Expression,
					     Num, Den, Max, Point, 1).

:- true pred ppl_Polyhedron_minimize_2(in(Handle),
                                       in(Linear_Expression),
                                       in(Num),
                                       in(Den),
                                       in(Min),
                                       go(Success))
  :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_minimize)).

ppl_Polyhedron_minimize(Handle, Linear_Expression, Num, Den, Min) :-
	ppl_Polyhedron_minimize_2(Handle, Linear_Expression, Num, Den, Min, 1).

:- true pred ppl_Polyhedron_minimize_with_point_2(in(Handle),
                                       in(Linear_Expression),
                                       in(Num),
                                       in(Den),
                                       in(Min),
                                       in(Point),
                                       go(Success))
  :: any_term * any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_minimize_with_point)).

ppl_Polyhedron_minimize_with_point(Handle, Linear_Expression,
				   Num, Den, Min, Point) :-
	ppl_Polyhedron_minimize_with_point_2(Handle, Linear_Expression,
					     Num, Den, Min, Point, 1).

:- true pred ppl_Polyhedron_is_topologically_closed_2(in(Handle),
                                                      go(Success))
  :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_topologically_closed)).

ppl_Polyhedron_is_topologically_closed(Handle) :-
	ppl_Polyhedron_is_topologically_closed_2(Handle, 1).


:- true pred ppl_Polyhedron_contains_Polyhedron_2(in(Handle1),
                                                  in(Handle2),
                                                  go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_contains_Polyhedron)).

ppl_Polyhedron_contains_Polyhedron(Handle1, Handle2) :-
	ppl_Polyhedron_contains_Polyhedron_2(Handle1, Handle2, 1).


:- true pred ppl_Polyhedron_strictly_contains_Polyhedron_2(in(Handle1),
                                                           in(Handle2),
                                                           go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_strictly_contains_Polyhedron)).

ppl_Polyhedron_strictly_contains_Polyhedron(Handle1, Handle2) :-
	ppl_Polyhedron_strictly_contains_Polyhedron_2(Handle1, Handle2, 1).

:- true pred ppl_Polyhedron_is_disjoint_from_Polyhedron_2(in(Handle1),
                                                          in(Handle2),
                                                          go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_is_disjoint_from_Polyhedron)).

ppl_Polyhedron_is_disjoint_from_Polyhedron(Handle1, Handle2) :-
	ppl_Polyhedron_is_disjoint_from_Polyhedron_2(Handle1, Handle2, 1).


:- true pred ppl_Polyhedron_equals_Polyhedron_2(in(Handle1),
                                                in(Handle2),
                                                go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_equals_Polyhedron)).

ppl_Polyhedron_equals_Polyhedron(Handle1, Handle2) :-
	ppl_Polyhedron_equals_Polyhedron_2(Handle1, Handle2, 1).

:- true pred ppl_Polyhedron_OK_2(in(Handle),
                                 go(Success))
  :: any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_OK)).

ppl_Polyhedron_OK(Handle) :-
	ppl_Polyhedron_OK_2(Handle, 1).

:- true pred ppl_Polyhedron_add_constraint(in(Handle), in(Constraint))
  :: any_term * any_term + foreign.


:- true pred ppl_Polyhedron_add_constraint_and_minimize_2(in(Handle),
                                                          in(Constraint),
                                                          go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_constraint_and_minimize)).

ppl_Polyhedron_add_constraint_and_minimize(Handle, Constraint) :-
        ppl_Polyhedron_add_constraint_and_minimize_2(Handle, Constraint, 1).

:- true pred ppl_Polyhedron_add_generator(in(Handle), in(Generator))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_generator_and_minimize_2(in(Handle),
                                                         in(Generator),
                                                         go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_generator_and_minimize)).

ppl_Polyhedron_add_generator_and_minimize(Handle, Generator) :-
        ppl_Polyhedron_add_generator_and_minimize_2(Handle, Generator, 1).

:- true pred ppl_Polyhedron_add_constraints(in(Handle), in(CList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_constraints_and_minimize_2(in(Handle),
                                                           in(CList),
                                                           go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_constraints_and_minimize)).

ppl_Polyhedron_add_constraints_and_minimize(Handle, CList) :-
        ppl_Polyhedron_add_constraints_and_minimize_2(Handle, CList, 1).

:- true pred ppl_Polyhedron_add_generators(in(Handle), in(GList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_generators_and_minimize_2(in(Handle),
                                                          in(GList),
                                                          go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_add_generators_and_minimize)).

ppl_Polyhedron_add_generators_and_minimize(Handle, GList) :-
        ppl_Polyhedron_add_generators_and_minimize_2(Handle, GList, 1).


:- true pred ppl_Polyhedron_intersection_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_intersection_assign_and_minimize_2(in(Handle1),
                                                          in(Handle2),
                                                          go(Success))
  :: any_term * any_term * int
  + (returns(Success),
	foreign(ppl_Polyhedron_intersection_assign_and_minimize)).

ppl_Polyhedron_intersection_assign_and_minimize(Handle1, Handle2) :-
        ppl_Polyhedron_intersection_assign_and_minimize_2(Handle1, Handle2, 1).

:- true pred ppl_Polyhedron_poly_hull_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_poly_hull_assign_and_minimize_2(in(Handle1),
                                                          in(Handle2),
                                                          go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_poly_hull_assign_and_minimize)).

ppl_Polyhedron_poly_hull_assign_and_minimize(Handle1, Handle2) :-
        ppl_Polyhedron_poly_hull_assign_and_minimize_2(Handle1, Handle2, 1).

:- true pred ppl_Polyhedron_poly_difference_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_affine_image(in(Handle), in(Var),
                                         in(Linear_Expression), in(Divisor))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_affine_preimage(in(Handle), in(Var),
                                            in(Linear_Expression), in(Divisor))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_bounded_affine_image(in(Handle), in(Var),
                                                 in(Lower_Bound),
                                                 in(Upper_Bound),
                                                 in(Divisor))
  :: any_term * any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_bounded_affine_preimage(in(Handle), in(Var),
                                                    in(Lower_Bound),
                                                    in(Upper_Bound),
                                                    in(Divisor))
  :: any_term * any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_generalized_affine_image(in(Handle),
                                                     in(Var), in(Rel),
                                                     in(Linear_Expression),
                                                     in(Divisor))
  :: any_term * any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_generalized_affine_preimage(in(Handle),
                                                        in(Var), in(Rel),
                                                        in(Linear_Expression),
                                                        in(Divisor))
  :: any_term * any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_generalized_affine_image_lhs_rhs(
                 in(Handle), in(LHS), in(Rel), in(RHS))
  :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_generalized_affine_preimage_lhs_rhs(
                 in(Handle), in(LHS), in(Rel), in(RHS))
  :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_time_elapse_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_topological_closure_assign(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_BHRZ03_widening_assign_with_tokens_2(
                 in(Handle1), in(Handle2), in(Tokens1), in(Tokens2),
                 go(Success))
  :: any_term * any_term * any_term * any_term * int
  + (returns(Success),
      foreign(ppl_Polyhedron_BHRZ03_widening_assign_with_tokens)).

ppl_Polyhedron_BHRZ03_widening_assign_with_tokens(
                  Handle1, Handle2, Tokens1, Tokens2) :-
      ppl_Polyhedron_BHRZ03_widening_assign_with_tokens_2(
                  Handle1, Handle2, Tokens1, Tokens2, 1).

:- true pred ppl_Polyhedron_BHRZ03_widening_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens_2(
                 in(Handle1), in(Handle2), in(CList), in(Tokens1), in(Tokens2),
                 go(Success))
  :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success),
      foreign(ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens)).

ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens(
                  Handle1, Handle2, CList, Tokens1, Tokens2) :-
      ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens_2(
                  Handle1, Handle2, CList, Tokens1, Tokens2, 1).

:- true pred ppl_Polyhedron_limited_BHRZ03_extrapolation_assign(in(Handle1),
                                                               in(CList),
                                                                in(Handle2))
  :: any_term * any_term * any_term + foreign.


:- true pred ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens_2(
                 in(Handle1), in(Handle2), in(CList), in(Tokens1), in(Tokens2),
                 go(Success))
  :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success),
      foreign(ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens)).

ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens(
                  Handle1, Handle2, CList, Tokens1, Tokens2) :-
      ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens_2(
                  Handle1, Handle2, CList, Tokens1, Tokens2, 1).

:- true pred ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign(in(Handle1),
                                                               in(CList),
                                                                in(Handle2))
             :: any_term * any_term * any_term + foreign.


:- true pred ppl_Polyhedron_H79_widening_assign_with_tokens_2(
                 in(Handle1), in(Handle2), in(Tokens1), in(Tokens2),
                 go(Success))
  :: any_term * any_term * any_term * any_term * int
  + (returns(Success), 
      foreign(ppl_Polyhedron_H79_widening_assign_with_tokens)).

ppl_Polyhedron_H79_widening_assign_with_tokens(
                  Handle1, Handle2, Tokens1, Tokens2) :-
      ppl_Polyhedron_H79_widening_assign_with_tokens_2(
                  Handle1, Handle2, Tokens1, Tokens2, 1).

:- true pred ppl_Polyhedron_H79_widening_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens_2(
                 in(Handle1), in(Handle2), in(CList), in(Tokens1), in(Tokens2),
                 go(Success))
  :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success),
      foreign(ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens)).

ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens(Handle1,
                                                           Handle2,
                                                           CList,
                                                           Tokens1,
                                                           Tokens2) :-
  ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens_2(Handle1,
                                                               Handle2,
                                                               CList,
                                                               Tokens1,
                                                               Tokens2,
                                                               1).

:- true pred ppl_Polyhedron_limited_H79_extrapolation_assign(in(Handle1),
                                                             in(Handle2),
                                                             in(CList))
  :: any_term * any_term * any_term + foreign.

:- true pred
  ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens_2(in(Handle1),
                                                               in(Handle2),
                                                               in(CList),
                                                               in(Tokens1),
                                                               in(Tokens2),
                                                               go(Success))
  :: any_term * any_term * any_term * any_term * any_term * int
  + (returns(Success),
    foreign(ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens)).

ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens(Handle1,
                                                           Handle2,
                                                           CList,
                                                           Tokens1,
                                                           Tokens2) :-
  ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens_2(Handle1,
                                                               Handle2,
                                                               CList,
                                                               Tokens1,
                                                               Tokens2,
                                                               1).

:- true pred ppl_Polyhedron_bounded_H79_extrapolation_assign(in(Handle1),
                                                             in(Handle2),
                                                             in(CList))
  :: any_term * any_term * any_term + foreign.

:- true pred
   ppl_Polyhedron_add_space_dimensions_and_project(in(Handle),
                                                   in(NDimensions_To_Add))
  :: any_term * any_term + foreign.

:- true pred
   ppl_Polyhedron_add_space_dimensions_and_embed(in(Handle),
                                                 in(NDimensions_To_Add))
  :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_concatenate_assign(in(Handle1), in(Handle2))
  :: any_term * any_term + foreign.

ppl_Polyhedron_remove_space_dimensions(Handle, VList) :-
	ppl_Polyhedron_remove_space_dimensions_2(Handle, VList, 1).

:- true pred ppl_Polyhedron_remove_space_dimensions_2(in(Handle), in(VList),
                                                go(Success))
  :: any_term * any_term * int
 + (returns(Success), foreign(ppl_Polyhedron_remove_space_dimensions)).

:- true pred ppl_Polyhedron_remove_higher_space_dimensions(in(Handle),
                                                           in(Dimensions))
  :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_expand_space_dimension(in(Handle),
                                                   in(Var),
                                                   in(Dimensions))
  :: any_term * any_term * any_term + foreign.

ppl_Polyhedron_fold_space_dimensions(Handle, VList, Var) :-
	ppl_Polyhedron_fold_space_dimensions_2(Handle, VList, Var, 1).

:- true pred ppl_Polyhedron_fold_space_dimensions_2(in(Handle),
                                                 in(VList),
                                                 in(Var),
                                                go(Success))
  :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_fold_space_dimensions)).


ppl_Polyhedron_map_space_dimensions(Handle, PIFunc) :-
	ppl_Polyhedron_map_space_dimensions_2(Handle, PIFunc, 1).

:- true pred ppl_Polyhedron_map_space_dimensions_2(in(Handle),
                                                 in(PIFunc),
                                                go(Success))
  :: any_term * any_term * int
  + (returns(Success), foreign(ppl_Polyhedron_map_space_dimensions)).

:- true pred ppl_new_LP_Problem_trivial_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_new_LP_Problem_trivial)).
 
ppl_new_LP_Problem_trivial(Term1) :-
   ppl_new_LP_Problem_trivial_2(Term1, 1).

:- true pred ppl_new_LP_Problem_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_new_LP_Problem)).
 
ppl_new_LP_Problem(Term1, Term2, Term3, Term4) :-
   ppl_new_LP_Problem_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_new_LP_Problem_from_LP_Problem_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_new_LP_Problem_from_LP_Problem)).
 
ppl_new_LP_Problem_from_LP_Problem(Term1, Term2) :-
   ppl_new_LP_Problem_from_LP_Problem_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_swap_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_swap)).
 
ppl_LP_Problem_swap(Term1, Term2) :-
   ppl_LP_Problem_swap_2(Term1, Term2, 1).

:- true pred ppl_delete_LP_Problem_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_delete_LP_Problem)).
 
ppl_delete_LP_Problem(Term1) :-
   ppl_delete_LP_Problem_2(Term1, 1).

:- true pred ppl_LP_Problem_space_dimension_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_space_dimension)).
 
ppl_LP_Problem_space_dimension(Term1, Term2) :-
   ppl_LP_Problem_space_dimension_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_constraints)).
 
ppl_LP_Problem_constraints(Term1, Term2) :-
   ppl_LP_Problem_constraints_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_objective_function_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_objective_function)).
 
ppl_LP_Problem_objective_function(Term1, Term2) :-
   ppl_LP_Problem_objective_function_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_optimization_mode_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_optimization_mode)).
 
ppl_LP_Problem_optimization_mode(Term1, Term2) :-
   ppl_LP_Problem_optimization_mode_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_clear_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_clear)).
 
ppl_LP_Problem_clear(Term1) :-
   ppl_LP_Problem_clear_2(Term1, 1).

:- true pred ppl_LP_Problem_add_constraint_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_add_constraint)).
 
ppl_LP_Problem_add_constraint(Term1, Term2) :-
   ppl_LP_Problem_add_constraint_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_add_constraints_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_add_constraints)).
 
ppl_LP_Problem_add_constraints(Term1, Term2) :-
   ppl_LP_Problem_add_constraints_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_set_objective_function_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_set_objective_function)).
 
ppl_LP_Problem_set_objective_function(Term1, Term2) :-
   ppl_LP_Problem_set_objective_function_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_set_optimization_mode_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_set_optimization_mode)).
 
ppl_LP_Problem_set_optimization_mode(Term1, Term2) :-
   ppl_LP_Problem_set_optimization_mode_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_is_satisfiable_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_is_satisfiable)).
 
ppl_LP_Problem_is_satisfiable(Term1) :-
   ppl_LP_Problem_is_satisfiable_2(Term1, 1).

:- true pred ppl_LP_Problem_solve_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_solve)).
 
ppl_LP_Problem_solve(Term1, Term2) :-
   ppl_LP_Problem_solve_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_feasible_point_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_feasible_point)).
 
ppl_LP_Problem_feasible_point(Term1, Term2) :-
   ppl_LP_Problem_feasible_point_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_optimizing_point_2(in(Term1), in(Term2), go(Success))
          :: any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_optimizing_point)).
 
ppl_LP_Problem_optimizing_point(Term1, Term2) :-
   ppl_LP_Problem_optimizing_point_2(Term1, Term2, 1).

:- true pred ppl_LP_Problem_optimal_value_2(in(Term1), in(Term2), in(Term3), go(Success))
          :: any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_optimal_value)).
 
ppl_LP_Problem_optimal_value(Term1, Term2, Term3) :-
   ppl_LP_Problem_optimal_value_2(Term1, Term2, Term3, 1).

:- true pred ppl_LP_Problem_evaluate_objective_function_2(in(Term1), in(Term2), in(Term3), in(Term4), go(Success))
          :: any_term * any_term * any_term * any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_evaluate_objective_function)).
 
ppl_LP_Problem_evaluate_objective_function(Term1, Term2, Term3, Term4) :-
   ppl_LP_Problem_evaluate_objective_function_2(Term1, Term2, Term3, Term4, 1).

:- true pred ppl_LP_Problem_OK_2(in(Term1), go(Success))
          :: any_term * int
  + (returns(Success), foreign(ppl_LP_Problem_OK)).
 
ppl_LP_Problem_OK(Term1) :-
   ppl_LP_Problem_OK_2(Term1, 1).

:- use_compiler('gcc').
:- include(library(ppl/ppl_decl_auto)).
:- use_foreign_source(['0_9/ppl_ciao.cc']). 
:- use_foreign_library(['pwl']).

:- impl_defined(
[
        ppl_version_major_2/2,
%        ppl_version_major/1,
        ppl_version_minor_2/2,
%        ppl_version_minor/1,
        ppl_version_revision_2/2,
%        ppl_version_revision/1,
        ppl_version_beta_2/2,
%        ppl_version_beta/1,
        ppl_version_2/2,
%        ppl_version/1,
        ppl_banner_2/2,
%        ppl_banner/1,
        ppl_max_space_dimension_2/2,
%        ppl_max_space_dimension/1,
        ppl_Coefficient_is_bounded_1/1,
        ppl_Coefficient_max_2/2,
        ppl_Coefficient_min_2/2,
        ppl_initialize/0,
        ppl_finalize/0,
        ppl_set_timeout_exception_atom/1,
%        ppl_timeout_exception_atom/1,
        ppl_timeout_exception_atom_2/2,
        ppl_set_timeout/1,
        ppl_reset_timeout/0,
%        ppl_new_Polyhedron_from_space_dimension/4,
        ppl_new_C_Polyhedron_from_space_dimension_2/4,
        ppl_new_NNC_Polyhedron_from_space_dimension_2/4,
%        ppl_new_Polyhedron_from_Polyhedron/4,
        ppl_new_C_Polyhedron_from_C_Polyhedron_2/3,
        ppl_new_C_Polyhedron_from_NNC_Polyhedron_2/3,
        ppl_new_NNC_Polyhedron_from_C_Polyhedron_2/3,
        ppl_new_NNC_Polyhedron_from_NNC_Polyhedron_2/3,
%        ppl_new_Polyhedron_from_constraints/3,
        ppl_new_C_Polyhedron_from_constraints_2/3,
        ppl_new_NNC_Polyhedron_from_constraints_2/3,
%        ppl_new_Polyhedron_from_generators/3,
        ppl_new_C_Polyhedron_from_generators_2/3,
        ppl_new_NNC_Polyhedron_from_generators_2/3,
%        ppl_new_Polyhedron_from_bounding_box/3,
        ppl_new_C_Polyhedron_from_bounding_box_2/3,
        ppl_new_NNC_Polyhedron_from_bounding_box_2/3,
        ppl_Polyhedron_swap/2,
        ppl_delete_Polyhedron/1,
%        ppl_Polyhedron_space_dimension/2,
        ppl_Polyhedron_space_dimension_2/3,
%        ppl_Polyhedron_affine_dimension/2,
        ppl_Polyhedron_affine_dimension_2/3,
%        ppl_Polyhedron_get_constraints/2,
        ppl_Polyhedron_get_constraints_2/3,
%        ppl_Polyhedron_get_minimized_constraints/2,
        ppl_Polyhedron_get_minimized_constraints_2/3,
%        ppl_Polyhedron_get_generators/2,
        ppl_Polyhedron_get_generators_2/3,
%        ppl_Polyhedron_get_minimized_generators/2,
        ppl_Polyhedron_get_minimized_generators_2/3,
%        ppl_Polyhedron_relation_with_constraint/3,
        ppl_Polyhedron_relation_with_constraint_2/4,
%        ppl_Polyhedron_relation_with_generator/3,
        ppl_Polyhedron_relation_with_generator_2/4,
%        ppl_Polyhedron_get_bounding_box/3,
        ppl_Polyhedron_get_bounding_box_2/4,
%        ppl_Polyhedron_is_empty/1,
        ppl_Polyhedron_is_empty_2/2,
%        ppl_Polyhedron_is_universe/1,
        ppl_Polyhedron_is_universe_2/2,
%        ppl_Polyhedron_is_bounded/1,
        ppl_Polyhedron_is_bounded_2/2,
%        ppl_Polyhedron_bounds_from_above/2,
        ppl_Polyhedron_bounds_from_above_2/3,
%        ppl_Polyhedron_bounds_from_below/2,
        ppl_Polyhedron_bounds_from_below_2/3,
%        ppl_Polyhedron_maximize/5,
        ppl_Polyhedron_maximize_2/6,
%        ppl_Polyhedron_maximize_with_point/6,
        ppl_Polyhedron_maximize_with_point_2/7,
%        ppl_Polyhedron_minimize/5,
        ppl_Polyhedron_minimize_2/6,
%        ppl_Polyhedron_minimize_with_point/6,
        ppl_Polyhedron_minimize_with_point_2/7,
%        ppl_Polyhedron_is_topologically_closed/1,
        ppl_Polyhedron_is_topologically_closed_2/2,
%        ppl_Polyhedron_contains_Polyhedron/2,
        ppl_Polyhedron_contains_Polyhedron_2/3,
%        ppl_Polyhedron_strictly_contains_Polyhedron/2,
        ppl_Polyhedron_strictly_contains_Polyhedron_2/3,
%        ppl_Polyhedron_is_disjoint_from_Polyhedron/2,
        ppl_Polyhedron_is_disjoint_from_Polyhedron_2/3,
%        ppl_Polyhedron_equals_Polyhedron/2,
        ppl_Polyhedron_equals_Polyhedron_2/3,
%        ppl_Polyhedron_OK/1,
        ppl_Polyhedron_OK_2/2,
        ppl_Polyhedron_add_constraint/2,
%        ppl_Polyhedron_add_constraint_and_minimize/2,
        ppl_Polyhedron_add_constraint_and_minimize_2/3,
        ppl_Polyhedron_add_generator/2,
%        ppl_Polyhedron_add_generator_and_minimize/2,
        ppl_Polyhedron_add_generator_and_minimize_2/3,
        ppl_Polyhedron_add_constraints/2,
%        ppl_Polyhedron_add_constraints_and_minimize/2,
        ppl_Polyhedron_add_constraints_and_minimize_2/3,
        ppl_Polyhedron_add_generators/2,
%        ppl_Polyhedron_add_generators_and_minimize/2,
        ppl_Polyhedron_add_generators_and_minimize_2/3,
        ppl_Polyhedron_intersection_assign/2,
%        ppl_Polyhedron_intersection_assign_and_minimize/2,
        ppl_Polyhedron_intersection_assign_and_minimize_2/3,
        ppl_Polyhedron_poly_hull_assign/2,
%        ppl_Polyhedron_poly_hull_assign_and_minimize/2,
        ppl_Polyhedron_poly_hull_assign_and_minimize_2/3,
        ppl_Polyhedron_poly_difference_assign/2,
        ppl_Polyhedron_affine_image/4,
        ppl_Polyhedron_affine_preimage/4,
        ppl_Polyhedron_bounded_affine_image/5,
        ppl_Polyhedron_bounded_affine_preimage/5,
        ppl_Polyhedron_generalized_affine_image/5,
        ppl_Polyhedron_generalized_affine_preimage/5,
        ppl_Polyhedron_generalized_affine_image_lhs_rhs/4,
        ppl_Polyhedron_generalized_affine_preimage_lhs_rhs/4,
        ppl_Polyhedron_time_elapse_assign/2,
        ppl_Polyhedron_topological_closure_assign/1,
%        ppl_Polyhedron_BHRZ03_widening_assign_with_tokens/4,
        ppl_Polyhedron_BHRZ03_widening_assign_with_tokens_2/5,
        ppl_Polyhedron_BHRZ03_widening_assign/2,
%        ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens/5,
        ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_tokens_2/6,
        ppl_Polyhedron_limited_BHRZ03_extrapolation_assign/3,
%        ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens/5,
        ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_tokens_2/6,
        ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign/3,
%        ppl_Polyhedron_H79_widening_assign_with_tokens/4,
        ppl_Polyhedron_H79_widening_assign_with_tokens_2/5,
        ppl_Polyhedron_H79_widening_assign/2,
%        ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens/5,
        ppl_Polyhedron_limited_H79_extrapolation_assign_with_tokens_2/6,
        ppl_Polyhedron_limited_H79_extrapolation_assign/3,
%        ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens/5,
        ppl_Polyhedron_bounded_H79_extrapolation_assign_with_tokens_2/6,
        ppl_Polyhedron_bounded_H79_extrapolation_assign/3,
        ppl_Polyhedron_add_space_dimensions_and_project/2,
        ppl_Polyhedron_add_space_dimensions_and_embed/2,
        ppl_Polyhedron_concatenate_assign/2,
%        ppl_Polyhedron_remove_space_dimensions/2,
        ppl_Polyhedron_remove_space_dimensions_2/3,
        ppl_Polyhedron_remove_higher_space_dimensions/2,
        ppl_Polyhedron_expand_space_dimension/3,
%        ppl_Polyhedron_fold_space_dimensions/3,
        ppl_Polyhedron_fold_space_dimensions_2/4,
%        ppl_Polyhedron_map_space_dimensions/2
        ppl_Polyhedron_map_space_dimensions_2/3,
	ppl_new_LP_Problem_trivial_2/2,
	ppl_new_LP_Problem_2/5,
	ppl_new_LP_Problem_from_LP_Problem_2/3,
	ppl_LP_Problem_swap_2/3,
	ppl_delete_LP_Problem_2/2,
	ppl_LP_Problem_space_dimension_2/3,
	ppl_LP_Problem_constraints_2/3,
	ppl_LP_Problem_objective_function_2/3,
	ppl_LP_Problem_optimization_mode_2/3,
	ppl_LP_Problem_clear_2/2,
	ppl_LP_Problem_add_constraint_2/3,
	ppl_LP_Problem_add_constraints_2/3,
	ppl_LP_Problem_set_objective_function_2/3,
	ppl_LP_Problem_set_optimization_mode_2/3,
	ppl_LP_Problem_is_satisfiable_2/2,
	ppl_LP_Problem_solve_2/3,
	ppl_LP_Problem_feasible_point_2/3,
	ppl_LP_Problem_optimizing_point_2/3,
	ppl_LP_Problem_optimal_value_2/4,
	ppl_LP_Problem_evaluate_objective_function_2/5,
	ppl_LP_Problem_OK_2/2
]).

:- doc(version_maintenance,off).

/*
***********************************************
This commnted code has been kept for future use
since the above version of this is temporary.
***********************************************

:- true ppl_version_major(in(Version))
             :: any_term + foreign.

:- true ppl_version_minor
             :: any_term + foreign.

:- true ppl_version_revision
             :: any_term + foreign.

:- true ppl_version_beta
             :: any_term + foreign.

:- true ppl_version
             :: any_term + foreign.

:- true ppl_max_space_dimension
             :: any_term + foreign.

:- true pred ppl_initialize + foreign.

:- true pred ppl_finalize + foreign.

:- true pred ppl_set_timeout_exception_atom(in(Atom))
             :: any_term + foreign.

:- true pred ppl_timeout_exception_atom(in(Term))
             :: any_term + foreign.

:- true pred ppl_set_timeout(in(Time))
             :: any_term + foreign.

:- true pred ppl_reset_timeout + foreign.

:- true pred ppl_new_Polyhedron_from_space_dimension(in(Kind),
                                               in(Dimension),
                                               in(Atom),
                                               in(Handle))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_new_Polyhedron_from_Polyhedron(in(Src_Kind),
                                                in(Src_Handle),
                                                in(Dst_Kind),
                                                in(Dst_Handle))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_new_Polyhedron_from_constraints(in(Kind),
                                                 in(CList),
                                                 in(Handle))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_new_Polyhedron_from_generators(in(Kind),
                                                in(GList),
                                                in(Handle))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_new_Polyhedron_from_bounding_box(in(Kind),
                                                  in(BBox),
                                                  in(Handle))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_swap(in(Handle1),
                                 in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_delete_Polyhedron(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_space_dimension(in(Handle), in(Dimension))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_affine_dimension(in(Handle), in(Dimension))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_get_constraints(in(Handle), in(CList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_get_minimized_constraints(in(Handle), in(CList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_get_generators(in(Handle), in(CList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_get_minimized_generators(in(Handle), in(CList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_relation_with_constraint(in(Handle),
                                                     in(Constraint),
                                                     in(RList))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_relation_with_generator(in(Handle),
                                                    in(Generator),
                                                    in(RList))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_get_bounding_box(in(Handle),
                                             in(Relation),
                                             in(BBox))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_is_empty(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_is_universe(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_is_bounded(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_bounds_from_above(in(Handle), in(Linear_Expression))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_bounds_from_below(in(Handle), in(Linear_Expression))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_maximize(in(Handle),
                                     in(Linear_Expression),
                                     in(Num),
                                     in(Den),
                                     in(Max))
             :: any_term * any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_maximize_with_point(in(Handle),
                                     in(Linear_Expression),
                                     in(Num),
                                     in(Den),
                                     in(Max),
                                     in(Point))
             :: any_term * any_term * any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_minimize(in(Handle),
                                     in(Linear_Expression),
                                     in(Num),
                                     in(Den),
                                     in(Min))
             :: any_term * any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_minimize_with_point(in(Handle),
                                     in(Linear_Expression),
                                     in(Num),
                                     in(Den),
                                     in(Min),
                                     in(Point))
             :: any_term * any_term * any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_is_topologically_closed(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_contains_Polyhedron(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_strictly_contains_Polyhedron(in(Handle1),
                                                         in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_is_disjoint_from_Polyhedron(in(Handle1),
                                                        in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_equals_Polyhedron(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_OK(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_add_constraint(in(Handle), in(Constraint))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_constraint_and_minimize(in(Handle), in(Constraint))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_generator(in(Handle), in(Generator))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_generator_and_minimize(in(Handle), in(Generator))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_constraints(in(Handle), in(CList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_constraints_and_minimize(in(Handle), in(CList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_generators(in(Handle), in(GList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_add_generators_and_minimize(in(Handle), in(GList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_intersection_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_intersection_assign_and_minimize(in(Handle1),
                                                             in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_poly_hull_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_poly_hull_assign_and_minimize(in(Handle1),
                                                             in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_poly_difference_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_affine_image(in(Handle), in(Var),
                                         in(Linear_Expression), in(Divisor))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_affine_preimage(in(Handle), in(Var),
                                            in(Linear_Expression), in(Divisor))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_bounded_affine_image(in(Handle), in(Var),
                                                 in(Lower_Bound),
                                                 in(Upper_Bound),
                                                 in(Divisor))
             :: any_term * any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_bounded_affine_preimage(in(Handle), in(Var),
                                                    in(Lower_Bound),
                                                    in(Upper_Bound),
                                                    in(Divisor))
             :: any_term * any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_generalized_affine_image(in(Handle),
                                                     in(Var), in(Rel),
                                                     in(Linear_Expression),
                                                     in(Divisor))
             :: any_term * any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_generalized_affine_preimage(in(Handle),
                                                        in(Var), in(Rel),
                                                        in(Linear_Expression),
                                                        in(Divisor))
             :: any_term * any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_generalized_affine_image_lhs_rhs(in(Handle),
                                                             in(LHS),
                                                             in(Rel), in(RHS))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_generalized_affine_preimage_lhs_rhs(in(Handle),
                                                                in(LHS),
                                                                in(Rel),
                                                                in(RHS))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_time_elapse_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_topological_closure_assign(in(Handle))
             :: any_term + foreign.

:- true pred ppl_Polyhedron_BHRZ03_widening_assign_with_token(in(Handle1),
                                                               in(Handle2),
                                                               in(Tokens))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_BHRZ03_widening_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token(in(Handle1),
                                                                in(Handle2),
                                                               in(CList),
                                                               in(Tokens))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_limited_BHRZ03_extrapolation_assign(in(Handle1),
                                                               in(CList),
                                                                in(Handle2))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token(in(Handle1),
                                                                in(Handle2),
                                                               in(CList),
                                                               in(Tokens))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign(in(Handle1),
                                                                in(Handle2),
                                                               in(CList))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_H79_widening_assign_with_token(in(Handle1),
                                                            in(Handle2),
                                                            in(Tokens))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_H79_widening_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_limited_H79_extrapolation_assign_with_token(in(Handle1),
                                                             in(Handle2),
                                                               in(CList),
                                                            in(Tokens))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_limited_H79_extrapolation_assign(in(Handle1),
                                                             in(Handle2),
                                                               in(CList))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token(in(Handle1),
                                                                in(Handle2),
                                                               in(CList),
                                                            in(Tokens))
             :: any_term * any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_bounded_H79_extrapolation_assign(in(Handle1),
                                                                in(Handle2),
                                                               in(CList))
             :: any_term * any_term * any_term + foreign.

:- true pred
   ppl_Polyhedron_add_space_dimensions_and_project(in(Handle),
                                                   in(NDimensions_To_Add))
             :: any_term * any_term + foreign.

:- true pred
   ppl_Polyhedron_add_space_dimensions_and_embed(in(Handle),
                                                 in(NDimensions_To_Add))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_concatenate_assign(in(Handle1), in(Handle2))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_remove_space_dimensions(in(Handle), in(VList))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_remove_higher_space_dimensions(in(Handle),
                                                           in(Dimensions))
             :: any_term * any_term + foreign.

:- true pred ppl_Polyhedron_expand_space_dimension(in(Handle),
                                                   in(Var),
                                                   in(Dimensions))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_fold_space_dimensions(in(Handle),
                                                  in(VList),
                                                  in(Var))
             :: any_term * any_term * any_term + foreign.

:- true pred ppl_Polyhedron_map_space_dimensions(in(Handle),
                                                 in(PIFunc))
             :: any_term * any_term + foreign.

:- extra_linker_opts('-L.libs').
:- use_foreign_library(ppl_ciao).

:- impl_defined(
[
        ppl_version_major/1,
        ppl_version_minor/1,
        ppl_version_revision/1,
        ppl_version_beta/1,
        ppl_version/1,
        ppl_banner/1,
        ppl_max_space_dimension/1,
        ppl_initialize/0,
        ppl_finalize/0,
        ppl_set_timeout_exception_atom/1,
        ppl_timeout_exception_atom/1,
        ppl_set_timeout/1,
        ppl_reset_timeout/0,
        ppl_new_Polyhedron_from_space_dimension/4,
        ppl_new_Polyhedron_from_Polyhedron/4,
        ppl_new_Polyhedron_from_constraints/3,
        ppl_new_Polyhedron_from_generators/3,
        ppl_new_Polyhedron_from_bounding_box/3,
        ppl_Polyhedron_swap/2,
        ppl_delete_Polyhedron/1,
        ppl_Polyhedron_space_dimension/2,
        ppl_Polyhedron_affine_dimension/2,
        ppl_Polyhedron_get_constraints/2,
        ppl_Polyhedron_get_minimized_constraints/2,
        ppl_Polyhedron_get_generators/2,
        ppl_Polyhedron_get_minimized_generators/2,
        ppl_Polyhedron_relation_with_constraint/3,
        ppl_Polyhedron_relation_with_generator/3,
        ppl_Polyhedron_get_bounding_box/3,
        ppl_Polyhedron_is_empty/1,
        ppl_Polyhedron_is_universe/1,
        ppl_Polyhedron_is_bounded/1,
        ppl_Polyhedron_bounds_from_above/2,
        ppl_Polyhedron_bounds_from_below/2,
        ppl_Polyhedron_maximize/5,
        ppl_Polyhedron_maximize_with_point/6,
        ppl_Polyhedron_minimize/5,
        ppl_Polyhedron_minimize_with_point/6,
        ppl_Polyhedron_is_topologically_closed/1,
        ppl_Polyhedron_contains_Polyhedron/2,
        ppl_Polyhedron_strictly_contains_Polyhedron/2,
        ppl_Polyhedron_is_disjoint_from_Polyhedron/2,
        ppl_Polyhedron_equals_Polyhedron/2,
        ppl_Polyhedron_OK/1,
        ppl_Polyhedron_add_constraint/2,
        ppl_Polyhedron_add_constraint_and_minimize/2,
        ppl_Polyhedron_add_generator/2,
        ppl_Polyhedron_add_generator_and_minimize/2,
        ppl_Polyhedron_add_constraints/2,
        ppl_Polyhedron_add_constraints_and_minimize/2,
        ppl_Polyhedron_add_generators/2,
        ppl_Polyhedron_add_generators_and_minimize/2,
        ppl_Polyhedron_intersection_assign/2,
        ppl_Polyhedron_intersection_assign_and_minimize/2,
        ppl_Polyhedron_poly_hull_assign/2,
        ppl_Polyhedron_poly_hull_assign_and_minimize/2,
        ppl_Polyhedron_poly_difference_assign/2,
        ppl_Polyhedron_affine_image/4,
        ppl_Polyhedron_affine_preimage/4,
        ppl_Polyhedron_bounded_affine_image/5,
        ppl_Polyhedron_bounded_affine_preimage/5,
        ppl_Polyhedron_generalized_affine_image/5,
        ppl_Polyhedron_generalized_affine_preimage/5,
        ppl_Polyhedron_generalized_affine_image_lhs_rhs/4,
        ppl_Polyhedron_generalized_affine_preimage_lhs_rhs/4,
        ppl_Polyhedron_time_elapse_assign/2,
        ppl_Polyhedron_topological_closure_assign/1,
        ppl_Polyhedron_BHRZ03_widening_assign_with_token/3,
        ppl_Polyhedron_BHRZ03_widening_assign/2,
        ppl_Polyhedron_limited_BHRZ03_extrapolation_assign_with_token/4,
        ppl_Polyhedron_limited_BHRZ03_extrapolation_assign/3,
        ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign_with_token/4,
        ppl_Polyhedron_bounded_BHRZ03_extrapolation_assign/3,
        ppl_Polyhedron_H79_widening_assign_with_token/3,
        ppl_Polyhedron_H79_widening_assign/2,
        ppl_Polyhedron_limited_H79_extrapolation_assign_with_token/4,
        ppl_Polyhedron_limited_H79_extrapolation_assign/3,
        ppl_Polyhedron_bounded_H79_extrapolation_assign_with_token/4,
        ppl_Polyhedron_bounded_H79_extrapolation_assign/3,
        ppl_Polyhedron_add_space_dimensions_and_project/2,
        ppl_Polyhedron_add_space_dimensions_and_embed/2,
        ppl_Polyhedron_concatenate_assign/2,
        ppl_Polyhedron_remove_space_dimensions/2,
        ppl_Polyhedron_remove_higher_space_dimensions/2,
        ppl_Polyhedron_expand_space_dimension/3
        ppl_Polyhedron_fold_space_dimensions/3
        ppl_Polyhedron_map_space_dimensions/2
]).

:- doc(version_maintenance,off).


*/
