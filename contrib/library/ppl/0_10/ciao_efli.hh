/* Ciao Prolog extended foreign language interface: declarations.
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

#ifndef PPL_ciao_efli_hh
#define PPL_ciao_efli_hh 1

#define PPL_NO_AUTOMATIC_INITIALIZATION
#include "ppl.hh"
// By including "ppl.hh" first, we ensure the limit macros provided by
// <stdint.h> and/or <inttypes.h> are already defined.
#include "ciao_cfli.hh"

namespace Parma_Polyhedra_Library {

namespace Interfaces {

namespace Prolog {

namespace Ciao {

/*!
  True if and only if the Prolog engine supports unbounded integers.
*/
extern bool Prolog_has_unbounded_integers;

/*!
  If \p Prolog_has_unbounded_integers is false, holds the minimum
  integer value representable by a Prolog integer.
  Holds zero otherwise.
*/
extern long Prolog_min_integer;

/*!
  If \p Prolog_has_unbounded_integers is false, holds the maximum
  integer value representable by a Prolog integer.
  Holds zero otherwise.
*/
extern long Prolog_max_integer;

/*!
  Performs system-dependent initialization.
*/
void
ppl_Prolog_sysdep_init();

/*!
  Perform system-dependent de-itialization.
*/
void
ppl_Prolog_sysdep_deinit();

// FIXME: write the documentation.
int
Prolog_get_Coefficient(Prolog_term_ref t, Coefficient& n);

// FIXME: write the documentation.
int
Prolog_unify_Coefficient(Prolog_term_ref t, const Coefficient& n);

// FIXME: write the documentation.
int
Prolog_put_Coefficient(Prolog_term_ref& t, const Coefficient& n);

} // namespace Ciao

} // namespace Prolog

} // namespace Interfaces

} // namespace Parma_Polyhedra_Library

#endif // !defined(PPL_ciao_efli_hh)
