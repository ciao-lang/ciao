/* Ciao Prolog extended foreign language interface: definitions.
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

#include "ciao_efli.hh"
#include "ppl_prolog_common.defs.hh"

namespace Parma_Polyhedra_Library {

namespace Interfaces {

namespace Prolog {

namespace Ciao {

bool Prolog_has_unbounded_integers;

long Prolog_min_integer;

long Prolog_max_integer;

void
ppl_Prolog_sysdep_init() {
  Prolog_has_unbounded_integers = true;
  Prolog_min_integer = 0;
  Prolog_max_integer = 0;
}

void
ppl_Prolog_sysdep_deinit() {
}

int
Prolog_get_Coefficient(Prolog_term_ref t, Coefficient& n) {
  assert(Prolog_is_integer(t));
  if (ciao_fits_in_int(t))
    n = ciao_to_integer(t);
  else {
    const char* s = ciao_get_number_chars(t);
    n = Coefficient(s);
    // TODO: remove the const_cast when the Ciao people fix ciao_prolog.h.
    ciao_free(const_cast<char*>(s));
  }
  return 1;
}

int
Prolog_put_Coefficient(Prolog_term_ref& t, const Coefficient& n) {
  int i;
  if (assign_r(i, n, ROUND_NOT_NEEDED) == V_EQ)
    t = ciao_integer(i);
  else {
    std::ostringstream s;
    s << n;
    std::string str = s.str();
    // TODO: remove the const_cast when the Ciao people fix ciao_prolog.h.
    t = ciao_put_number_chars(const_cast<char*>(str.c_str()));
  }
  return 1;
}

int
Prolog_unify_Coefficient(Prolog_term_ref t, const Coefficient& n) {
  Prolog_term_ref u = Prolog_new_term_ref();
  Prolog_put_Coefficient(u, n);
  return ciao_unify(t, u);
}

} // namespace Ciao

} // namespace Prolog

} // namespace Interfaces

} // namespace Parma_Polyhedra_Library

extern "C" void
init() {
  ppl_initialize();
}
