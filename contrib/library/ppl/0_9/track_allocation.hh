/* Code for keeping track of polyhedra allocations and deallocations.
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

/* January 2009. Pathed by Remy Haemmerle for CLIP */

#ifndef PPL_track_allocation_hh
#define PPL_track_allocation_hh

#ifndef PROLOG_TRACK_ALLOCATION
#define PROLOG_TRACK_ALLOCATION 0
#endif

#if PROLOG_TRACK_ALLOCATION

#include <set>
#include <iostream>

namespace Parma_Polyhedra_Library {

class Poly_Tracker {
public:
  void insert(const void* pp);
  void check(const void* pp) const;
  void remove(const void* pp);

  Poly_Tracker();
  ~Poly_Tracker();

private:
  typedef std::set<const void*, std::less<const void*> > Set;
  Set s;
};

inline
Poly_Tracker::Poly_Tracker() {
}

inline
Poly_Tracker::~Poly_Tracker() {
  Set::size_type n = s.size();
  if (n > 0)
    std::cerr << "Poly_Tracker: " << n << " polyhedra leaked!" << std::endl;
}

inline void
Poly_Tracker::insert(const void* pp) {
  std::pair<Set::iterator, bool> stat = s.insert(pp);
  if (!stat.second) {
    std::cerr << "Poly_Tracker: two polyhedra at the same address "
	      << "at the same time?!" << std::endl;
    abort();
  }
}

inline void
Poly_Tracker::check(const void* pp) const {
  if (s.find(pp) == s.end()) {
    std::cerr << "Poly_Tracker: attempt to access an inexistent polyhedron."
	      << std::endl;
    abort();
  }
}

void
Poly_Tracker::remove(const void* pp) {
  if (s.erase(pp) != 1) {
    std::cerr << "Poly_Tracker: attempt to deallocate "
	      << "an inexistent polyhedron."
	      << std::endl;
    abort();
  }
}

namespace {

inline Poly_Tracker&
poly_tracker() {
  static Poly_Tracker pt;
  return pt;
}

} // namespace

} // namespace Parma_Polyhedra_Library


#define REGISTER(x) Parma_Polyhedra_Library::poly_tracker().insert(x)
#define UNREGISTER(x) Parma_Polyhedra_Library::poly_tracker().remove(x)
#define CHECK(x) Parma_Polyhedra_Library::poly_tracker().check(x)

#else

#define REGISTER(x)
#define UNREGISTER(x)
#define CHECK(x)

#endif

#endif // !defined(PPL_track_allocation_hh)
