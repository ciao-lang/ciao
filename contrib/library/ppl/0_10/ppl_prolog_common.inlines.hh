/* Common part of the Prolog interfaces: inline functions.
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

#ifndef PPL_ppl_prolog_common_inlines_hh
#define PPL_ppl_prolog_common_inlines_hh 1

#if PROLOG_TRACK_ALLOCATION || NOISY_PROLOG_TRACK_ALLOCATION

#include <typeinfo>
#include <iomanip>

template <typename T>
void
Allocation_Tracker::insert(const T* p) {
#if NOISY_PROLOG_TRACK_ALLOCATION
  std::cerr << "inserting " << typeid(*p).name()
            << " at " << std::hex << (void*) p << std::endl;
#endif
  std::pair<Set::iterator, bool> stat = s.insert(p);
  if (!stat.second) {
    std::cerr << "Interfaces::Prolog::Allocation_Tracker:"
                 " two objects at the same address at the same time?!"
              << std::endl;
    abort();
  }
}

template <typename T>
void
Allocation_Tracker::weak_insert(const T* p) {
#if NOISY_PROLOG_TRACK_ALLOCATION
  std::cerr << "inserting weak " << typeid(*p).name()
            << " at " << std::hex << (void*) p << std::endl;
#endif
  weak_s.insert(p);
}

template <typename T>
void
Allocation_Tracker::remove(const T* p) {
#if NOISY_PROLOG_TRACK_ALLOCATION
  std::cerr << "removing " << typeid(*p).name()
            << " at " << std::hex << (void*) p << std::endl;
#endif
  if (s.erase(p) != 1) {
    std::cerr << "Interfaces::Prolog::Allocation_Tracker:"
                 " attempt to deallocate a nonexistent polyhedron."
              << std::endl;
    abort();
  }
}

template <typename T>
void
Allocation_Tracker::check(const T* p) const {
  if (s.find(p) == s.end()
      && weak_s.find(p) == weak_s.end()) {
    std::cerr << "Interfaces::Prolog::Allocation_Tracker:"
                  " attempt to access a nonexistent "
              << typeid(*p).name()
              << " at " << std::hex << (void*) p << std::endl;
    abort();
  }
}

#endif // PROLOG_TRACK_ALLOCATION || NOISY_PROLOG_TRACK_ALLOCATION

#endif // !defined(PPL_ppl_prolog_common_inlines_hh)
