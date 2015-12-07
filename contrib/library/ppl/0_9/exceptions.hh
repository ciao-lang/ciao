/* Exceptions used internally by the Prolog interfaces.
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

class internal_exception {
private:
  Prolog_term_ref tr;

public:
  explicit internal_exception(Prolog_term_ref t)
    : tr(t) {
  }

  virtual ~internal_exception() {
  }

  virtual Prolog_term_ref term() const {
    return tr;
  }
};

class Prolog_unsigned_out_of_range : public internal_exception {
private:
  unsigned long m;

public:
  explicit Prolog_unsigned_out_of_range(Prolog_term_ref t, unsigned long max)
    : internal_exception(t),
      m(max) {
  }

  unsigned long max() const {
    return m;
  }

};

class non_linear : public internal_exception {
private:
  const char* w;

public:
  explicit non_linear(const char* s, Prolog_term_ref t)
    : internal_exception(t), w(s) {
  }

  const char* where() const {
    return w;
  }
};

class not_an_integer : public internal_exception {
public:
  explicit not_an_integer(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class not_unsigned_integer : public internal_exception {
public:
  explicit not_unsigned_integer(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class not_a_variable : public internal_exception {
public:
  explicit not_a_variable(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class not_a_polyhedron_kind : public internal_exception {
public:
  explicit not_a_polyhedron_kind(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class not_a_polyhedron_handle : public internal_exception {
public:
  explicit not_a_polyhedron_handle(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class not_an_optimization_mode : public internal_exception {
public:
  explicit not_an_optimization_mode(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class not_an_lp_problem_handle : public internal_exception {
public:
  explicit not_an_lp_problem_handle(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class not_a_complexity_class : public internal_exception {
public:
  explicit not_a_complexity_class(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class not_universe_or_empty : public internal_exception {
public:
  explicit not_universe_or_empty(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class not_a_relation : public internal_exception {
public:
  explicit not_a_relation(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class not_a_nil_terminated_list : public internal_exception {
public:
  explicit not_a_nil_terminated_list(Prolog_term_ref t)
    : internal_exception(t) {
  }
};

class PPL_integer_out_of_range {
private:
  Parma_Polyhedra_Library::Coefficient n;

public:
  explicit
  PPL_integer_out_of_range(const Parma_Polyhedra_Library::Coefficient& i)
    : n(i) {
  }

  const Parma_Polyhedra_Library::Coefficient i() const {
    return n;
  }
};

class unknown_interface_error {
private:
  const char* w;

public:
  unknown_interface_error(const char* s)
    : w(s) {
  }

  const char* where() const {
    return w;
  }
};
