/*
 The SWI-Prolog interface to MiniSat SAT solver
 http://www.cs.chalmers.se/Cs/Research/FormalMethods/MiniSat/MiniSat.html

 Copyright (c) 2006, Michael Codish, Vitaly Lagoon, and Peter J. Stuckey
 
 Permission is hereby granted, free of charge, to any person obtaining a
 copy of this software and associated documentation files (the
 "Software"), to deal in the Software without restriction, including
 without limitation the rights to use, copy, modify, merge, publish,
 distribute, sublicense, and/or sell copies of the Software, and to
 permit persons to whom the Software is furnished to do so, subject to
 the following conditions:
 
 The above copyright notice and this permission notice shall be included
 in all copies or substantial portions of the Software.
 
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/* TODO (JFMC): include ciao-minisat.h */
#include <ciao_prolog.h>
#include <stdio.h>
#include <assert.h>
#include "solver.h"

#define MAX_CLAUSE_LENGTH 10000
#define PROLOG_SUCCESS 1;
#define PROLOG_FAILURE 0;

static lit clause_buffer[MAX_CLAUSE_LENGTH];
static solver *s=NULL;



//------------------------------------------------------
//
// Predefined Prolog atoms
//



static inline int get_cons(ciao_term c, ciao_term* h, ciao_term* t) {
  if (!ciao_is_list(c)) return 0;

  *h = ciao_list_head(c);
  *t = ciao_list_tail(c);
  return 1;
}


//
// copy a list - the elemnts are the same just the list structure is copied
//
static inline ciao_term copy_list(ciao_term l) {
  ciao_term head, tail;

  if ( ciao_is_list(l) ) {
    get_cons(l,&head,&tail);
    return ciao_list(head, copy_list(tail));
  } else {
    return l;
  }
}


static inline lit pl2lit(ciao_term pl_literal)
{
    int pl_lit_int;
    pl_lit_int = ciao_to_integer(pl_literal);
    return (pl_lit_int<0) ? neg(toLit(-pl_lit_int)) : toLit(pl_lit_int);
}




ciao_bool minisat_new_solver(void)
{
    s = solver_new();
    return PROLOG_SUCCESS;
}


ciao_bool minisat_delete_solver(void)
{
    if (s) {
	solver_delete(s);
	s = NULL;
    }
    return PROLOG_SUCCESS;
}



ciao_bool minisat_add_clause(ciao_term l)
{
    int pl_literal;
    ciao_term head;
    ciao_term list = copy_list(l);
    int *ptr = clause_buffer;
    bool status;

    while( get_cons(list, &head, &list) ) {
      *ptr = pl2lit(head);
      ptr++;
    }
    
    /* assert(PL_get_nil(list)); */
    
    if (solver_addclause(s,clause_buffer,ptr)) {
      PROLOG_SUCCESS;
    } else {
      PROLOG_FAILURE;
    }
}


ciao_bool minisat_solve(ciao_term res)
{
    if (!solver_simplify(s)) PROLOG_FAILURE;
    if (!solver_solve(s,NULL,NULL)) PROLOG_FAILURE;

    {
	ciao_term l;
	ciao_term pl_lit;
	int pl_lit_int;
	int i = solver_nvars(s);
	void **vals = vec_begin(&(s->model));
	
	l = ciao_atom("[]");

	while( --i >= 0 ) {
	    switch((int)((lbool)vals[i])) {
	    case  1: pl_lit_int = i;  break;
	    case -1: pl_lit_int = -i; break;
	    case  0: pl_lit = 0;
	    default : assert(0);
	    }
	    if (pl_lit_int) {
		pl_lit = ciao_integer(pl_lit_int);
		l=ciao_list(pl_lit, l);
	    }
	}
	return ciao_unify(res, l);
    }
}
