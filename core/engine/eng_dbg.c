/*
 *  eng_dbg.c
 *
 *  Support for debugging and tracing the engine code
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2002-2015 Ciao Development Team
 */

#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/io_basic.h>
#include <ciao/streams_basic.h>
#include <ciao/eng_dbg.h>

#if defined(DEF_WR_DEBUG)

/* local declarations */
/*
myDEREF(Xderef,X) 
     tagged_t Xderef, X;
{ 
  tagged_t m_i, m_j; 
  tagged_t aux1, aux2, aux3;
  tagged_t *aux4;
  m_i = X; 

  if (IsVar(m_i)) 
    do {
      aux1 = (tagged_t)(m_i) & POINTERMASK;
      aux2 = aux1 + SMALLPTR_BASE;
      aux4 = (tagged_t *)aux2;
      aux3 = *aux4;
      m_i = aux3;
      if (m_i == m_j)
	break;
    }
    while (IsVar(m_i=m_j)); 

  Xderef = m_i; 
}
*/

static tagged_t safe_deref(tagged_t t)
{
   tagged_t aux;
   
   DerefSwitch(t,aux,;);
 
   return (t & ~3);
}

CVOID__PROTO(wr_tagged, tagged_t t) {
  wr_tagged_rec(Arg,t);
  putchar('\n');
}

CVOID__PROTO(wr_tagged_rec, tagged_t t) {
  tagged_t temp;
  int arity,i;

  t = safe_deref(t);
  switch(TagOf(t)) {
  case LST:
    putchar('[');
    RefCar(temp,t);
    wr_tagged_rec(Arg,temp);
    RefCdr(temp,t);
    t = safe_deref(temp);
    while(TagIsLST(t))	{
      putchar(',');
      RefCar(temp,t);
      wr_tagged_rec(Arg,temp);
      RefCdr(temp,t);
      t = safe_deref(temp);
    }
    if(t!=atom_nil) {
      putchar('|');
      wr_tagged_rec(Arg,t);
    }
    putchar(']');
    break;
  case STR:
    if (STRIsLarge(t))
      goto number;
    wr_tagged_rec(Arg,TagToHeadfunctor(t));
    putchar('(');
    arity = Arity(TagToHeadfunctor(t));
    for(i=1; i<=arity; i++){
      if(i>1) putchar(',');
      RefArg(temp,t,i);
      wr_tagged_rec(Arg,temp);
    }
    putchar(')');
    break;
  case UBV:
  case SVA:
  case HVA:
  case CVA:
    print_variable(Arg,stream_user_output,t);
    break;
  case ATM:
    print_atom(Arg,stream_user_output,t);
    break;
  case NUM:
  number:
  print_number(Arg, stream_user_output,t);
  break;
  }
}

void wr_functor_1(definition_t *func);

void wr_functor(char *s, definition_t *func)
{
  printf("%s: ",s);
  wr_functor_1(func);
  putchar('\n');
}

/* unused */
/*
static definition_t *which_parent(definition_t *func)
{
  definition_t *func1;

  do
    func1 = func,
    func = (definition_t *)TagToPointer(func1->printname);
  while (!(func1->printname & 2));
  return func;
}
*/

/* unused */
/*
static which_child(definition_t *func)
{
   int i; 
  definition_t *f1;  

  for (i=1, f1 = which_parent(func)->code.incoreinfo->subdefs;
       f1 != func;
       i++, f1 = (definition_t *)TagToPointer(f1->printname))
    ;

  return i;

  printf("Out of order!!\n");
}
*/

void wr_functor_1(definition_t *func)
{
  if (!(func->printname & 1))
    printf("%s/%d", GetString(func->printname), func->arity);
  else
    printf("(?)");
/*
    {
      putchar('(');
      wr_functor_1(which_parent(func));
      printf("-%d)/%d", which_child(func), func->arity);
    }
*/
} 

CVOID__PROTO(display_term, tagged_t term, stream_node_t *stream, bool_t quoted);

#define TRACE_CALLS_SHOW_ARG1  1
//#define TRACE_CALLS_SHOW_ARGS  1

CVOID__PROTO(wr_call, char *s, definition_t *func) {
  int i;

  printf("%s: ",s);

  if (!(func->printname & 1)) {
#if defined(TRACE_CALLS_SHOW_ARG1) || defined(TRACE_CALLS_SHOW_ARGS) /* display args */
    printf("%s", GetString(func->printname));
    if (func->arity > 0) {
      putchar('(');
      DEREF(X(0),X(0));
      display_term(Arg,X(0),Output_Stream_Ptr, TRUE);
#if defined(TRACE_CALLS_SHOW_ARGS) /* display all args */
      for (i = 1; i < func->arity; i++) {
	printf(",");
        display_term(Arg,X(i),Output_Stream_Ptr, TRUE);
      }
#else
      for (i = 1; i < func->arity; i++) printf(",_");
#endif
      putchar(')');
    }
#else /* only display functor */
    printf("%s/%d", GetString(func->printname), func->arity);
#endif
  } else {
    printf("(?)");
  }

  putchar('\n');
}

CVOID__PROTO(wr_functor_spec, tagged_t t) {
  wr_tagged(Arg,t);
  printf("/%" PRIdm "\n", Arity(t));
}

#else

int eng_dbg__dummy[0]; /* prevent "no symbols" warnings in .a creation */

#endif
