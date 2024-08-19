#include <stdlib.h>

#include <ciao/eng.h>


/*

 In Numerical Recipes in C: The Art of Scientific Computing (William H.
 Press, Brian P. Flannery, Saul A. Teukolsky, William T.  Vetterling; New
 York: Cambridge University Press, 1990 (1st ed, p. 207)), the following
 comments are made:
  
   "If you want to generate a random integer between 1 and 10, you should
   always do it by

                  j=1+(int) (10.0*rand()/(RAND_MAX+1.0));

   and never by anything resembling
                                      
                     j=1+((int) (1000000.0*rand()) % 10);
                                      
   (which uses lower-order bits)."

*/

#if defined(Solaris)
long random(void);
void srandom(unsigned seed);
#endif


#if defined(LINUX)||defined(EMSCRIPTEN)||defined(BSD)
#  define RANDOM_MAX RAND_MAX
#else
#  define RANDOM_MAX 2147483647
#endif

/* This is for RANDOM in [0 1)
#define RANDOM ((flt64_t) -random()/(-RANDOM_MAX-1))
*/

/* This is for RANDOM in [0 1] */
#define RANDOM ((flt64_t)random()/RANDOM_MAX)

CBOOL__PROTO(prolog_random) {
  ERR__FUNCTOR("random:random", 1);
  DEREF(X(0),X(0));

  if (!IsVar(X(0))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR,atom_nil,1);
  }

  CBOOL__LASTUNIFY(BoxFloat(RANDOM),X(0));
}

CBOOL__PROTO(prolog_random3)
{
  ERR__FUNCTOR("random:random", 3);
  DEREF(X(0),X(0));
  if (!IsNumber(X(0))) {
    ERROR_IN_ARG(X(0),1,ERR_type_error(number));
  }

  DEREF(X(1),X(1));
  if (!IsNumber(X(1))) {
    ERROR_IN_ARG(X(1),2,ERR_type_error(number));
  }

  DEREF(X(2),X(2));
  if (!IsVar(X(2))) {
    BUILTIN_ERROR(INSTANTIATION_ERROR,atom_nil,3);
  }

  if (IsInteger(X(0)) && IsInteger(X(1))) {
    intmach_t low = TaggedToIntmach(X(0));
    intmach_t up  = TaggedToIntmach(X(1));
    /* former (uses low order bits, which very often are not that random):
    intmach_t r = low+(random() % (up-low+1));
    */
    intmach_t r = low + (intmach_t)(RANDOM*(up-low+1));
    CBOOL__LASTUNIFY(IntvalToTagged(r), X(2));
  } else{
    flt64_t low = TaggedToFloat(X(0));
    flt64_t up  = TaggedToFloat(X(1));
    flt64_t r = low+RANDOM*(up-low);
    CBOOL__LASTUNIFY(BoxFloat(r), X(2));
  }
}

CBOOL__PROTO(prolog_srandom) {
  ERR__FUNCTOR("random:srandom", 1);
  DEREF(X(0),X(0));

  if (IsVar(X(0))) {
    srandom(1);
  } else if (IsInteger(X(0))) {
    srandom((int)GetSmall(X(0)));
  } else {
    ERROR_IN_ARG(X(1),1,ERR_type_error(integer));
  }

  return TRUE;
}

