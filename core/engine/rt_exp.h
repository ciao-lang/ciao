/*
 *  rt_exp.h
 *
 *  See Copyright Notice in ciaoengine.pl
 */

#ifndef _CIAO_RT_EXP_H
#define _CIAO_RT_EXP_H

#if defined(OPTIM_COMP)
CBOOL__PROTO(predicate_def, definition_t *f, intmach_t bits);
CBOOL__PROTO(define_predicate);
#endif

CBOOL__PROTO(current_predicate);
CBOOL__PROTO(nd_current_predicate);
CBOOL__PROTO(predicate_property);
CBOOL__PROTO(nd_predicate_property);

#endif /* _CIAO_RT_EXP_H */
