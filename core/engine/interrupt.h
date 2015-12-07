/*
 *  interrupt.h
 *
 *  Interrupt handlers.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_INTERRUPT_H
#define _CIAO_INTERRUPT_H

extern definition_t *int_address;

CVOID__PROTO(control_c_normal);
void enable_conditions(void);

#endif /* _CIAO_INTERRUPT_H */
