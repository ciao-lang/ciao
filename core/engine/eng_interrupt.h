/*
 *  eng_interrupt.h
 *
 *  Interrupt handlers.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#ifndef _CIAO_ENG_INTERRUPT_H
#define _CIAO_ENG_INTERRUPT_H

extern definition_t *int_address;

CVOID__PROTO(control_c_normal);
void enable_conditions(void);

#endif /* _CIAO_ENG_INTERRUPT_H */
