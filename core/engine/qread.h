/*
 *  qread.h
 *
 *  Reader of quickload objects.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#ifndef _CIAO_QREAD_H
#define _CIAO_QREAD_H

CBOOL__PROTO(pop_qlinfo);
CBOOL__PROTO(prolog_qread);
CBOOL__PROTO(push_qlinfo);
CBOOL__PROTO(qread1, FILE *qfile, tagged_t *rungoal);

#endif /* _CIAO_QREAD_H */
