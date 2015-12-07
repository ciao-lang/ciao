/*
 *  startgoal.h
 *
 *  Support code for starting goal execution.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#ifndef _CIAO_STARTGOAL_H
#define _CIAO_STARTGOAL_H

int firstgoal(goal_descriptor_t *firstworker, tagged_t goal_term);
THREAD_RES_T startgoal(THREAD_ARG wo);
THREAD_RES_T make_backtracking(THREAD_ARG wo);

#endif /* _CIAO_STARTGOAL_H */
