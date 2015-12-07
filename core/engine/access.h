/*
 *  access.h
 *
 *  Access macros for the various WAM areas.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

/*
 * The point is to hide the memory model (SRI/vanilla) as well as
 * implementation details such as growth directions of stacks.
 */

#ifndef _CIAO_ACCESS_H
#define _CIAO_ACCESS_H

#define Offset(X,O)	((tagged_t *)(X) + (O))
#define CharOffset(X,O)	((tagged_t *)((char *)(X) + (O)))

/* THE HEAP */

/* assuming heap growth in positive direction */

#define OnHeap(t)       (HeapYounger(Heap_End,t) && !HeapYounger(Heap_Start,t))
#define OffHeaptop(t,H)          (!HeapYounger(H,t))
#define HeapYounger(X,Y)	((tagged_t *)(X)>(tagged_t *)(Y))
#define HeapDifference(X,Y)	((tagged_t *)(Y) - (tagged_t *)(X))
#define HeapCharDifference(X,Y)	((char *)(Y) - (char *)(X))
#define HeapOffset(X,O)		((tagged_t *)(X) + (O))
#define HeapCharOffset(X,O)	((tagged_t *)((char *)(X) + (O)))
#define HeapNext(X)		(*(X)++)
#define HeapPush(H,X)		(*(H) = (X), (H)++) /* X often contains H */
#define HeapDecr(H)		(--(H))

/* THE FRAME STACK */

/* assuming stack growth in positive direction */

#define OnStack(t) (StackYounger(Stack_End,t) && !StackYounger(Stack_Start,t))
#define OffStacktop(t,H)         (!StackYounger(H,t))
#define StackYounger(X,Y)	((tagged_t *)(X)>(tagged_t *)(Y))
#define StackDifference(X,Y)	((tagged_t *)(Y) - (tagged_t *)(X))
#define StackCharDifference(X,Y)	((char *)(Y) - (char *)(X))
#define StackOffset(X,O)	((tagged_t *)(X) + (O))
#define StackCharOffset(X,O)	((frame_t *)((char *)(X) + (O)))
#define StackNext(P)		(*(P)++)
#define StackDecr(P)		(--(P))

/* THE TRAIL */

/* assuming trail growth in positive direction */

#define OnTrail(t)  (TrailYounger(Trail_End,t) && !TrailYounger(Trail_Start,t))
#define OffTrailtop(t,P)	(!TrailYounger(P,t))
#define TrailYounger(X,Y)	((tagged_t *)(X)>(tagged_t *)(Y))
#define TrailDifference(X,Y)	((tagged_t *)(Y) - (tagged_t *)(X))
#define TrailCharDifference(X,Y)	((char *)(Y) - (char *)(X))
#define TrailOffset(X,O)	((tagged_t *)(X) + (O))
#define TrailCharOffset(X,O)	((tagged_t *)((char *)(X) + (O)))
#define TrailPop(P)		(*--(P))
#define TrailNext(P)		(*(P)++)
#define TrailPush(P,X)		(*(P)++ = (X))
#define TrailPushCheck(P,X)     trail_push_check(Arg,X)



/* THE CHOICEPOINT STACK */

/* assuming choicestack growth in negative direction */

#define OnChoice(t) (ChoiceYounger(t,Choice_Start) && !ChoiceYounger(t,Choice_End))
#define OffChoicetop(t,B)	ChoiceYounger(t,B)
#define ChoiceYounger(X,Y)	((tagged_t *)(X)<(tagged_t *)(Y))
#define ChoiceOffset(X,O)	((tagged_t *)(X) - (O))
#define ChoiceCharOffset(X,O)	((node_t *)((char *)(X) - (O)))
#define ChoiceDifference(X,Y)	((tagged_t *)(X) - (tagged_t *)(Y))
#define ChoiceCharDifference(X,Y)	((char *)(X) - (char *)(Y))
#define ChoicePrev(P)		(*(P)++)
#define ChoiceNext(P)		(*--(P))
#define ChoicePush(P,X)		(*--(P) = (X))



#if defined(USE_TAGGED_CHOICE_START)
#define ChoiceFromInt(Y) (ChoiceCharOffset(Tagged_Choice_Start,Y))
#define ChoiceToInt(Y)	 (ChoiceCharDifference(Tagged_Choice_Start,Y))
#else
#define ChoiceFromInt(Y) ((node_t *)ChoiceOffset(Choice_Start,(GetSmall(Y))))
#define ChoiceToInt(Y)	 (MakeSmall(ChoiceDifference(Choice_Start,Y)))
#endif

#define RefHeap(To,From) \
{ To = *(From); }

#define RefCar(To,From) \
{ To = *TagToCar(From); }

#define RefCdr(To,From) \
{ To = *TagToCdr(From); }

#define RefArg(To,From,I) \
{ To = *TagToArg(From,I); }

#define RefHeapNext(To,From) \
{ To = *(From)++; }

#define PushRefHeapNext(To,From) \
{ *(To)++ = *(From)++; }

#define RefStack(To,From) \
{ To = *(From); }

#define HeapPushRefStack(To,From) \
{ *(To)++ = *(From); }

#define RefHVA(To,From) \
{ To = *TagToHVA(From); }

#define RefCVA(To,From) \
{ To = *TagToCVA(From); }

#define RefSVA(To,From) \
{ To = *TagToSVA(From); }

#define LoadSVA(Y)		{Y = TagSVA(&Y); }
#define Load2SVA(X,Y)		{X = Y = TagSVA(&Y); }
#define PreLoadHVA(X,H)		{X = TagHVA(H); }
#define ConstrHVA(H)		{HeapPush(H,TagHVA(H)); }
#define LoadHVA(To,H)		{HeapPush(H,To = TagHVA(H)); }
#define Load2HVA(To1,To2,H)	{HeapPush(H,To1 = To2 = TagHVA(H)); }
#define LoadCVA(To,H)		{HeapPush(H,To = TagCVA(H)); }

#endif /* _CIAO_ACCESS_H */
