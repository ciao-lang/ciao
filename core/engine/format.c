/*
 *  format.c
 *
 *  Support for the format/2, format/3 predicates.
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 */

#include <string.h>

#include <ciao/datadefs.h>
#include <ciao/support_macros.h>
#include <ciao/alloc.h>
#include <ciao/float_tostr.h>
#include <ciao/format.h>
#include <ciao/wambuiltin.h> /* fu1_integer() */
#include <ciao/term_support.h>
#include <ciao/io_basic.h>

/*
 * bool_t prolog_format_print_float(formatChar,arg,precision)
 * char formatChar;	Selects type of format.
 * flt64_t arg;		Value to be printed.
 * intmach_t precision;	Precision of printed item.
 *
 * Description: Print a FLOAT value on file file according to the format
 * specified by formatChar and the precision specified by precision.
 * Precision is number of digits after decimal point for some formats and
 * total number of digits for some FORMATS.
 */

#define MAX_OUTPUT_DIGITS 1023
#define BUFF_SIZE         2048

CBOOL__PROTO(prolog_format_print_float)
{
  intmach_t precision;
  char buf[BUFF_SIZE], formatChar;
  double f;

  DEREF(X(0),X(0));
  formatChar = GetInteger(X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  precision = GetInteger(X(2));
  
  /* New code (Edison): */

  f = GetFloat(X(1));

  if(formatChar=='f' && precision > 1023)
    precision = 1023;
  else if(precision<0)
    precision = 6;
  float_to_string(buf, precision, formatChar, f, 10);
  print_string(Output_Stream_Ptr, buf);

  return TRUE;
}

/*
 * bool_t prolog_format_print_integer(formatChar,arg,precision)
 * char formatChar;	Selects type of format.
 * intmach_t arg;	Value to be printed.
 * intmach_t precision;	Precision or radix of printed item.
 *
 * Description: Print an INTEGER value on file file according to the format
 * specified by formatChar and the precision specified by precision.
 * Precision is number of digits after decimal point for some formats and
 * radix for some formats.
 */

CBOOL__PROTO(prolog_format_print_integer)
{
  char formatChar;
  int precision, base;
  
  DEREF(X(0),X(0));
  formatChar = GetSmall(X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));
  precision = GetInteger(X(2));

  if (formatChar=='r')
    base = ((precision<2 || precision>36) ? 8 : precision);
  else if (formatChar=='R')
    base = ((precision<2 || precision>36) ? -8 : -precision);
  else
    base = 10;

  if (IsFloat(X(1)))
    Numstack_End = NULL,
    X(1) = fu1_integer(Arg,X(1), NULL);
  number_to_string(Arg, X(1), base);
  
  if ((formatChar=='d' || formatChar=='D') && precision > 0)
    {
      int usilen = strlen(Atom_Buffer) - (Atom_Buffer[0]=='-');
      int n = precision-usilen+1;
      
      if (n>0)
	{
	  int i;
	  int dig1 = (Atom_Buffer[0] == '-');
	  int slen = strlen(Atom_Buffer);
	  
	  while (slen+n+1 > (i=Atom_Buffer_Length)) {
	    Atom_Buffer_Length <<= 1;
	    Atom_Buffer = checkrealloc_ARRAY(char,
					     i,
					     Atom_Buffer_Length,
					     Atom_Buffer);
	  }
	  UpdateHeapMargins();
	  
	  for (i=slen; i>=dig1; i--)
	    Atom_Buffer[i+n] = Atom_Buffer[i];
	  for (i=dig1+n-1; i>=dig1; i--)
	    Atom_Buffer[i] = '0';
	}

      {
	int i;
	int slen = strlen(Atom_Buffer);
	int ppos = slen-precision;
	
	if (slen+2 > (i=Atom_Buffer_Length)) {
	  Atom_Buffer_Length <<= 1;
	  Atom_Buffer = checkrealloc_ARRAY(char,
					   i,
					   Atom_Buffer_Length,
					   Atom_Buffer);
	  UpdateHeapMargins();
	}
	
	for (i=slen; i>=ppos; i--)
	  Atom_Buffer[i+1] = Atom_Buffer[i];
	Atom_Buffer[ppos] = '.';
      }
    }
  if (formatChar=='D')
    {
      int i, count;
      int slen = strlen(Atom_Buffer);
      int dig1 = (Atom_Buffer[0]=='-');
      int ppos = slen;
      
      for (i=dig1, count=0; i<ppos; i++)
	{
	  if (Atom_Buffer[i]=='.') ppos=i;
	  else count++;
	}
      count = (count-1)/3;
      
      if (count>0)
	{
	  if (slen+count+1 > (i=Atom_Buffer_Length)) {
	    Atom_Buffer_Length <<= 1;
	    Atom_Buffer = checkrealloc_ARRAY(char,
					     i,
					     Atom_Buffer_Length,
					     Atom_Buffer);
	    UpdateHeapMargins();
	  }
	  
	  for (i=slen; i>=ppos; i--) {
	    Atom_Buffer[i+count] = Atom_Buffer[i];
	  }
	  for (i=ppos-1; count>0; count--) {
	    Atom_Buffer[i+count] = Atom_Buffer[i]; i--;
	    Atom_Buffer[i+count] = Atom_Buffer[i]; i--;
	    Atom_Buffer[i+count] = Atom_Buffer[i]; i--;
	    Atom_Buffer[i+count] = ',';
	  }
	}
    }
  
  print_string(Output_Stream_Ptr, Atom_Buffer);
  return TRUE;
}

