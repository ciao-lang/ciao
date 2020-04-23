/*
 *  unicode_gen.c
 *
 *  Generation of Unicode data for Ciao (C part)
 *
 *  Copyright (c) 2020 Jose F. Morales
 */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CODE_POINT 0x110000

/* Category (a subset) */
#define UCAT_Whitespace    1
#define UCAT_Sx_or_Px      2
#define UCAT_No            3
#define UCAT_Lu            4
/* Derived properties */
#define UCAT_XID_Start     8
#define UCAT_XID_Continue 16

/* --------------------------------------------------------------------------- */

char tbl_prop[MAX_CODE_POINT]; // character category and properties

static inline int get_tbl_cat(int c) {
  return tbl_prop[c] & 7;
}

static inline int has_tbl_prop(int c, int p) {
  return (tbl_prop[c] & p) != 0;
}

int read_unicode_data() {
  char line[1024];
  size_t rlen, len;
  char *s[16];
  FILE *f;
  f = fopen("UnicodeData.txt", "r");
  if (f == NULL) return 0;

  while (fgets(line, 1024, f) != NULL) {
    //printf("Retrieved line: %s", line);
    char *k = line;
    int i;
    for (i=0; i<14; i++) {
      s[i] = k;
      k = strchr(k, ';');
      //printf("field at %p\n", k);
      *k=0;
      //printf("field %d: %s |\n", i, s[i]);
      k++;
    }
    s[i] = k;
    // for (int i=0; i<=14; i++) {
    //   printf("field %d: %s |", i, s[i]);
    // }
    int code = (int)strtol(s[0],NULL,16);
    char *category = s[2];
    if (strcmp(category, "Zl")==0 ||
        strcmp(category, "Zp")==0 ||
        strcmp(category, "Zs")==0) {
      tbl_prop[code] = UCAT_Whitespace;
    }
    if (strcmp(category, "Cc")==0 &&
        (strcmp(s[4], "WS")==0 ||
         strcmp(s[4], "S")==0 ||
         strcmp(s[4], "B")==0)) {
      tbl_prop[code] = UCAT_Whitespace;
    }
    if (strcmp(category, "Sm")==0 ||
        strcmp(category, "Sc")==0 ||
        strcmp(category, "Sk")==0 ||
        strcmp(category, "So")==0 ||
        strcmp(category, "Pc")==0 ||
        strcmp(category, "Pd")==0 ||
        strcmp(category, "Ps")==0 ||
        strcmp(category, "Pe")==0 ||
        strcmp(category, "Pi")==0 ||
        strcmp(category, "Pf")==0 ||
        strcmp(category, "Po")==0) {
      tbl_prop[code] = UCAT_Sx_or_Px;
      //printf("%x %s SWI=%d\n", code, s[2], tbl_swi[code]);
    }
    if (strcmp(category, "No")==0) {
      tbl_prop[code] = UCAT_No;
    }
    if (strcmp(category, "Lu")==0) {
      tbl_prop[code] = UCAT_Lu;
    }
    //printf("%d %s %d\n", code, s[2], tbl_swi[code]);
    //if (code >= 128) printf("%d %s\n", tbl_swi[code], s[2]);
  }

  fclose(f);
  return 1;
}

int read_derived_properties() {
  char line[1024];
  size_t rlen, len;
  char *s[16];
  FILE *f;
  int c=0;
  f = fopen("DerivedCoreProperties.txt", "r");
  if (f == NULL) return 0;

  while (fgets(line, 1024, f) != NULL) {
    //printf("Retrieved line: %s", line);
    char *semicolon = strchr(line, ';');
    if (semicolon == NULL) continue;
    char *prop = semicolon+1;
    if (*prop == ' ') prop++;
    char *prop_end = strchr(prop, ' ');
    if (prop_end == NULL) continue;
    *prop_end = 0;

    char *code1 = line;
    char *point = strchr(line, '.');
    if (point >= semicolon) point = NULL;
    char *code2 = NULL;
    if (point && point[0]=='.' && point[1]=='.') {
      *point = 0;
      code2 = point+2;
    }
    char *blank = strchr(line, ' ');
    if (blank == NULL || blank >= semicolon) blank = semicolon;
    *blank = 0;

    int range1 = (int)strtol(code1,NULL,16);
    int range2;
    if (code2 != NULL) {
      range2 =  (int)strtol(code2,NULL,16);
    } else {
      range2 = range1;
    }

    int bit = 0;
    if (strcmp(prop, "XID_Start") == 0) {
      bit = UCAT_XID_Start;
    } else if (strcmp(prop, "XID_Continue") == 0) {
      bit = UCAT_XID_Continue;
    }
    if (bit) {
      for (int i=range1; i<=range2; i++) {
        tbl_prop[i]|=bit;
      }
    }
  }

  fclose(f);
  return 1;
}

int read_unicode_db() {
  for (int i=0; i<MAX_CODE_POINT; i++) {
    tbl_prop[i] = 0;
  }
  return read_unicode_data() && read_derived_properties();
}

/* --------------------------------------------------------------------------- */
/* Classification for Ciao source code */

/* (See also io_basic:code_class/2) */
#define RUNECLASS_BLK 1 /* whitespace ("Z*" unicode category) */
#define RUNECLASS_VAR 2 /* first char for variables (XID_Start and "Lu" category) */
#define RUNECLASS_ATM 3 /* first char for atoms (XID_Start and not "Lu" category) */
#define RUNECLASS_IDC 4 /* rest of chars identifier char not included above (XID_Continue + "No" category" - XID_Start) */
#define RUNECLASS_SYM 5 /* symbols (any of "S*" or "P*" unicode category) */

char rune_class[MAX_CODE_POINT];
int max_rune;

int classify_unicode() {
  int i;

  for (i=0; i<MAX_CODE_POINT; i++) {
    if (get_tbl_cat(i) == UCAT_Whitespace) { // Whitespace
      rune_class[i]=RUNECLASS_BLK;
    } else if (has_tbl_prop(i, UCAT_XID_Start)) { // XID_Start
      if (get_tbl_cat(i) == UCAT_Lu) {
        rune_class[i]=RUNECLASS_VAR;
      } else {
        rune_class[i]=RUNECLASS_ATM;
      }
      // check that XID_Start is included in XID_Continue
      if (!has_tbl_prop(i, UCAT_XID_Continue)) {
        fprintf(stderr, "error: 0x%x is XID_Start but not XID_Continue\n", i);
        return 0;
      }
    } else if (has_tbl_prop(i, UCAT_XID_Continue)) { // XID_Continue
      rune_class[i]=RUNECLASS_IDC;
    } else if (get_tbl_cat(i) == UCAT_No) { // or 'No'
      rune_class[i]=RUNECLASS_IDC;
    } else if (get_tbl_cat(i) == UCAT_Sx_or_Px) { // S* or P*
      rune_class[i]=RUNECLASS_SYM;
    } else {
      rune_class[i]=0; // None
    }
  }

#if 0 // Show all codes
  for (i=0; i<MAX_CODE_POINT; i++) {
    printf("0x%x:%d\n", i, rune_class[i]);
  }
#endif

  /* Compute max code (last one that is 0) */
  for (i=MAX_CODE_POINT-1; i>=0; i--) {
    if (rune_class[i] != 0) break;
  }
  max_rune = i+1;

  return 1;
}

/* (For testing) */
int get_rune_class(int c) {
  if (c >= MAX_CODE_POINT) return 0;
  return rune_class[c];
}

/* --------------------------------------------------------------------------- */
/* Tables */

typedef uint32_t block_t;
#define CLBITS      4                          /* Bits per code class */
#define BLOCK_ELEMS (8*sizeof(block_t)/CLBITS) /* Code classes in a block */
#define VAL_SHIFT   21                         /* Position of ELEM field */

#define DICT_MAX 512
#define RANGES_MAX 4096

block_t dict[DICT_MAX];
int dict_n;
uint32_t ranges[RANGES_MAX];
int ranges_n;

int gen_tables() {
  int i;
  int prev;
  dict_n=0;
  ranges_n=0;
  prev=-1;
  for (i=0; i<max_rune; i+=BLOCK_ELEMS) {    
    int val;
    int j;
    block_t w;
    w = 0;
    for (j=0; j<BLOCK_ELEMS; j++) {
      w |= (block_t)rune_class[i+j] << (j*CLBITS);
    }
    for (j=0; j<dict_n; j++) {
      if (dict[j] == w) { val = j; break; }
    }
    if (j==dict_n) {
      if (dict_n == DICT_MAX) {
        printf("aborted: dict array is full! (%d)\n", dict_n);
        return 0;
      }
      // insert in dict
      val=dict_n; 
      dict_n++; 
      dict[val] = w;
      //printf("dict[%d]=%04X\n", val, w);
    }
    if (prev != val) {
      // encode 'val' in the higher bits
      if (ranges_n == RANGES_MAX) {
        printf("aborted: ranges array is full! (%d)\n", ranges_n);
        return 0;
      }
      ranges[ranges_n++] = (i/BLOCK_ELEMS) | (val << VAL_SHIFT);
      prev=val;
    }
  }
  return 1;
}

int print_tables(char *file) {
  int i;

  FILE *f = fopen(file, "w");
  if (f == NULL) return 0;
  printf("Writing table to %s\n", file);

  fprintf(f, "/* Automatically generated from unicode_gen.pl */\n");
  fprintf(f, "\n");
  fprintf(f, "typedef uint32_t block_t;\n");
  fprintf(f, "#define CLBITS %d\n", (int)CLBITS);
  fprintf(f, "#define BLOCK_ELEMS %d\n", (int)BLOCK_ELEMS);
  fprintf(f, "#define VAL_SHIFT %d\n", (int)VAL_SHIFT);
  fprintf(f, "\n");
  fprintf(f, "/* Size of tables %ld bytes */\n",
          dict_n*sizeof(block_t)+ranges_n*sizeof(uint32_t));

  /* Print dict array */
  fprintf(f, "#define MAX_RUNE 0x%x\n", max_rune);
  fprintf(f, "static const block_t dict[%d]={\n  ", dict_n);
  for (i=0; i<dict_n;i++) {
    if (i!=0 && (i&7)==0) fprintf(f, "\n  ");
    fprintf(f, "0x%llx", (uint64_t)dict[i]);
    if (i!=(dict_n-1)) fprintf(f, ",");
  }
  if ((i&7)!=0) fprintf(f, "\n");
  fprintf(f, "};\n");

  /* Print ranges[] array */
  fprintf(f, "#define RANGES_COUNT %d\n", ranges_n);
  fprintf(f, "static const uint32_t ranges[%d]={\n  ", ranges_n);
  for (i=0; i<ranges_n;i++) {
    if (i!=0 && (i&7)==0) fprintf(f, "\n  ");
    fprintf(f, "0x%x", ranges[i]);
    if (i!=(ranges_n-1)) fprintf(f, ",");
  }
  if ((i&7)!=0) fprintf(f, "\n");
  fprintf(f, "};\n");

  fclose(f);

  return 1;
}


