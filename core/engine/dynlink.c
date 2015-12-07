/*
 *  dynlink.c
 *
 *  Load dynamic object code onto a running process
 *
 *  Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP
 *
 *  Authors:
 *    Manuel Carro
 *    Jose F. Morales
 */

#include <ciao/datadefs.h>
#include <ciao/support_macros.h>

#if defined(FOREIGN_FILES)

#if defined(_WIN32) || defined(_WIN64)
#else
#include <dlfcn.h>
#endif

#include <string.h>

#include <ciao/dynlink.h>
#include <ciao/alloc.h>

static void unload_if_present(char *module_name);

static void add_to_loaded_objects(char *module_name,
				  void *handle,
				  void (*end_func)(char *));

/* Length of filenames */

#if !defined(MAX_FILENAME)
# if defined(FILENAME_MAX)
# define MAX_FILENAME FILENAME_MAX
# elif defined(MAXPATHLEN)
# define MAX_FILENAME MAXPATHLEN
# endif
#endif

/* Dynamic linking options.  This is really OS dependant. */
/* And any other object can reference objects in the one we are loading */

#if defined(Solaris)
# if defined(RTLD_PARENT)
#  define LIB_LOADING_TYPE RTLD_LAZY | RTLD_LOCAL | RTLD_PARENT
# else
#  define LIB_LOADING_TYPE RTLD_LAZY | RTLD_LOCAL
# endif
#elif defined(LINUX) || defined(WIN32) || defined(DARWIN)  || defined(BSD) 
# define LIB_LOADING_TYPE RTLD_LAZY | RTLD_LOCAL     
#else /* Default options for new architecture */ 
# define LIB_LOADING_TYPE RTLD_LAZY | RTLD_LOCAL
#endif


/* We maintain a list of objects loaded, in order to free up space if they
  are reloaded. The object is identified by its module_name and accesses to
  by a handle */

typedef struct lobj_ lobj_t;
struct lobj_ {
  lobj_t *next;
  void *handle;
  char *module_name;
  void (*end_func)(char *);                                       /* JFMC */
};

lobj_t *all_loaded_objects = NULL;                        /* Shared */



/* dynlink(+File, +Module): dynamically load the File .so library, and set
  the Prolog-accesible predicates to be loaded inside Module. We delete it
  from the list of loaded objects, if needed, and then we add it. */

CBOOL__PROTO(prolog_dynlink)
{
  char *lib_name;
  char *module_name;
  int strindx;
  char libfunction[STATICMAXATOM];
  void *lib_handle;
  void (*init_func)(char *);
  void (*end_func)(char *);
  char errmsg[1024];

  /* Extract the module name */

  DEREF(X(1), X(1));

  if (!TagIsATM(X(1)))
    USAGE_FAULT("dynlink/2: second argument must be an atom");
  module_name = (char *)GetString(X(1));

   /* Extract the string and *check* it is really a string */

  DEREF(X(0), X(0));
  if (!TagIsATM(X(0))) {
    sprintf(errmsg, "dynlink(_, %s): first argument must be an atom",
	    module_name);
    USAGE_FAULT(errmsg);
  }
  lib_name = (char *)GetString(X(0));

#if defined(DEBUG)
  if (debug_dynlink) fprintf(stderr, "Linking %s\n", lib_name);
#endif


#if defined(USE_ATOM_LEN)
  if ((strindx = GetAtomLen(X(0))) > MAX_FILENAME)
    USAGE_FAULT("dynlink/2: full filename too long");
#else
  if ((strindx = strlen(lib_name)) > MAX_FILENAME)
    USAGE_FAULT("dynlink/2: full filename too long");
#endif

 /* If already loaded, unload it before. */

  unload_if_present(module_name);

#if defined(_WIN32) || defined(_WIN64)
  lib_handle = NULL;
#else
  lib_handle = dlopen(lib_name, LIB_LOADING_TYPE); /* Open the .so library */
#endif
  if (lib_handle == NULL) {
#if defined(_WIN32) || defined(_WIN64)
    sprintf(errmsg, "dynlink(%s, %s): could not load library\n %s)",
	    lib_name, module_name, "unknown error"/*dlerror()*/);
#else
    sprintf(errmsg, "dynlink(%s, %s): could not load library\n %s)",
	    lib_name, module_name, dlerror());
#endif
    USAGE_FAULT(errmsg);
  }


  /* Get the address of init_func */ 

  strcpy(libfunction, module_name);                 /* Construct the name */
  strcat(libfunction, "_init");               /* of the init function */
#if defined(_WIN32) || defined(_WIN64)
  init_func = NULL;
#else
  init_func = (void (*)(char *))dlsym(lib_handle, libfunction);
#endif
  if (init_func == NULL){
#if defined(_WIN32) || defined(_WIN64)
#else
    dlclose(lib_handle);
#endif
    sprintf(errmsg, "dynlink(%s, %s): could not find initialization function",
	    lib_name, module_name);
    USAGE_FAULT(errmsg);
  }

  /* JFMC: Get the address of the end_func */

  strcpy(libfunction, module_name);             /* Construct the name */
  strcat(libfunction, "_end");                 /* of the end function */
#if defined(_WIN32) || defined(_WIN64)
  end_func = NULL;
#else
  end_func = (void (*)(char *))dlsym(lib_handle, libfunction);     
#endif
  if (end_func == NULL) {
#if defined(_WIN32) || defined(_WIN64)
#else
    dlclose(lib_handle);    
#endif
    USAGE_FAULT("dynlink/2: could not find end function");
  }

  /* It is already in our address space. Add it to our list. */

  add_to_loaded_objects(module_name, lib_handle, end_func);

  /* Call the init function with the name of the module */

  (*init_func)(module_name);

  return TRUE;
}


/* dynunlink(+File): dynamically unload the File .so library */ /* JFMC */

CBOOL__PROTO(prolog_dynunlink)
{
  char *module_name;

   /* Extract the string and *check* it is really a string */

  DEREF(X(0), X(0));
  if (!TagIsATM(X(0)))
    USAGE_FAULT("dynunlink/1: first argument must be an atom");
  module_name = (char *)GetString(X(0));

#if defined(DEBUG)
  if (debug_dynlink) fprintf(stderr, "Unlinking %s\n", module_name);
#endif

 /* If loaded, unload it. */

  unload_if_present(module_name);

  return TRUE;
}

/* Find and remove from the process space the object associated to
   module_name */

void unload_if_present(char *module_name)
{
  lobj_t *previous_object;
  lobj_t *to_remove = NULL;
  bool_t found = FALSE;

#if defined(DEBUG)
  if (debug_dynlink) fprintf(stderr, "Unloading %s\n",module_name);
#endif

  if (all_loaded_objects){
    if (!strcmp(all_loaded_objects->module_name,module_name)){/* First in list? */
      to_remove = all_loaded_objects;
      all_loaded_objects = all_loaded_objects->next;
    } else {                            /* Second or third, or fourth...  */
      to_remove = all_loaded_objects->next;
      previous_object = all_loaded_objects;
      while (!found && to_remove)
        if (!strcmp(to_remove->module_name, module_name)) {
          previous_object->next = to_remove->next;
          found = TRUE;
        } else {
          previous_object = to_remove;
          to_remove = to_remove->next;
        }
    }

    if (to_remove) {
#if defined(DEBUG)
      if (debug_dynlink) fprintf(stderr, "Closing handle for %s\n",module_name);
#endif
      (*to_remove->end_func)(to_remove->module_name);     /* JFMC / MCL */
#if defined(_WIN32) || defined(_WIN64)
#else
      dlclose(to_remove->handle);
#endif
      checkdealloc_ARRAY(char,
			 strlen(to_remove->module_name)+1,
			 to_remove->module_name);
      checkdealloc_TYPE(lobj_t, to_remove);
    }
  }
#if defined(DEBUG)
  if (debug_dynlink) fprintf(stderr, "all_loaded_objects = 0x%p\n", all_loaded_objects);
#endif
}


/* Associate an object with its module_name */

void add_to_loaded_objects(char *module_name,
			   void *handle,
			   void (*end_func)(char *))
{
  lobj_t *new_object = checkalloc_TYPE(lobj_t);

#if defined(DEBUG)
  if (debug_dynlink) fprintf(stderr, "Adding %s\n",module_name);
#endif
  new_object->module_name = checkalloc_ARRAY(char, strlen(module_name)+1);
  strcpy(new_object->module_name, module_name);
  new_object->handle = handle;
  new_object->end_func = end_func; /* JFMC: will be called just before
				      dlclose */
  new_object->next = (lobj_t *)all_loaded_objects;
  all_loaded_objects = new_object;
#if defined(DEBUG)
  if (debug_dynlink) fprintf(stderr, "all_loaded_objects = 0x%p\n", all_loaded_objects);
#endif
}
#else 
CBOOL__PROTO(prolog_dynlink) {
  ENG_TTYPRINTF("{ERROR: dynlink: emulator not created with foreign files interface}\n");
  ENG_TTYPRINTF("{Please recompile with foreing files option turned on}\n");
  return FALSE;
}

CBOOL__PROTO(prolog_dynunlink) {
  ENG_TTYPRINTF("{ERROR: dynunlink: emulator not created with foreign files interface}\n");
  ENG_TTYPRINTF("{Please recompile with foreing files option turned on}\n");
  return FALSE;
}
#endif
