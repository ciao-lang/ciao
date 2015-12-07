# -*- mode: Makefile; -*-
# ---------------------------------------------------------------------------
# A generic Makefile for engine builds (called from build_engine.sh)
#
# External parameters:
#   BLD_CDIR
#   BLD_OBJDIR
#   ENG_CFG_MK
#   ENG_DEPLIBS, ENG_ADDOBJ
#   CONFIG_HFILE
# ---------------------------------------------------------------------------

# Load the engine config_mk file
ifndef ENG_CFG_MK
$(error "INTERNAL ERROR: ENG_CFG_MK is not set")
endif
include $(ENG_CFG_MK)

# ---------------------------------------------------------------------------

VPATH=$(BLD_CDIR):$(BLD_OBJDIR)

INCLUDE =

# Engine info (ENG_CFILES, ENG_HFILES, etc.)
include $(BLD_CDIR)/eng_info_mk

# ---------------------------------------------------------------------------

##MAKEDEPEND = $(CC) -M $(CFLAGS) $(INCLUDE) -o $*.d $<
MAKEDEPEND = $(CC) -MM $(CFLAGS) $(INCLUDE) -o $*.d $<

OBJFILES = $(ENG_CFILES:.c=.o)

# Note: ENG_STUBMAIN should not have any dependency with any header!
ENG_STUBMAIN_OBJ = $(ENG_STUBMAIN:.c=.o)

# ---------------------------------------------------------------------------
# Using advanced autodependency generation (like automake)
# (see http://mad-scientist.net/make/autodep.html)

%.o : %.c
	$(MAKEDEPEND); \
	cp $*.d $*.P; \
	sed -e 's/#.*//' -e 's/^[^:]*: *//' -e 's/ *\\$$//' \
	    -e '/^$$/ d' -e 's/$$/ :/' < $*.d >> $*.P; \
	rm -f $*.d
	# $(CC) -S -emit-llvm $(CFLAGS) $(INCLUDE) $<
	# $(CC) -c $(CFLAGS) -Rpass=.* -Rpass-missed=.* -Rpass-analysis=.* $(INCLUDE) -o $@ $< > $*.cclog 2>&1
	$(CC) -c $(CFLAGS) $(INCLUDE) -o $@ $<

-include $(addprefix $(BLD_OBJDIR), $(ENG_CFILES:.c=.P))

# ---------------------------------------------------------------------------
# Rules for building ENG

# Name of the executable, shared, and static libraries
ENG_EXEC = $(ENG_NAME)$(EXECSUFFIX)
ENG_SO := lib$(ENG_NAME)$(SOSUFFIX)
ENG_A := lib$(ENG_NAME).a

# Fixes for Mac OS X
ifeq ($(shell uname -s),Darwin)
    # Make rpath work
    ENG_SO_INSTALL_NAME := -install_name '@rpath/'$(ENG_SO)
    # Make uninitialized global variables in .a work (another option
    # is using -fno-common during compilation, but it somehow produced
    # slower executables)
    RANLIB_OPTS := -c
endif

.PHONY: engexec engexec_0 engexec_1 englib

# Target for engine as an executable
engexec: $(ENG_EXEC) ;

# Target for engine as libs (shared and static)
englib: $(ENG_SO) $(ENG_A) ;

# Engine as an executable using ENG_STUBMAIN
$(ENG_EXEC): engexec_$(ENG_STUBMAIN_DYNAMIC)

# (statically linked against the engine library)
engexec_0: $(ENG_STUBMAIN_OBJ) $(ENG_ADDOBJ) $(ENG_A) 
	$(LD) $(LDFLAGS) \
	      $(ENG_STUBMAIN_OBJ) $(ENG_ADDOBJ) \
	      $(ENG_A) \
	      $(ENG_DEPLIBS) \
	      -o $(ENG_EXEC)

# (dynamically linked against the engine library)
engexec_1: $(ENG_STUBMAIN_OBJ) $(ENG_ADDOBJ) $(ENG_SO)
	$(LD) $(LIBENG_LDFLAGS) \
	      $(ENG_STUBMAIN_OBJ) $(ENG_ADDOBJ) \
	      $(ENG_DEPLIBS) \
	      -L./ -l$(ENG_NAME) \
	      -o $(ENG_EXEC)

# Engine as a shared library
$(ENG_SO): $(OBJFILES)
	$(LD) $(LDSHARED) $(OBJFILES) \
	      $(ENG_DEPLIBS) \
	      -o $(ENG_SO) $(ENG_SO_INSTALL_NAME)

# Engine as a static library
# TODO: how deal with ENG_DEPLIBS?
$(ENG_A): $(OBJFILES)
	@ar -c -r $(ENG_A) $(OBJFILES)
	@ranlib $(RANLIB_OPTS) $(ENG_A)

# TODO: partial-link all OBJFILES in a single .o (useful?)
#       where LDCOMBINE=-r
# $(LD) $(LDCOMBINE) $(OBJFILES) -o $(ENG_NAME).o

# ---------------------------------------------------------------------------
# Configuration (a simpler version of ENG which checks the system)

# NOTE: To get a correct MallocBase the configuration executable
#   follows the same linkage than the engine (ENG_STUBMAIN_DYNAMIC), in an
#   attempt to get a similar map of addresses.

CONFIG_BASE = configure
CONFIG_STUBMAIN = configure_main.c
CONFIG_CFILES = configure.c own_mmap.c win32_mman.c

CONFIG_STUBMAIN_OBJ = $(CONFIG_STUBMAIN:.c=.o)

# Name of the executable and shared library
CONFIG_EXEC = $(CONFIG_BASE)$(EXECSUFFIX)
CONFIG_SO = lib$(CONFIG_BASE)$(SOSUFFIX)

CONFIG_OBJFILES = $(CONFIG_CFILES:.c=.o)

LIBCONFIG_CFLAGS = $(CFLAGS)
LIBCONFIG_LDFLAGS = $(LDFLAGS)

.PHONY: configexec_0 configexec_1

# Executable
$(CONFIG_EXEC): configexec_$(ENG_STUBMAIN_DYNAMIC) ;

configexec_0: $(CONFIG_STUBMAIN_OBJ) $(CONFIG_OBJFILES)
	$(LD) $(LDFLAGS) \
	      $(CONFIG_STUBMAIN_OBJ) \
	      $(CONFIG_OBJFILES) \
	      -o $(CONFIG_EXEC)

configexec_1: $(CONFIG_STUBMAIN_OBJ) $(CONFIG_SO)
	$(LD) $(LIBCONFIG_LDFLAGS) \
	      $(CONFIG_STUBMAIN_OBJ) \
	      -L./ -l$(CONFIG_BASE) \
	      -o $(CONFIG_EXEC)

# Shared library
$(CONFIG_SO): $(CONFIG_OBJFILES)
	$(LD) $(LDSHARED) $(CONFIG_OBJFILES) \
	      -o $(CONFIG_SO)

# ---------------------------------------------------------------------------
# Generation of configure.h

.PHONY: engconf

engconf: $(CONFIG_EXEC)
	@./$(CONFIG_EXEC) "$(CFLAGS)" > $(CONFIG_HFILE)

# ---------------------------------------------------------------------------

.PHONY: fix_size_exec

FIX_SIZE_EXEC := fix_size$(EXECSUFFIX)

fix_size_exec: fix_size.c
	$(CC) -o $(FIX_SIZE_EXEC) $<

