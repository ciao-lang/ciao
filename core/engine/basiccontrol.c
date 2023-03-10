/*
 *  basiccontrol.c
 *
 *  Emulator kernel.
 *
 *  Copyright (C) 1996-2002 UPM-CLIP
 *  Copyright (C) 2020 The Ciao Development Team
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#include <ciao/os_signal.h>
#include <ciao/eng.h>
#include <ciao/basiccontrol.h>

#include <ciao/attributes.h>
#include <ciao/eng_registry.h>
#include <ciao/io_basic.h>
#include <ciao/eng_interrupt.h>
#include <ciao/eng_start.h>
#include <ciao/rt_exp.h>
#include <ciao/runtime_control.h>
#include <ciao/dynamic_rt.h>
#include <ciao/eng_gc.h>
#include <ciao/timing.h>
#include <ciao/eng_profile.h>
#include <ciao/tabling.h>

#include <ciao/bc_aux.h>
#include "wamloop.c"
