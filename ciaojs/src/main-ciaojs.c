/*
 *  main-ciaojs.c (for Ciao.js)
 *
 *  Main file for Ciao.js interface to the Ciao engine.
 *
 *  Copyright (C) 2016 Jose F. Morales
 */

#include <emscripten.h>
#include <ciao_prolog.h>

ciao_ctx ctx;

int EMSCRIPTEN_KEEPALIVE ciaojs_init(char *bootfile) {
  const char *boot_path = NULL;

  /* Set engine options and initialize */
  ciao_opts(NULL, 0, NULL, 0, NULL, &boot_path);
  ciao_init(NULL);

  ctx = ciao_ctx_new();

  ciao_load_qfile_s(ctx, bootfile);

  return 0; // OK
}

int EMSCRIPTEN_KEEPALIVE ciaojs_boot() {
  ciao_frame_begin_s(ctx); // TODO: it should not be needed
  ciao_boot(ctx); //firstgoal(ctx, init_atom_check("internals:boot"));
  ciao_frame_end_s(ctx);
  return 0;
}

/* Run a 0-ary predicate */
int EMSCRIPTEN_KEEPALIVE ciaojs_run(char *atom) {
  /* wam->next_insn set to boot code in local_init_each_time */
  /*w->node->global_top = w->global_top;*/     /* Isn't this unnecessary? */
  /*  Fills in worker_entry */
  ciao_bool r;
  ciao_frame_begin_s(ctx);
  r = ciao_commit_call_term_s(ctx, ciao_structure_s(ctx, atom, 0));
  ciao_frame_end_s(ctx);
  return r;
}

