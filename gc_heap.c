/* gc_heap.h -- heap packing, run-time image generation    */
/* Copyright (c) 2016 Chris Walsh.  All rights reserved.   */
/* BSD-style license: http://synthcode.com/license.txt     */

#include "chibi/gc_heap.h"

#if SEXP_USE_IMAGE_LOADING

#define ERR_STR_SIZE 256
char gc_heap_err_str[ERR_STR_SIZE];


static sexp_uint_t sexp_gc_allocated_bytes (sexp ctx, sexp *types, size_t types_cnt, sexp x) {
  sexp_uint_t res = 0;
  if (!sexp_pointerp(x) || (sexp_pointer_tag(x) >= types_cnt)) {
    res = 1;
  } else {
    res = sexp_type_size_of_object(types[sexp_pointer_tag(x)], x) + SEXP_GC_PAD;
  }
  return sexp_heap_align(res);
}


sexp sexp_gc_heap_walk(sexp ctx,
                       sexp_heap h,  /* normally set to sexp_context_heap(ctx) */
                       sexp *t,      /* normally set to sexp_context_types(ctx) */
                       size_t t_cnt, /* normally set to sexp_context_num_types(ctx) */
                       void *user,
                       sexp (*heap_callback)(sexp ctx, sexp_heap h, void *user),
                       sexp (*free_callback)(sexp ctx, sexp_free_list f, void *user),
                       sexp (*sexp_callback)(sexp ctx, sexp s, void *user))
{
  sexp res = SEXP_FALSE;

  size_t size = 0;
  while (h) {
    sexp p = sexp_heap_first_block(h);
    sexp_free_list q = h->free_list;
    sexp end = sexp_heap_end(h);

    while (p < end) {
      /* find the preceding and succeeding free list pointers */
      sexp_free_list r = q->next;
      while (r && ((unsigned char*)r < (unsigned char*)p)) {
        q = r;
        r = r->next;
      }
      
      if ( (unsigned char*)r == (unsigned char*)p ) {
        if (free_callback && (res = free_callback(ctx, r, user)) != SEXP_TRUE) {
          return res; }
        size = r ? r->size : 0;
      } else {
        if (sexp_callback && (res = sexp_callback(ctx, p, user)) != SEXP_TRUE) {
          return res; }
        size = sexp_gc_allocated_bytes(ctx, t, t_cnt, p);
        if (size == 0) {
          strcpy(gc_heap_err_str, "Heap element with a zero size detected");
          goto done;
        }
      }
      p = (sexp)(((unsigned char*)p) + size);
    }
    
    if (heap_callback && (res = heap_callback(ctx, h, user)) != SEXP_TRUE) {
      return res; }
    h = h->next;
  }
  res = SEXP_TRUE;
done:
  if (res != SEXP_TRUE) res = sexp_user_exception(ctx, NULL, gc_heap_err_str, NULL);
  return res;
}


struct sexp_remap {
  sexp srcp;
  sexp dstp;
};

struct sexp_remap_state {
  size_t index, heaps_count, sexps_count, sexps_size;
  sexp p, end, ctx_src, ctx_dst;
  sexp_heap heap;
  int mode;
  struct sexp_remap *remap;
};


static sexp heap_callback_count(sexp ctx, sexp_heap h, void *user) {
  struct sexp_remap_state* state = user;
  state->heaps_count += 1;
  return SEXP_TRUE;
}

static sexp sexp_callback_count(sexp ctx, sexp s, void *user) {
  struct sexp_remap_state* state = user;
  size_t size = sexp_gc_allocated_bytes(ctx, sexp_context_types(ctx),
                                        sexp_context_num_types(ctx), s);
  state->sexps_count += 1;
  state->sexps_size  += size;
  return SEXP_TRUE;
}

static sexp heap_callback_remap(sexp ctx, sexp_heap h, void *user) {
  return SEXP_NULL;
}

static sexp sexp_callback_remap(sexp ctx, sexp s, void *user) {
  struct sexp_remap_state* state = user;
  size_t size = sexp_gc_allocated_bytes(ctx, sexp_context_types(ctx),
                                        sexp_context_num_types(ctx), s);
  if (state->p >= state->end) {
    snprintf(gc_heap_err_str, ERR_STR_SIZE, "callback_remap i=%zu p>end internal error", state->index);
    return SEXP_FALSE; }
  memcpy(state->p, s, size);
  
  state->remap[state->index].srcp = s;
  state->remap[state->index].dstp = state->p;
  if (ctx == s) state->ctx_dst = state->p;
  
  state->p = (sexp)(((unsigned char*)state->p) + size);
  state->index += 1;

  return SEXP_TRUE;
}



/* Return a destination (remapped) pointer for a given source pointer */
static sexp sexp_gc_heap_pack_src_to_dst(void* adata, sexp srcp) {

  struct sexp_remap_state* state = adata;
  sexp_sint_t imin = 0;
  sexp_sint_t imax = state->sexps_count - 1;

  while (imin <= imax) {
    sexp_sint_t imid = ((imax - imin) / 2) + imin;
    sexp midp = state->remap[imid].srcp;
    if (midp == srcp) {
      return state->remap[imid].dstp;
    } else if (midp < srcp) {
      imin = imid + 1;
    } else {
      imax = imid - 1;
    }
  }
  strcpy(gc_heap_err_str, "Source SEXP not found in src->dst mapping");
  return SEXP_FALSE;
}


static sexp sexp_adjust_fields(sexp dstp, sexp* types, sexp (* adjust_fn)(void *, sexp), void *adata) {
  sexp_tag_t tag           = sexp_pointer_tag(dstp);
  sexp       type_spec     = types[tag];
  size_t     type_sexp_cnt = sexp_type_num_slots_of_object(type_spec, dstp);
  sexp*      vec           = (sexp*)((unsigned char*)dstp + sexp_type_field_base(type_spec));
  int        i;
  
  for (i = 0; i < type_sexp_cnt; i++) {
    sexp src = vec[i];
    sexp dst = src;
    if (src && sexp_pointerp(src)) {
      dst = adjust_fn(adata, src);
      if (!sexp_pointerp(dst)) { 
        size_t sz = strlen(gc_heap_err_str);
        snprintf(gc_heap_err_str + sz, ERR_STR_SIZE - sz, " from adjust fields, tag=%u i=%d", tag, i);
        return SEXP_FALSE; }
    }
    vec[i] = dst;
  }
  return SEXP_TRUE;
}


static sexp sexp_adjust_bytecode(sexp dstp, sexp (*adjust_fn)(void *, sexp), void *adata) {
  sexp res = SEXP_FALSE;
  sexp   src, dst;
  sexp*  vec;
  int    i;
  
  for (i=0; i < sexp_bytecode_length(dstp); ) {
    switch (sexp_bytecode_data(dstp)[i++]) {
    case SEXP_OP_FCALL0:      case SEXP_OP_FCALL1:
    case SEXP_OP_FCALL2:      case SEXP_OP_FCALL3:
    case SEXP_OP_FCALL4:      case SEXP_OP_CALL:
    case SEXP_OP_TAIL_CALL:   case SEXP_OP_PUSH:
    case SEXP_OP_GLOBAL_REF:  case SEXP_OP_GLOBAL_KNOWN_REF:
#if SEXP_USE_GREEN_THREADS
    case SEXP_OP_PARAMETER_REF:
#endif
#if SEXP_USE_EXTENDED_FCALL
    case SEXP_OP_FCALLN:
#endif
      vec = (sexp*)(&(sexp_bytecode_data(dstp)[i]));
      src = vec[0];
      if (src && sexp_pointerp(src)) {
        dst = adjust_fn(adata, src);
        if (!sexp_pointerp(dst)) {
          size_t sz = strlen(gc_heap_err_str);
          snprintf(gc_heap_err_str + sz, ERR_STR_SIZE - sz, " from adjust bytecode, FCALLN");
          goto done; }
        vec[0] = dst;
      }
      /* ... FALLTHROUGH ... */
    case SEXP_OP_JUMP:        case SEXP_OP_JUMP_UNLESS:
    case SEXP_OP_STACK_REF:   case SEXP_OP_CLOSURE_REF:
    case SEXP_OP_LOCAL_REF:   case SEXP_OP_LOCAL_SET:
    case SEXP_OP_TYPEP:
#if SEXP_USE_RESERVE_OPCODE
    case SEXP_OP_RESERVE:
#endif
      i += sizeof(sexp); break;
    case SEXP_OP_MAKE: case SEXP_OP_SLOT_REF: case SEXP_OP_SLOT_SET:
      i += 2*sizeof(sexp); break;
    case SEXP_OP_MAKE_PROCEDURE:
      vec = (sexp*)(&(sexp_bytecode_data(dstp)[i]));
      src = vec[2];
      if (src && sexp_pointerp(src)) {
        dst = adjust_fn(adata, src);
        if (!sexp_pointerp(dst)) { 
          size_t sz = strlen(gc_heap_err_str);
          snprintf(gc_heap_err_str + sz, ERR_STR_SIZE - sz, " from adjust bytecode, PROCEDURE");
          goto done; }
        vec[2] = dst;
      }
      i += 3*sizeof(sexp); break;
    }
  }
  res = SEXP_TRUE;
done:
  return res;
}

static sexp sexp_gc_heap_pack_adjust(sexp dstp, sexp* types, struct sexp_remap_state* state) {
  sexp res = NULL;
  /* Adjust internal types which contain fields of sexp pointer(s)
     within in the heap */
  if ((res = sexp_adjust_fields(dstp, types, sexp_gc_heap_pack_src_to_dst, state)) != SEXP_TRUE) {
    goto done; }
    
  /* Other adjustments - context heap pointer, bytecode pointers */
  if (sexp_contextp(dstp)) {
    sexp_context_heap(dstp) = state->heap;
  } else if (sexp_bytecodep(dstp)) {
    if ((res = sexp_adjust_bytecode(dstp, sexp_gc_heap_pack_src_to_dst, state)) != SEXP_TRUE) {
      goto done; }
  }
  res = SEXP_TRUE;
done:
  return res;
}


static sexp_heap sexp_gc_packed_heap_make(size_t packed_size, size_t free_size) {
  if (free_size > 0 && free_size < 2*sexp_free_chunk_size) {
    free_size = 2*sexp_free_chunk_size;
  }
  free_size = sexp_heap_align(free_size);
  size_t req_size = packed_size + free_size + sexp_free_chunk_size + 128;
  sexp_heap heap = sexp_make_heap(sexp_heap_align(req_size), 0, 0);
  if (!heap) {
    strcpy(gc_heap_err_str, "Could not allocate memory for heap");
    return NULL;
  }
  sexp base = sexp_heap_first_block(heap);
  size_t pad = (unsigned char *)base - (unsigned char *)heap->data;
  heap->size = packed_size + free_size + pad;
  heap->free_list->size = 0;
  if (free_size == 0) {
    heap->free_list->next = NULL;
  } else {
    heap->free_list->next = (sexp_free_list)((unsigned char *)base + packed_size);
    heap->free_list->next->next = NULL;
    heap->free_list->next->size = free_size;
  }
  return heap;
}

static int heaps_compar(const void* v1, const void* v2) {
  sexp_heap h1 = *((sexp_heap*)v1);
  sexp_heap h2 = *((sexp_heap*)v2);
  return 
    (h1 < h2) ? -1 : 
    (h1 > h2) ?  1 : 0;
}

/* Pack the heap.  Return a new context with a unified, packed heap.  No change to original context. */
sexp sexp_gc_heap_pack(sexp ctx_src, sexp_uint_t heap_free_size) {

  sexp res = NULL;
  sexp_gc(ctx_src, NULL);
  sexp_heap* heaps = NULL;
  int i = 0;

  /* 1.  Collect statistics - sexp count, size, heap count */
  
  struct sexp_remap_state state;
  memset(&state, 0, sizeof(struct sexp_remap_state));
  state.ctx_src = ctx_src;
  if ((res = sexp_gc_heap_walk(ctx_src, sexp_context_heap(ctx_src), 
                               sexp_context_types(ctx_src), sexp_context_num_types(ctx_src),
                               &state, heap_callback_count, NULL, sexp_callback_count)) != SEXP_TRUE) {
    goto done; }

  /* 2.  Make a new heap of the correct size to hold the sexps from the old heap. */

  state.heap = sexp_gc_packed_heap_make(state.sexps_size, heap_free_size);
  if (!state.heap) {
    res = sexp_global(ctx_src, SEXP_G_OOM_ERROR);
    goto done; }

  /* 3.  Create a list of heaps sorted by increasing memory address, for srcp search lookup */

  heaps = malloc(sizeof(sexp_heap) * state.heaps_count);
  if (!heaps) {
    res = sexp_global(ctx_src, SEXP_G_OOM_ERROR);
    goto done; }
  sexp_heap h = sexp_context_heap(ctx_src);
  for (i = 0; h; i++, h=h->next) { heaps[i] = h; }
  qsort(heaps, state.heaps_count, sizeof(sexp_heap), heaps_compar);

  /* 4.  Pack the sexps into the new heap */

  state.p     = sexp_heap_first_block(state.heap);
  state.end   = sexp_heap_end(state.heap);
  state.index = 0;
  state.remap = malloc(sizeof(struct sexp_remap) * state.sexps_count);
  if (!state.remap) {
    res = sexp_global(ctx_src, SEXP_G_OOM_ERROR);
    goto done; }
  
  for (i = 0; i < state.heaps_count; i++) {
    res = sexp_gc_heap_walk(ctx_src, heaps[i],
                            sexp_context_types(ctx_src), sexp_context_num_types(ctx_src),
                            &state, heap_callback_remap, NULL, sexp_callback_remap);
    if (!(res == SEXP_TRUE || res == SEXP_NULL)) { 
      size_t sz = strlen(gc_heap_err_str);
      snprintf(gc_heap_err_str + sz, ERR_STR_SIZE - sz, "; remap heap %d %p walk heap_pack", i, heaps[i]);
      goto done; }
  }

  /* 5.  Adjust sexp pointers to new locations inside the new heap */

  sexp* types = sexp_context_types(state.ctx_src);
  int idx;
  for (idx = 0; idx < state.sexps_count; idx++) {
    sexp dstp = state.remap[idx].dstp;
    res = sexp_gc_heap_pack_adjust(dstp, types, &state);
    if (res != SEXP_TRUE) { 
      size_t sz = strlen(gc_heap_err_str);
      snprintf(gc_heap_err_str + sz, ERR_STR_SIZE - sz, "; src->dst idx=%d heap_pack", idx);
      goto done; }
  }

  res = SEXP_TRUE;

done:
  /* 6. Clean up. */

  if (state.heap && res != SEXP_TRUE) { sexp_free_heap(state.heap); }
  if (state.remap) { free(state.remap); }
  if (heaps) { free(heaps); }

  return (res == SEXP_TRUE) ? state.ctx_dst : res;
}


#define SEXP_IMAGE_MAGIC "\a\achibi\n\0"
#define SEXP_IMAGE_MAJOR_VERSION 1
#define SEXP_IMAGE_MINOR_VERSION 1

struct sexp_image_header_t {
  char magic[8];
  short major, minor;
  sexp_abi_identifier_t abi;
  sexp_uint_t size;
  sexp base;
  sexp context;
};


sexp sexp_save_image (sexp ctx_in, const char* filename) {
  sexp_heap heap = NULL;
  sexp res = NULL;
  FILE *fp = fopen(filename, "wb");
  if (!fp) {
    snprintf(gc_heap_err_str, ERR_STR_SIZE, "Could not open image file for writing: %s", filename);
    goto done;
  }
  
  /* Save ONLY packed, active SEXPs.  No free list structures or padding. */
  sexp ctx_out = sexp_gc_heap_pack(ctx_in, 0);
  if (!ctx_out || !sexp_contextp(ctx_out)) {
    goto done;
  }
  heap = sexp_context_heap(ctx_out);
  sexp   base = sexp_heap_first_block(heap);
  size_t pad  = (size_t)((unsigned char *)base - (unsigned char *)heap->data);
  size_t size = heap->size - pad;
  
  struct sexp_image_header_t header;
  memcpy(&header.magic, SEXP_IMAGE_MAGIC, sizeof(header.magic));
  memcpy(&header.abi, SEXP_ABI_IDENTIFIER, sizeof(header.abi));
  header.major   = SEXP_IMAGE_MAJOR_VERSION;
  header.minor   = SEXP_IMAGE_MINOR_VERSION;
  header.size    = size;
  header.base    = base;
  header.context = ctx_out;

  if (! (fwrite(&header, sizeof(header), 1, fp) == 1 &&
         fwrite(base, size, 1, fp) == 1)) {
    snprintf(gc_heap_err_str, ERR_STR_SIZE, "Error writing image file: %s", filename);
    goto done;
  }
  
  res = SEXP_TRUE;
done:
  if (fp) fclose(fp);
  if (heap) sexp_free_heap(heap);
  if (res != SEXP_TRUE) res = sexp_user_exception(ctx_in, NULL, gc_heap_err_str, NULL);
  return res;
}



#if SEXP_USE_DL

#ifdef __APPLE__
#define SEXP_RTLD_DEFAULT RTLD_SELF
#else
#define SEXP_RTLD_DEFAULT RTLD_DEFAULT
#endif

struct load_image_state {
  sexp_sint_t offset;
  sexp_heap heap;
  sexp *types;
  size_t types_cnt;
};

/* Return a destination (remapped) pointer for a given source pointer */
static sexp load_image_src_to_dst(void* adata, sexp srcp) {
  struct load_image_state* state = adata;
  return (sexp)((unsigned char *)srcp + state->offset);
}


static sexp load_image_callback_p1 (sexp ctx, sexp p, void *user) {
  sexp res = NULL;
  struct load_image_state* state = user;

  if ((res = sexp_adjust_fields(p, state->types, load_image_src_to_dst, state)) != SEXP_TRUE) {
    goto done; }
    
  if (sexp_contextp(p)) {
#if SEXP_USE_GREEN_THREADS
    sexp_context_ip(p) += state->offset;
#endif
    sexp_context_last_fp(p) += state->offset;
    sexp_stack_top(sexp_context_stack(p)) = 0;
    sexp_context_saves(p) = NULL;
    sexp_context_heap(p) = state->heap;
  
  } else if (sexp_bytecodep(p)) {
    if ((res = sexp_adjust_bytecode(p, load_image_src_to_dst, state)) != SEXP_TRUE) {
      goto done; }
    
  } else if (sexp_portp(p) && sexp_port_stream(p)) {
    sexp_port_stream(p) = 0;
    sexp_port_openp(p) = 0;
    sexp_freep(p) = 0;
    
  } else if (sexp_dlp(p)) {
    sexp_dl_handle(p) = NULL;

  }
  res = SEXP_TRUE;
done:
  return res;
}

static void* load_image_fn(sexp ctx, sexp dl, sexp name) {
  sexp ls;
  void *fn = NULL;
  char *file_name, *rel_name=NULL, *new_file_name;
  char *handle_name = "<static>";
  char *symbol_name = sexp_string_data(name);
  if (dl && sexp_dlp(dl)) {
    if (!sexp_dl_handle(dl)) {
      /* try exact file, then the search path */
      file_name = sexp_string_data(sexp_dl_file(dl));
      sexp_dl_handle(dl) = dlopen(file_name, RTLD_LAZY);
      if (!sexp_dl_handle(dl)) {
        for (ls = sexp_global(ctx, SEXP_G_MODULE_PATH); sexp_pairp(ls); ls=sexp_cdr(ls)) {
          if (strstr(file_name, sexp_string_data(sexp_car(ls))) == file_name) {
            rel_name = file_name + sexp_string_size(sexp_car(ls));
            while (*rel_name == '/')
              ++rel_name;
            new_file_name = sexp_find_module_file_raw(ctx, rel_name);
            if (new_file_name) {
              sexp_dl_handle(dl) = dlopen(new_file_name, RTLD_LAZY);
              free(new_file_name);
              if (sexp_dl_handle(dl))
                break;
            }
          }
        }
        if (!sexp_dl_handle(dl)) {
          handle_name = sexp_string_data(sexp_dl_file(dl));
          snprintf(gc_heap_err_str, ERR_STR_SIZE, "dlopen failure: %s",
                   handle_name);
          return NULL;
        }
      }
    }
    fn = dlsym(sexp_dl_handle(dl), symbol_name);
  } else {
    fn = dlsym(SEXP_RTLD_DEFAULT, symbol_name);
  }
  if (!fn) {
    snprintf(gc_heap_err_str, ERR_STR_SIZE,
             "dynamic function lookup failure: %s %s",
             handle_name, symbol_name);
  }
  return fn;
}

static sexp load_image_callback_p2 (sexp ctx, sexp dstp, void *user) {
  sexp res = NULL;
  sexp name = NULL;
  void *fn = NULL;

  if (sexp_opcodep(dstp) && sexp_opcode_func(dstp)) {
    if (sexp_opcode_data2(dstp) && sexp_stringp(sexp_opcode_data2(dstp))) {
      name = sexp_opcode_data2(dstp);
    } else {
      name = sexp_opcode_name(dstp);
    }
    if (!name) {
      snprintf(gc_heap_err_str, ERR_STR_SIZE, "opcode func field missing function name");
      return SEXP_FALSE;
    }
    
    fn = load_image_fn(ctx, sexp_opcode_dl(dstp), name);
    if (!fn) {
      return SEXP_FALSE;
    }
    sexp_opcode_func(dstp) = fn;
  
  } else if (sexp_typep(dstp) && sexp_type_finalize(dstp)) {
    name = sexp_type_finalize_name(dstp);
    if (!name) {
      snprintf(gc_heap_err_str, ERR_STR_SIZE, "type finalize field missing function name");
      return SEXP_FALSE;
    }
    fn = load_image_fn(ctx, sexp_type_dl(dstp), name);
    if (!fn) {
      return SEXP_FALSE;
    }
    sexp_type_finalize(dstp) = fn;
  }
  res = SEXP_TRUE;
  return res;
}


int load_image_header(FILE *fp, struct sexp_image_header_t* header) {
  if (!fp || !header) { return 0; }
  
  if (fread(header, sizeof(struct sexp_image_header_t), 1, fp) != 1) {
    strcpy(gc_heap_err_str, "couldn't read image header");
    return 0;
  }
  if (memcmp(header->magic, SEXP_IMAGE_MAGIC, sizeof(header->magic)) != 0) {
    snprintf(gc_heap_err_str, ERR_STR_SIZE, "invalid image file magic %s\n", header->magic);
    return 0;
  } else if (header->major != SEXP_IMAGE_MAJOR_VERSION
             || header->major < SEXP_IMAGE_MINOR_VERSION) {
    snprintf(gc_heap_err_str, ERR_STR_SIZE, "unsupported image version: %d.%d\n",
             header->major, header->minor);
    return 0;
  } else if (!sexp_abi_compatible(NULL, header->abi, SEXP_ABI_IDENTIFIER)) {
    snprintf(gc_heap_err_str, ERR_STR_SIZE, "unsupported ABI: %s (expected %s)\n",
             header->abi, SEXP_ABI_IDENTIFIER);
    return 0;
  }
  return 1;
}

char* sexp_load_image_err() {
  gc_heap_err_str[ERR_STR_SIZE-1] = 0;
  return gc_heap_err_str;
}

static const char* all_paths[] = {sexp_default_module_path, sexp_default_user_module_path};

sexp sexp_load_image (const char* filename, off_t offset, sexp_uint_t heap_free_size, sexp_uint_t heap_max_size) {
  struct load_image_state state;
  struct sexp_image_header_t header;
  const char *mod_path, *colon, *end;
  char path[512];
  FILE *fp;
  int i;
  sexp res = NULL, ctx = NULL, base, *ctx_globals, *ctx_types;

  gc_heap_err_str[0] = 0;

  memset(&state, 0, sizeof(struct load_image_state));

  fp = fopen(filename, "rb");
  /* fallback to the default search path (can't use sexp_find_module_file */
  /* since there's no context yet) */
  for (i=0; !fp && i<sizeof(all_paths)/sizeof(all_paths[0]); ++i) {
    for (mod_path=all_paths[i]; *mod_path; mod_path=colon+1) {
      colon = strchr(mod_path, ':');
      end = colon ? colon : mod_path + strlen(mod_path);
      strncpy(path, mod_path, end-mod_path);
      if (end[-1] != '/') path[end-mod_path] = '/';
      strcpy(path + (end-mod_path) + (end[-1] == '/' ? 0 : 1), filename);
      fp = fopen(path, "rb");
      if (fp || !colon) break;
    }
  }
  if (!fp) {
    snprintf(gc_heap_err_str, ERR_STR_SIZE, "couldn't open image file for reading: %s\n", filename);
    goto done;
  }
  if (offset > 0 && fseek(fp, offset, SEEK_SET) < 0) {
    snprintf(gc_heap_err_str, ERR_STR_SIZE, "couldn't seek to image offset: %s -> "PRIoff": %s\n", filename, offset, strerror(errno));
    goto done;
  }

  if (!load_image_header(fp, &header)) { goto done; }

  state.heap = sexp_gc_packed_heap_make(header.size, heap_free_size);
  if (!state.heap) {
    snprintf(gc_heap_err_str, ERR_STR_SIZE, "couldn't malloc heap\n");
    goto done;
  }
  base = sexp_heap_first_block(state.heap);

  if (fread(base, 1, header.size, fp) != header.size) {
    snprintf(gc_heap_err_str, ERR_STR_SIZE, "error reading image\n");
    goto done;
  }

  /* Adjust pointers in loaded packed heap. */

  state.offset = (sexp_sint_t)((sexp_sint_t)base - (sexp_sint_t)header.base);
  ctx = (sexp)((unsigned char *)header.context + state.offset);
  sexp_context_heap(ctx) = state.heap;

  /* Type information (specifically, how big types are) is stored as sexps in the
     heap.  This information is needed to sucessfully walk an arbitrary heap.  A
     copy of the type array pointers with correct offsets is applied is created outside
     of the new heap to be used with the pointer adjustment process.
  */
  ctx_globals = sexp_vector_data((sexp)((unsigned char*)sexp_context_globals(ctx) + state.offset));
  ctx_types   = sexp_vector_data((sexp)((unsigned char*)(ctx_globals[SEXP_G_TYPES]) + state.offset));
  state.types_cnt   = sexp_unbox_fixnum(ctx_globals[SEXP_G_NUM_TYPES]);
  state.types = malloc(sizeof(sexp) * state.types_cnt);
  if (!state.types) goto done;
  for (i = 0; i < state.types_cnt; i++) {
    state.types[i] = (sexp)((unsigned char *)ctx_types[i] + state.offset);
  }

  if (sexp_gc_heap_walk(ctx, sexp_context_heap(ctx), state.types, state.types_cnt,
                        &state, NULL, NULL, load_image_callback_p1) != SEXP_TRUE)
    goto done;

  /* Second pass to fix code references */
  if (sexp_gc_heap_walk(ctx, sexp_context_heap(ctx), state.types, state.types_cnt,
                        &state, NULL, NULL, load_image_callback_p2) != SEXP_TRUE)
    goto done;

  if (heap_max_size > SEXP_INITIAL_HEAP_SIZE) {
    sexp_context_heap(ctx)->max_size = heap_max_size;
  }

  res = ctx;
done:
  if (fp) fclose(fp);
  if (state.heap && !ctx) free(state.heap);
  if (state.types) free(state.types);
  return res;
}

#else

sexp sexp_load_image (const char* filename, sexp_uint_t heap_free_size, sexp_uint_t heap_max_size) {
  return NULL;
}

#endif




/****************** Debugging ************************/

#define SEXP_CORE_TYPES_MAX 255

struct sexp_stats_entry {
  size_t count;
  size_t size_all;
  size_t size_min;
  size_t size_max;
};

struct sexp_stats {
  struct sexp_stats_entry sexps[SEXP_CORE_TYPES_MAX+1];
  struct sexp_stats_entry heaps;
  struct sexp_stats_entry frees;
  size_t sexp_count;
};

static void sexp_stats_entry_set(struct sexp_stats_entry *entry, size_t value) {
  entry->count += 1;
  entry->size_all += value;
  if (entry->size_min == 0 || value < entry->size_min) entry->size_min = value;
  if (value > entry->size_max) entry->size_max = value;
}

static sexp heap_stats_callback(sexp ctx, sexp_heap h, void *user) {
  struct sexp_stats *stats = user;
  sexp_stats_entry_set(&(stats->heaps), h->size);
  return SEXP_TRUE;
}

static sexp free_stats_callback(sexp ctx, sexp_free_list f, void *user) {
  struct sexp_stats *stats = user;
  sexp_stats_entry_set(&(stats->frees), f->size);
  return SEXP_TRUE;
}

static sexp sexp_stats_callback(sexp ctx, sexp s, void *user) {
  struct sexp_stats *stats = user;
  int tag = sexp_pointer_tag(s);
  size_t size = sexp_gc_allocated_bytes(ctx, sexp_context_types(ctx),
                                        sexp_context_num_types(ctx), s);
  if (tag > SEXP_CORE_TYPES_MAX) tag = SEXP_CORE_TYPES_MAX;
  sexp_stats_entry_set(&(stats->sexps[tag]), size);
  stats->sexp_count += 1;
  return SEXP_TRUE;
}

void sexp_gc_heap_stats_print(sexp ctx)
{
  if (!ctx || !sexp_contextp(ctx)) return;
  
  struct sexp_stats stats;
  memset(&stats, 0, sizeof(struct sexp_stats));
  sexp_gc_heap_walk(ctx, sexp_context_heap(ctx), sexp_context_types(ctx), sexp_context_num_types(ctx),
                    &stats, heap_stats_callback, free_stats_callback, sexp_stats_callback);
  
  printf("Heap Stats\n    %6zu %7zu\n",
         stats.heaps.count, stats.heaps.size_all);
  printf("Free Stats\n    %6zu %7zu %5zu %5zu\n",
         stats.frees.count, stats.frees.size_all, stats.frees.size_min, stats.frees.size_max);
  printf("Sexp Stats\n");
  size_t total_count = 0;
  size_t total_size = 0;
  int i;
  for (i = 0; i <= SEXP_CORE_TYPES_MAX; i++) {
    if (stats.sexps[i].count == 0) continue;
    printf("%3d %6zu %7zu %5zu %5zu\n", i,
           stats.sexps[i].count, stats.sexps[i].size_all, stats.sexps[i].size_min, stats.sexps[i].size_max);
    total_count += stats.sexps[i].count;
    total_size  += stats.sexps[i].size_all;
  }
  printf(" ========================================\n");
  printf("    %6zu %7zu\n", total_count, total_size);
}

#endif  /* SEXP_USE_IMAGE_LOADING */
