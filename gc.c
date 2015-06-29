/*  gc.c -- simple mark&sweep garbage collector               */
/*  Copyright (c) 2009-2015 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#if ! SEXP_USE_BOEHM && ! SEXP_USE_MALLOC

#include "chibi/sexp.h"

#if SEXP_USE_TIME_GC
#include <sys/resource.h>
#endif

#if SEXP_USE_MMAP_GC
#include <sys/mman.h>
#endif

#ifdef __APPLE__
#define SEXP_RTLD_DEFAULT RTLD_SELF
#else
#define SEXP_RTLD_DEFAULT RTLD_DEFAULT
#endif

#define SEXP_BANNER(x) ("**************** GC "x"\n")

#define SEXP_MINIMUM_OBJECT_SIZE (sexp_heap_align(1))

#if SEXP_USE_GLOBAL_HEAP
sexp_heap sexp_global_heap;
#endif

#if SEXP_USE_CONSERVATIVE_GC
static sexp* stack_base;
#endif

#if SEXP_USE_DEBUG_GC
#define sexp_debug_printf(fmt, ...) fprintf(stderr, SEXP_BANNER(fmt),__VA_ARGS__)
#else
#define sexp_debug_printf(fmt, ...)
#endif

static sexp_heap sexp_heap_last (sexp_heap h) {
  while (h->next) h = h->next;
  return h;
}

static size_t sexp_heap_total_size (sexp_heap h) {
  size_t total_size = 0;
  for (; h; h=h->next)
    total_size += h->size;
  return total_size;
}

#if ! SEXP_USE_GLOBAL_HEAP
void sexp_free_heap (sexp_heap heap) {
#if SEXP_USE_MMAP_GC
  munmap(heap, sexp_heap_pad_size(heap->size));
#else
  free(heap);
#endif
}
#endif

#if SEXP_USE_LIMITED_MALLOC
static sexp_sint_t allocated_bytes=0, max_allocated_bytes=-1;
void* sexp_malloc(size_t size) {
  char* max_alloc;
  void* res;
  if (max_allocated_bytes < 0) {
    max_alloc = getenv("CHIBI_MAX_ALLOC");
    max_allocated_bytes = max_alloc ? atoi(max_alloc) : 8192000; /* 8MB */
  }
  if (max_allocated_bytes > 0 && allocated_bytes + size > max_allocated_bytes)
    return NULL;
  if (!(res = malloc(size))) return NULL;
  allocated_bytes += size;
  return res;
}
/* TODO: subtract freed memory from max_allocated_bytes */
void sexp_free(void* ptr) {
  free(ptr);
}
#endif

void sexp_preserve_object(sexp ctx, sexp x) {
  sexp_global(ctx, SEXP_G_PRESERVATIVES) = sexp_cons(ctx, x, sexp_global(ctx, SEXP_G_PRESERVATIVES));
}

void sexp_release_object(sexp ctx, sexp x) {
  sexp ls1, ls2;
  for (ls1=NULL, ls2=sexp_global(ctx, SEXP_G_PRESERVATIVES); sexp_pairp(ls2);
       ls1=ls2, ls2=sexp_cdr(ls2))
    if (sexp_car(ls2) == x) {
      if (ls1) sexp_cdr(ls1) = sexp_cdr(ls2);
      else sexp_global(ctx, SEXP_G_PRESERVATIVES) = sexp_cdr(ls2);
      break;
    }
}

sexp_uint_t sexp_allocated_bytes (sexp ctx, sexp x) {
  sexp_uint_t res;
  sexp t;
  if (!sexp_pointerp(x) || (sexp_pointer_tag(x) >= sexp_context_num_types(ctx)))
    return sexp_heap_align(1);
  t = sexp_object_type(ctx, x);
  res = sexp_type_size_of_object(t, x) + SEXP_GC_PAD;
#if SEXP_USE_DEBUG_GC
  if (res == 0) {
    fprintf(stderr, SEXP_BANNER("%p zero-size object: %p"), ctx, x);
    return 1;
  }
#endif
  return res;
}

#if SEXP_USE_SAFE_GC_MARK

#if SEXP_USE_DEBUG_GC > 2
int sexp_valid_heap_position(sexp ctx, sexp_heap h, sexp x) {
  sexp p = sexp_heap_first_block(h), end = sexp_heap_end(h);
  sexp_free_list q = h->free_list, r;
  while (p < end) {
    for (r=q->next; r && ((char*)r<(char*)p); q=r, r=r->next)
      ;
    if ((char*)r == (char*)p) {
      p = (sexp) (((char*)p) + r->size);
      continue;
    }
    if (p == x) {
      return 1;
    } else if (p > x) {
      fprintf(stderr, SEXP_BANNER("bad heap position: %p free: %p-%p : %p-%p"),
              x, q, ((char*)q)+q->size, r, ((char*)r)+r->size);
      return 0;
    }
    p = (sexp) (((char*)p)+sexp_heap_align(sexp_allocated_bytes(ctx, p)));
  }
  fprintf(stderr, SEXP_BANNER("bad heap position: %p heap: %p-%p"), x, h, end);
  return 0;
}
#else
#define sexp_valid_heap_position(ctx, h, x) 1
#endif

int sexp_in_heap_p(sexp ctx, sexp x) {
  sexp_heap h;
  if ((sexp_uint_t)x & (sexp_heap_align(1)-1)) {
    fprintf(stderr, SEXP_BANNER("invalid heap alignment: %p"), x);
    return 0;
  }
  for (h=sexp_context_heap(ctx); h; h=h->next)
    if (((sexp)h < x) && (x < (sexp)(h->data + h->size)))
      return sexp_valid_heap_position(ctx, h, x);
  fprintf(stderr, SEXP_BANNER("invalid object outside heap: %p"), x);
  return 0;
}
#endif

#if SEXP_USE_DEBUG_GC > 1
int sexp_valid_object_type_p (sexp ctx, sexp x) {
  if (sexp_pointer_tag(x)<=0 || sexp_pointer_tag(x)>sexp_context_num_types(ctx)){
    fprintf(stderr, SEXP_BANNER("%p mark: bad object at %p: tag: %d"),
            ctx, x, sexp_pointer_tag(x));
    return 0;
  }
  return 1;
}
#else
#define sexp_valid_object_type_p(ctx, x) 1
#endif

#if SEXP_USE_HEADER_MAGIC
int sexp_valid_header_magic_p (sexp ctx, sexp x) {
  if (sexp_pointer_magic(x)  != SEXP_POINTER_MAGIC
      && sexp_pointer_tag(x) != SEXP_TYPE && sexp_pointer_tag(x) != SEXP_OPCODE
      && sexp_pointer_tag(x) != SEXP_CORE && sexp_pointer_tag(x) != SEXP_STACK) {
    fprintf(stderr, SEXP_BANNER("%p mark: bad magic at %p: %x"),
            ctx, x, sexp_pointer_magic(x));
    return 0;
  }
  return 1;
}
#else
#define sexp_valid_header_magic_p(ctx, x) 1
#endif

#if SEXP_DEBUG_GC > 1 || SEXP_USE_SAFE_GC_MARK || SEXP_USE_HEADER_MAGIC
int sexp_valid_object_p (sexp ctx, sexp x) {
  return sexp_in_heap_p(ctx, x) && sexp_valid_object_type_p(ctx, x)
    && sexp_valid_header_magic_p(ctx, x);
}
#define sexp_gc_pass_ctx(x) x,
#else
#define sexp_gc_pass_ctx(x)
#endif

void sexp_mark_one (sexp_gc_pass_ctx(sexp ctx) sexp* types, sexp x) {
  sexp_sint_t len;
  sexp t, *p, *q;
  struct sexp_gc_var_t *saves;
 loop:
  if (!x || !sexp_pointerp(x) || !sexp_valid_object_p(ctx, x) || sexp_markedp(x))
    return;
  sexp_markedp(x) = 1;
  if (sexp_contextp(x)) {
    for (saves=sexp_context_saves(x); saves; saves=saves->next)
      if (saves->var) sexp_mark_one(sexp_gc_pass_ctx(ctx) types, *(saves->var));
  }
  t = types[sexp_pointer_tag(x)];
  len = sexp_type_num_slots_of_object(t, x) - 1;
  if (len >= 0) {
    p = (sexp*) (((char*)x) + sexp_type_field_base(t));
    q = p + len;
    while (p < q && ! (*q && sexp_pointerp(*q)))
      q--;                      /* skip trailing immediates */
    while (p < q && *q == q[-1])
      q--;                      /* skip trailing duplicates */
    while (p < q)
      sexp_mark_one(sexp_gc_pass_ctx(ctx) types, *p++);
    x = *p;
    goto loop;
  }
}

void sexp_mark (sexp ctx, sexp x) {
  sexp_mark_one(sexp_gc_pass_ctx(ctx) sexp_vector_data(sexp_global(ctx, SEXP_G_TYPES)), x);
}

#if SEXP_USE_CONSERVATIVE_GC

int stack_references_pointer_p (sexp ctx, sexp x) {
  sexp *p;
  for (p=(&x)+1; p<stack_base; p++)
    if (*p == x)
      return 1;
  return 0;
}

#if SEXP_USE_TRACK_ALLOC_BACKTRACE
void sexp_print_gc_trace(sexp ctx, sexp p) {
  int i;
  char **debug_text = backtrace_symbols(p->backtrace, SEXP_BACKTRACE_SIZE);
  for (i=0; i < SEXP_BACKTRACE_SIZE; i++)
    fprintf(stderr, SEXP_BANNER("    : %s"), debug_text[i]);
  free(debug_text);
}
#else
#define sexp_print_gc_trace(ctx, p)
#endif

void sexp_conservative_mark (sexp ctx) {
  sexp_heap h = sexp_context_heap(ctx);
  sexp p, end;
  sexp_free_list q, r;
  for ( ; h; h=h->next) {   /* just scan the whole heap */
    p = sexp_heap_first_block(h);
    q = h->free_list;
    end = sexp_heap_end(h);
    while (p < end) {
      for (r=q->next; r && ((char*)r<(char*)p); q=r, r=r->next)
        ;
      if ((char*)r == (char*)p) {
        p = (sexp) (((char*)p) + r->size);
        continue;
      }
      if (!sexp_markedp(p) && stack_references_pointer_p(ctx, p)) {
#ifdef SEXP_USE_CONSERVATIVE_GC_PRESERVE_TAG
        if (sexp_pointer_tag(p) == SEXP_USE_CONSERVATIVE_GC_PRESERVE_TAG)
#endif
        if (1) {
#if SEXP_USE_DEBUG_GC > 3
          if (p && sexp_pointerp(p)) {
            fprintf(stderr, SEXP_BANNER("MISS: %p [%d]: %s"), p,
                    sexp_pointer_tag(p), sexp_pointer_source(p));
            sexp_print_gc_trace(ctx, p);
            fflush(stderr);
          }
#endif
          sexp_mark(ctx, p);
        }
      }
      p = (sexp) (((char*)p)+sexp_heap_align(sexp_allocated_bytes(ctx, p)));
    }
  }
}

#else
#define sexp_conservative_mark(ctx)
#endif

#if SEXP_USE_WEAK_REFERENCES
int sexp_reset_weak_references(sexp ctx) {
  int i, len, broke, all_reset_p;
  sexp_heap h;
  sexp p, t, end, *v;
  sexp_free_list q, r;
  if (sexp_not(sexp_global(ctx, SEXP_G_WEAK_OBJECTS_PRESENT)))
    return 0;
  broke = 0;
  /* just scan the whole heap */
  for (h = sexp_context_heap(ctx) ; h; h=h->next) {
    p = sexp_heap_first_block(h);
    q = h->free_list;
    end = sexp_heap_end(h);
    while (p < end) {
      /* find the preceding and succeeding free list pointers */
      for (r=q->next; r && ((char*)r<(char*)p); q=r, r=r->next)
        ;
      if ((char*)r == (char*)p) { /* this is a free block, skip it */
        p = (sexp) (((char*)p) + r->size);
        continue;
      }
      if (sexp_valid_object_p(ctx, p) && sexp_markedp(p)) {
        t = sexp_object_type(ctx, p);
        if (sexp_type_weak_base(t) > 0) {
          all_reset_p = 1;
          v = (sexp*) ((char*)p + sexp_type_weak_base(t));
          len = sexp_type_num_weak_slots_of_object(t, p);
          for (i=0; i<len; i++) {
            if (v[i] && sexp_pointerp(v[i]) && ! sexp_markedp(v[i])) {
              v[i] = SEXP_FALSE;
              sexp_brokenp(p) = 1;
            } else {
              all_reset_p = 0;
            }
          }
          if (all_reset_p) {      /* ephemerons */
            broke++;
            len += sexp_type_weak_len_extra(t);
            for ( ; i<len; i++) v[i] = SEXP_FALSE;
          }
        }
      }
      p = (sexp) (((char*)p)+sexp_heap_align(sexp_allocated_bytes(ctx, p)));
    }
  }
  sexp_debug_printf("%p (broke %d weak references)", ctx, broke);
  return broke;
}
#else
#define sexp_reset_weak_references(ctx) 0
#endif

#if SEXP_USE_FINALIZERS
sexp sexp_finalize (sexp ctx) {
  size_t size;
  sexp p, t, end;
  sexp_free_list q, r;
  sexp_proc2 finalizer;
  sexp_sint_t finalize_count = 0;
  sexp_heap h = sexp_context_heap(ctx);
#if SEXP_USE_DL
  sexp_sint_t free_dls = 0, pass = 0;
 loop:
#endif
  /* scan over the whole heap */
  for ( ; h; h=h->next) {
    p = sexp_heap_first_block(h);
    q = h->free_list;
    end = sexp_heap_end(h);
    while (p < end) {
      /* find the preceding and succeeding free list pointers */
      for (r=q->next; r && ((char*)r<(char*)p); q=r, r=r->next)
        ;
      if ((char*)r == (char*)p) { /* this is a free block, skip it */
        p = (sexp) (((char*)p) + r->size);
        continue;
      }
      size = sexp_heap_align(sexp_allocated_bytes(ctx, p));
      if (!sexp_markedp(p)) {
        t = sexp_object_type(ctx, p);
        finalizer = sexp_type_finalize(t);
        if (finalizer) {
          finalize_count++;
#if SEXP_USE_DL
          if (sexp_type_tag(t) == SEXP_DL && pass <= 0)
            free_dls = 1;
          else
#endif
            finalizer(ctx, NULL, 1, p);
        }
      }
      p = (sexp) (((char*)p)+size);
    }
  }
#if SEXP_USE_DL
  if (free_dls && pass++ <= 0) goto loop;
#endif
  return sexp_make_fixnum(finalize_count);
}
#endif

sexp sexp_sweep (sexp ctx, size_t *sum_freed_ptr) {
  size_t freed, max_freed=0, sum_freed=0, size;
  sexp_heap h = sexp_context_heap(ctx);
  sexp p, end;
  sexp_free_list q, r, s;
  /* scan over the whole heap */
  for ( ; h; h=h->next) {
    p = sexp_heap_first_block(h);
    q = h->free_list;
    end = sexp_heap_end(h);
    while (p < end) {
      /* find the preceding and succeeding free list pointers */
      for (r=q->next; r && ((char*)r<(char*)p); q=r, r=r->next)
        ;
      if ((char*)r == (char*)p) { /* this is a free block, skip it */
        p = (sexp) (((char*)p) + r->size);
        continue;
      }
      size = sexp_heap_align(sexp_allocated_bytes(ctx, p));
#if SEXP_USE_DEBUG_GC > 1
      if (!sexp_valid_object_p(ctx, p))
        fprintf(stderr, SEXP_BANNER("%p sweep: invalid object at %p"), ctx, p);
      if ((char*)q + q->size > (char*)p)
        fprintf(stderr, SEXP_BANNER("%p sweep: bad size at %p < %p + %lu"),
                ctx, p, q, q->size);
      if (r && ((char*)p)+size > (char*)r)
        fprintf(stderr, SEXP_BANNER("%p sweep: bad size at %p + %lu > %p"),
                ctx, p, size, r);
#endif
      if (!sexp_markedp(p)) {
        /* free p */
        sum_freed += size;
        if (((((char*)q) + q->size) == (char*)p) && (q != h->free_list)) {
          /* merge q with p */
          if (r && r->size && ((((char*)p)+size) == (char*)r)) {
            /* ... and with r */
            q->next = r->next;
            freed = q->size + size + r->size;
            p = (sexp) (((char*)p) + size + r->size);
          } else {
            freed = q->size + size;
            p = (sexp) (((char*)p)+size);
          }
          q->size = freed;
        } else {
          s = (sexp_free_list)p;
          if (r && r->size && ((((char*)p)+size) == (char*)r)) {
            /* merge p with r */
            s->size = size + r->size;
            s->next = r->next;
            q->next = s;
            freed = size + r->size;
          } else {
            s->size = size;
            s->next = r;
            q->next = s;
            freed = size;
          }
          p = (sexp) (((char*)p)+freed);
        }
        if (freed > max_freed)
          max_freed = freed;
      } else {
        sexp_markedp(p) = 0;
        p = (sexp) (((char*)p)+size);
      }
    }
  }
  if (sum_freed_ptr) *sum_freed_ptr = sum_freed;
  return sexp_make_fixnum(max_freed);
}

#if SEXP_USE_GLOBAL_SYMBOLS
void sexp_mark_global_symbols(sexp ctx) {
  int i;
  for (i=0; i<SEXP_SYMBOL_TABLE_SIZE; i++)
    sexp_mark(ctx, sexp_symbol_table[i]);
}
#else
#define sexp_mark_global_symbols(ctx)
#endif

sexp sexp_gc (sexp ctx, size_t *sum_freed) {
  sexp res, finalized SEXP_NO_WARN_UNUSED;
#if SEXP_USE_TIME_GC
  sexp_uint_t gc_usecs;
  struct rusage start, end;
  getrusage(RUSAGE_SELF, &start);
  sexp_debug_printf("%p (heap: %p size: %lu)", ctx, sexp_context_heap(ctx),
                    sexp_heap_total_size(sexp_context_heap(ctx)));
#endif
  sexp_mark_global_symbols(ctx);
  sexp_mark(ctx, ctx);
  sexp_conservative_mark(ctx);
  sexp_reset_weak_references(ctx);
  finalized = sexp_finalize(ctx);
  res = sexp_sweep(ctx, sum_freed);
#if SEXP_USE_TIME_GC
  getrusage(RUSAGE_SELF, &end);
  gc_usecs = (end.ru_utime.tv_sec - start.ru_utime.tv_sec) * 1000000 +
    end.ru_utime.tv_usec - start.ru_utime.tv_usec;
  ++sexp_context_gc_count(ctx);
  sexp_context_gc_usecs(ctx) += gc_usecs;
  sexp_debug_printf("%p (freed: %lu max_freed: %lu finalized: %lu time: %luus)",
                    ctx, (sum_freed ? *sum_freed : 0), sexp_unbox_fixnum(res),
                    sexp_unbox_fixnum(finalized), gc_usecs);
#endif
  return res;
}

sexp_heap sexp_make_heap (size_t size, size_t max_size, size_t chunk_size) {
  sexp_free_list free, next;
  sexp_heap h;
#if SEXP_USE_MMAP_GC
  h =  mmap(NULL, sexp_heap_pad_size(size), PROT_READ|PROT_WRITE|PROT_EXEC,
            MAP_ANON|MAP_PRIVATE, 0, 0);
#else
  h =  sexp_malloc(sexp_heap_pad_size(size));
#endif
  if (! h) return NULL;
  h->size = size;
  h->max_size = max_size;
  h->chunk_size = chunk_size;
  h->data = (char*) sexp_heap_align(sizeof(h->data)+(sexp_uint_t)&(h->data));
  free = h->free_list = (sexp_free_list) h->data;
  h->next = NULL;
  next = (sexp_free_list) (((char*)free)+sexp_heap_align(sexp_free_chunk_size));
  free->size = 0; /* actually sexp_heap_align(sexp_free_chunk_size) */
  free->next = next;
  next->size = size - sexp_heap_align(sexp_free_chunk_size);
  next->next = NULL;
#if SEXP_USE_DEBUG_GC
  fprintf(stderr, SEXP_BANNER("heap: %p-%p data: %p-%p"),
          h, ((char*)h)+sexp_heap_pad_size(size), h->data, h->data + size);
  fprintf(stderr, SEXP_BANNER("first: %p end: %p"),
          sexp_heap_first_block(h), sexp_heap_end(h));
  fprintf(stderr, SEXP_BANNER("free1: %p-%p free2: %p-%p"),
          free, ((char*)free)+free->size, next, ((char*)next)+next->size);
#endif
  return h;
}

int sexp_grow_heap (sexp ctx, size_t size, size_t chunk_size) {
  size_t cur_size, new_size;
  sexp_heap tmp, h = sexp_heap_last(sexp_context_heap(ctx));
#if SEXP_USE_FIXED_CHUNK_SIZE_HEAPS
  for (tmp=sexp_context_heap(ctx); tmp; tmp=tmp->next)
    if (tmp->chunk_size == size) {
      h = tmp;
      chunk_size = size;
      break;
    }
#endif
  cur_size = h->size;
  new_size = sexp_heap_align(((cur_size > size) ? cur_size : size) * 2);
  tmp = sexp_make_heap(new_size, h->max_size, chunk_size);
  tmp->next = h->next;
  h->next = tmp;
  return (h->next != NULL);
}

void* sexp_try_alloc (sexp ctx, size_t size) {
  sexp_free_list ls1, ls2, ls3;
  sexp_heap h;
  for (h=sexp_context_heap(ctx); h; h=h->next) {
#if SEXP_USE_FIXED_CHUNK_SIZE_HEAPS
    if (h->chunk_size && h->chunk_size != size)
      continue;
#endif
    for (ls1=h->free_list, ls2=ls1->next; ls2; ls1=ls2, ls2=ls2->next) {
      if (ls2->size >= size) {
#if SEXP_USE_DEBUG_GC > 1
        ls3 = (sexp_free_list) sexp_heap_end(h);
        if (ls2 >= ls3)
          fprintf(stderr, "alloced %lu bytes past end of heap: %p (%lu) >= %p"
                  " next: %p (%lu)\n", size, ls2, ls2->size, ls3, ls2->next,
                  (ls2->next ? ls2->next->size : 0));
#endif
        if (ls2->size >= (size + SEXP_MINIMUM_OBJECT_SIZE)) {
          ls3 = (sexp_free_list) (((char*)ls2)+size); /* the tail after ls2 */
          ls3->size = ls2->size - size;
          ls3->next = ls2->next;
          ls1->next = ls3;
        } else {                  /* take the whole chunk */
          ls1->next = ls2->next;
        }
        memset((void*)ls2, 0, size);
        return ls2;
      }
    }
  }
  return NULL;
}

void* sexp_alloc (sexp ctx, size_t size) {
  void *res;
  size_t max_freed, sum_freed, total_size;
  sexp_heap h = sexp_context_heap(ctx);
  size = sexp_heap_align(size) + SEXP_GC_PAD;
  res = sexp_try_alloc(ctx, size);
  if (! res) {
    max_freed = sexp_unbox_fixnum(sexp_gc(ctx, &sum_freed));
    total_size = sexp_heap_total_size(sexp_context_heap(ctx));
    if (((max_freed < size)
         || ((total_size > sum_freed)
             && (total_size - sum_freed) > (total_size*SEXP_GROW_HEAP_RATIO)))
        && ((!h->max_size) || (total_size < h->max_size)))
      sexp_grow_heap(ctx, size, 0);
    res = sexp_try_alloc(ctx, size);
    if (! res) {
      res = sexp_global(ctx, SEXP_G_OOM_ERROR);
      sexp_debug_printf("ran out of memory allocating %lu bytes => %p", size, res);
    }
  }
  return res;
}

#if ! SEXP_USE_GLOBAL_HEAP

void sexp_offset_heap_pointers (sexp_heap heap, sexp_heap from_heap, sexp* types, sexp flags) {
  sexp_sint_t i, off, len, freep, loadp;
  sexp_free_list q;
  sexp p, t, end, *v;
#if SEXP_USE_DL
  sexp name;
#endif
  freep = sexp_unbox_fixnum(flags) & sexp_unbox_fixnum(SEXP_COPY_FREEP);
  loadp = sexp_unbox_fixnum(flags) & sexp_unbox_fixnum(SEXP_COPY_LOADP);

  off = (sexp_sint_t)((sexp_sint_t)heap - (sexp_sint_t)from_heap);
  heap->data += off;
  end = (sexp) (heap->data + heap->size);

  /* adjust the free list */
  heap->free_list = (sexp_free_list) ((char*)heap->free_list + off);
  for (q=heap->free_list; q->next; q=q->next)
    q->next = (sexp_free_list) ((char*)q->next + off);

  /* adjust data by traversing over the new heap */
  p = (sexp) (heap->data + sexp_heap_align(sexp_free_chunk_size));
  q = heap->free_list;
  while (p < end) {
    /* find the next free list pointer */
    for ( ; q && ((char*)q < (char*)p); q=q->next)
      ;
    if ((char*)q == (char*)p) { /* this is a free block, skip it */
      p = (sexp) (((char*)p) + q->size);
    } else {
      t = (sexp)((char*)(types[sexp_pointer_tag(p)])
                 + ((char*)types > (char*)p ? off : 0));
      len = sexp_type_num_slots_of_object(t, p);
      v = (sexp*) ((char*)p + sexp_type_field_base(t));
      /* offset any pointers in the _destination_ heap */
      for (i=0; i<len; i++)
        if (v[i] && sexp_pointerp(v[i]))
          v[i] = (sexp) ((char*)v[i] + off);
      /* don't free unless specified - only the original cleans up */
      if (! freep)
        sexp_freep(p) = 0;
      /* adjust context heaps, don't copy saved sexp_gc_vars */
      if (sexp_contextp(p)) {
#if SEXP_USE_GREEN_THREADS
        sexp_context_ip(p) += off;
#endif
        sexp_context_last_fp(p) += off;
        sexp_stack_top(sexp_context_stack(p)) = 0;
        sexp_context_saves(p) = NULL;
        sexp_context_heap(p) = heap;
      } else if (sexp_bytecodep(p) && off != 0) {
        for (i=0; i<sexp_bytecode_length(p); ) {
          switch (sexp_bytecode_data(p)[i++]) {
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
              v = (sexp*)(&(sexp_bytecode_data(p)[i]));
              if (v[0] && sexp_pointerp(v[0])) v[0] = (sexp) (((char*)v[0]) + off);
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
              v = (sexp*)(&(sexp_bytecode_data(p)[i]));
              if (v[2] && sexp_pointerp(v[2])) v[2] = (sexp) (((char*)v[2]) + off);
              i += 3*sizeof(sexp); break;
          }
        }
      } else if (sexp_portp(p) && sexp_port_stream(p)) {
        sexp_port_stream(p) = 0;
        sexp_port_openp(p) = 0;
        sexp_freep(p) = 0;
#if SEXP_USE_DL
      } else if (loadp && sexp_dlp(p)) {
        sexp_dl_handle(p) = NULL;
#endif
      }
      p = (sexp) (((char*)p)+sexp_heap_align(sexp_type_size_of_object(t, p))+SEXP_GC_PAD);
    }
  }

  /* make a second pass to fix code references */
  if (loadp) {
    p = (sexp) (heap->data + sexp_heap_align(sexp_free_chunk_size));
    q = heap->free_list;
    while (p < end) {
      /* find the next free list pointer */
      for ( ; q && ((char*)q < (char*)p); q=q->next)
        ;
      if ((char*)q == (char*)p) { /* this is a free block, skip it */
        p = (sexp) (((char*)p) + q->size);
      } else {
#if SEXP_USE_DL
        if (sexp_opcodep(p) && sexp_opcode_func(p)) {
          name = (sexp_opcode_data2(p) && sexp_stringp(sexp_opcode_data2(p))) ? sexp_opcode_data2(p) : sexp_opcode_name(p);
          if (sexp_dlp(sexp_opcode_dl(p))) {
            if (!sexp_dl_handle(sexp_opcode_dl(p)))
              sexp_dl_handle(sexp_opcode_dl(p)) = dlopen(sexp_string_data(sexp_dl_file(sexp_opcode_dl(p))), RTLD_LAZY);
            sexp_opcode_func(p) = dlsym(sexp_dl_handle(sexp_opcode_dl(p)), sexp_string_data(name));
          } else {
            sexp_opcode_func(p) = dlsym(SEXP_RTLD_DEFAULT, sexp_string_data(name));
          }
        } else
#endif
        if (sexp_typep(p)) {
          if (sexp_type_finalize(p)) {
            /* TODO: handle arbitrary finalizers in images */
#if SEXP_USE_DL
            if (sexp_type_tag(p) == SEXP_DL)
              sexp_type_finalize(p) = SEXP_FINALIZE_DL;
            else
#endif
              sexp_type_finalize(p) = SEXP_FINALIZE_PORT;
          }
        }
        t = types[sexp_pointer_tag(p)];
        p = (sexp) (((char*)p)+sexp_heap_align(sexp_type_size_of_object(t, p)+SEXP_GC_PAD));
      }
    }
  }
}

sexp sexp_copy_context (sexp ctx, sexp dst, sexp flags) {
  sexp_sint_t off;
  sexp_heap to, from = sexp_context_heap(ctx);

  /* validate input, creating a new heap if needed */
  if (from->next) {
    return sexp_user_exception(ctx, NULL, "can't copy a non-contiguous heap", ctx);
  } else if (! dst || sexp_not(dst)) {
    to = sexp_make_heap(from->size, from->max_size, from->chunk_size);
    if (!to) return sexp_global(ctx, SEXP_G_OOM_ERROR);
    dst = (sexp) ((char*)ctx + ((char*)to - (char*)from));
  } else if (! sexp_contextp(dst)) {
    return sexp_type_exception(ctx, NULL, SEXP_CONTEXT, dst);
  } else if (sexp_context_heap(dst)->size < from->size) {
    return sexp_user_exception(ctx, NULL, "destination context too small", dst);
  } else {
    to = sexp_context_heap(dst);
  }

  /* copy the raw data */
  off = (char*)to - (char*)from;
  memcpy(to, from, sexp_heap_pad_size(from->size));

  /* adjust the pointers */
  sexp_offset_heap_pointers(to, from, sexp_context_types(ctx) + off, flags);

  return dst;
}

#endif

#if SEXP_USE_COMPACTING_GC
#define sexp_forward_pointer(x) (((sexp*)&((x)->value))[0])

struct sexp_type_gc_info {
  short field_base, field_len_base, field_len_off;
  unsigned short field_len_scale;
  short size_base, size_off;
  unsigned short size_scale;
};

struct sexp_type_gc_info* sexp_get_type_gc_info(sexp ctx) {
  int i;
  sexp t;
  struct sexp_type_gc_info* res =
    malloc(sizeof(struct sexp_type_gc_info) * sexp_context_num_types(ctx));
  for (i=0; i < sexp_context_num_types(ctx); ++i) {
    t = sexp_type_by_index(ctx, i);
    res[i].field_base = sexp_type_field_base(t);
    res[i].field_len_base = sexp_type_field_len_base(t);
    res[i].field_len_off = sexp_type_field_len_off(t);
    res[i].field_len_scale = sexp_type_field_len_scale(t);
    res[i].size_base = sexp_type_size_base(t);
    res[i].size_off = sexp_type_size_off(t);
    res[i].size_scale = sexp_type_size_scale(t);
  }
  return res;
}

static size_t sexp_heap_allocated_bytes(struct sexp_type_gc_info* t, sexp x) {
  if (!x || !sexp_pointerp(x))
    return sexp_heap_align(1);
  return sexp_heap_align((((sexp_uint_t*)((char*)x + t->size_off))[0]
                          * t->size_scale + t->size_base));
}

static void sexp_cheney_mark (struct sexp_type_gc_info* type_gc_info, sexp y, char** end) {
  sexp_sint_t ysize;
  if (!sexp_markedp(y)) {
    ysize = sexp_heap_allocated_bytes(&type_gc_info[sexp_pointer_tag(y)], y);
    memcpy(*end, (char*)y, ysize);
    sexp_markedp(y) = 1;
    sexp_forward_pointer(y) = (sexp)*end;
    *end += ysize;
  }
}

#if SEXP_USE_ALIGNED_BYTECODE
#define sexp_align_index(i) i = sexp_word_align((sexp_uint_t)i)
#else
#define sexp_align_index(i) 0
#endif

/* cheney's algorithm: iterative BFS copy */
char* sexp_cheney (struct sexp_type_gc_info* type_gc_info, sexp x, char* end) {
  struct sexp_type_gc_info* t;
  sexp_sint_t i, len;
  sexp y, *p, *q;
  while ((char*)x < end) {
    /* walk fields */
    t = &(type_gc_info[sexp_pointer_tag(x)]);
    len = (((sexp_uint_t*)((char*)x + t->field_len_off))[0]
           * t->field_len_scale + t->field_len_base);
    if (len > 0) {
      p = (sexp*) (((char*)x) + t->field_base);
      q = p + len;
      for ( ; p < q; ++p) {
        y = *p;
        if (y && sexp_pointerp(y)) {
          sexp_cheney_mark(type_gc_info, y, &end);
          *p = sexp_forward_pointer(y);
        }
      }
    }
    /* walk pointer values in bytecode */
    if (sexp_bytecodep(x)) {
      for (i=0; i<sexp_bytecode_length(x); ) {
        switch (sexp_bytecode_data(x)[i++]) {
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
          sexp_align_index(i);
          p = (sexp*)(&(sexp_bytecode_data(x)[i]));
          y = *p;
          if (y && sexp_pointerp(y)) {
            sexp_cheney_mark(type_gc_info, y, &end);
            *p = sexp_forward_pointer(y);
          }
          i += sizeof(sexp);
          break;
        case SEXP_OP_JUMP:        case SEXP_OP_JUMP_UNLESS:
        case SEXP_OP_STACK_REF:   case SEXP_OP_CLOSURE_REF:
        case SEXP_OP_LOCAL_REF:   case SEXP_OP_LOCAL_SET:
        case SEXP_OP_TYPEP:
#if SEXP_USE_RESERVE_OPCODE
        case SEXP_OP_RESERVE:
#endif
          sexp_align_index(i);
          i += sizeof(sexp);
          break;
        case SEXP_OP_MAKE: case SEXP_OP_SLOT_REF: case SEXP_OP_SLOT_SET:
          sexp_align_index(i);
          i += 2*sizeof(sexp);
          break;
        case SEXP_OP_MAKE_PROCEDURE:
          sexp_align_index(i);
          p = (sexp*)(&(sexp_bytecode_data(x)[i]));
          y = p[2];
          if (y && sexp_pointerp(y)) {
            sexp_cheney_mark(type_gc_info, y, &end);
            p[2] = sexp_forward_pointer(y);
          }
          i += 3*sizeof(sexp);
          break;
        }
      }
    }
    x = (sexp)((char*)x + sexp_heap_allocated_bytes(t, x));
  }
  return end;
}

/* Compact the ctx heap into the context dst, or a new heap if NULL.    */
/* ctx may have several heaps, but if specified dst must have a single  */
/*   heap of sufficient size.  */
/* Returns the location of ctx in the new heap.  */
sexp sexp_compact_heap (sexp ctx, sexp dst) {
  char *end;
  sexp_free_list free_ls, next;
  struct sexp_type_gc_info* type_gc_info;
  sexp_heap to, from = sexp_context_heap(ctx);
  sexp_sint_t from_size = sexp_heap_total_size(from);

  /* validate input, creating a new heap if needed */
  if (! dst || sexp_not(dst)) {
    to = sexp_make_heap(from_size, 0, 0);
    if (!to) return sexp_global(ctx, SEXP_G_OOM_ERROR);
    free_ls = to->free_list = (sexp_free_list)to->data;
    next = (sexp_free_list)
      (((char*)free_ls)+sexp_heap_align(sexp_free_chunk_size));
  } else if (!sexp_contextp(dst)) {
    return sexp_type_exception(ctx, NULL, SEXP_CONTEXT, dst);
  } else if (sexp_context_heap(dst)->size < from_size) {
    return sexp_user_exception(ctx, NULL, "destination context too small", dst);
  } else {
    to = sexp_context_heap(dst);
    free_ls = to->free_list = (sexp_free_list)to->data;
    next = (sexp_free_list)
      (((char*)free_ls)+sexp_heap_align(sexp_free_chunk_size));
    free_ls->size = 0;  /* actually sexp_heap_align(sexp_free_chunk_size) */
    free_ls->next = next;
    next->size = from->size - sexp_heap_align(sexp_free_chunk_size);
    next->next = NULL;
  }

  /* extract the necessary type info outside the heap */
  /* this simplifies processing (since the types get moved), */
  /* and helps to keep the info in cache */
  type_gc_info = sexp_get_type_gc_info(ctx);

  /* recursively copy into the new heap starting with the root context */
  end = (char*)next;
  memcpy(end, ctx, sexp_sizeof(context));
  end += sexp_heap_align(sexp_sizeof(context));
  /* update forward pointers */
  sexp_markedp(ctx) = 1;
  sexp_forward_pointer(ctx) = (sexp)next;
  ctx = (sexp)next;
  /* run cheney (this does all the work) */
  free_ls = to->free_list = (sexp_free_list)sexp_cheney(type_gc_info, ctx, end);
  /* fixup the new free list */
  next = (sexp_free_list)
    (((char*)free_ls)+sexp_heap_align(sexp_free_chunk_size));
  free_ls->size = 0;
  free_ls->next = next;
  next->size = (char*)sexp_heap_end(to) - (char*)next;
  next->next = NULL;

  /* cleanup and return the new ctx location */
  free(type_gc_info);
  return ctx;
}
#endif

void sexp_gc_init (void) {
#if SEXP_USE_GLOBAL_HEAP || SEXP_USE_CONSERVATIVE_GC
  sexp_uint_t size = sexp_heap_align(SEXP_INITIAL_HEAP_SIZE);
#endif
#if SEXP_USE_GLOBAL_HEAP
  sexp_global_heap = sexp_make_heap(size, SEXP_MAXIMUM_HEAP_SIZE, 0);
#endif
#if SEXP_USE_CONSERVATIVE_GC
  /* the +32 is a hack, but this is just for debugging anyway */
  stack_base = ((sexp*)&size) + 32;
#endif
}

#endif
