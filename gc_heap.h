/* gc_heap.h -- heap packing, run-time image generation    */
/* Copyright (c) 2016 Chris Walsh.  All rights reserved.   */
/* BSD-style license: http://synthcode.com/license.txt     */

#ifndef GC_HEAP_H
#define GC_HEAP_H

#include "chibi/sexp.h"

/* Iterate the heap associated with the context argument 'ctx',
   calling user provided callbacks for the individual heap elements.
   
   For each heap found, heap_callback is called.
   For each free segment found, free_callback is called.
   For each valid sexp found, sexp_callback is called.
     
   Callbacks are skipped if the associated function
   pointer argument is NULL.

   A callback return value of SEXP_TRUE allows the heap walk to 
   continue normally.  Any other value terminates the heap walk
   with the callback result being returned.
 
   The sexp_gc_heap_walk return value of SEXP_TRUE indicates all
   elements of the heap were walked normally.  Any other return 
   value indicates an abnormal return condition.
*/
sexp sexp_gc_heap_walk(sexp ctx,         /* a possibly incomplete context */
                       sexp *types,      /* normally set to sexp_context_types(ctx) */
                       size_t types_cnt, /* normally set to sexp_context_num_types(ctx) */
                       void *user,   /* arbitrary data passed to callbacks */
                       sexp (*heap_callback)(sexp ctx, sexp_heap h, void *user),
                       sexp (*free_callback)(sexp ctx, sexp_free_list f, void *user),
                       sexp (*sexp_callback)(sexp ctx, sexp s, void *user));


/* Returns a new context which contains a single, packed heap.
   
   The original ctx or heap are not altered, leaving two copies
   of all sexps.  For runtime use where you are packing the heap
   to make accesses more efficient, the old heap and context should
   be discarded after a sucessful call to heap pack; finalizers do
   not need to be called since all active objects are in the new heap.
 
   The input heap_size specifies the amount of free space to allocate
   at the end of the packed heap.  A heap_size of zero will produce a
   single packed heap just large enough to hold all sexps from the
   original heap.
*/
sexp sexp_gc_heap_pack(sexp ctx, sexp_uint_t heap_free_size);


/* Creates a new packed heap from the provided context, and saves
   the contents of the packed heap to the file named filename.

   If sucessful, SEXP_TRUE is returned.  If a problem was encountered
   in either creating the packed heap or saving to a file, then either
   SEXP_FALSE or an exception is returned.  Because of shared code with
   sexp_load_image, sexp_load_image_err() can also be used to return the
   error condition.

   In all cases, upon completion the temporary packed context is deleted 
   and the context provided as an argument is not changed.
*/
sexp sexp_save_image (sexp ctx, const char* filename);


/* Loads a previously saved image, and returns the context associated with
   that image.  If the context could not be loaded, either NULL or an exception
   are returned instead.

   A new context is created with the contents of filename loaded into the
   heap.  The heap_free_size parameter specifies the size of the heap to be
   created in addition to the heap image on disk.  A size of zero will
   result in an initial heap exactly the size of the disk image which will
   be expanded with an additional heap when the system requests storage space.
 
   The return value is either the context of the loaded image, or NULL.  In
   the case of a NULL context, the function sexp_load_image_err() can be called
   to provide a description of the error encountered.  An sexp exception cannot be
   returned because there is not a valid context in which to put the exception.
*/
sexp sexp_load_image (const char* filename, sexp_uint_t heap_free_size, sexp_uint_t heap_max_size);


/* In the case that sexp_load_image() returns NULL, this function will return
   a string containing a description of the error condition.
*/
char* sexp_load_image_err();


/* Debugging tool.  Prints a summary of the heap structure to stdout.
 */
void sexp_gc_heap_stats_print(sexp ctx);


#endif

