#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include <sys/resource.h>
#include <sys/time.h>

#define Nothing ((value) 0)

#ifndef caml_uerror
#  define caml_uerror uerror
#endif

extern void caml_uerror (const char * cmdname, value arg);

#define timeval_to_microsecs(tv) (tv.tv_sec * 1000000L + tv.tv_usec)

CAMLprim value caml_unix_getrusage(value flag){

    CAMLparam1(flag);
    CAMLlocal1(res);
    
    struct rusage usage;

    if( getrusage(Int_val(flag) ? RUSAGE_CHILDREN : RUSAGE_SELF, &usage) ) {
        caml_uerror("getrusage", Nothing);
    }

    res = caml_alloc_tuple(16);
    Store_field( res,  0, caml_copy_int64(timeval_to_microsecs(usage.ru_utime)));
    Store_field( res,  1, caml_copy_int64(timeval_to_microsecs(usage.ru_stime)));
    Store_field( res,  2, caml_copy_int64(usage.ru_maxrss) );
    Store_field( res,  3, caml_copy_int64(usage.ru_ixrss) );
    Store_field( res,  4, caml_copy_int64(usage.ru_idrss) );
    Store_field( res,  5, caml_copy_int64(usage.ru_isrss) );
    Store_field( res,  6, caml_copy_int64(usage.ru_minflt) );
    Store_field( res,  7, caml_copy_int64(usage.ru_majflt) );
    Store_field( res,  8, caml_copy_int64(usage.ru_nswap) );
    Store_field( res,  9, caml_copy_int64(usage.ru_inblock) );
    Store_field( res, 10, caml_copy_int64(usage.ru_oublock) );
    Store_field( res, 11, caml_copy_int64(usage.ru_msgsnd) );
    Store_field( res, 12, caml_copy_int64(usage.ru_msgrcv) );
    Store_field( res, 13, caml_copy_int64(usage.ru_nsignals) );
    Store_field( res, 14, caml_copy_int64(usage.ru_nvcsw) );
    Store_field( res, 15, caml_copy_int64(usage.ru_nivcsw) );

    CAMLreturn(res);
}
