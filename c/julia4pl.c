/*
 *  Copyright (C) 2017 Samer Abdallah
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 */

#include <SWI-Prolog.h>
#include <stdio.h>
#include <julia.h>

static functor_t colon_1;
static atom_t hash;

static int unify_reversed_list_sizes(term_t list, size_t *x, int n)
{
  list=PL_copy_term_ref(list);

  for (int i=n-1; i>=0; i--) {
    term_t head=PL_new_term_ref();
    term_t tail=PL_new_term_ref();
    if (!PL_unify_list(list,head,tail)) PL_fail;
    if (!PL_unify_integer(head,x[i])) PL_fail;
    list=tail;
  }
  return PL_unify_nil(list);
}

static int unify_list_ints(term_t list, int64_t *x, size_t n)
{
  list=PL_copy_term_ref(list);

  for (size_t i=0; i<n; i++) {
    term_t head=PL_new_term_ref();
    term_t tail=PL_new_term_ref();
    if (!PL_unify_list(list,head,tail)) PL_fail;
    if (!PL_unify_int64(head,x[i])) PL_fail;
    list=tail;
  }
  return PL_unify_nil(list);
}

static int unify_list_doubles(term_t list, double *x, size_t n)
{
  list=PL_copy_term_ref(list);

  for (size_t i=0; i<n; i++) {
    term_t head=PL_new_term_ref();
    term_t tail=PL_new_term_ref();
    if (!PL_unify_list(list,head,tail)) PL_fail;
    if (!PL_unify_float(head,x[i])) PL_fail;
    list=tail;
  }
  return PL_unify_nil(list);
}

// read list of integers from term and write to int array
int get_list_integers(term_t list,  int64_t maxlen, int64_t *len, int64_t *vals)
{
  term_t  head=PL_new_term_ref();
  int64_t    n;

  // copy term ref so as not to modify original
  list=PL_copy_term_ref(list);
  for (n=0;n<maxlen && PL_get_list(list,head,list);n++) {
      if (!PL_get_int64(head,&vals[n])) return FALSE;
  }
  if (!PL_get_nil(list)) return FALSE;
  *len=n;
  return TRUE;
}

// read list of floats from term and write to double array
int get_list_doubles(term_t list, int64_t len, double *vals)
{
  term_t  head=PL_new_term_ref();
  int64_t    n;

  // copy term ref so as not to modify original
  list=PL_copy_term_ref(list);
  for (n=0;n<len && PL_get_list(list,head,list);n++) {
      if (!PL_get_float(head,&vals[n])) return FALSE;
  }
  return PL_get_nil(list);
}

int get_list_terms(term_t list, int64_t len, term_t *terms)
{
  int64_t    i;
  list=PL_copy_term_ref(list);
  for (i=0;i<len && PL_get_list(list,terms[i],list);i++);
  return PL_get_nil(list);
}

// ---------------------------------------------------------------------------

typedef int array_getter_t(term_t, int64_t, void **);
typedef int array_unifier_t(term_t, size_t, void **);

static int get_nested(array_getter_t *getter, term_t list, int64_t ndims, int64_t *dims, void **pp) {
   if (ndims==1) return getter(list, *dims, pp);
   else {
      term_t  head=PL_new_term_ref();
      int64_t i, ndims1=ndims-1, len=dims[ndims1];

      list=PL_copy_term_ref(list);
      for (i=0; i<len && PL_get_list(list,head,list); i++) {
         if (!get_nested(getter, head, ndims1, dims, pp)) return FALSE;
      }
      return PL_get_nil(list);
   }
}

static int unify_nested(array_unifier_t *unifier, term_t list, int ndims, size_t *dims, void **pp) {
   if (ndims==1) return unifier(list, *dims, pp);
   else {
      size_t i, ndims1=ndims-1, len=dims[ndims1];

      list=PL_copy_term_ref(list);
      for (i=0; i<len; i++) {
         term_t head=PL_new_term_ref(), tail=PL_new_term_ref();
         if (!PL_unify_list(list, head, tail)) PL_fail;
         if (!unify_nested(unifier, head, ndims1, dims, pp)) PL_fail;
         list=tail;
      }
      return PL_unify_nil(list);
   }
}

static int int64_getter(term_t vals, int64_t n, void **pp) {
   int64_t m;
   return get_list_integers(vals, n, &m, (int64_t *)*pp)
       && (*pp += n*sizeof(int64_t), TRUE);
}

static int float64_getter(term_t vals, int64_t n, void **pp) {
   return get_list_doubles(vals, n, (double *)*pp)
       && (*pp += n*sizeof(double), TRUE);
}

static int int64_unifier(term_t vals, size_t n, void **pp) {
   return unify_list_ints(vals, (int64_t *)*pp, n)
       && (*pp += n*sizeof(int64_t), TRUE);
}

static int float64_unifier(term_t vals, size_t n, void **pp) {
   return unify_list_doubles(vals, (double *)*pp, n)
       && (*pp += n*sizeof(double), TRUE);
}

// ---------------------------------------------------------------------------

install_t install();

foreign_t pjl_exec(term_t expr);
foreign_t pjl_eval(term_t expr, term_t res);
foreign_t pjl_apply(term_t expr, term_t arg1, term_t arg2, term_t res);
foreign_t pjl_apply(term_t expr, term_t n, term_t args, term_t res);
foreign_t pjl_apply_(term_t expr, term_t n, term_t args);

static int pjl_on_halt(int rc, void *p) {
   jl_atexit_hook(rc);
   return 0;
}

install_t install() {
   PL_register_foreign("jl_exec",   1, (void *)pjl_exec, 0);
   PL_register_foreign("jl_eval",   2, (void *)pjl_eval, 0);
   PL_register_foreign("jl_apply",  4, (void *)pjl_apply, 0);
   PL_register_foreign("jl_apply_", 3, (void *)pjl_apply_, 0);

   colon_1 = PL_new_functor(PL_new_atom(":"),1);
   hash = PL_new_atom("#");
   printf("Opening Julia...\n");
   jl_init();
   PL_on_halt(pjl_on_halt, 0);
   jl_eval_string("Base.load_juliarc()");
}

/* utility function to extract UTF-8 encoded character array from
 * a Prolog string, code list, or atom. */
static int term_to_utf8_string(term_t t, char **s) {
	return PL_get_chars(t,(char **)s, CVT_ATOM | CVT_STRING | CVT_LIST | BUF_RING | REP_UTF8);
}

// throws a Prolog exception to signal type error
static int type_error(term_t actual, const char *expected)
{
   term_t ex = PL_new_term_ref();
   int rc;

   rc = PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
            PL_FUNCTOR_CHARS, "type_error", 2,
              PL_CHARS, expected,
              PL_TERM, actual,
            PL_VARIABLE);

   return PL_raise_exception(ex);
}

static int result_error(char *context, char *type)
{
   term_t ex = PL_new_term_ref();
   int rc;

   return PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
                        PL_FUNCTOR_CHARS, "unsupported_julia_return_type", 2,
                        PL_CHARS, context, PL_CHARS, type, 
                        PL_VARIABLE)
       && PL_raise_exception(ex);
}

static char *sym_name(jl_value_t *v) { return jl_symbol_name((jl_sym_t *)v); }

static int unify_tree(term_t t, jl_value_t *v) {
   jl_datatype_t *dt = (jl_datatype_t *)jl_typeof(v);
   int rc;

   // FIXME this needs to handle literal values, QuoteNode and possibly other Julia types...
   if (dt==jl_float64_type)    rc = PL_unify_float(t, jl_unbox_float64(v));
   else if (dt==jl_int64_type) rc = PL_unify_integer(t, jl_unbox_int64(v));
   else if (jl_is_string(v))  rc = PL_unify_chars(t, PL_STRING | REP_UTF8, -1, jl_string_ptr(v));
   else if (jl_is_bool(v))    rc = PL_unify_atom_chars(t, jl_unbox_bool(v) ? "true" : "false");
   else if (jl_is_nothing(v)) rc = PL_unify_atom_chars(t, "nothing");
   else if (jl_is_symbol(v))  rc = PL_unify_chars(t, PL_ATOM | REP_UTF8, -1, sym_name(v));
   else if (jl_is_quotenode(v)) rc = result_error("expression", "<QuoteNode>");
   else if (jl_is_expr(v)) {
      int  i, n = jl_expr_nargs(v);
      char *name = jl_symbol_name(((jl_expr_t *)v)->head);
      term_t a = PL_new_term_refs(n);

      switch (n) {
         case 1: rc=PL_unify_term(t, PL_FUNCTOR_CHARS, name, n, PL_TERM, a); break;
         case 2: rc=PL_unify_term(t, PL_FUNCTOR_CHARS, name, n, PL_TERM, a, PL_TERM, a+1); break;
         case 3: rc=PL_unify_term(t, PL_FUNCTOR_CHARS, name, n, PL_TERM, a, PL_TERM, a+1, PL_TERM, a+2); break;
         case 4: rc=PL_unify_term(t, PL_FUNCTOR_CHARS, name, n, PL_TERM, a, PL_TERM, a+1, PL_TERM, a+2, PL_TERM, a+3); break;
         otherwise: rc=FALSE;
      }
      for (i=0; rc && i<n; i++) rc &= unify_tree(a+i, jl_exprarg(v,i));
   } else rc = result_error("expression", jl_symbol_name(((jl_datatype_t *)jl_typeof(v))->name->name));
   return rc;
}

static int unify_expr(term_t t, jl_value_t *v) {
   term_t root = PL_new_term_ref();
   return PL_unify_functor(t, colon_1)
       && PL_get_arg(1, t, root)
       && unify_tree(root, v);
}

static int jval_term(jl_value_t *, term_t);

static int unify_tuple(term_t t, jl_datatype_t *type, jl_value_t *v) {
   int arity = jl_nparams(type), rc, i;
   functor_t hash_n = PL_new_functor(hash, arity);
   rc = PL_unify_functor(t, hash_n);
   for (i=0; rc && i<arity; i++) {
      term_t ai = PL_new_term_ref();
      rc = PL_unify_arg(i+1, t, ai) && jval_term(jl_get_nth_field(v,i), ai);
   }
   return rc;
}

static int unify_md_array(term_t t, jl_array_t *v) {
   term_t l = PL_new_term_ref(), shape=PL_new_term_ref();
   jl_datatype_t *et=(jl_datatype_t *)jl_array_eltype((jl_value_t *)v);
   array_unifier_t *unifier;
   void *pdata = jl_array_data(v);
   int   ndims = jl_array_ndims(v);
   size_t *dims = &(v->nrows);
   char *fname;

   if      (et==jl_float64_type) { unifier = float64_unifier; fname = "float64"; }
   else if (et==jl_int64_type)   { unifier = int64_unifier; fname = "int64"; }
   else    return result_error("array", jl_symbol_name(et->name->name));

   return unify_reversed_list_sizes(shape, dims, ndims)
       && unify_nested(unifier, l, ndims, dims, &pdata)
       && PL_unify_term(t, PL_FUNCTOR_CHARS, fname, 2, PL_TERM, shape, PL_TERM, l);
}

static int jval_term(jl_value_t *v, term_t t) {
   jl_datatype_t *dt = (jl_datatype_t *)jl_typeof(v);
   int rc;

   if      (dt==jl_float64_type) rc = PL_unify_float(t, jl_unbox_float64(v));
   else if (dt==jl_int64_type)   rc = PL_unify_integer(t, jl_unbox_int64(v));
   else if (jl_is_string(v))     rc = PL_unify_chars(t, PL_STRING | REP_UTF8, -1, jl_string_ptr(v));
   else if (jl_is_bool(v))       rc = PL_unify_atom_chars(t, jl_unbox_bool(v) ? "true" : "false");
   else if (jl_is_symbol(v))     rc = PL_unify_term(t, PL_FUNCTOR_CHARS, ":", 1, PL_UTF8_CHARS, sym_name(v));
   else if (jl_is_nothing(v))    rc = PL_unify_atom_chars(t, "nothing");
   else if (jl_is_array(v))      rc = unify_md_array(t, (jl_array_t *)v);
   else if (jl_is_tuple(v))      rc = unify_tuple(t, dt, v);
   else if (jl_is_expr(v))       rc = unify_expr(t, v);
   else rc = result_error("result", jl_symbol_name(((jl_datatype_t *)jl_typeof(v))->name->name));
   return rc;
}

static int get_expr(term_t a, jl_value_t **pv) {
   char *x;
   if (PL_is_atom(a)) {
      return PL_get_chars(a,&x,CVT_ATOM | REP_UTF8) 
          && (*pv=(jl_value_t *)jl_symbol(x), TRUE);
   }
   // FIXME I don't know how to construct a jl_expr...
   return FALSE;
}

static int get_array(int n, term_t vals, jl_value_t **pv) {
   jl_value_t *array_type = jl_apply_array_type((jl_value_t *)jl_float64_type, 1); // ndims = 1
   jl_array_t *x          = jl_alloc_array_1d(array_type, n); // also array_2d...
   return get_list_doubles(vals, n, (double *)jl_array_data(x))
       && (*pv = (jl_value_t *)x, TRUE);
}

static int get_md_array(jl_datatype_t *jl_type, array_getter_t *getter,
                        int64_t ndims, int64_t *dims, term_t vals, jl_value_t **pv) {
   int64_t n, i;
   jl_value_t *array_type = jl_apply_array_type((jl_value_t *)jl_type, ndims);
   jl_array_t *x;
   void *pdata;
   switch (ndims) {
      case 1: x = jl_alloc_array_1d(array_type, dims[0]); break;
      case 2: x = jl_alloc_array_2d(array_type, dims[0], dims[1]); break;
      case 3: x = jl_alloc_array_3d(array_type, dims[0], dims[1], dims[2]); break;
   }
   pdata = jl_array_data(x);
   return get_nested(getter, vals, ndims, dims, &pdata)
       && (*pv = (jl_value_t *)x, TRUE);
}

static void reverse(int64_t n, int64_t *x, int64_t *y) { for (int i=n; i>0; i--) y[i-1] = x[n-i]; }

static int term_to_array(jl_datatype_t *type, array_getter_t *getter, term_t t, jl_value_t **pv) {
   term_t vals=PL_new_term_ref(), shape=PL_new_term_ref();
   int64_t revdims[3], dims[3], ndims;
   return PL_get_arg(1,t,shape) && PL_get_arg(2,t,vals)
       && get_list_integers(shape, 3, &ndims, revdims)
       && (reverse(ndims, revdims, dims), TRUE)
       && get_md_array(type, getter, ndims, dims, vals, pv);
}

static int term_jval(term_t, jl_value_t **);

static int term_tuple(term_t t, int arity, jl_value_t **pv) {
   int rc=TRUE, i;
   jl_value_t **args = calloc(arity, sizeof(jl_value_t *));
   jl_value_t **types = calloc(arity, sizeof(jl_value_t *));
   term_t ai = PL_new_term_ref();

   for (i=0; rc && i<arity; i++) {
      if (PL_get_arg(i+1, t, ai) && term_jval(ai, &args[i])) types[i] = jl_typeof(args[i]);
      else rc=FALSE;
   }
   if (rc) {
      jl_tupletype_t *tt = jl_apply_tuple_type_v(types, arity);
      *pv = jl_new_structv(tt, args, arity);
   }
   // !!! free arrays here or give types to Julia GC?
   free(args); free(types);
   return rc;
}

static int term_jval(term_t t, jl_value_t **pv) {
   switch (PL_term_type(t)) {
      case PL_INTEGER: {
         int64_t x;
         return PL_get_int64(t,&x) && (*pv=jl_box_int64(x), TRUE);
      }
      case PL_FLOAT: {
         double x;
         return PL_get_float(t,&x) && (*pv=jl_box_float64(x), TRUE);
      }
      case PL_STRING: {
         char *x;
         return PL_get_chars(t,&x,CVT_STRING | BUF_RING | REP_UTF8)
             && (*pv=jl_cstr_to_string(x), TRUE);
      }
      case PL_ATOM: {
         char *x;
         if (!PL_get_chars(t,&x,CVT_ATOM)) return FALSE;
         if (!strcmp(x,"true"))       { *pv = jl_true; return TRUE; }
         else if (!strcmp(x,"false")) { *pv = jl_false; return TRUE; }
         else if (!strcmp(x,"nothing")) { *pv = jl_nothing; return TRUE; }
         break;
      }
      case PL_TERM: {
         atom_t head;
         int    arity;
         const char *name;
         if (!PL_get_name_arity(t,&head,&arity)) return FALSE;
         name = PL_atom_chars(head);
			if (!strcmp("#", name)) return term_tuple(t, arity, pv);
         switch (arity) {
            case 1:
               if (!strcmp(":", name)) {
                  term_t a=PL_new_term_ref();
                  return PL_get_arg(1,t,a) && get_expr(a,pv);
               }
               break;
            case 2:
               if (!strcmp("arr", name)) {
                  term_t vals=PL_new_term_ref(), len=PL_new_term_ref();
                  int n;
                  return PL_get_arg(1,t,len) && PL_get_arg(2,t,vals)
                      && PL_get_integer(len, &n)
                      && get_array(n,vals,pv);
               } else if (!strcmp("int64", name)) {
                  return term_to_array(jl_int64_type, int64_getter, t, pv);
               } else if (!strcmp("float64", name)) {
                  return term_to_array(jl_float64_type, float64_getter, t, pv);
               }
               break;
         }
         break;
      }
   }
   return type_error(t, "integer | float | string | bool | symbol | void");
}

static int check() {
   if (!jl_exception_occurred()) return TRUE;
   else {
      term_t ex = PL_new_term_ref();

      jl_call2(jl_get_function(jl_base_module, "showerror"),
               jl_stderr_obj(),
               jl_exception_occurred());
      jl_printf(jl_stderr_stream(), "\n");

      return PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
                           PL_FUNCTOR_CHARS, "julia_error", 0,
                           PL_VARIABLE)
         &&  PL_raise_exception(ex);
   }
}

foreign_t pjl_exec(term_t expr) {
   char *str;
   jl_value_t *jval;
   return term_to_utf8_string(expr, &str)
       && (jval=jl_eval_string(str), check());
}

foreign_t pjl_eval(term_t expr, term_t result) {
   char *str;
   jl_value_t *jval;
   return term_to_utf8_string(expr, &str)
       && (jval=jl_eval_string(str), check())
       && jval_term(jval, result);
}

static int apply(jl_value_t *jfn, int N, term_t list, jl_value_t **pval) {
   jl_value_t **jargs = calloc(N, sizeof(jl_value_t *));
   term_t head = PL_new_term_ref();
   int i, rc=(jargs != NULL);

   list=PL_copy_term_ref(list);
   for (i=0;rc && i<N && PL_get_list(list,head,list);i++)
      rc = term_jval(head, &jargs[i]);
   rc = rc && PL_get_nil(list)
           && (*pval=jl_call(jfn, jargs, N), check());
   free(jargs);
   return rc;
}

static int apply_jval(term_t fn, term_t n, term_t args, jl_value_t **pval) {
   char *str;
   int N;
   jl_value_t *jfn;
   return term_to_utf8_string(fn, &str)
       && (jfn=jl_eval_string(str), check())
       && PL_get_integer(n, &N)
       && apply(jfn, N, args, pval);
}

foreign_t pjl_apply(term_t fn, term_t n, term_t args, term_t res) {
   jl_value_t *jval;
   return apply_jval(fn, n, args, &jval)
       && jval_term(jval, res);
}

foreign_t pjl_apply_(term_t fn, term_t n, term_t args) {
   jl_value_t *jval;
   return apply_jval(fn, n, args, &jval);
}
