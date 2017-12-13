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

static int unify_list_ints(term_t list, int64_t *x, int n)
{
  list=PL_copy_term_ref(list);

  for (int i=0; i<n; i++) {
    term_t head=PL_new_term_ref();
    term_t tail=PL_new_term_ref();
    if (!PL_unify_list(list,head,tail)) PL_fail;
    if (!PL_unify_int64(head,x[i])) PL_fail;
    list=tail;
  }
  return PL_unify_nil(list);
}

static int unify_list_doubles(term_t list, double *x, int n)
{
  list=PL_copy_term_ref(list);

  for (int i=0; i<n; i++) {
    term_t head=PL_new_term_ref();
    term_t tail=PL_new_term_ref();
    if (!PL_unify_list(list,head,tail)) PL_fail;
    if (!PL_unify_float(head,x[i])) PL_fail;
    list=tail;
  }
  return PL_unify_nil(list);
}

// read list of integers from term and write to int array
int get_list_integers(term_t list, long *len, int64_t *vals)
{
  term_t  head=PL_new_term_ref();
  long    n;

  // copy term ref so as not to modify original
  list=PL_copy_term_ref(list);
  for (n=0;PL_get_list(list,head,list);n++) {
      if (!PL_get_int64(head,&vals[n])) return FALSE;
  }
  if (!PL_get_nil(list)) return FALSE;
  *len=n;
  return TRUE;
}

// read list of floats from term and write to double array
int get_list_doubles(term_t list, long len, double *vals)
{
  term_t  head=PL_new_term_ref();
  long    n;

  // copy term ref so as not to modify original
  list=PL_copy_term_ref(list);
  for (n=0;n<len && PL_get_list(list,head,list);n++) {
      if (!PL_get_float(head,&vals[n])) return FALSE;
  }
  if (!PL_get_nil(list)) return FALSE;
  return TRUE;
}


/* // Convert Matlab numerical (REAL) array to list */
/* foreign_t mlMxGetReals(term_t mxterm, term_t a) */
/* { */
/*     mxArray *mx = term_to_mx(mxterm); */
/*     int       n = mxGetNumberOfElements(mx); */

/*     if (!mxIsDouble(mx)) return PL_type_error("mx(double)",mxterm); */
/*     return unify_list_doubles(a,mxGetPr(mx),n); */
/* } */


// ---------------------------------------------------------------------------

install_t install();

/* foreign_t pjl_open(); */
/* foreign_t pjl_close(); */
foreign_t pjl_exec(term_t expr);
foreign_t pjl_eval(term_t expr, term_t res);
foreign_t pjl_call1(term_t expr, term_t arg1, term_t res);
foreign_t pjl_call2(term_t expr, term_t arg1, term_t arg2, term_t res);

static int pjl_on_halt(int rc, void *p) {
   jl_atexit_hook(rc);
   return 0;
}

install_t install() {
   /* PL_register_foreign("jl_open",   0, (void *)pjl_open, 0); */
   /* PL_register_foreign("jl_close",  0, (void *)pjl_open, 0); */
   PL_register_foreign("jl_exec",   1, (void *)pjl_exec, 0);
   PL_register_foreign("jl_eval",   2, (void *)pjl_eval, 0);
   PL_register_foreign("jl_call",   3, (void *)pjl_call1, 0);
   PL_register_foreign("jl_call",   4, (void *)pjl_call2, 0);

   colon_1 = PL_new_functor(PL_new_atom(":"),1);
   printf("Opening Julia...\n");
   jl_init();
   PL_on_halt(pjl_on_halt, 0);
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

static int unify_array(term_t t, jl_array_t *v) {
   term_t l = PL_new_term_ref();
   int rc, n = jl_array_len(v);
   int ndims = jl_array_ndims(v);
   jl_datatype_t *et=(jl_datatype_t *)jl_array_eltype((jl_value_t *)v);

   if (ndims!=1) return result_error("array", "multidimensional");
   if      (et==jl_float64_type) rc = unify_list_doubles(l, (double *)jl_array_data(v), n);
   else if (et==jl_int64_type)   rc = unify_list_ints(l, (int64_t *)jl_array_data(v), n);
   else return result_error("array", jl_symbol_name(et->name->name));
   return PL_unify_term(t, PL_FUNCTOR_CHARS, "arr", 2, PL_INT, n, PL_TERM, l);
   // FIXME check shape!
   // alternatively, unify as one term with n args?
}
/* size_t size0 = jl_array_dim(x,0); */
/* size_t size1 = jl_array_dim(x,1); */
/* for(size_t i=0; i<size1; i++) */
/*     for(size_t j=0; j<size0; j++) */
/*         p[j + size0*i] = i + j; */

static int jval_term(jl_value_t *v, term_t t) {
   jl_datatype_t *dt = (jl_datatype_t *)jl_typeof(v);
   int rc;

   if      (dt==jl_float64_type) rc = PL_unify_float(t, jl_unbox_float64(v));
   else if (dt==jl_int64_type)   rc = PL_unify_integer(t, jl_unbox_int64(v));
   else if (jl_is_string(v))     rc = PL_unify_chars(t, PL_STRING | REP_UTF8, -1, jl_string_ptr(v));
   else if (jl_is_bool(v))       rc = PL_unify_atom_chars(t, jl_unbox_bool(v) ? "true" : "false");
   else if (jl_is_symbol(v))     rc = PL_unify_term(t, PL_FUNCTOR_CHARS, ":", 1, PL_UTF8_CHARS, sym_name(v));
   else if (jl_is_nothing(v))    rc = PL_unify_atom_chars(t, "nothing");
   else if (jl_is_array(v))      rc = unify_array(t, (jl_array_t *)v);
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

static int get_array(term_t vals, int n, jl_value_t **pv) {
   jl_value_t *array_type = jl_apply_array_type((jl_value_t *)jl_float64_type, 1); // ndims = 1
   jl_array_t *x          = jl_alloc_array_1d(array_type, n); // also array_2d...
   return get_list_doubles(vals, n, (double *)jl_array_data(x))
       && (*pv = (jl_value_t *)x, TRUE);
}
/* double *existingArray = (double*)malloc(sizeof(double)*10); */
/* jl_array_t *x = jl_ptr_to_array_1d(array_type, existingArray, 10, 0); */

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
      }
      case PL_TERM: {
         atom_t name;
         int    arity;
         if (!PL_get_name_arity(t,&name,&arity)) return FALSE;
         if (arity==1 && !strcmp(":", PL_atom_chars(name))) {
            term_t a=PL_new_term_ref();
            return PL_get_arg(1,t,a) && get_expr(a,pv);
         } else if (arity==2 && !strcmp("arr", PL_atom_chars(name))) {
            term_t vals=PL_new_term_ref(), len=PL_new_term_ref();
            int n;
            return PL_get_arg(1,t,len) && PL_get_arg(2,t,vals) 
                && PL_get_integer(len, &n)
                && get_array(vals,n,pv);
         }
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

static int terms_jvals(int n, term_t *ts, jl_value_t **jvs) {
   int i, rc;
   for (i=0, rc=TRUE; rc && i<n; i++) rc=term_jval(ts[i], &jvs[i]);
   return rc;
}

foreign_t pjl_call1(term_t fn, term_t arg1, term_t res) {
   char *str;
   jl_value_t *jfn, *jarg1, *jval;
   return term_to_utf8_string(fn, &str)
       && (jfn=jl_eval_string(str), check())
       && term_jval(arg1, &jarg1)
       && (jval=jl_call1(jfn, jarg1), check())
       && jval_term(jval, res);
}

foreign_t pjl_call2(term_t fn, term_t arg1, term_t arg2, term_t res) {
   char *str;
   jl_value_t *jfn, *jarg1, *jarg2, *jval;
   return term_to_utf8_string(fn, &str)
       && (jfn=jl_eval_string(str), check())
       && term_jval(arg1, &jarg1)
       && term_jval(arg2, &jarg2)
       && (jval=jl_call2(jfn, jarg1, jarg2), check())
       && jval_term(jval, res);
}
