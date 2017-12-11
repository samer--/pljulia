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

static functor_t int_1, float_1, double_1, string_1;

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


/* // Convert Matlab numerical (REAL) array to list */
/* foreign_t mlMxGetReals(term_t mxterm, term_t a) */
/* { */
/*   try { */
/*     mxArray *mx = term_to_mx(mxterm); */
/*     int       n = mxGetNumberOfElements(mx); */

/*     if (!mxIsDouble(mx)) return PL_type_error("mx(double)",mxterm); */
/*     return unify_list_doubles(a,mxGetPr(mx),n); */
/*   } catch (PlException &e) { */
/*     return e.plThrow(); */
/*   } */
/* } */


// ---------------------------------------------------------------------------

install_t install();

foreign_t pjl_open();
foreign_t pjl_close();
foreign_t pjl_exec(term_t expr);
foreign_t pjl_eval(term_t expr, term_t res);

// BLOB functions
/* int arr_release(atom_t a) { */
/* 	PL_blob_t *type; */
/* 	size_t    len; */
/* 	void *p=PL_blob_data(a,&len,&type); */
/* 	/1* if (p) lo_address_free(*(lo_address *)p); *1/ */
/* 	return TRUE; */
/* } */

static int pjl_on_halt(int rc, void *p) {
	jl_atexit_hook(rc);
	return 0;
}

install_t install() { 
	PL_register_foreign("jl_open",   0, (void *)pjl_open, 0);
	PL_register_foreign("jl_close",   0, (void *)pjl_open, 0);
	PL_register_foreign("jl_exec",   1, (void *)pjl_exec, 0);
	PL_register_foreign("jl_eval",   2, (void *)pjl_eval, 0);

	/* arr_blob.magic = PL_BLOB_MAGIC; */
	/* arr_blob.flags = PL_BLOB_UNIQUE; */
	/* arr_blob.name = "julia_value"; */
	/* arr_blob.acquire = 0; */
	/* arr_blob.release = arr_release; */
	/* arr_blob.write   = 0; // arr_write; */
	/* arr_blob.compare = 0; */ 

   int_1     = PL_new_functor(PL_new_atom("int"),1);
   float_1   = PL_new_functor(PL_new_atom("float"),1);
   double_1  = PL_new_functor(PL_new_atom("double"),1);
   string_1  = PL_new_functor(PL_new_atom("string"),1);

	printf("Opening Julia...\n");
	jl_init();
	PL_on_halt(pjl_on_halt, 0);
}

/* utility function to extract UTF-8 encoded character array from 
 * a Prolog string, code list, or atom. */
static int term_to_utf8_string(term_t t, char **s)
{
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

static int arg_error(const char *type, term_t arg)
{ 
	term_t ex = PL_new_term_ref();
	int rc;

  rc=PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 1,
		      PL_FUNCTOR_CHARS, "arg_error", 2,
		        PL_CHARS, type,
		        PL_TERM, arg);

  return PL_raise_exception(ex);
}

static int julia_error()
{ 
	term_t ex = PL_new_term_ref();
	int rc;

  rc = PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
		      PL_FUNCTOR_CHARS, "julia_error", 0,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}

static int jval_term(jl_value_t *v, term_t head) {
	atom_t name;
	int    rc, arity;
	const char  *type;

	if (!PL_get_name_arity(head,&name,&arity)) return type_error(head,"term");
	type=PL_atom_chars(name);
	switch (arity) {
	case 1: {
			term_t a1=PL_new_term_ref();
			rc=PL_get_arg(1,head,a1); // !!!! check return value

			if (!strcmp(type,"int")) {
            return PL_unify_integer(a1, jl_unbox_int64(v));
			} else if (!strcmp(type,"float")) {
            return PL_unify_float(a1, jl_unbox_float64(v));
			} else if (!strcmp(type,"string")) {
				return PL_unify_chars(a1, PL_STRING | REP_UTF8, -1, jl_string_ptr(v));
				/* return PL_unify_string_chars(a1, jl_string_ptr(v)); */
			} else if (!strcmp(type,"atom")) {
				return PL_unify_chars(a1, PL_ATOM | REP_UTF8, -1, jl_symbol_name((jl_sym_t *)v));
				/* return PL_unify_atom_chars(a1, jl_symbol_name(v)); */
			} else {
				return arg_error(type,head);
			}


			break;
		}
	case 0: return arg_error("unary term", head); // !!!! FIXME
	}
	return FALSE;
}

static int eval_string(char *str, jl_value_t **pv) {
   *pv = jl_eval_string(str);
   if (jl_exception_occurred()) {
      jl_call2(jl_get_function(jl_base_module, "showerror"),
               jl_stderr_obj(),
               jl_exception_occurred());
      jl_printf(jl_stderr_stream(), "\n");
      return julia_error();
   } else return TRUE;
}

foreign_t pjl_open() { return TRUE; }

foreign_t pjl_close() { return TRUE; }

foreign_t pjl_exec(term_t expr) {
   char *str;
   jl_value_t *jval;
   return term_to_utf8_string(expr, &str)
       && eval_string(str, &jval);
}

foreign_t pjl_eval(term_t expr, term_t result) {
   char *str;
   jl_value_t *jval;
   return term_to_utf8_string(expr, &str)
       && eval_string(str, &jval)
       && jval_term(jval, result);
}
