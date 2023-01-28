#include <string.h>
#include <SWI-Prolog.h>
#include <lbfgs.h>
#include <stdio.h>

/*
  This file is part of YAP-LBFGS.
  Copyright (C) 2009 Bernd Gutmann

  YAP-LBFGS is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  YAP-LBFGS is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with YAP-LBFGS.  If not, see <http://www.gnu.org/licenses/>.
*/

#define RETURN_IF_FAIL if (ret!=TRUE) return ret;


// These constants describe the internal state
#define OPTIMIZER_STATUS_NONE        0
#define OPTIMIZER_STATUS_INITIALIZED 1
#define OPTIMIZER_STATUS_RUNNING     2
#define OPTIMIZER_STATUS_CB_EVAL     3
#define OPTIMIZER_STATUS_CB_PROGRESS 4

install_t init_lbfgs_predicates( void ) ;

typedef struct
{
int optimizer_status;   // the internal state
int n;                                        // the size of the parameter vector
lbfgsfloatval_t *x;                           // pointer to the parameter vector x[0],...,x[n-1]
lbfgsfloatval_t *g;                           // pointer to the gradient vector g[0],...,g[n-1]
lbfgs_parameter_t param;                      // the parameters used for lbfgs
module_t module;
functor_t fcall3, fprogress8;
record_t extra_arg;
} environment;

static lbfgsfloatval_t evaluate(
    void *instance,
    const lbfgsfloatval_t *x,
    lbfgsfloatval_t *g_tmp,
    const int n,
    const lbfgsfloatval_t step
    )
{
  environment *env=(environment*)instance;
  term_t call=PL_new_term_ref();
  term_t env_term=PL_new_term_ref();
  term_t nterm=PL_new_term_ref();
  term_t stepterm=PL_new_term_ref();
  term_t var=PL_new_term_ref();
  double floatres;
  int result,intres;
//  YAP_Int s1;

//  YAP_Term t[3];

//  t[0] = YAP_MkVarTerm();
//  t[1] = YAP_MkIntTerm(n);
//  t[2] = YAP_MkFloatTerm(step);
  
//  call = YAP_MkApplTerm(fcall3, 3, t);
  PL_put_variable(var);
  PL_put_integer(nterm,env->n);
  PL_put_float(stepterm,step);
  term_t extra_arg_term=PL_new_term_ref();

  PL_recorded(env->extra_arg,extra_arg_term);
  int ret=PL_put_pointer(env_term,(void *) env);
  RETURN_IF_FAIL
//  PL_put_term_from_chars(extra_arg,REP_UTF8,(size_t)-1,"plunit_bongard");
  PL_cons_functor(call,env->fcall3,env_term,var,nterm,stepterm,extra_arg_term);
  env->g=g_tmp;  

//  s1 = YAP_InitSlot(call);
  env->optimizer_status=OPTIMIZER_STATUS_CB_EVAL;
  result=PL_call(call,env->module);
//  result=YAP_CallProlog(call);
  env->optimizer_status=OPTIMIZER_STATUS_RUNNING;

  if (result==FALSE) {
    printf("ERROR: Calling the evaluate call back function in YAP.\n");
    // Goal did not succeed
    return FALSE;
  }

//  call = YAP_GetFromSlot( s1 );

//  a1 = YAP_ArgOfTerm(1,call);
  if (PL_is_float(var)) {
//      YAP_ShutdownGoal( TRUE );
      PL_get_float(var,&floatres);
//  printf("float %f\n",floatres);
      return (lbfgsfloatval_t) floatres;
  } else if (PL_is_integer(var)) {
//    YAP_ShutdownGoal( TRUE );
//      printf("int\n");
      PL_get_integer(var,&intres);
    return (lbfgsfloatval_t) intres;
  }

  fprintf(stderr, "ERROR: The evaluate call back function did not return a number as first argument.\n");
  return 0;
}

static int progress(
   void *instance,
    const lbfgsfloatval_t *local_x,
    const lbfgsfloatval_t *local_g,
    const lbfgsfloatval_t fx,
    const lbfgsfloatval_t xnorm,
    const lbfgsfloatval_t gnorm,
    const lbfgsfloatval_t step,
    int n,
    int k,
    int ls
    )
{
  environment *env=(environment*)instance;
  term_t call=PL_new_term_ref();
  term_t env_term=PL_new_term_ref();
  term_t fxterm=PL_new_term_ref();
  term_t xnormterm=PL_new_term_ref();
  term_t gnormterm=PL_new_term_ref();
  term_t stepterm=PL_new_term_ref();
  term_t nterm=PL_new_term_ref();
  term_t kterm=PL_new_term_ref();
  term_t lsterm=PL_new_term_ref();
  term_t var=PL_new_term_ref();
  int result,intres;

  PL_put_float(fxterm,fx);
  PL_put_float(xnormterm,xnorm);
  PL_put_float(gnormterm,gnorm);
  PL_put_float(stepterm,step);
  PL_put_integer(nterm,n);
  PL_put_integer(kterm,k);
  PL_put_integer(lsterm,ls);
  PL_put_variable(var);
  int ret=PL_put_pointer(env_term,(void *) env);
  RETURN_IF_FAIL

  term_t extra_arg_term=PL_new_term_ref();

  PL_recorded(env->extra_arg,extra_arg_term);

//    extra_arg=PL_new_term_ref();

//  PL_put_term_from_chars(extra_arg,REP_UTF8,(size_t)-1,"plunit_bongard");


  PL_cons_functor(call,env->fprogress8,env_term,fxterm,xnormterm,gnormterm,stepterm,
  nterm,kterm,lsterm,var,extra_arg_term);


  env->optimizer_status=OPTIMIZER_STATUS_CB_PROGRESS;
  result=PL_call(call,env->module);
  env->optimizer_status=OPTIMIZER_STATUS_RUNNING;


  if (result==FALSE) {
   printf("ERROR: Calling the progress call back function in YAP.\n");
    // Goal did not succeed
    return FALSE;
  }

  if (PL_is_integer(var))
  {
    PL_get_integer(var,&intres);
    return intres;
  }

  fprintf(stderr, "ERROR: The progress call back function did not return an integer as last argument\n");
  return 1;
}


static foreign_t set_x_value(term_t t1,term_t t2, term_t t3) {
  int i=0,xiint;
  double xi;
  int ret;
  environment *env;

  ret=PL_get_pointer(t1,(void **)&env);
  RETURN_IF_FAIL

  if (env->optimizer_status!=OPTIMIZER_STATUS_INITIALIZED) {
    printf("ERROR: set_x_value/2 can be called only when the optimizer is initialized and not running.\n");
    PL_fail;
  }
  
  if (PL_is_integer(t2)) {
    PL_get_integer(t2,&i);
  } else {
    PL_succeed;
  }

  if (i<0 || i>=env->n) {
    printf("ERROR: invalid index for set_x_value/2.\n");
    PL_fail;
  }

  if (PL_is_float(t3)) {
    PL_get_float(t3,&xi);
    env->x[i]=(lbfgsfloatval_t) xi;
  } else if (PL_is_integer(t3)) {
    PL_get_integer(t3,&xiint);
    env->x[i]=(lbfgsfloatval_t) xiint;
  } else {
    PL_fail;
  }

  PL_succeed;
}

static foreign_t get_x_value(term_t t1,term_t t2, term_t t3) {
  int i=0;
  term_t xi=PL_new_term_ref();
  int ret;
  environment *env;

  ret=PL_get_pointer(t1,(void **)&env);
  RETURN_IF_FAIL

  if (env->optimizer_status==OPTIMIZER_STATUS_NONE) {
    printf("ERROR: set_x_value/2 can be called only when the optimizer is initialized.\n");
    PL_fail;
  }
  
  if (PL_is_integer(t2)) {
    PL_get_integer(t2,&i);
  } else {
    PL_fail;
  }

  if (i<0 || i>=env->n) {
    printf("ERROR: invalid index for set_x_value/2.\n");
    PL_fail;
  }
  PL_put_float(xi,env->x[i]);
  return PL_unify(t3,xi);
}




static foreign_t set_g_value(term_t t1,term_t t2, term_t t3) {
  int i=0,giint;
  double gi;
  int ret;
  environment *env;

  ret=PL_get_pointer(t1,(void **)&env);
  RETURN_IF_FAIL

  if (env->optimizer_status != OPTIMIZER_STATUS_CB_EVAL) {
    printf("ERROR: optimizer_set_g/2 can only be called by the evaluation call back function.\n");
    return FALSE;
  }
  
  if (PL_is_integer(t2)) {
    PL_get_integer(t2,&i);
  } else {
    PL_fail;
  }

  if (i<0 || i>=env->n) {
    PL_fail;
  }

  if (PL_is_float(t3)) {
    PL_get_float(t3,&gi);
    env->g[i]=(lbfgsfloatval_t) gi;
  } else if (PL_is_integer(t3)) {
    PL_get_integer(t3,&giint);
    env->g[i]=(lbfgsfloatval_t) giint;
  } else {
    PL_fail;
  }
  PL_succeed;

}

static foreign_t get_g_value(term_t t1,term_t t2, term_t t3) {
  int i=0;
  term_t gi=PL_new_term_ref();
  int ret;
  environment *env;

  ret=PL_get_pointer(t1,(void **)&env);
  RETURN_IF_FAIL

  if (env->optimizer_status != OPTIMIZER_STATUS_RUNNING && env->optimizer_status != OPTIMIZER_STATUS_CB_EVAL && env->optimizer_status != OPTIMIZER_STATUS_CB_PROGRESS) {
    printf("ERROR: optimizer_get_g/2 can only be called while the optimizer is running.\n");
    PL_fail;
  }
  
  if (PL_is_integer(t2)) {
    PL_get_integer(t2,&i);
  } else {
    PL_fail;
  }

  if (i<0 || i>=env->n) {
    PL_fail;
  }
  PL_put_float(gi,env->g[i]);
  return PL_unify(t3,gi);
}


static foreign_t optimizer_initialize(term_t t1, term_t t2, term_t t3, term_t t4, term_t t5, term_t t6) {
  int temp_n=0;
 
//printf("LBFGSERR_ROUNDING_ERROR %d\n",LBFGSERR_ROUNDING_ERROR);
  term_t env_t;

  if (! PL_is_integer(t1)) {
    PL_fail;
  }

  PL_get_integer(t1,&temp_n);

  if (temp_n<1) {
    PL_fail;
  }
  environment * env;
  env=(environment *)malloc(sizeof(environment));
    env_t=PL_new_term_ref();
  env->x = lbfgs_malloc(temp_n);
  //Initialize the parameters for the L-BFGS optimization.
  lbfgs_parameter_init(&(env->param));
  if (env->x == NULL) {
        printf("ERROR: Failed to allocate a memory block for variables.\n");
        PL_fail;
  }
  
  PL_get_module(t2, &env->module);
  env->n=temp_n;
  atom_t fcall3atom;
  atom_t fprogress8atom;
  PL_get_atom(t3, &fcall3atom);
  PL_get_atom(t4, &fprogress8atom);
  env->fcall3=PL_new_functor(fcall3atom,5);
  env->fprogress8=PL_new_functor(fprogress8atom,10);
  env->extra_arg=PL_record(t5);

  env->optimizer_status=OPTIMIZER_STATUS_INITIALIZED;


  int ret=PL_put_pointer(env_t,(void *) env);
  RETURN_IF_FAIL
  return(PL_unify(env_t,t6));
}

static foreign_t optimizer_run(term_t t1,term_t t2, term_t t3) {
  int ret = 0;
  term_t s1=PL_new_term_ref();
  term_t s2=PL_new_term_ref();
  lbfgsfloatval_t fx;
  lbfgsfloatval_t * tmp_x;
  environment *env;

  ret=PL_get_pointer(t1,(void **)&env);
  RETURN_IF_FAIL
  tmp_x=env->x;

 if (env->optimizer_status == OPTIMIZER_STATUS_NONE) {
    printf("ERROR: Memory for parameter vector not initialized, please call optimizer_initialize/1 first.\n");
    return FALSE;
  }
  
  if (env->optimizer_status != OPTIMIZER_STATUS_INITIALIZED) {
    printf("ERROR: Optimizer is running right now. Please wait till it is finished.\n");
    return FALSE;
  }

  
  // both arguments have to be variables
  if (! PL_is_variable(t2) || ! PL_is_variable(t3)) {
    return FALSE;
  }
  env->optimizer_status = OPTIMIZER_STATUS_RUNNING;
  ret = lbfgs(env->n, env->x, &fx, evaluate, progress, (void *)env, &(env->param));
  env->x=tmp_x;
  env->optimizer_status = OPTIMIZER_STATUS_INITIALIZED;
  
  PL_put_float(s1,fx);
  PL_put_integer(s2,ret);
  PL_unify(t2,s1);
  PL_unify(t3,s2);

  return TRUE;
}



static foreign_t optimizer_finalize( term_t t1 ) {
  int ret;
  environment *env;

  ret=PL_get_pointer(t1,(void **)&env);
  RETURN_IF_FAIL  
  if (env->optimizer_status == OPTIMIZER_STATUS_NONE) {
     printf("Error: Optimizer is not initialized.\n");
     return FALSE;
  }

  if (env->optimizer_status == OPTIMIZER_STATUS_INITIALIZED) {
      lbfgs_free(env->x);
      env->optimizer_status = OPTIMIZER_STATUS_NONE;
      PL_erase(env->extra_arg);
      free(env);
      return TRUE;
  }

  printf("ERROR: Optimizer is running right now. Please wait till it is finished.\n");
  return FALSE;
 }



static foreign_t optimizer_set_parameter(term_t t1,term_t t2, term_t t3) {
  char * name;
  int ret;
  environment *env;

  ret=PL_get_pointer(t1,(void **)&env);
  RETURN_IF_FAIL
  if (env->optimizer_status != OPTIMIZER_STATUS_NONE && env->optimizer_status != OPTIMIZER_STATUS_INITIALIZED){
    printf("ERROR: Optimizer is running right now. Please wait till it is finished.\n");
    return FALSE;
  }


  if (! PL_is_atom(t2)) {
    return FALSE;
  }

  PL_get_atom_chars(t2,&name);

  if ((strcmp(name, "m") == 0)) {
    if (! PL_is_integer(t3)) {
	return FALSE;
    }
    PL_get_integer(t3,&(env->param.m));
  } else if  ((strcmp(name, "epsilon") == 0)) {
    lbfgsfloatval_t v;
      
    if (PL_is_float(t3)) {
      PL_get_float(t3,&v);
    } else if (PL_is_integer(t3)) {
      int vi;
      PL_get_integer(t3,&vi);
      v=(lbfgsfloatval_t) vi;
    } else {
      return FALSE;
    }

    env->param.epsilon=v;
  } else if  ((strcmp(name, "past") == 0)) {
    if (! PL_is_integer(t3)) {
	return FALSE;
    }
    PL_get_integer(t3,&(env->param.past));
  } else if  ((strcmp(name, "delta") == 0)) {
    lbfgsfloatval_t v;
      
    if (PL_is_float(t3)) {
      PL_get_float(t3,&v);
    } else if (PL_is_integer(t3)) {
      int vi;
      PL_get_integer(t3,&vi);
      v=(lbfgsfloatval_t) vi;
    } else {
      return FALSE;
    }

    env->param.delta=v;
  } else if  ((strcmp(name, "max_iterations") == 0)) {
    if (! PL_is_integer(t3)) {
	return FALSE;
    }
    PL_get_integer(t3,&(env->param.max_iterations));
  } else if  ((strcmp(name, "linesearch") == 0)) {
    if (! PL_is_integer(t3)) {
	return FALSE;
    }
    PL_get_integer(t3,&(env->param.linesearch));
  } else if  ((strcmp(name, "max_linesearch") == 0)) {
    if (! PL_is_integer(t3)) {
	return FALSE;
    }
    PL_get_integer(t3,&(env->param.max_linesearch));
  } else if  ((strcmp(name, "min_step") == 0)) {
    lbfgsfloatval_t v;
      
    if (PL_is_float(t3)) {
      PL_get_float(t3,&v);
    } else if (PL_is_integer(t3)) {
      int vi;
      PL_get_integer(t3,&vi);
      v=(lbfgsfloatval_t) vi;
    } else {
      return FALSE;
    }

    env->param.min_step=v;
  } else if  ((strcmp(name, "max_step") == 0)) {
    lbfgsfloatval_t v;
    if (PL_is_float(t3)) {
      PL_get_float(t3,&v);
    } else if (PL_is_integer(t3)) {
      int vi;
      PL_get_integer(t3,&vi);
      v=(lbfgsfloatval_t) vi;
    } else {
      return FALSE;
    }

    env->param.max_step=v;
  } else if  ((strcmp(name, "ftol") == 0)) {
    lbfgsfloatval_t v;
    if (PL_is_float(t3)) {
      PL_get_float(t3,&v);
    } else if (PL_is_integer(t3)) {
      int vi;
      PL_get_integer(t3,&vi);
      v=(lbfgsfloatval_t) vi;
    } else {
      return FALSE;
    }

    env->param.ftol=v;
  } else if  ((strcmp(name, "gtol") == 0)) {
    lbfgsfloatval_t v;
      
    if (PL_is_float(t3)) {
      PL_get_float(t3,&v);
    } else if (PL_is_integer(t3)) {
      int vi;
      PL_get_integer(t3,&vi);
      v=(lbfgsfloatval_t) vi;
    } else {
      return FALSE;
    }

    env->param.gtol=v;
  } else if  ((strcmp(name, "xtol") == 0)) {
    lbfgsfloatval_t v;
      
    if (PL_is_float(t3)) {
      PL_get_float(t3,&v);
    } else if (PL_is_integer(t3)) {
      int vi;
      PL_get_integer(t3,&vi);
      v=(lbfgsfloatval_t) vi;
    } else {
      return FALSE;
    }

    env->param.xtol=v;
  } else if  ((strcmp(name, "orthantwise_c") == 0)) {
    lbfgsfloatval_t v;
      
    if (PL_is_float(t3)) {
      PL_get_float(t3,&v);
    } else if (PL_is_integer(t3)) {
      int vi;
      PL_get_integer(t3,&vi);
      v=(lbfgsfloatval_t) vi;
    } else {
      return FALSE;
    }

    env->param.orthantwise_c=v;
  } else if  ((strcmp(name, "orthantwise_start") == 0)) {
    if (! PL_is_integer(t3)) {
	return FALSE;
    }
    PL_get_integer(t3,&(env->param.orthantwise_start));
  } else if  ((strcmp(name, "orthantwise_end") == 0)) {
    if (! PL_is_integer(t3)) {
	return FALSE;
    }
    PL_get_integer(t3,&(env->param.orthantwise_end));
  } else {
      printf("ERROR: The parameter %s is unknown.\n",name);
      return FALSE;
  }
  
  return TRUE;
}

static foreign_t optimizer_get_parameter(term_t t1, term_t t2, term_t t3) {
  char * name;
  term_t s2=PL_new_term_ref();
  int ret;
  environment *env;

  ret=PL_get_pointer(t1,(void **)&env);
  RETURN_IF_FAIL
  if (! PL_is_atom(t2)) {
    return FALSE;
  }
  PL_get_atom_chars(t2,&name);

  if ((strcmp(name, "m") == 0)) {
    PL_put_integer(s2,env->param.m);
    return PL_unify(t3,s2);
  } else if  ((strcmp(name, "epsilon") == 0)) {
    PL_put_float(s2,env->param.epsilon);
    return PL_unify(t3,s2);
  } else if  ((strcmp(name, "past") == 0)) {
    PL_put_integer(s2,env->param.past);
    return PL_unify(t3,s2);
  } else if  ((strcmp(name, "delta") == 0)) {
    PL_put_float(s2,env->param.delta);
    return PL_unify(t3,s2);
  } else if  ((strcmp(name, "max_iterations") == 0)) {
    PL_put_integer(s2,env->param.max_iterations);
    return PL_unify(t3,s2);
  } else if  ((strcmp(name, "linesearch") == 0)) {
    PL_put_integer(s2,env->param.linesearch);
    return PL_unify(t3,s2);
  } else if  ((strcmp(name, "max_linesearch") == 0)) {
    PL_put_integer(s2,env->param.max_linesearch);
    return PL_unify(t3,s2);
  } else if  ((strcmp(name, "min_step") == 0)) {
    PL_put_float(s2,env->param.min_step);
    return PL_unify(t3,s2);
  } else if  ((strcmp(name, "max_step") == 0)) {
    PL_put_float(s2,env->param.max_step);
    return PL_unify(t3,s2);
  } else if  ((strcmp(name, "ftol") == 0)) {
    PL_put_float(s2,env->param.ftol);
    return PL_unify(t3,s2);
  } else if  ((strcmp(name, "gtol") == 0)) {
    PL_put_float(s2,env->param.gtol);
    return PL_unify(t3,s2);
  } else if  ((strcmp(name, "xtol") == 0)) {
    PL_put_float(s2,env->param.xtol);
    return PL_unify(t3,s2);
  } else if  ((strcmp(name, "orthantwise_c") == 0)) {
    PL_put_float(s2,env->param.orthantwise_c);
    return PL_unify(t3,s2);
  } else if  ((strcmp(name, "orthantwise_start") == 0)) {
    PL_put_float(s2,env->param.orthantwise_start);
    return PL_unify(t3,s2);
  } else if  ((strcmp(name, "orthantwise_end") == 0)) {
    PL_put_float(s2,env->param.orthantwise_end);
    return PL_unify(t3,s2);
  }

  printf("ERROR: The parameter %s is unknown.\n",name);
  return FALSE;
}





install_t init_lbfgs_predicates( void ) 
{ 
 
//  fcall3 = YAP_MkFunctor(YAP_LookupAtom("$lbfgs_callback_evaluate"), 3);
//  fprogress8 = YAP_MkFunctor(YAP_LookupAtom("$lbfgs_callback_progress"), 8);




  PL_register_foreign("optimizer_reserve_memory",6,optimizer_initialize,0);
  PL_register_foreign("optimizer_run",3,optimizer_run,0);
  PL_register_foreign("optimizer_free_memory",1,optimizer_finalize,0);

  PL_register_foreign("optimizer_set_x",3,set_x_value,0);
  PL_register_foreign("optimizer_get_x",3,get_x_value,0);
  PL_register_foreign("optimizer_set_g",3,set_g_value,0);
  PL_register_foreign("optimizer_get_g",3,get_g_value,0);

  PL_register_foreign("optimizer_set_parameter",3,optimizer_set_parameter,0);
  PL_register_foreign("optimizer_get_parameter",3,optimizer_get_parameter,0);
}  
