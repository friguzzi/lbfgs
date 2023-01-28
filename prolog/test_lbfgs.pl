
:- use_module(library(plunit)).

:- begin_tests(ex1, []).
:-ensure_loaded(library(examples_lbfgs/ex1)).

test(ex1):-
  demo(Sol,BestF),
  Sol< 4.712389+0.0001,
  Sol> 4.712389-0.0001,
  BestF< -1.0+0.0001,
  BestF> -1.0-0.0001.

:- end_tests(ex1).

:- begin_tests(ex2, []).
:-ensure_loaded(library(examples_lbfgs/ex2)).

test(ex2):-
  demo(X0,X1,BestF),
  X0< 2.0+0.0001,
  X0> 2.0-0.0001,
  X1< 1.0+0.0001,
  X0> 1.0-0.0001,
  BestF< 0.0+0.0001,
  BestF> 0.0-0.0001.

:- end_tests(ex2).


