%%% -*- Mode: Prolog; -*-

%  This file is part of YAP-LBFGS.
%  Copyright (C) 2009 Bernd Gutmann
%
%  YAP-LBFGS is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%
%  YAP-LBFGS is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with YAP-LBFGS.  If not, see <http://www.gnu.org/licenses/>.



:- module(lbfgs,[optimizer_initialize/3,
		 optimizer_initialize/4,
		 optimizer_initialize/6,
		 optimizer_run/2,
		 optimizer_get_x/2,
		 optimizer_set_x/2,

		 optimizer_get_g/2,
		 optimizer_set_g/2,

		 optimizer_finalize/0,

		 optimizer_set_parameter/2,
		 optimizer_get_parameter/2,
		 optimizer_parameters/0,
		 interpret_return_value/2]).

% switch on all the checks to reduce bug searching time
% :- yap_flag(unknown,error).
% :- style_check(single_var).

:- dynamic initialized/0.

:-use_foreign_library(foreign(swi_lbfgs),init_lbfgs_predicates).


optimizer_initialize(N,Call_Evaluate,Call_Progress) :-
	optimizer_initialize(N,user,Call_Evaluate,Call_Progress).
optimizer_initialize(N,Module,Call_Evaluate,Call_Progress) :-
	\+ initialized,

	integer(N),
	N>0,

	% check whether there are such call back functions
	current_predicate(Module:Call_Evaluate/3),
	current_predicate(Module:Call_Progress/8),

	optimizer_reserve_memory(N,Module),

	% install call back predicates in the user module which call
	% the predicates given by the arguments		
	EvalGoal =.. [Call_Evaluate,E1,E2,E3],
	ProgressGoal =.. [Call_Progress,P1,P2,P3,P4,P5,P6,P7,P8],
	retractall( Module:'$lbfgs_callback_evaluate'(_E1,_E2,_E3) ),
	retractall( Module:'$lbfgs_callback_progress'(_P1,_P2,_P3,_P4,_P5,_P6,_P7,_P8) ),
	assert( (Module:'$lbfgs_callback_evaluate'(E1,E2,E3) :- Module:EvalGoal, !) ),
	assert( (Module:'$lbfgs_callback_progress'(P1,P2,P3,P4,P5,P6,P7,P8) :- Module:ProgressGoal, !) ),
	assert(initialized).

optimizer_initialize(N,Module,Call_Evaluate,ArgsEv,Call_Progress,ArgsProg) :-
	\+ initialized,

	integer(N),
	N>0,

	% check whether there are such call back functions
	current_module(Module),
	length(ArgsEv,NE),
	ArE is 3+NE,
	length(ArgsProg,NP),
	ArP is 8+NP,
	current_predicate(Module:Call_Evaluate/ArE),
	current_predicate(Module:Call_Progress/ArP),

	optimizer_reserve_memory(N),

	% install call back predicates in the user module which call
	% the predicates given by the arguments		
	EvalGoal =.. [Call_Evaluate,E1,E2,E3|ArgsEv],
	ProgressGoal =.. [Call_Progress,P1,P2,P3,P4,P5,P6,P7,P8|ArgsProg],
	retractall( lbfgs:'$lbfgs_callback_evaluate'(_E1,_E2,_E3) ),
	retractall( lbfgs:'$lbfgs_callback_progress'(_P1,_P2,_P3,_P4,_P5,_P6,_P7,_P8) ),
	assert( (lbfgs:'$lbfgs_callback_evaluate'(E1,E2,E3) :- Module:EvalGoal, !) ),
	assert( (lbfgs:'$lbfgs_callback_progress'(P1,P2,P3,P4,P5,P6,P7,P8) :- Module:ProgressGoal, !) ),
	assert(initialized).

optimizer_finalize :-
	initialized,
	optimizer_free_memory,
	retractall(lbfgs:'$lbfgs_callback_evaluate'(_,_,_)),
	retractall(lbfgs:'$lbfgs_callback_progress'(_,_,_,_,_,_,_,_)),
	retractall(initialized).

optimizer_parameters :-
	optimizer_get_parameter(m,M),
	optimizer_get_parameter(epsilon,Epsilon),
	optimizer_get_parameter(past,Past),
	optimizer_get_parameter(delta,Delta),
	optimizer_get_parameter(max_iterations,Max_Iterations),
	optimizer_get_parameter(linesearch,Linesearch),
	optimizer_get_parameter(max_linesearch,Max_Linesearch),
	optimizer_get_parameter(min_step,Min_Step),
	optimizer_get_parameter(max_step,Max_Step),
	optimizer_get_parameter(ftol,Ftol),
	optimizer_get_parameter(gtol,Gtol),
	optimizer_get_parameter(xtol,Xtol),
	optimizer_get_parameter(orthantwise_c,Orthantwise_C),
	optimizer_get_parameter(orthantwise_start,Orthantwise_Start),
	optimizer_get_parameter(orthantwise_end,Orthantwise_End),

	format('/******************************************************************************************~n',[]),
	print_param('Name','Value','Description','Type'),
	format('******************************************************************************************~n',[]),
	print_param(m,M,'The number of corrections to approximate the inverse hessian matrix.',int),
	print_param(epsilon,Epsilon,'Epsilon for convergence test.',float),
	print_param(past,Past,'Distance for delta-based convergence test.',int),
	print_param(delta,Delta,'Delta for convergence test.',float),
	print_param(max_iterations,Max_Iterations,'The maximum number of iterations',int),
	print_param(linesearch,Linesearch,'The line search algorithm.',int),
	print_param(max_linesearch,Max_Linesearch,'The maximum number of trials for the line search.',int),
	print_param(min_step,Min_Step,'The minimum step of the line search routine.',float),
	print_param(max_step,Max_Step,'The maximum step of the line search.',float),
	print_param(ftol,Ftol,'A parameter to control the accuracy of the line search routine.',float),
	print_param(gtol,Gtol,'A parameter to control the accuracy of the line search routine.',float),
	print_param(xtol,Xtol,'The machine precision for floating-point values.',float),
	print_param(orthantwise_c,Orthantwise_C,'Coefficient for the L1 norm of variables',float),
	print_param(orthantwise_start,Orthantwise_Start,'Start index for computing the L1 norm of the variables.',int),
	print_param(orthantwise_end,Orthantwise_End,'End index for computing the L1 norm of the variables.',int),
	format('******************************************************************************************/~n',[]),
	format(' use optimizer_set_paramater(Name,Value) to change parameters~n',[]),
	format(' use optimizer_get_parameter(Name,Value) to see current parameters~n',[]),
	format(' use optimizer_parameters to print this overview~2n',[]).


print_param(Name,Value,Text,Dom) :-
	format(user,'~w~10+~w~19+~w~15+~w~30+~n',[Dom,Name,Value,Text]).

interpret_return_value(0,"LBFGS_SUCCESS"):-!.

interpret_return_value(1,"LBFGS_STOP"):-!.

interpret_return_value(2,"LBFGS_ALREADY_MINIMIZED"):-!.

interpret_return_value(N,M):-
	I is N+1024,
	nth0(I, ["LBFGSERR_UNKNOWNERROR", "LBFGSERR_LOGICERROR", "LBFGSERR_OUTOFMEMORY", "LBFGSERR_CANCELED", 
			"LBFGSERR_INVALID_N", "LBFGSERR_INVALID_N_SSE", "LBFGSERR_INVALID_X_SSE", "LBFGSERR_INVALID_EPSILON",
			"LBFGSERR_INVALID_TESTPERIOD", "LBFGSERR_INVALID_DELTA", "LBFGSERR_INVALID_LINESEARCH", "LBFGSERR_INVALID_MINSTEP",
			"LBFGSERR_INVALID_MAXSTEP", "LBFGSERR_INVALID_FTOL", "LBFGSERR_INVALID_WOLFE", "LBFGSERR_INVALID_GTOL",
			"LBFGSERR_INVALID_XTOL", "LBFGSERR_INVALID_MAXLINESEARCH", "LBFGSERR_INVALID_ORTHANTWISE", "LBFGSERR_INVALID_ORTHANTWISE_START",
			"LBFGSERR_INVALID_ORTHANTWISE_END", "LBFGSERR_OUTOFINTERVAL", "LBFGSERR_INCORRECT_TMINMAX", "LBFGSERR_ROUNDING_ERROR",
			"LBFGSERR_MINIMUMSTEP", "LBFGSERR_MAXIMUMSTEP", "LBFGSERR_MAXIMUMLINESEARCH", "LBFGSERR_MAXIMUMITERATION",
			"LBFGSERR_WIDTHTOOSMALL", "LBFGSERR_INVALIDPARAMETERS", "LBFGSERR_INCREASEGRADIENT"], M).
	