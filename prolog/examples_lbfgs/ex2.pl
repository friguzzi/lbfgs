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
:- module(ex2,[demo/0,demo/3]).

:- use_module(library(lbfgs)).


% This is the call back function which evaluates F and the gradient of F
evaluate(Env,FX,_N,_Step,_) :-
	optimizer_get_x(Env,0,X0),
	optimizer_get_x(Env,1,X1),

	FX is (X0-2)*(X0-2) + (X1-1)*(X1-1),
	G0 is 2*(X0-2),
	G1 is 2*(X1-1),
	
	optimizer_set_g(Env,0,G0),
	optimizer_set_g(Env,1,G1).

% This is the call back function which is invoked to report the progress
% if the last argument is set to anywhting else than 0, the optimizer will
% stop right now
progress(Env,FX,X_Norm,G_Norm,Step,_N,Iteration,Ls,0,_) :-
	optimizer_get_x(Env,0,X0),
	optimizer_get_x(Env,1,X1),
	format('~d. Iteration : (x0,x1)=(~4f,~4f)  f(X)=~4f  |X|=~4f  |X\'|=~4f  Step=~4f  Ls=~4f~n',[Iteration,X0,X1,FX,X_Norm,G_Norm,Step,Ls]).



demo :-
	demo(_X0,_X1,_F).

demo(BestX0,BestX1,BestF):-
	format('Optimizing the function f(x0,x1) = (x0-2)^2 + (x1-1)^2~n',[]),
	optimizer_initialize(2,ex2,evaluate,progress,[],Env),


	StartX0 is random_float*1000-500,
	StartX1 is random_float*1000-500,

	format('We start the search at the random position (x0,x1)=(~5f,~5f)~2n',[StartX0,StartX1]),
	optimizer_set_x(Env,0,StartX0),
	optimizer_set_x(Env,1,StartX1),
	
	optimizer_run(Env,BestF,Status),
	optimizer_get_x(Env,0,BestX0),
	optimizer_get_x(Env,1,BestX1),
	
	optimizer_finalize(Env),
	format('~2nOptimization done~nWe found a minimum at f(~f,~f)=~f~2nLBFGS Status=~w~n',[BestX0,BestX1,BestF,Status]).


