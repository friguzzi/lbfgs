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
:- module(ex1,[demo/0,demo/2]).

:- use_module(library(lbfgs)).


% This is the call back function which evaluates F and the gradient of F
evaluate(Env,FX,_N,_Step,_) :-
	optimizer_get_x(Env,0,X0),
	FX is sin(X0),
	G0 is cos(X0),
	optimizer_set_g(Env,0,G0).

% This is the call back function which is invoked to report the progress
% if the last argument is set to anywhting else than 0, the optimizer will
% stop right now
progress(Env,FX,X_Norm,G_Norm,Step,_N,Iteration,Ls,0,_) :-
	optimizer_get_x(Env,0,X0),
	format('~d. Iteration : x0=~4f  f(X)=~4f  |X|=~4f  |X\'|=~4f  Step=~4f  Ls=~4f~n',[Iteration,X0,FX,X_Norm,G_Norm,Step,Ls]).



demo :-
	demo(_Sol,_BestF).

demo(BestX0,BestF):-
	format('Optimizing the function f(x0) = sin(x0)~n',[]),
	optimizer_initialize(1,ex1,evaluate,progress,[],Env),


	StartX is 3.95801,% random_float*10,
	format('We start the search at the random position x0=~5f~2n',[StartX]),
	optimizer_set_x(Env,0,StartX),
	
	optimizer_run(Env,BestF,Status),
	optimizer_get_x(Env,0,BestX0),
	optimizer_finalize(Env),
	format('~2nOptimization done~nWe found a minimum at f(~f)=~f~2nLBFGS Status=~w~n',[BestX0,BestF,Status]).


