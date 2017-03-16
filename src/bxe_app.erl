%%%----------------------------------------------------------------------
%%% File    : bxe_app.erl
%%% Author  : Zachary Dean <contact@zadean.com>
%%% Purpose : 
%%% Created : 17 Mar 2017 by Zachary Dean <contact@zadean.com>
%%%----------------------------------------------------------------------

-module(bxe_app).
-author('contact@zadean.com').
-behaviour(application).
-export([start/2, stop/1]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
start(_Type, _StartArgs) ->
    bxe_sup:start_link().

%% stop/1
stop(_State) ->
    ok.
