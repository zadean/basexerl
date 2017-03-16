%%%----------------------------------------------------------------------
%%% File    : bxe_sup.erl
%%% Author  : Zachary Dean <contact@zadean.com>
%%% Purpose : Do nothing supervisor
%%% Created : 17 Mar 2017 by Zachary Dean <contact@zadean.com>
%%%----------------------------------------------------------------------
-module(bxe_sup).
-author('contact@zadean.com').

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% ====================================================================
%% API functions
%% ====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
init([]) ->
    {ok,{{one_for_one,10,1}, []}}.

