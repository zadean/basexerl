%%%----------------------------------------------------------------------
%%% File    : bxe_client.erl
%%% Author  : Zachary Dean <contact@zadean.com>
%%% Purpose : Protocol functions
%%% Created : 17 Mar 2017 by Zachary Dean <contact@zadean.com>
%%%----------------------------------------------------------------------

-module(bxe_client).
-author('contact@zadean.com').
-behaviour(gen_server).

-define(N        ,<<0>>). 
%% DB Commands
-define(CREATE   ,<<8>>).
-define(ADD      ,<<9>>).
-define(REPLACE  ,<<12>>).
-define(STORE    ,<<13>>).
-define(UPDATING ,<<30>>).
-define(FULL     ,<<31>>).
%% XQuery Commands
-define(QUERY    ,<<0>>).
-define(CLOSE    ,<<2>>).
-define(BIND     ,<<3>>).
-define(EXECUTE  ,<<5>>).
-define(INFO     ,<<6>>).
-define(OPTIONS  ,<<7>>).
-define(CONTEXT  ,<<14>>).

-define(TCP_OPTIONS, [ {packet, 0}, {active, false}, {reuseaddr, true}, {header, 1}]).
-define(SERVER, ?MODULE).

-record(state, {ohost, oport, ouser, opass, socket}).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, start_link/1]).

start([Host, Port, User, Pass]) ->
    gen_server:start(?SERVER, [Host, Port, User, Pass], []).

start_link([Host, Port, User, Pass]) ->
    gen_server:start_link(?SERVER, [Host, Port, User, Pass], []).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
init([Host, Port, User, Pass]) ->
    case open_socket({Host, Port}) of
        {ok, Sock} ->
            State = #state{ohost = Host, 
                           oport = Port, 
                           ouser = User, 
                           opass = Pass, 
                           socket = Sock},
            connect(State);
        Err ->
            {stop, Err}
    end.

% handle_call/3
handle_call({add, Path, Input}, _From, State) -> 
   do_command(State, 
              [?ADD,
               Path,  ?N,
               Input, ?N]);
handle_call({create, Name, Input}, _From, State) -> 
   do_command(State, 
              [?CREATE,
               Name,  ?N,
               Input, ?N]);
handle_call({execute, Command}, _From, State) -> 
   do_execute(State, 
              [Command,?N]);
handle_call({replace, Path, Input}, _From, State) -> 
   do_command(State, 
              [?REPLACE,
               Path,  ?N,
               Input, ?N]);
handle_call({store, Path, Input}, _From, State) -> 
   do_command(State, 
              [?STORE,
               Path,  ?N,
               Input, ?N]);
handle_call({query, Query}, _From, State) -> 
   do_query(State, 
            [?QUERY,
             Query, ?N]);
handle_call({q_bind, Qid, Name, Value, Type}, _From, State) -> 
   do_query(State, 
            [?BIND,
             Qid,   ?N,
             Name,  ?N,
             Value, ?N,
             Type,  ?N]);
handle_call({q_close, Qid}, _From, State) -> 
    do_query(State, 
             [?CLOSE,
              Qid, ?N]);
handle_call({q_context, Qid, Value, Type}, _From, State) -> 
    do_query(State, 
             [?CONTEXT,
              Qid,   ?N,
              Value, ?N,
              Type,  ?N]);
handle_call({q_execute, Qid}, _From, State) -> 
   do_query(State, 
            [?EXECUTE,
             Qid, ?N]);
handle_call({q_info, Qid}, _From, State) -> 
    do_query(State, 
             [?INFO,
              Qid, ?N]);
handle_call({q_options, Qid}, _From, State) -> 
    do_query(State, 
             [?OPTIONS,
              Qid, ?N]);
handle_call(disconnect, _From, State) ->
    Sock = State#state.socket,
    close_socket(Sock),
    Reply = ok,
    {stop, normal, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = {error, unexpected_call},
    {reply, Reply, State}.

%% handle_cast/2
handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle_info/2
handle_info({socket, _Sock, Condition}, State) ->
    {stop, {socket, Condition}, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% terminate/2
terminate(_Reason, _State) ->
    ok.

%% code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%log in to the server
connect(State) ->
   User = State#state.ouser,
   Pass = State#state.opass,
   Sock = State#state.socket,
   Cookie = read_socket(Sock),
   Res1 = string:sub_word(Cookie, 1, $:),
   Res2 = string:sub_word(Cookie, 2, $:),
   Hash = list_to_binary(md5(Res1, Res2, User, Pass)),
   ok = write_socket(Sock, binary_to_list(Hash)),
   case okay(Sock) of
      ok -> 
          {ok, State};
      _ -> 
          close_socket(Sock),
          {error, login_failed}
   end.    

do_execute(State, Data) ->
   Sock = State#state.socket,
   ok = write_socket(Sock, Data),
   Result = read_socket(Sock),
   Info   = read_socket(Sock),
   case okay(Sock) of
       ok ->
           {reply, {ok, Result, Info}, State};
       _ ->
           {reply, {error, Info}, State}
   end.

do_command(State, Data) ->
   Sock = State#state.socket,
   ok = write_socket(Sock, Data),
   Info = read_socket(Sock),
   case okay(Sock) of
       ok ->
           {reply, Info, State};
       _ ->
           {reply, {error, Info}, State}
   end.

do_query(State, Data) ->
   Sock = State#state.socket,
   ok = write_socket(Sock, Data),
   Result = read_socket(Sock),
   case okay(Sock) of
       ok ->
           {reply, {ok, Result}, State};
       _ ->
           Err = read_socket(Sock),
           {reply, {error, Err}, State}
   end.

% {ok, Sock} | {error, Error}
open_socket({Host, Port}) ->
   case gen_tcp:connect(Host, Port, ?TCP_OPTIONS, 5000) of
       {ok, Sock} ->
           {ok, Sock};
       Err ->
           Err
    end.

close_socket(Sock) ->
    gen_tcp:close(Sock).

write_socket(Socket, Packet) ->
    gen_tcp:send(Socket, Packet).

read_socket(Socket) ->
    read_socket(Socket, 1000).
read_socket(Socket, Timeout) ->
    read_socket(Socket, Timeout, []).
read_socket(Socket, Timeout, Buffer) ->
    case gen_tcp:recv(Socket, 1, Timeout) of
        {ok, Byte} ->
            case Byte of
                [0] ->
                    lists:flatten(lists:reverse(Buffer));
                _ ->
                    read_socket(Socket, Timeout, [Byte|Buffer])
            end;
        Other ->
            io:format("got ~p~n", [Other]),
            Other
    end.

okay(Sock) -> 
   case gen_tcp:recv(Sock, 1, 500) of
      {ok, [0]} -> ok;
      {ok, Code} -> {error, Code};
      Err -> Err
   end.

md5(Res1, [], User, Pass) -> 
   Code = Pass,
   Nonce = Res1,
   [User,?N,md5([md5(Code) , Nonce]),?N] ;
md5(Res1, Res2, User, Pass) -> 
   Code = lists:flatten([User,":",Res1,":",Pass]),
   Nonce = Res2,
   [User,?N,md5([md5(Code) , Nonce]),?N].

md5(String) -> 
   MD5 = crypto:hash(md5,String),
   lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X>> <= MD5]).
