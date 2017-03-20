%%%----------------------------------------------------------------------
%%% File    : bxe_client.erl
%%% Author  : Zachary Dean <contact@zadean.com>
%%% Purpose : Protocol functions
%%% Created : 17 Mar 2017 by Zachary Dean <contact@zadean.com>
%%%----------------------------------------------------------------------

-module(bxe_client).
-author('contact@zadean.com').
-behaviour(gen_server).

-include("basexerl.hrl").

-define(TIMEOUT, 60000).
-define(BUFFER , 4096).

-define(N      , <<0>>). 

-define(TCP_OPTIONS, [binary,
                      {packet, 0}, 
                      {buffer, ?BUFFER},
                      {active, false}, 
                      {reuseaddr, true}, 
                      {nodelay, true}]).
-define(SERVER, ?MODULE).

-record(state, {ohost, oport, ouser, opass, socket}).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, start_link/1]).
-export([encode_bin/1]).

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
   Bin = encode_bin(Input),
   do_command(State, 
              [?STORE,
               Path,  ?N,
               Bin, ?N]);
handle_call({retrieve, Path}, _From, State) -> 
   case do_retrieve(State, 
              [?RETRIEVE,
               Path,  ?N]) of
      {ok, Bin} ->
         {ok, Bin};
      Other ->
         Other
   end;
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
handle_call({q_results, Qid}, _From, State) -> 
   do_result_set(State,
                 [?RESULTS,
                  Qid, ?N]);
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
   Magic = read_socket(Sock),
   {ok, Cookie} = okay(Magic),
   [Res1, Res2] = binary:split(Cookie, <<":">>),
   Hash = md5(Res1, Res2, User, Pass),
   ok = write_socket(Sock, Hash),
   case read_socket(Sock) of
      ?N -> 
          {ok, State};
      _ -> 
          close_socket(Sock),
          {error, login_failed}
   end.

do_execute(State, Data) ->
   Sock = State#state.socket,
   ok = write_socket(Sock, Data),
   Raw = read_socket(Sock),
   case okay(Raw) of
       {ok, Response} ->
           [Result, Info, _] = binary:split(Response, <<0>>, [global]),
           {reply, {ok, Result, Info}, State};
       {error, Info} ->
           {reply, {error, Info}, State}
   end.

do_command(State, Data) ->
   Sock = State#state.socket,
   ok = write_socket(Sock, Data),
   Raw = read_socket(Sock),
   case okay(Raw) of
       {ok, Response} ->
           [Info, _] = binary:split(Response, <<0>>, []),
           {reply, {ok, Info}, State};
       {error, Info} ->
           {reply, {error, Info}, State}
   end.

do_retrieve(State, Data) ->
   Sock = State#state.socket,
   ok = write_socket(Sock, Data),
   Raw = read_socket(Sock),
   case okay(Raw) of
       {ok, Response} ->
          % remove okay for info
          {ok, Response2} = okay(Response),
          List1 = binary_to_list(Response2),
          List2 = lists:reverse(List1),
          Bin1 = list_to_binary(List2),
          % remove info
          [_Binfo, Bresult] = binary:split(Bin1, <<0>>, []),
          % decode and reverse binary
          Bin2 = decode_bin(Bresult),
          List3 = lists:reverse(Bin2),
          Bin3 = list_to_binary(List3),
          {reply, {ok, Bin3}, State};
       {error, Info} ->
           {reply, {error, Info}, State}
   end.

do_query(State, Data) ->
   Sock = State#state.socket,
   ok = write_socket(Sock, Data),
   Raw = read_socket(Sock),
   case binary:match(Raw, <<1>>) of
       nomatch ->
           case okay(Raw) of
               {ok, Response} ->
                   [Reply, _] = binary:split(Response, <<0>>, []),
                   {reply, {ok, Reply}, State};
               {error, Info} ->
                   {reply, {error, Info}, State}
           end;
       _ ->
           [_, Error] = binary:split(Raw, <<1>>, []),
           [Info, _] = binary:split(Error, <<0>>, []),
           {reply, {error, Info}, State}
   end.

do_result_set(State, Data) ->
   Sock = State#state.socket,
   ok = write_socket(Sock, Data),
   Raw = read_socket(Sock),
   case binary:match(Raw, <<1>>) of
       nomatch ->
           case okay(Raw) of
               {ok, Response} ->
                   List = binary:split(Response, <<0>>, [global]),
                   TypedList = [ type_result(E) || E <- List, bit_size(E) > 0 ],
                   {reply, {ok, TypedList}, State};
               {error, Info} ->
                   {reply, {error, Info}, State}
           end;
       _ ->
           [_, Error] = binary:split(Raw, <<1>>, []),
           [Info, _] = binary:split(Error, <<0>>, []),
           {reply, {error, Info}, State}
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

write_socket(_Socket, []) ->
    ok;
write_socket(Socket, List) when is_list(List) ->
    write_socket(Socket, list_to_binary(List));
write_socket(Socket, Bin) when byte_size(Bin) >= ?BUFFER ->
   {Pack, Rest} = split_binary(Bin, ?BUFFER),
   gen_tcp:send(Socket, Pack),
   write_socket(Socket, Rest);
write_socket(Socket, Rest) ->
   gen_tcp:send(Socket, Rest),
   write_socket(Socket, []).

read_socket(Sock) ->
   case gen_tcp:recv(Sock, 0, 5000) of
      {ok, <<>>} ->
         <<>>;
      {ok, Packet} ->
         Sz = byte_size(Packet),
         Lt = binary:last(Packet),
         case is_end(Sock, Sz, Lt) of
            false ->
               <<Packet/binary, (read_socket(Sock))/binary>>;
            {ok, Data} ->
               <<Packet/binary, Data:8/binary, (read_socket(Sock))/binary>>;
            _ ->
               Packet
        end;
    {error, timeout} -> 
        [];
    Err -> 
        Err
   end.

is_end(_Sock, ?BUFFER, _Char) ->
   false;
is_end(_Sock, _Len, Char) when Char > 1 ->
   false;
is_end(Sock, _Len, _Char) ->
   % now make absolutely sure, should only happen with large 
   % packets with 0x00 or 0xff in them
   case gen_tcp:recv(Sock, 0, 10) of
      {ok, Test} ->
         {ok, Test};
      _ ->
         true
   end.

%% get the last byte to show if ok or error
okay(<<>>) -> 
   {error, <<"no data",1,0>>};
okay(Packet) -> 
   Sz = byte_size(Packet),
   Stat = binary:at(Packet, Sz-1),
   Data = case Sz of 
            0 ->
               <<>>;
            _ ->
               binary_part(Packet, 0, (Sz-1))
          end,
   case Stat of 
       0 ->
           {ok, Data};
       1 ->
           {error, Data};
       _ ->
           {error, invalid}
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

% prefix all 0x00 and 0xff bytes with 0xff
encode_bin(<<255>>) ->
   [255,255];
encode_bin(<<0>>) ->
   [255,0];
encode_bin(<<255,T/binary>>) ->
   lists:append([255,255],encode_bin(T));
encode_bin(<<0,T/binary>>) ->
   lists:append([255,0],encode_bin(T));
encode_bin(<<H,T/binary>>) ->
   [H|encode_bin(T)];
encode_bin(<<>>) -> [].

% remove 0xff prefix from all 0x00 and 0xff bytes 
% in reverse order!!
decode_bin(<<X,255>>) ->
   [X];
decode_bin(<<X,255,T/binary>>) ->
   [X|decode_bin(T)];
decode_bin(<<H,T/binary>>) ->
   [H|decode_bin(T)];
decode_bin(<<>>) -> [].

type_result(<<>>) ->
    [];
type_result(<<H,T/binary>>) ->
    {get_type(H), T}.

get_type(7) -> 'function item';
get_type(8) -> 'node()';
get_type(9) -> 'text()';
get_type(10) -> 'processing-instruction()';
get_type(11) -> 'element()';
get_type(12) -> 'document-node()';
get_type(13) -> 'document-node(element())';
get_type(14) -> 'attribute()';
get_type(15) -> 'comment()';
get_type(32) -> 'item()';
get_type(33) -> 'xs:untyped';
get_type(34) -> 'xs:anyType';
get_type(35) -> 'xs:anySimpleType';
get_type(36) -> 'xs:anyAtomicType';
get_type(37) -> 'xs:untypedAtomic';
get_type(38) -> 'xs:string';
get_type(39) -> 'xs:normalizedString';
get_type(40) -> 'xs:token';
get_type(41) -> 'xs:language';
get_type(42) -> 'xs:NMTOKEN';
get_type(43) -> 'xs:Name';
get_type(44) -> 'xs:NCName';
get_type(45) -> 'xs:ID';
get_type(46) -> 'xs:IDREF';
get_type(47) -> 'xs:ENTITY';
get_type(48) -> 'xs:float';
get_type(49) -> 'xs:double';
get_type(50) -> 'xs:decimal';
get_type(51) -> 'xs:precisionDecimal';
get_type(52) -> 'xs:integer';
get_type(53) -> 'xs:nonPositiveInteger';
get_type(54) -> 'xs:negativeInteger';
get_type(55) -> 'xs:long';
get_type(56) -> 'xs:int';
get_type(57) -> 'xs:short';
get_type(58) -> 'xs:byte';
get_type(59) -> 'xs:nonNegativeInteger';
get_type(60) -> 'xs:unsignedLong';
get_type(61) -> 'xs:unsignedInt';
get_type(62) -> 'xs:unsignedShort';
get_type(63) -> 'xs:unsignedByte';
get_type(64) -> 'xs:positiveInteger';
get_type(65) -> 'xs:duration';
get_type(66) -> 'xs:yearMonthDuration';
get_type(67) -> 'xs:dayTimeDuration';
get_type(68) -> 'xs:dateTime';
get_type(69) -> 'xs:dateTimeStamp';
get_type(70) -> 'xs:date';
get_type(71) -> 'xs:time';
get_type(72) -> 'xs:gYearMonth';
get_type(73) -> 'xs:gYear';
get_type(74) -> 'xs:gMonthDay';
get_type(75) -> 'xs:gDay';
get_type(76) -> 'xs:gMonth';
get_type(77) -> 'xs:boolean';
get_type(78) -> 'basex:binary';
get_type(79) -> 'xs:base64Binary';
get_type(80) -> 'xs:hexBinary';
get_type(81) -> 'xs:anyURI';
get_type(82) -> 'xs:QName';
get_type(83) -> 'xs:NOTATION';
get_type(_)  -> 'UNKNOWN'.
