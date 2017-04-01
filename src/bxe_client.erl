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

reconnect(#state{ohost = Host, 
                 oport = Port} = State) ->
    case open_socket({Host, Port}) of
        {ok, Sock} ->
            connect(State#state{socket = Sock});
        Err ->
            {stop, Err}
    end.

% handle_call/3
handle_call({add, Path, Input}, _From, State) -> 
   do_execute(State, 
              [?ADD,
               Path,  ?N,
               Input, ?N]);
handle_call({create, Name, Input}, _From, State) -> 
   do_execute(State, 
              [?CREATE,
               Name,  ?N,
               Input, ?N]);
handle_call({execute, Command}, _From, State) -> 
   do_execute(State, 
              [Command,?N]);
handle_call({replace, Path, Input}, _From, State) -> 
   do_execute(State, 
              [?REPLACE,
               Path,  ?N,
               Input, ?N]);
handle_call({store, Path, Input}, _From, State) -> 
   Bin = encode_bin(Input),
   do_execute(State, 
              [?STORE,
               Path,  ?N,
               Bin, ?N]);
handle_call({retrieve, Path}, _From, State) -> 
   do_retrieve(State, 
              [?RETRIEVE,
               Path,  ?N]);
handle_call({query, Query}, _From, State) -> 
   do_query(State, 
            [?QUERY,
             Query, ?N]);
handle_call({q_bind, Qid, Name, _Value, _Type}, _From, State) when is_tuple(Name) -> 
   {Nm, Vals} = Name,
   Cmd = encode_seq_var(Vals),
   do_query(State, 
            [?BIND,
             Qid,   ?N,
             Nm,    ?N,
             Cmd,   ?N,
             ?N]);
handle_call({q_bind, Qid, Name, Value, Type}, _From, State) -> 
   do_query(State, 
            [?BIND,
             Qid,   ?N,
             Name,  ?N,
             Value, ?N,
             Type,  ?N]);
handle_call({q_results, Qid}, _From, State) -> 
   do_query(State,
            [?RESULTS,
             Qid, ?N],
            true);
handle_call({q_context, Qid, Value, _Type}, _From, State) when is_tuple(Value) -> 
   {context, Vals} = Value,
   Cmd = encode_seq_var(Vals),   
   do_query(State, 
            [?CONTEXT,
             Qid,   ?N,
             Cmd,   ?N,
             ?N]);
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
handle_cast({q_close, Qid}, State) ->
    do_query(State, 
             [?CLOSE,
              Qid, ?N]),
    {noreply, State};
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
connect(#state{ouser = User,
               opass = Pass,
               socket = Sock} = State) ->
   Cookie = list_to_binary(read_socket(Sock)),
   [Res1, Res2] = binary:split(Cookie, <<":">>),
   Hash = md5(Res1, Res2, User, Pass),
   ok = write_socket(Sock, Hash),
   case read_socket(Sock) of
      [] -> 
          {ok, State};
      _ -> 
          close_socket(Sock),
          {error, login_failed}
   end.

do_execute(State, Data) ->
   Sock = State#state.socket,
   ok = write_socket(Sock, Data),
   case read_socket(Sock) of
      {error, econnaborted} ->
         {ok, NewState} = reconnect(State),
         do_execute(NewState, Data);
      {error, Error} ->
         {reply, {error, Error}, State};
      BinList ->
         case has_error(BinList) of
            false ->
               case BinList of
                  [Result, Info] ->
                     {reply, {ok, Result, Info}, State};
                  [Result] ->
                     {reply, {ok, Result}, State}
               end;
            _ ->
               [Error, _] = BinList, 
               {reply, {error, Error}, State}
         end
   end.

do_retrieve(State, Data) ->
   Sock = State#state.socket,
   ok = write_socket(Sock, Data),
   Raw = list_to_binary(read_socket(Sock, binary)),
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
          case Info of
             econnaborted ->
                {ok, NewState} = reconnect(State),
                do_retrieve(NewState, Data);
             _ ->
               [Info2] = binary:split(Info, <<0>>, [global, trim_all]),
               {reply, {error, Info2}, State}
          end
   end.

do_query(State, Data) ->
   do_query(State, Data, false).

do_query(State, Data, AsList) ->
   Sock = State#state.socket,
   ok = write_socket(Sock, Data),
   case read_socket(Sock) of
      {error, econnaborted} ->
         {ok, NewState} = reconnect(State),
         do_query(NewState, Data, AsList);
      {error, Error} ->
         {reply, {error, Error}, State};
      BinList ->
         %BinList = ll_to_lb(RawList),
         case get_error(BinList) of
            [] ->
               case AsList of
                  true ->
                     TypedList = [ type_result(E) || E <- BinList, bit_size(E) > 0 ],
                     {reply, {ok, TypedList}, State};
                  _ ->
                     {reply, {ok, BinList}, State}
               end;
            Error ->
               Error1 = binary:part(Error, 1, byte_size(Error) -1),
               {reply, {error, Error1}, State}
         end
   end.

has_error([]) ->
   [];
has_error(List) ->
   Fn = fun(<<>>) ->
              false;
           (Bin) ->
              binary:first(Bin) == 1
        end,
   lists:any(Fn, List).

get_error([]) ->
   [];
get_error(List) ->
   Fn = fun(<<>>) ->
              false;
           (Bin) ->
              binary:first(Bin) == 1
        end,
   lists:filter(Fn, List).

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
   List = read_socket1(Sock, <<>>, []),
   lists:reverse(List).

read_socket1(Sock, TmpAcc, Acc) ->
   case gen_tcp:recv(Sock, 0, 1000) of
      {ok, Packet} ->
         Size = byte_size(Packet),
         Tail = binary:last(Packet),
         % when there is maybe more, add it
         {Packet1, Tail1, End} =   case is_end(Sock, Size, Tail) of
                                     {ok, Data} ->
                                         Packet2 = <<Packet/binary, Data/binary>>,
                                         Tail2   = binary:last(Packet2),
                                         {Packet2, Tail2, false};
                                      I ->
                                         {Packet, Tail, I}
                                   end,
         % split the packet by 0
         List = binary:split(Packet1, <<0>>, [global, trim_all]),
         % concat the Acc and the first packet part, if any
         ListLen = length(List),
         TmpAcc1 = case ListLen of
                      0 ->
                         TmpAcc;
                      _ ->
                         <<TmpAcc/binary, (lists:nth(1, List))/binary>>
                   end,
         Sublist = fun(Alist, Len, Right) when (Len  - Right) > 0 ->
                         lists:sublist(Alist, 2, (Len - Right));
                      (_, _, _) ->
                         []
                   end,
         case {End, TmpAcc1, Tail1} of
            {true, <<>>, _} ->
               Sub = Sublist(List, ListLen, 1),
               prepend(Sub, Acc);
            {true, _, _} ->
               Sub = Sublist(List, ListLen, 1),
               % temp acc finished so add to list
               List1 = [TmpAcc1 | Sub],
               prepend(List1, Acc);
            {false, _, 0} -> 
               Sub = Sublist(List, ListLen, 1),
               % temp acc finished so add to list
               List1 = [TmpAcc1 | Sub],
               NewAcc = prepend(List1, Acc),
               read_socket1(Sock, <<>>, NewAcc);
            {false, _, _} -> 
               case Sublist(List, ListLen, 2) of
                  [] ->
                     % nothing new to add
                     read_socket1(Sock, TmpAcc1, Acc);
                  Sub ->
                     % temp acc finished so add to list
                     List1 = [TmpAcc1 | Sub],
                     NewAcc = prepend(List1, Acc),
                     read_socket1(Sock, lists:last(List), NewAcc)
               end
         end;
   {error, timeout} -> 
      prepend([TmpAcc], Acc);
   Err -> 
      Err
   end.

read_socket(Sock, binary) ->
   case gen_tcp:recv(Sock, 0, 5000) of
      {ok, Packet} ->
         Size = byte_size(Packet),
         Tail = binary:last(Packet),
         case is_end(Sock, Size, Tail) of
            false ->
               [Packet | read_socket(Sock)];
            {ok, Data} ->
               [<<Packet/binary, Data/binary>> | read_socket(Sock)];
            _ ->
               [Packet]
        end;
    {error, timeout} -> 
        [];
    Err -> 
        Err
   end.

is_end(_Sock, ?BUFFER, Char) when Char =/= 0 -> 
   % full sized packet, this can cause a problem when it so happens that the last byte is 0
   false;
is_end(_Sock, _Len, Char) when Char > 1 -> % some errors can end with 1
   false;
is_end(_Sock, Len, _Char) when Len < 1024 -> % too short
   true;
is_end(Sock, _Len, _Char) ->
   % now make absolutely sure, should only need to happen with large 
   % packets with 0x00 in them as the last byte read.
   case gen_tcp:recv(Sock, 0, 1) of
      {ok, _} = T ->
         T;
      _ ->
         true
    end.

%% get the last byte to show if ok or error
okay(<<>>) -> 
   {error, <<"no data",1,0>>};
okay({error, Reason}) -> 
   {error, Reason};
okay(Packet) -> 
   Size = byte_size(Packet),
   Stat = binary:last(Packet),
   Data = case Size of 
            0 ->
               <<>>;
            _ ->
               binary_part(Packet, 0, (Size-1))
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


encode_seq_var(Vals) ->
   Concat = fun({Val, Type}) ->
                  [Val, 2, Type];
               ({Val}) ->
                  [Val, 2]
            end,
   List = [Concat(E) || E <- Vals],
   Join = list_join(List, 1),
   lists:flatten(Join).

list_join([H|T], Sep) ->
   [H|list_join_1(T, Sep)].
list_join_1([H|T], Sep) ->
   [Sep,H|list_join_1(T, Sep)];
list_join_1([], _) ->
   [].

prepend([], List2) ->
   List2;
prepend([H|T], List2) ->
   prepend(T, [H|List2]).

type_result(<<>>) ->
    [];
type_result(<<H,T/binary>>) ->
    {T, get_type(H)}.

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
