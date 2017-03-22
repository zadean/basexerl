%%%----------------------------------------------------------------------
%%% File    : basexerl.erl
%%% Author  : Zachary Dean <contact@zadean.com>
%%% Purpose : API for a BaseX client
%%% Created : 17 Mar 2017 by Zachary Dean <contact@zadean.com>
%%%----------------------------------------------------------------------

-module(basexerl).
-author('contact@zadean.com').

-define(DEFHOST, "localhost").
-define(DEFPORT, 1984).
-define(DEFUSER, "admin").
-define(DEFPASS, "admin").

-define(TIMEOUT, 60000).


%% ==========================================================
%%                   API functions
%% ==========================================================

-export([connect/0, connect/2,connect/4, 
         disconnect/1]).

-export([execute/2, 
         create/2, create/3, 
         add/3, 
         replace/3, 
         store/3,
         retrieve/2]).

-export([query/2, 
         q_bind/3, q_bind/4, q_bind/5, 
         q_context/3,q_context/4, 
         q_results/2, 
         q_execute/2, 
         q_info/2, 
         q_options/2, 
         q_close/2]).


%% ==========================================================
%%                  connection stuff
%% ==========================================================

%% returns {ok, Pid} for the new DB connection
connect() ->
   connect([], [], [], []).
connect(User, Pass) ->
   connect([], [], User, Pass).
connect(Host, Port, User, Pass) ->
   Host1 = bxe_util:nvl(Host, ?DEFHOST),
   Port1 = bxe_util:nvl(Port, ?DEFPORT),
   User1 = bxe_util:nvl(User, ?DEFUSER),
   Pass1 = bxe_util:nvl(Pass, ?DEFPASS),
   bxe_client:start([Host1, Port1, User1, Pass1]).

%% disconnects from the DB for the given connection Pid
disconnect(Conn) ->
    gen_server:call(Conn, disconnect).

%% ==========================================================
%%                  database stuff
%% ==========================================================

%% Executes a command and returns the result.
%% returns {ok, Result, Info}
execute(Conn, Command) ->
    gen_server:call(Conn, {execute, Command}, ?TIMEOUT).

%% Creates a database.
%% returns {ok, Info}
create(Conn, Name) ->
    create(Conn, Name, []).
create(Conn, Name, Input) ->
    gen_server:call(Conn, {create, Name, Input}, ?TIMEOUT).

%% Adds a document to a database.
%% returns {ok, Info}
add(Conn, Path, Input) ->
    gen_server:call(Conn, {add, Path, Input}, ?TIMEOUT).

%% Replaces a document in a database.
%% returns {ok, Info}
replace(Conn, Path, Input) ->
    gen_server:call(Conn, {replace, Path, Input}, ?TIMEOUT).

%% Stores a binary resource in a database.
%% returns {ok, Info}
store(Conn, Path, Input) when is_binary(Input) ->
    gen_server:call(Conn, {store, Path, Input}, ?TIMEOUT).

%% Gets a binary resource from the open database.
%% returns {ok, Binary}
retrieve(Conn, Path) ->
    gen_server:call(Conn, {retrieve, Path}, ?TIMEOUT).


%% ==========================================================
%%                     XQuery stuff
%% ==========================================================

%% get the ID for this query
%% returns {ok, Qid}
query(Conn, Query) ->
    gen_server:call(Conn, {query, Query}, ?TIMEOUT).

%% Bind a value to an external variable.
%% Sequences for single variables should be in Name as a tuple with Name, then a list of tuples 
%% {Value, Type} | {Value}: {"$varName", [{"123", "xs:integer"}, {"ABC"} ]}
%% returns {ok, Info}
q_bind(Conn, Qid, NamedSequence) ->
    q_bind(Conn, Qid, NamedSequence, [], []).
q_bind(Conn, Qid, Name, Value) ->
    q_bind(Conn, Qid, Name, Value, []).
q_bind(Conn, Qid, Name, Value, Type) ->
    gen_server:call(Conn, {q_bind, Qid, Name, Value, Type}, ?TIMEOUT).

%% Bind a value to the context item.
%% Sequences for context should be in form {context, [{Value, Type} | {Value}]}
%% returns {ok, Info}
q_context(Conn, Qid, Value) ->
    q_context(Conn, Qid, Value, []).
q_context(Conn, Qid, Value, Type) ->
    gen_server:call(Conn, {q_context, Qid, Value, Type}, ?TIMEOUT).

%% Return the entire result of the query.
%% returns {ok, Result}
q_execute(Conn, Qid) ->
    gen_server:call(Conn, {q_execute, Qid}, ?TIMEOUT).

%% Returns all resulting items as strings, prefixed by a single byte (\xx) 
%% that represents the Type ID. 
%% returns {ok, Result}
q_results(Conn, Qid) ->
    gen_server:call(Conn, {q_results, Qid}, ?TIMEOUT).

%% Return query info in a string.
%% returns {ok, Info}
q_info(Conn, Qid) ->
    gen_server:call(Conn, {q_info, Qid}, ?TIMEOUT).

%% Return serialization parameters in a string.
%% returns {ok, Info}
q_options(Conn, Qid) ->
    gen_server:call(Conn, {q_options, Qid}, ?TIMEOUT).

%% Closes the query. It's a good idea to do this!
%% returns {ok, Info}
q_close(Conn, Qid) ->
    gen_server:call(Conn, {q_close, Qid}, ?TIMEOUT).
