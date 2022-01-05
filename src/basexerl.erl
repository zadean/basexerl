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

-export([
    connect/0, connect/2, connect/4,
    disconnect/1
]).

-export([
    execute/2,
    create/2, create/3,
    add/3,
    replace/3,
    store/3,
    retrieve/2
]).

-export([
    query/2,
    q_bind/3, q_bind/4, q_bind/5,
    q_context/3, q_context/4,
    q_results/2,
    q_execute/2,
    q_info/2,
    q_options/2,
    q_close/2
]).

%% ==========================================================
%%                  connection stuff
%% ==========================================================

%% returns {ok, Pid} for the new DB connection using default values
-spec connect() -> {ok, pid()}.
connect() ->
    connect([], [], [], []).
%% returns {ok, Pid} for the new DB connection using default values for host and port
-spec connect(User :: string(), Password :: string()) -> {ok, pid()}.
connect(User, Pass) ->
    connect([], [], User, Pass).
%% returns {ok, Pid} for the new DB connection using given values
-spec connect(
    Host :: string() | [],
    Port :: integer() | [],
    User :: string(),
    Password :: string()
) -> {ok, pid()}.
connect(Host, Port, User, Pass) ->
    Host1 = bxe_util:nvl(Host, ?DEFHOST),
    Port1 = bxe_util:nvl(Port, ?DEFPORT),
    User1 = bxe_util:nvl(User, ?DEFUSER),
    Pass1 = bxe_util:nvl(Pass, ?DEFPASS),
    bxe_client:start([Host1, Port1, User1, Pass1]).

%% disconnects from the DB for the given connection Pid
-spec disconnect(Conn :: pid()) -> ok.
disconnect(Conn) ->
    gen_server:call(Conn, disconnect).

%% ==========================================================
%%                  database stuff
%% ==========================================================

%% Executes a command and returns the result.
-spec execute(Conn :: pid(), Command :: binary() | string()) -> Results when
    Results ::
        {ok, Info}
        | {ok, Result, Info}
        | {error, Reason :: term()},
    Result :: binary(),
    Info :: binary().
execute(Conn, Command) ->
    gen_server:call(Conn, {execute, Command}, ?TIMEOUT).

%% Creates a database.
-spec create(Conn, Name) -> Results when
    Conn :: pid(),
    Name :: binary() | string(),
    Results :: {ok, Info} | {error, Reason :: term()},
    Info :: binary().
create(Conn, Name) ->
    create(Conn, Name, []).
%% Creates and opens a database.
-spec create(Conn, Name, Input) -> Results when
    Conn :: pid(),
    Name :: binary() | string(),
    Input :: binary() | string(),
    Results :: {ok, Info} | {error, Reason :: term()},
    Info :: binary().
create(Conn, Name, Input) ->
    gen_server:call(Conn, {create, Name, Input}, ?TIMEOUT).

%% Adds a document to the open database.
-spec add(Conn, Path, Input) -> Results when
    Conn :: pid(),
    Path :: binary() | string(),
    Input :: binary() | string(),
    Results :: {ok, Info} | {error, Reason :: term()},
    Info :: binary().
add(Conn, Path, Input) ->
    gen_server:call(Conn, {add, Path, Input}, ?TIMEOUT).

%% Replaces a document in the open database.
-spec replace(Conn, Path, Input) -> Results when
    Conn :: pid(),
    Path :: binary() | string(),
    Input :: binary() | string(),
    Results :: {ok, Info} | {error, Reason :: term()},
    Info :: binary().
replace(Conn, Path, Input) ->
    gen_server:call(Conn, {replace, Path, Input}, ?TIMEOUT).

%% Stores a binary resource in the open database.
-spec store(Conn, Path, Input) -> Results when
    Conn :: pid(),
    Path :: binary() | string(),
    Input :: binary(),
    Results :: {ok, Info} | {error, Reason :: term()},
    Info :: binary().
store(Conn, Path, Input) when is_binary(Input) ->
    gen_server:call(Conn, {store, Path, Input}, ?TIMEOUT).

%% Gets a binary resource from the open database.
-spec retrieve(Conn, Path) -> Results when
    Conn :: pid(),
    Path :: binary() | string(),
    Results :: {ok, Result} | {error, Reason :: term()},
    Result :: binary().
retrieve(Conn, Path) ->
    gen_server:call(Conn, {retrieve, Path}, ?TIMEOUT).

%% ==========================================================
%%                     XQuery stuff
%% ==========================================================

%% Registers the XQuery with the server. Returns the ID for the query.
-spec query(Conn, Query) -> Results when
    Conn :: pid(),
    Query :: binary() | string() | [binary()] | [string()],
    Results :: {ok, Qid} | {error, Reason :: term()},
    Qid :: binary().
query(Conn, Query) ->
    gen_server:call(Conn, {query, Query}, ?TIMEOUT).

%% Bind a value to an external variable.
%% Sequences for single variables should be in tagged-tuple with Name, then a list of value tuples
%% {"$varName", [{"123", "xs:integer"}, {"ABC"} ]}
-spec q_bind(Conn, Qid, NamedSequence) -> Results when
    Conn :: pid(),
    Qid :: binary(),
    NamedSequence :: {Name :: string(), [{Value, Type} | {Value}]},
    Value :: string() | binary(),
    Type :: string() | binary(),
    Results :: {ok, Info} | {error, Reason :: term()},
    Info :: binary().
q_bind(Conn, Qid, NamedSequence) ->
    q_bind(Conn, Qid, NamedSequence, [], []).
%% Bind a single value to an external variable.
-spec q_bind(Conn, Qid, Name, Value) -> Results when
    Conn :: pid(),
    Qid :: binary(),
    Name :: string(),
    Value :: string() | binary(),
    Results :: {ok, Info} | {error, Reason :: term()},
    Info :: binary().
q_bind(Conn, Qid, Name, Value) ->
    q_bind(Conn, Qid, Name, Value, []).
%% Bind a single typed value to an external variable.
-spec q_bind(Conn, Qid, Name, Value, Type) -> Results when
    Conn :: pid(),
    Qid :: binary(),
    Name :: string() | {Name :: string(), [{Value, Type} | {Value}]},
    Value :: string() | binary(),
    Type :: string() | binary(),
    Results :: {ok, Info} | {error, Reason :: term()},
    Info :: binary().
q_bind(Conn, Qid, Name, Value, Type) ->
    gen_server:call(Conn, {q_bind, Qid, Name, Value, Type}, ?TIMEOUT).

%% Bind a value/s to the context item.
%% Sequences for context should be in form {context, [{Value, Type} | {Value}]}
-spec q_context(Conn, Qid, Value) -> Results when
    Conn :: pid(),
    Qid :: binary(),
    Value :: string() | binary() | {context, [{Val, Type} | {Val}]},
    Val :: string() | binary(),
    Type :: string() | binary(),
    Results :: {ok, Info} | {error, Reason :: term()},
    Info :: binary().
q_context(Conn, Qid, Value) ->
    q_context(Conn, Qid, Value, []).
%% Bind a typed value to the context item.
-spec q_context(Conn, Qid, Value, Type) -> Results when
    Conn :: pid(),
    Qid :: binary(),
    Value :: string() | binary() | {context, [{Val, Type} | {Val}]},
    Val :: string() | binary(),
    Type :: string() | binary(),
    Results :: {ok, Info} | {error, Reason :: term()},
    Info :: binary().
q_context(Conn, Qid, Value, Type) ->
    gen_server:call(Conn, {q_context, Qid, Value, Type}, ?TIMEOUT).

%% Return the entire result of the query as one value.
-spec q_execute(Conn, Qid) -> Results when
    Conn :: pid(),
    Qid :: binary(),
    Results :: {ok, Result} | {error, Reason :: term()},
    Result :: binary().
q_execute(Conn, Qid) ->
    gen_server:call(Conn, {q_execute, Qid}, ?TIMEOUT).

%% Returns all resulting items of the query as a list of tuples
%% [{Value, Type}]
-spec q_results(Conn, Qid) -> Results when
    Conn :: pid(),
    Qid :: binary(),
    Results :: {ok, Result} | {error, Reason :: term()},
    Result :: [{Value :: binary(), Type :: atom()}].
q_results(Conn, Qid) ->
    gen_server:call(Conn, {q_results, Qid}, ?TIMEOUT).

%% Return query info for a given Qid.
-spec q_info(Conn, Qid) -> Results when
    Conn :: pid(),
    Qid :: binary(),
    Results :: {ok, Info} | {error, Reason :: term()},
    Info :: binary().
q_info(Conn, Qid) ->
    gen_server:call(Conn, {q_info, Qid}, ?TIMEOUT).

%% Return serialization parameters in place for the query.
-spec q_options(Conn, Qid) -> Results when
    Conn :: pid(),
    Qid :: binary(),
    Results :: {ok, Info} | {error, Reason :: term()},
    Info :: binary().
q_options(Conn, Qid) ->
    gen_server:call(Conn, {q_options, Qid}, ?TIMEOUT).

%% De-registers the query with the server. It's a good idea to do this!
-spec q_close(Conn, Qid) -> Results when
    Conn :: pid(),
    Qid :: binary(),
    Results :: ok.
q_close(Conn, Qid) ->
    gen_server:cast(Conn, {q_close, Qid}).
