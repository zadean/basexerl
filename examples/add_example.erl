-module(add_example).

-export([run/0]).

run() ->
   {ok, Db} = basexerl:connect(),

   {ok, Info1} = basexerl:create(Db, "database"),
   io:format("~s~n", [Info1]),
   
   {ok, Info2} = basexerl:add(Db, "world/world.xml", "<x>Hello World!</x>"),
   io:format("~s~n", [Info2]),
   
   {ok, Info3} = basexerl:add(Db, "universe.xml", "<x>Hello Universe!</x>"),
   io:format("~s~n", [Info3]),
   
   {ok, Info4} = basexerl:execute(Db, "xquery collection('database')"),
   io:format("~s~n", [Info4]),
   
   {ok, Info5} = basexerl:replace(Db, "universe.xml", "<x>Hello Replacement!</x>"),
   io:format("~s~n", [Info5]),
   
   {ok, Info6} = basexerl:execute(Db, "xquery collection('database')"), 
   io:format("~s~n", [Info6]),
   
   {ok, Info7} = basexerl:execute(Db, "drop db database"), 
   io:format("~s", [Info7]),
   
   ok.
