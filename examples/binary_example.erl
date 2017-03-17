-module(binary_example).

-export([run/0]).

run() ->
   {ok, Db} = basexerl:connect(),

   {ok, Info1} = basexerl:create(Db, "database"),
   io:format("~s~n", [Info1]),
   
   Bin = list_to_binary([<<X>> || X <- lists:seq(1,256)]),
   
   {ok, Info2} = basexerl:store(Db, "test.bin", Bin),
   io:format("~s~n", [Info2]),
   
   {ok, Bin2, Info3} = basexerl:execute(Db, "retrieve test.bin"),
   io:format("~s~n", [Info3]),

   io:format("They match?: ~s~n", [matches(list_to_binary(Bin2))]),

   {ok, Results4, Info7} = basexerl:execute(Db, "drop db database"), 
   io:format("~s~s", [Info7, Results4]),
   
   ok.

matches(Bin) ->
   case list_to_binary([<<X>> || X <- lists:seq(1,256)]) of
      Bin ->
         true;
      _ ->
         false
   end.
