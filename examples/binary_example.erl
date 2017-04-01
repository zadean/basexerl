-module(binary_example).

-export([run/0]).

run() ->
   {ok, Db} = basexerl:connect("localhost", 6001, "admin", "admin"),

   Bin = list_to_binary([<<X>> || X <- lists:seq(1,256)]),
   
   {ok, Info1} = basexerl:create(Db, "database2"),
   io:format("~s~n", [Info1]),

   {ok, Info2} = basexerl:store(Db, "test.bin", Bin),
   io:format("~s~n", [Info2]),
   
   {ok, Bin2} = basexerl:retrieve(Db, "test.bin"),
   io:format("Bin : ~p~n", [Bin]),
   io:format("Bin2: ~p~n", [Bin2]),

   io:format("They match?: ~s~n", [matches(Bin2)]),

   {ok, Info3} = basexerl:execute(Db, "drop db database2"), 
   io:format("~s~n", [Info3]),
   
   ok.

matches(Bin) ->
   case list_to_binary([<<X>> || X <- lists:seq(1,256)]) of
      Bin ->
         true;
      _ ->
         false
   end.
 