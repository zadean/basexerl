-module(query_context_example).

-export([run/0]).

run() ->
   {ok, Db} = basexerl:connect(),
   {ok, Qid} = basexerl:query(Db, 
                              [
                               ". "                              
                              ]),
   {ok, _Info} = basexerl:q_context(Db, Qid, {context, [
                                                   {"123", "xs:integer"},
                                                   {"ABC"}
                                                   ]}, []),
   {ok, Result} = basexerl:q_results(Db, Qid),
   
   io:format("~p~n", [Result]),
   ok.
