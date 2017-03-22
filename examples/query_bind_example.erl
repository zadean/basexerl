-module(query_bind_example).

-export([run/0]).

run() ->
   {ok, Db} = basexerl:connect(),
   {ok, Qid} = basexerl:query(Db, 
                              [
                               "declare variable $name external; ",
                               "declare variable $what external; ", 
                               "declare variable $seq external; ", 
                               "for $i in 1 to 10 ",
                               "return element { $name } {"
                               " attribute none { $what }, $i },"
                               "$seq "                              
                              ]),

   {ok, _Info} = basexerl:q_bind(Db, Qid, "$name", "number", "xs:string"),
   {ok, _Info} = basexerl:q_bind(Db, Qid, "$what", "some"),
   {ok, _Info} = basexerl:q_bind(Db, Qid, {"$seq", [
                                                   {"123", "xs:integer"},
                                                   {"ABC"}
                                                   ]}, []),
   
   {ok, Result} = basexerl:q_execute(Db, Qid),
   
   io:format("~s~n", [Result]),
   ok.
