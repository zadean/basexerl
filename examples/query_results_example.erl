-module(query_results_example).

-export([run/0]).

run() ->
   {ok, Db} = basexerl:connect(),
   {ok, Qid} = basexerl:query(Db, 
               [
                "declare variable $name external; " 
                "'this is a string', "
                "1.05, (:decimal:) "
                "document { element root {"
                "for $i in 1 to 10 "
                "return element { $name } { $i } "
                "}},"
                "for $i in 1 to 10 "
                "return element { $name } { $i } "
                ","
                "<?processing instruction?>, "
                "comment {'text - comment'}, "
                "fn:substring('something', 1, 4), "
                "fn:substring(?, 1, 4), "
                "('Apple', 'Pear', 'Peach') "
               ]),

   {ok, _Info} = basexerl:q_bind(Db, Qid, "$name", "number", "xs:string"),
   
   {ok, Results} = basexerl:q_results(Db, Qid),
    io:format("~p~n", [Results]),
   _ = basexerl:disconnect(Db),
   ok.

