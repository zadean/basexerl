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
                "('Apple', 'Pear', 'Peach'), trace('some text', 'hidden') "
               ]),

   {ok, _Info} = basexerl:q_bind(Db, Qid, "$name", "number", "xs:string"),
   
   {ok, Results} = basexerl:q_results(Db, Qid),
   {ok, _Info} = basexerl:q_bind(Db, Qid, "$name", "NaN", "xs:string"),
   {ok, Results2} = basexerl:q_results(Db, Qid),
   io:format("~p~n", [Results]),
   io:format("~p~n", [Results2]),
   _ = basexerl:disconnect(Db),
   ok.

