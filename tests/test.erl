f().
{ok, Db3} = basexerl:connect().
basexerl:create(Db3, "test1").
MyBin = erlang:term_to_binary(supervisor:count_children(bxe_sup)).
basexerl:store(Db3, "erl.cnt", MyBin).
{ok, Bin3, _} = basexerl:execute(Db3, "retrieve erl.cnt").

f().
{ok, Db} = basexerl:connect().
basexerl:create(Db, "test").

MyBin = erlang:term_to_binary(supervisor:count_children(bxe_sup)).
basexerl:store(Db3, "erl.cnt", MyBin).
{ok, Bin3, _} = basexerl:execute(Db3, "retrieve erl.cnt").


fprof:apply({query_results_example,run}, []).
fprof:profile().
fprof:analyse().

