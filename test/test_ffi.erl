-module(test_ffi).

-export([id/1, set/1, get/0]).

-doc("Identity function.\n\nUseful to transform values to Dynamic "
     "in gleam-land").

id(Term) ->
    Term.

-doc("Saves the DB connection to the server and starts it.\n\nThis "
     "is deliberately the only way to write to it, to keep state "
     "nice and immutable.").

set(Conn) ->
    {ok, _Pid} = connection_server:start_link(Conn),
    nil.

-doc("Get the DB connection from the server.").

-spec get() -> Connection :: term().
get() ->
    gen_server:call(connection_server, get).
