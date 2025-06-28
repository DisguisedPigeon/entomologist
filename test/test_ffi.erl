-module(test_ffi).

-export([id/1, set/1, get/0]).

id(Term) ->
    Term.

set(Conn) ->
    {ok, _Pid} = connection_server:start_link(Conn),
    nil.

get() ->
    gen_server:call(connection_server, get).
