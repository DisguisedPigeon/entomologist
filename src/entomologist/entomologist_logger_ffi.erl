-module(entomologist_logger_ffi).

-moduledoc("A erlang logger custom handler.\n\nIt saves logs to a DB with "
           "a specific schema.").

-export([log/2, configure/1]).

-type result() :: ok | error.
-type cooler_result() :: {ok, nil} | {error, nil}.

-spec configure(term()) -> cooler_result().
configure(Connection) ->
        Result = logger:add_handler(entomologist,
                                    entomologist_logger_ffi,
                                    #{config => #{connection => Connection}}),
        case Result of
                ok ->
                        {ok, nil};
                {error, _} ->
                        {error, nil}
        end.

-spec log(LogEvent :: logger:log_event(), Config :: logger_handler:config()) -> result().
-doc("**Erlang logger callback**.\n\nIt returns ok if the logged "
     "message was saved correctly and error if anything stopped it.\n\nThe "
     "app should not crash if error is returned, since if it did, "
     "the library might enter an infinite loop").

log(#{level := Level,
      msg := Message,
      meta :=
              #{time := Timestamp,
                mfa := {Module, Function, Arity},
                file := Filename,
                line := Line}},
    #{config := #{connection := Connection}}) ->
        Data = case Message of
                       {string, String} when is_binary(String) ->
                               String;
                       {string, String} ->
                               binary_representation(String);
                       Idk ->
                               binary_representation(Idk)
               end,

        Metadata =
                {metadata,
                 Timestamp,
                 binary_representation(Module),
                 binary_representation(Function),
                 Arity,
                 binary_representation(Filename),
                 Line},

        case entomologist:save_to_db({log, Level, Data, Metadata}, {connection, Connection}) of
                {ok, _} ->
                        ok;
                {error, _} ->
                        error
        end;
log(_, _) ->
        error.

-spec binary_representation(term()) -> binary().
binary_representation(Term) ->
        list_to_binary(io_lib:format("~p", [Term])).
