-module(entomologist_logger_ffi).

-moduledoc("A erlang logger custom handler.\n\nIt saves logs to a DB with "
           "a specific schema defined in github.com/DisguisedPigeon/entomologist"
           "'s README.md").

-export([log/2, configure/1, to_charlist/1]).

-behaviour(logger_handler).

-doc("entomologist configuration function for entomologist's the "
     "logger handler.\n\nIt adds this module to the logger registered "
     "handlers with a gleam `pog.Connection` value in the config.").

configure(DbConnection) ->
    case logger:add_handler(entomologist,
                            entomologist_logger_ffi,
                            #{config => #{connection => DbConnection}})
    of
        ok ->
            {ok, nil};
        {error, _} ->
            {error, nil}
    end.

-doc("Saves a log message to DB using the gleam entomologist module.\n\nTh"
     "is is a callback called by kernel:logger.\n\nIt casts the msg "
     "field to a string in various manners if needed.").

log(#{msg := {string, String},
      level := Level,
      meta := Metadata},
    #{config := #{connection := Connection}})
    when is_binary(String) ->
    entomologist@internal@logger_api:save_to_db(#{msg_str => String,
                                                  level => atom_to_list(Level),
                                                  meta => Metadata,
                                                  rest => filter_parsed_and_to_json(Metadata)},
                                                Connection),
    nil;
log(#{msg := {string, String},
      level := Level,
      meta := Metadata},
    #{config := #{connection := Connection}}) ->
    entomologist@internal@logger_api:save_to_db(#{msg_str => binary_representation(String),
                                                  level => atom_to_list(Level),
                                                  meta => Metadata,
                                                  rest => filter_parsed_and_to_json(Metadata)},
                                                Connection),
    nil;
log(#{msg := {report, Msg},
      level := Level,
      meta := Metadata},
    #{config := #{connection := Connection}}) ->
    entomologist@internal@logger_api:save_to_db(#{msg_dict => Msg,
                                                  level => atom_to_list(Level),
                                                  meta => Metadata,
                                                  rest => filter_parsed_and_to_json(Metadata)},
                                                Connection);
log(_, _) ->
    error.

-doc("Represents an arbitrary value as a json string.\n\n This is "
     "used to save the metadata in gleam-land case it holds custom "
     "fields").

filter_parsed_and_to_json(Meta) ->
    iolist_to_binary(json:encode(
                         maps:filter(fun(K, _) ->
                                        lists:any(fun(K2) -> K == K2 end,
                                                  [time, module, function, arity, file, line])
                                     end,
                                     Meta))).

-doc("Represents an arbitrary value as a string.\n\n This is used "
     "in case the log function is called with an unrecognized type "
     "on the msg field.").

binary_representation(Term) ->
    list_to_binary(io_lib:format("~p", [Term])).

to_charlist(List) when is_list(List) ->
    IsValidChar = fun(C) -> is_integer(C) and (C >= 0) and (C =< 255) end,
    case lists:all(IsValidChar, List) of
        true ->
            {ok, List};
        false ->
            {error, nil}
    end;
to_charlist(_) ->
    {error, nil}.
