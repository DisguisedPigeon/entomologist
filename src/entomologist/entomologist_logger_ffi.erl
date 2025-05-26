-module(entomologist_logger_ffi).

-moduledoc("A erlang logger custom handler.\n\nIt saves logs to a DB with "
           "a specific schema.").

-export([log/2, configure/1]).

-behaviour(logger_handler).

configure(DbConnection) ->
        logger:add_handler(entomologist,
                           entomologist_logger_ffi,
                           #{config => #{connection => DbConnection}}),
        {ok, nil}.

log(#{msg := {string, String},
      level := Level,
      meta := Metadata},
    #{config := #{connection := Connection}})
        when is_binary(String) ->
        entomologist:save_to_db(#{msg => String,
                                  level => atom_to_list(Level),
                                  meta => Metadata,
                                  rest => filter_parsed_and_to_json(Metadata)},
                                Connection);
log(#{msg := {string, String},
      level := Level,
      meta := Metadata},
    #{config := #{connection := Connection}}) ->
        entomologist:save_to_db(#{msg => binary_representation(String),
                                  level => atom_to_list(Level),
                                  meta => Metadata,
                                  rest => filter_parsed_and_to_json(Metadata)},
                                Connection);
log(#{msg := Msg,
      level := Level,
      meta := Metadata},
    #{config := #{connection := Connection}}) ->
        entomologist:save_to_db(#{msg => binary_representation(Msg),
                                  level => atom_to_list(Level),
                                  meta => Metadata,
                                  rest => filter_parsed_and_to_json(Metadata)},
                                Connection);
log(_, _) ->
        error.

filter_parsed_and_to_json(Meta) ->
        iolist_to_binary(json:encode(
                                 maps:filter(fun(K, _) ->
                                                lists:any(fun(K2) -> K == K2 end,
                                                          [time,
                                                           module,
                                                           function,
                                                           arity,
                                                           file,
                                                           line])
                                             end,
                                             Meta))).

binary_representation(Term) ->
        list_to_binary(io_lib:format("~p", [Term])).
