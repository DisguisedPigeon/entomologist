-module(entomologist_logger_ffi).

-moduledoc """
A erlang logger custom handler.

It saves logs to a DB with the specific schema defined in github.com/DisguisedPigeon/entomologist's README.md
""".

-behaviour(logger_handler).
-export([log/2, configure/1, id/1]).

-doc """
Entomologist configuration function for entomologist's logger handler.

It adds this module to the logger registered
handlers with a gleam `pog.Connection` value in the config.
""".

-spec configure(
    DbConnection :: pgo_pool:conn()
) -> {ok, nil} | {error, nil}.
configure(DbConnection) ->
    case
        logger:add_handler(
            entomologist,
            entomologist_logger_ffi,
            #{config => #{connection => DbConnection}}
        )
    of
        ok ->
            {ok, nil};
        {error, _} ->
            {error, nil}
    end.

-doc """
Saves a log message to DB using the gleam entomologist module.

This is a callback called by kernel:logger.

It casts the msg field to a string in case its needed.
""".
-spec log(Msg, Config) -> nil() when
    Msg :: #{
        msg :=
            {string, string()}
            | {string, term()}
            | {report, term()},
        level := atom(),
        meta := term()
    },
    Config :: #{
        config := #{connection := term()}
    }.
log(
    #{
        msg := {string, String},
        level := Level,
        meta := Metadata
    },
    #{config := #{connection := Connection}}
) when
    is_binary(String)
->
    % elp:ignore W0017 (undefined_function)
    entomologist@internal@logger_api:save_to_db(
        #{
            msg_str => String,
            level => atom_to_list(Level),
            meta => Metadata,
            rest => filter_parsed_and_to_json(Metadata)
        },
        Connection
    ),
    nil;
log(
    #{
        msg := {string, String},
        level := Level,
        meta := Metadata
    },
    #{config := #{connection := Connection}}
) ->
    % elp:ignore W0017 (undefined_function)
    entomologist@internal@logger_api:save_to_db(
        #{
            msg_str => binary_representation(String),
            level => atom_to_list(Level),
            meta => Metadata,
            rest => filter_parsed_and_to_json(Metadata)
        },
        Connection
    ),
    nil;
log(
    #{
        msg := {report, Msg},
        level := Level,
        meta := Metadata
    },
    #{config := #{connection := Connection}}
) ->
    % elp:ignore W0017 (undefined_function)
    entomologist@internal@logger_api:save_to_db(
        #{
            msg_dict => Msg,
            level => atom_to_list(Level),
            meta => Metadata,
            rest => filter_parsed_and_to_json(Metadata)
        },
        Connection
    );
log(_, _) ->
    error.

-doc """
Represents an arbitrary value as a json string.

This is used to save the metadata in gleam-land in case it holds custom fields.
""".

-spec filter_parsed_and_to_json(Meta) -> binary() when
    Meta :: #{
        time | module | function | arity | file | line := term(),
        term := term()
    }.
filter_parsed_and_to_json(Meta) ->
    iolist_to_binary(
        json:encode(
            maps:filter(
                fun(K, _) ->
                    lists:any(
                        fun(K2) -> K == K2 end,
                        [time, module, function, arity, file, line]
                    )
                end,
                Meta
            )
        )
    ).

-doc """
Represents an arbitrary value as a string.

This is used in case the log function is called with an unrecognized type on the msg field.
""".

-spec binary_representation(term()) -> binary().
binary_representation(Term) ->
    list_to_binary(io_lib:format("~p", [Term])).

-doc """
Returns the same term that is provided. This functions is useful to transform gleam types through the ffi.
""".
-spec id(term()) -> term().
id(Term) ->
    Term.
