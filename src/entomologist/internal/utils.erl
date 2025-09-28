-module(utils).

-export([charlist_decoder/1]).

charlist_decoder(List) when is_list(List) ->
    IsValidChar = fun(C) -> is_integer(C) and (C >= 0) and (C =< 255) end,
    case lists:all(IsValidChar, List) of
        true ->
            {ok, List};
        false ->
            {error, nil}
    end;
charlist_decoder(_) ->
    {error, nil}.
