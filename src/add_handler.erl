-module(add_handler).
-behaviour(cowboy_rest).

-export([init/2, allowed_methods/2, content_types_accepted/2]).
-export([from_text/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"text">>, <<"plain">>, '*'}, from_text}], Req, State}.

from_text(Req0, State) ->
    {ok, Body, Req1} = read_body(Req0, <<>>),
    case process_body(Body) of
        {ok, Date={Y, M, D}, Author, Content} ->
            logit_db:add_entry(Date, Author, Content),
            Req = cowboy_req:set_resp_body(<<"ok">>, Req1),
            {{true, io_lib:format("/entry/~w-~w-~w", [Y, M, D])}, Req, State};
        {error, Message} ->
            Req = cowboy_req:set_resp_body(Message, Req1),
            {false, Req, State}
    end.

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

process_body(Body) ->
    case binary:split(Body, [<<"\n">>, <<"\r">>], [global]) of
        [Date, Author|Rest] ->
            case logit_html:process_date(Date) of
                {Y, M, D} ->
                    SpacedContent = lists:join(<<"\n">>, Rest),
                    {ok, {Y, M, D}, Author,
                     binary:list_to_bin(SpacedContent)};
                _ -> {error, <<"invalid date">>}
            end;
        _ -> {error, <<"invalid file layout">>}
    end.
