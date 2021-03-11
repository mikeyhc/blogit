-module(entry_handler).
-behaviour(cowboy_rest).

-export([init/2, resource_exists/2]).
-export([to_html/2]).

init(Req, _State) ->
    {cowboy_rest, Req, #{}}.

resource_exists(Req, State) ->
    TextDate = cowboy_req:binding(date, Req),
    case logit_html:process_date(TextDate) of
        false -> {false, Req, State};
        Date ->
            case logit_db:entry(Date) of
                {ok, Entry} ->
                    {true, Req, State#{entry => Entry}};
                {error, no_entry} ->
                    {false, Req, State}
            end
    end.

to_html(Req, State=#{entry := Entry}) ->
    {htmerl:render(logit_html:log_page(Entry)), Req, State}.
