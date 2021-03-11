-module(latest_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Entry = logit_db:latest_entry(),
    Document = logit_html:log_page(Entry),
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html">>},
                           htmerl:render(Document),
                           Req0),
    {ok, Req, State}.
