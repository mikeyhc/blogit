-module(latest_handler).
-behaviour(cowboy_handler).

-export([init/2]).

make_date({Y, M, D}) ->
    io_lib:format("~4..0w-~2..0w-~2..0w", [Y, M, D]).

log_header(#{date := Date, author := Author}) ->
    htmerl:head(
      [htmerl:title([make_date(Date), " - ", Author]),
       htmerl:add_attributes(htmerl:link(),
                             [htmerl:rel(<<"stylesheet">>),
                              htmerl:href(<<"styles/logit.css">>)
                             ])
      ]).

build_link(Date, Class, Name) ->
    htmerl:add_attributes(
      htmerl:anchor(Name),
      [htmerl:href(["/entry/", make_date(Date)]),
       htmerl:class(Class)]).

next_link(undefined) -> [];
next_link(#{date := Date}) ->
    build_link(Date, "next", "< next").

prev_link(undefined) -> [];
prev_link(#{date := Date}) ->
    build_link(Date, "prev", "prev >").

log_title() ->
    htmerl:h1(<<"Open Development Blog">>).

log_nav(Prev, Next) ->
    [next_link(Next),
     prev_link(Prev)
    ].

log_contents(#{author := Author, date := Date, content := Content}) ->
    [htmerl:h1(make_date(Date)),
     htmerl:h3(Author),
     Content
    ].

log_body(Entry, Prev, Next) ->
    htmerl:add_attributes(
      htmerl:div_tag([htmerl:header(log_title()),
                      htmerl:nav(log_nav(Prev, Next)),
                      htmerl:article(log_contents(Entry))
                     ]),
      [htmerl:class("inner")]).

log_page(Entry, Prev, Next) ->
    htmerl:make_document(log_header(Entry),
                         htmerl:body(log_body(Entry, Prev, Next))).

init(Req0, State) ->
    Entry = logit_db:latest_entry(),
    PrevEntry = logit_db:prev_entry(Entry),
    Document = log_page(Entry, PrevEntry, undefined),
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html">>},
                           htmerl:render(Document),
                           Req0),
    {ok, Req, State}.
