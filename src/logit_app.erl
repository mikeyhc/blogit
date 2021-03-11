%%%-------------------------------------------------------------------
%% @doc logit public API
%% @end
%%%-------------------------------------------------------------------

-module(logit_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Routes = [{'_', [{"/", latest_handler, []},
                     {"/admin/add", add_handler, []},
                     {"/styles/[...]", cowboy_static,
                      {priv_dir, logit, "styles"}}
                    ]}
             ],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_clear(logit_listener,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}
                                ),
    logit_db:wait_for_tables(),
    logit_sup:start_link().


stop(_State) ->
    ok.

%% internal functions
