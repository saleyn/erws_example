-module(erws_app).
-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1]).

-export([init/1]).

%% Application's callback

start(_StartType, _StartArgs) ->
    %% {Host, list({Path, Handler, Opts})}
    %% Dispatch the requests (whatever the host is) to
    %% erws_handler, without any additional options.
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {file, "priv/index.html"}},
            {"/chart.html", cowboy_static, {file, "priv/chart.html"}},
            {"/erlb.js", cowboy_static, {file, "priv/erlb.js"}},
            {'_', erws_handler, []}
        ]}
    ]),

    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    %% Listen in 10100/tcp for http connections.
    cowboy:start_http(
        erws_websocket, 100, [{port, 40000}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

%% Supervisor's callback

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.
