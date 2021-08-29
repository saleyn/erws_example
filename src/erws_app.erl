-module(erws_app).
-behaviour(application).
-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([start/0, start/2, stop/1]).

-export([init/1]).

start() ->
    application:ensure_all_started(erws).

%% Application's callback

start(_StartType, _StartArgs) ->
    %% {Host, list({Path, Handler, Opts})}
    %% Dispatch the requests (whatever the host is) to
    %% erws_handler, without any additional options.
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/",           cowboy_static, {file, "priv/index.html"}},
            {"/chart.html", cowboy_static, {file, "priv/chart.html"}},
            {"/erlb.js",    cowboy_static, {file, "priv/erlb.js"}},
            {'_',           erws_handler,  []}
        ]}
    ]),

    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    %% Listen in 10100/tcp for http connections.
    cowboy:start_clear(
        erws_websocket, [{port, 40000}],
        #{env => #{dispatch => Dispatch}}
    ),
    ?LOG_NOTICE("For more detailed logging verbosity run:\n"
                "  logger:set_application_level(erws, info).\n", []),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

%% Supervisor's callback

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.
