-module(erws_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

% Behaviour cowboy_http_handler
-export([init/3, handle/2, terminate/3]).

% Behaviour cowboy_http_websocket_handler
-export([
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).

-record(state, {
    timeout,
    dummy
}).

% Called to know how to dispatch a new connection.
init({tcp, http}, Req, _Opts) ->
    lager:debug("Request: ~p", [Req]),
    % "upgrade" every request to websocket,
    % we're not interested in serving any other content.
    {upgrade, protocol, cowboy_websocket}.

% Should never get here.
handle(Req, State) ->
    lager:debug("Unexpected request: ~p", [Req]),
    {ok, Req2} = cowboy_http_req:reply(404, [
        {'Content-Type', <<"text/html">>}
    ]),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

% Called for every new websocket connection.
websocket_init(_Any, Req, _) ->
    lager:info("New client"),
    Timeout=10,
    erlang:start_timer(Timeout, self(), ticker1),
    erlang:start_timer(Timeout div 3, self(), ticker2),
    Req2 = cowboy_req:compact(Req),
    {ok, Req2, #state{timeout=Timeout, dummy=binary:copy(<<"0">>, 1024)}, hibernate}.

% Called when a text message arrives.
websocket_handle({text, Msg}, Req, State) ->
    lager:info("Received: ~p", [Msg]),
    {reply,
        {text, << "Responding to ", Msg/binary >>},
        Req, State, hibernate
    };

% Called when a binary message arrives.
websocket_handle({binary, Bin}, Req, State) ->
    Reply = case binary_to_term(Bin) of
            {ping, _} -> Bin;
            Other     -> term_to_binary({reply, Other})
            end,
    {reply, {binary, Reply}, Req, State, hibernate};

% With this callback we can handle other kind of
% messages, like binary.
websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({timeout, _Ref, Ticker}, Req, #state{timeout = T, dummy=D} = State) ->
    TO = case Ticker of
         ticker1 -> T;
         _       -> T div 3
         end,
    erlang:start_timer(TO, self(), Ticker),
    X = math:pow(random:uniform() * 2 - 1, 5) * 10000,
    {reply, {binary, term_to_binary({Ticker, X, D})}, Req, State};

% Other messages from the system are handled here.
websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    lager:info("Client disconnected"),
    ok.
