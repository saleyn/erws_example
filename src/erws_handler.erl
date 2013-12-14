-module(erws_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

% Behaviour cowboy_http_handler
-export([init/3, handle/2, terminate/3]).

% Behaviour cowboy_websocket_handler
-export([
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).

-record(state, {
    timeout :: integer(),   % How often to send messages to client
    dummy   :: binary()     % Dummy payload to send to client
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
    {ok, Req2} = cowboy_http_req:reply(
        404, [{'Content-Type', <<"text/html">>}
    ]),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

% Called for every new websocket connection.
websocket_init(_Any, Req, _) ->
    lager:info("New client"),
    Timeout = 5,
    F = fun() ->
        case random:uniform(2) of
        1 -> ticker1;
        2 -> ticker2
        end
    end,
    erlang:start_timer(Timeout, self(), F),
    Req2 = cowboy_req:compact(Req),
    {ok, Req2, #state{timeout=Timeout, dummy=binary:copy(<<"0">>, 1024)}, hibernate}.

% Called when a text message arrives.
websocket_handle({text, Msg}, Req, State) ->
    lager:info("Received: ~p", [Msg]),
    {reply,
        {text, <<"Server's response: ", Msg/binary>>},
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

websocket_info({timeout, _Ref, TickerF}, Req, #state{timeout = T, dummy=D} = State) ->
    erlang:start_timer(T, self(), TickerF),
    TickerName = TickerF(),
    X = math:pow(random:uniform() * 2 - 1, 3) * 1000,
    {reply, {binary, term_to_binary({TickerName, X, D})}, Req, State};

% Other messages from the system are handled here.
websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    lager:info("Client disconnected"),
    ok.
