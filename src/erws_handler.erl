-module(erws_handler).

-include_lib("kernel/include/logger.hrl").

% Handle HTTP requests
-export([init/2, handle/2, terminate/3]).

% Handle Websocket requests after upgrate of connection from HTTP
-export([
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

-record(state, {
    timeout :: integer(),   % How often to send messages to client
    dummy   :: binary()     % Dummy payload to send to client
}).

% Called to know how to dispatch a new connection.
init(Req, _Opts) ->
    ?LOG_INFO("New client"),
    ?LOG_DEBUG("Request: ~p", [Req]),
    % "upgrade" every request to websocket,
    % we're not interested in serving any other content.
    Timeout = 1,
    %Req2  = cowboy_req:compact(Req),
    State = #state{timeout=Timeout, dummy=binary:copy(<<"0">>, 1024)},
    {cowboy_websocket, Req, State}.

% Should never get here.
handle(Req, State) ->
    ?LOG_INFO("Unexpected request: ~p", [Req]),
    {ok, Req2} = cowboy_http_req:reply(
        404, [{'Content-Type', <<"text/html">>}
    ]),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ?LOG_INFO("Client disconnected"),
    ok.

websocket_init(#state{timeout = Timeout} = State) ->
    F = fun() ->
        case rand:uniform(2) of
        1 -> ticker1;
        2 -> ticker2
        end
    end,
    erlang:start_timer(Timeout, self(), F),
    {ok, State}.

% Called when a text message arrives.
websocket_handle({text, Msg}, State) ->
    ?LOG_INFO("Received: ~p", [Msg]),
    {reply,
        {text, <<"Server's response: ", Msg/binary>>},
        State, hibernate
    };

% Called when a binary message arrives.
websocket_handle({binary, Bin}, State) ->
    Reply = case binary_to_term(Bin) of
            {ping, _} -> Bin;
            Other     -> term_to_binary({reply, Other})
            end,
    {reply, {binary, Reply}, State, hibernate};

% With this callback we can handle other kind of
% messages, like binary.
websocket_handle(_Any, State) ->
    {ok, State}.

websocket_info({timeout, _Ref, TickerF}, #state{timeout = T, dummy=D} = State) ->
    erlang:start_timer(T, self(), TickerF),
    TickerName = TickerF(),
    X = math:pow(rand:uniform() * 2 - 1, 3) * 250,
    {reply, {binary, term_to_binary({TickerName, X, D})}, State};

% Other messages from the system are handled here.
websocket_info(_Info, State) ->
    {ok, State, hibernate}.
