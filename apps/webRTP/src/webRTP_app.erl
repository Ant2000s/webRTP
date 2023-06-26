%%%-------------------------------------------------------------------
%% @doc webRTP public API
%% @end
%%%-------------------------------------------------------------------

-module(webRTP_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    webRTP_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
