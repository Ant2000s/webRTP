%%%-------------------------------------------------------------------
%% @doc webRTP top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(webRTP_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => rest_for_one,
        intensity => 0,
        period => 1},
    ChildSpecs = [
        #{
            id => db_manager,
            start => {db_manager, start, []},
            restart => permanent
        },
        #{
            id => sender,
            start => {sender, start, []},
            restart => permanent
        },
         #{
             id => http_handler,
             start => {http_handler, start, []},
             restart => permanent
         }
    ],
    {ok, {SupFlags, ChildSpecs}}.

