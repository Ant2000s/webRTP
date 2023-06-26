%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(db_manager).

-behavior(gen_server).

-include("abonent.hrl").

%% API
-export([start/0, get_abonent/1, get_abonents/0, post_abonent/2, delete_abonent/1]).

%Callback
-export([init/1, handle_call/3]).

start() ->
 gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_abonent(ID) ->
  gen_server:call(?MODULE, {get_abonent, ID}).

get_abonents() ->
  gen_server:call(?MODULE, get_abonents).

post_abonent(ID, Password) ->
  gen_server:call(?MODULE, {post_abonent, ID, Password}).

delete_abonent(ID) ->
  gen_server:call(?MODULE, {delete_abonent, ID}).

%Callback
init(_Args) ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  %mnesia:create_table(abonent, [{disc_copies, [node()]}, {attributes, record_info(fields, abonent)}]),
  mnesia:create_table(abonent, [{attributes, record_info(fields, abonent)}]),
%%  case mnesia:wait_for_tables([abonent], 30000) of
%%    ok -> lager:info("Database loaded successfully");
%%    _ ->
%%      lager:error("Failed to load database"),
%%      error(db_not_load)
%%  end,
  {ok, no_state}.

handle_call({get_abonent, ID}, _From, State) ->
  {atomic, Result} =
    mnesia:transaction(
      fun() ->
        mnesia:read(abonent, ID)
      end
    ),
  {reply, Result, State};

handle_call(get_abonents, _From, State) ->
  {atomic, Result} =
    mnesia:transaction(
      fun() ->
        mnesia:foldl(
          fun(A, Acc) ->
            [A | Acc] end,
          [],
          abonent
        )
      end
    ),
  {reply, Result, State};

handle_call({post_abonent, ID, Password}, _From, State) ->
  {_, Result} = mnesia:transaction(
    fun() ->
      mnesia:write(#abonent{id = ID, pass = Password})
    end
  ),
  case Result of
    ok -> lager:debug("Abonent added successfully");
    Reason ->
      lager:debug("Abonent has not been added. Reason: [~p]", [Reason])
  end,
  {reply, Result, State};

handle_call({delete_abonent, ID}, _From, State) ->
  {atomic, Result} =
    mnesia:transaction(
      fun() ->
        mnesia:delete({abonent, ID})
      end
    ),
  lager:debug("Abonent [~p] deleted successfully", [ID]),
  {reply, Result, State}.
