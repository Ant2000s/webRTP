%%%-------------------------------------------------------------------
%%% @doc
%%% Runs mnesia and loads the database, after that it is
%%% engaged in the formation of transactions to it.
%%% @end
%%%-------------------------------------------------------------------
-module(db_manager).

-behavior(gen_server).

-include("abonent.hrl").

%% API
-export([start/0, get_abonent/1, get_abonents/0, post_abonent/2, delete_abonent/1]).

% Callbacks
-export([init/1, handle_call/3]).

%%% API %%%

%% @doc Module start; loading or creating a database.
-spec start() -> {ok, pid()}.
start() ->
 gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Getting abonent data by ID.
-spec get_abonent(integer()) -> [tuple()].
get_abonent(ID) ->
  gen_server:call(?MODULE, {get_abonent, ID}).

%% @doc Getting data of all abonents in database.
-spec get_abonents() -> [tuple()].
get_abonents() ->
  gen_server:call(?MODULE, get_abonents).

%% @doc Add abonent to database.
-spec post_abonent(integer(), string()) -> ok | term().
post_abonent(ID, Password) ->
  gen_server:call(?MODULE, {post_abonent, ID, Password}).

%% @doc Delete abonent from database.
-spec delete_abonent(integer()) -> ok | term().
delete_abonent(ID) ->
  gen_server:call(?MODULE, {delete_abonent, ID}).

%%% CALLBACKS %%%

init(_Args) ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:change_table_copy_type(schema, node(), disc_copies),
  mnesia:create_table(abonent, [{disc_copies, [node()]}, {attributes, record_info(fields, abonent)}]),
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
