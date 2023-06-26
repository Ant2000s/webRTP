%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(http_handler).

-include("abonent.hrl").
%API
-export([start/0]).

-export([init/2, handler/4]).

start() ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/call/abonent/[...]", http_handler, [call_abonent]},
      {"/call/broadcast/[...]", http_handler, [broadcast]},
      {"/abonent/[...]", http_handler, [abonent]},
      {"/abonents/", http_handler, [abonents]}
    ]}
  ]),

  {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
    env => #{dispatch => Dispatch}
  }).

init(Req, State) ->
  Method = cowboy_req:method(Req),
  PathInfo = cowboy_req:path_info(Req),

  NewReq = handler(Method, State, PathInfo, Req),
  {ok, NewReq, State}.

handler(<<"GET">>, [call_abonent], [BinID, BinMessage], Req) ->
  ID = binary_to_integer(BinID),
  lager:info("Call to abonent [~p]", [ID]),
  Result = db_manager:get_abonent(ID),
  ResultFile = "output.wav",
  % Checking presence abonent in database
  {Code, ResultMessage} = case Result of
    [] ->
      lager:debug("Abonent [~p] not found in database", [ID]),
      {400, <<"Fail. Abonent not found">>};
    [Abonent] ->
      lager:debug("Abonent [~p] successfully found in database", [ID]),
      % Message validation
      [Message] = string:lexemes(binary_to_list(BinMessage), [$\"]),
      case converter:text_to_wav(Message, ResultFile) of
        {error, convert_failed} ->
          {400, <<"Fail. Text not converted">>};
        ok ->
          % Сall to abonent
          case sender:call(ID, Abonent#abonent.pass, ResultFile) of
            ok ->
              {200, <<"Success. Message delivered">>};
            {error, metadata_not_received} ->
              {400, <<"Fail. Connection established but message not sent">>};
            {error, call_failed} ->
              {400, <<"Fail. Call failed">>}
          end
      end
  end,
  cowboy_req:reply(Code,
    #{<<"content-type">> => <<"text/plain">>},
    ResultMessage,
    Req),
  ok;

handler(<<"GET">>, [broadcast], [BinMessage], Req) ->
  lager:info("Request for calling abonents"),
  AbonentList = db_manager:get_abonents(),
  ResultFile = "output.wav",
  % Message validation
  [Message] = string:lexemes(binary_to_list(BinMessage), [$\"]),
  case converter:text_to_wav(Message, ResultFile) of
    {error, convert_failed} ->
      cowboy_req:reply(400,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Fail. Text not converted">>,
        Req);
    ok ->
      % Сall to abonents
      lager:debug("The broadcast has started. ~p calls will be made", [length(AbonentList)]),
      AnswerList = [{list_to_atom("abonent_" ++ integer_to_list(ID)),
        sender:call(ID, Pass, ResultFile)}
        || {_, ID, Pass} <- AbonentList],
      lager:debug("The broadcast has ended"),
      cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsone:encode(maps:from_list(AnswerList)),
        Req)
  end,
  ok;

handler(<<"POST">>, [abonent], _, Req) ->
  lager:info("Request to add abonent"),
  {ok, Body, _} = cowboy_req:read_body(Req),
  Decoded = jsone:decode(Body),
  ID = maps:get(<<"ID">>, Decoded, undefined),
  Pass = maps:get(<<"Pass">>, Decoded, undefined),
  {Code, Result} = case (ID =:= undefined) or (Pass =:= undefined) of
    true ->
      lager:debug("Required fields are missing in the request body [~p]", [Decoded]),
      {400, <<"Fail. Required fields missing">>};
    false ->
      ok = db_manager:post_abonent(ID, Pass),
      {200, <<"Success. Abonent add to database">>}
  end,
  cowboy_req:reply(Code,
    #{<<"content-type">> => <<"text/plain">>},
    Result, Req),
  ok;

handler(<<"GET">>, [abonent], [BinID], Req) ->
  lager:info("Abonent get request"),
  ID = binary_to_integer(BinID),
  Result = db_manager:get_abonent(ID),
  case Result of
    [] ->
      cowboy_req:reply(400,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Fail. Abonent not found">>, Req);
    [Abonent] ->
      cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsone:encode(#{ id => Abonent#abonent.id, pass => Abonent#abonent.pass}), Req)
  end;
handler(<<"DELETE">>, [abonent], [ID], Req) ->
  lager:info("Abonent delete request"),
  Result = db_manager:delete_abonent(binary_to_integer(ID)),
  cowboy_req:reply(200, #{
    <<"content-type">> => <<"application/json">>},
    jsone:encode(#{result => Result}), Req),
  ok;

handler(<<"GET">>, [abonents], _, Req) ->
  lager:info("Abonents list requested"),
  ListID = [ID || {_, ID, _} <- db_manager:get_abonents()],
  cowboy_req:reply(200, #{
    <<"content-type">> => <<"application/json">>},
    jsone:encode(#{result => ListID}), Req),
  ok;

handler(_, _, _, Req) ->
  lager:debug("Bad Request"),
  cowboy_req:reply(400, #{
    <<"content-type">> => <<"text/plain">>},
    <<"Bad Request">>, Req),
  ok.