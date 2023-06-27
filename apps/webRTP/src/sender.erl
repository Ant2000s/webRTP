%%%-------------------------------------------------------------------
%%% @doc
%%% Performs abonent registration on server and then makes calls.
%%% @end
%%%-------------------------------------------------------------------
-module(sender).

-include_lib("nksip/include/nksip.hrl").

-behaviour(gen_server).
%% API
-export([start/0, call/3]).

% Callback
-export([init/1, handle_call/3]).

-define(ID, 101).
-define(PASS, "1234").
-define(DOMAIN, "test.domain").
-define(IP, "10.0.20.11").

%%% API %%%

%% @doc Module start; registration abonent on the server.
-spec start() -> {ok, pid()}.
start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

%% @doc Calls abonent and plays voice message.
-spec call(integer(), string(), string()) -> ok | {error,
                      metadata_not_received | call_failed}.
call(AbonentId, AbonentPass, VoiceFile) ->
  gen_server:call(?MODULE, {AbonentId, AbonentPass, VoiceFile}, 25000).

%%% CALLBACKS %%%

init(_Args) ->
  FirstAbonent = "sip:" ++ integer_to_list(?ID) ++ "@" ++ ?DOMAIN,
  Options = #{
    sip_from => FirstAbonent,
    plugins => [nksip_uac_auto_auth],
    sip_listen => "<sip:all:5551>"
  },
  case nksip:start_link(first_abonent, Options) of
    {ok, _} ->
      lager:debug("Nksip start with abonent ~p", [FirstAbonent]),
      case nksip_uac:register(first_abonent, "sip:" ++ ?IP, [{sip_pass, ?PASS}, contact, {get_meta, [<<"contact">>]}]) of
        {ok, 200, _} ->
          lager:info("Successful registration abonent [~p] with URI [~p]", [first_abonent, "sip:" ++ ?IP]),
          SDP = nksip_sdp:new(?IP, [{<<"audio">>, 5060, [{rtpmap, 0, <<"PCMU/8000">>}]}]),
          {ok, SDP};
        {error, Error} ->
          lager:error("Unsuccessful attempt to register abonent [~p] with URI [~p] ", [first_abonent, "sip:" ++ ?IP]),
          {error, Error}
      end;
    {error, Error} ->
      lager:error("Failed to start Nksip"),
      {error, Error}
  end.

handle_call({AbonentId, AbonentPass, VoiceFile}, _From, SDP) ->
  Abonent = "sip:" ++ integer_to_list(AbonentId) ++"@" ++ ?IP,
  Result =
    case nksip_uac:invite(first_abonent, Abonent, [auto_2xx_ack, {sip_pass, AbonentPass},{body, SDP}]) of
      {ok, 200, [{dialog, DialogId}]} ->
        lager:info("Successful connection with the abonent [~p]", [Abonent]),
        %nksip_uac:ack(DialogId, []),
        case nksip_dialog:get_meta(invite_remote_sdp, DialogId) of
          {ok, AnswerSDP} ->
            lager:debug("Successful metadata acquisition"),
            [SDP_M | _] = AnswerSDP#sdp.medias,
            Port = SDP_M#sdp_m.port,
            {_, _, VoiceIP} = SDP_M#sdp_m.connect,
            Cmd = "./voice_client " ++ VoiceFile ++ " " ++
              binary_to_list(VoiceIP) ++ " " ++ integer_to_list(Port),
            os:cmd(Cmd),
            lager:debug("Voice message sent, call ends"),
            nksip_uac:bye(DialogId, []),
            ok;
          {error, _Error} ->
            lager:error("Failed to get metadata, unable to send voice message"),
            {error, metadata_not_received}
        end;
      _Error ->
        lager:error("Failed to call abonent [~p]", [Abonent]),
        {error, call_failed}
    end,
  {reply, Result, SDP}.