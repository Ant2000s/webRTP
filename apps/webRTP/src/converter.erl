%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(converter).

%% API
-export([text_to_wav/2]).

text_to_wav(Text, ResultFile) ->
  GenVoice = "wget -O generate.wav \"https://tts.voicetech.yandex.net/generate?format=wav&lang=ru_RU&key=069b6659-984b-4c5f-880e-aaedcfd84102&text="
    ++ Text ++ "\"",
  ConvertVoice = "ffmpeg -i generate.wav -codec:a pcm_mulaw -ar 8000 -ac 1 "++ ResultFile ++ " -y",
  case string:find(os:cmd(GenVoice), "'generate.wav' saved", trailing) of
    nomatch ->
      lager:error("Unable to convert text to voice message"),
      {error, convert_failed};
    _ ->
      lager:debug("Successful convert text to voice message"),
      case string:find(os:cmd(ConvertVoice),"Output #0, wav, to '" ++ ResultFile ++ "'", trailing) of
        nomatch ->
          lager:error("Unable to convert voice message for transfer"),
          {error, convert_failed};
        _ ->
          lager:info("Successful convert voice message for transfer"),
          ok
      end
  end.