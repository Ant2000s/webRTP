
```
ConvertVoice = "ffmpeg -i priv/voice/generate.wav -codec:a pcm_mulaw -ar 8000 -ac 1 priv/voice/output.wav -y",
StartVoice = "./voice_client priv/voice/output.wav " ++ PBX_IP ++ " " ++ erlang:integer_to_list(Port),
Cmd = ConvertVoice ++ " && " ++ StartVoice,
Res = os:cmd(Cmd),
```