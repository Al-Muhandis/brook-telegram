unit telegramwebhookaction;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, brooktelegramaction, BrookHttpDefs;

type

  { TMyAction }

  TMyAction = class(TWebhookAction)
  public
    constructor Create(ARequest: TBrookRequest; AResponse: TBrookResponse); overload;
      override;
    procedure Post; override;
  end;

implementation

uses BrookException, sysutils, eventlog, brokers;

constructor TMyAction.Create(ARequest: TBrookRequest; AResponse: TBrookResponse
  );
begin
  inherited Create(ARequest, AResponse);
  { This isnt real telegram token! Please get token https://core.telegram.org/bots#botfather }
  Token:='123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11';
  StartText:='Hi! It is simplest HelloBot based on brookframeork and TGBotLazarus!';
  HelpText:='This help text for the bot...';
  { Please enter XXXXXX - [your ]user ID (integer value) for availabality of
  admin commands (/stat, /statf /terminate)}
  UserPermissions.Add('XXXXXX=a');
  { You can do not create this log. If its value is nil,
  then the logging just will not be maintained }
  StatLogger.Paused:=False; // run statistics log
  Logger:=BLogger;
end;

procedure TMyAction.Post;
begin
  { If is is not a valid token passed from parameterized url then error raised }
  if AnsiSameStr(Values.Values['token'], Token) then
    inherited Post
  else
    raise EBrookHttp404.Create('');
end;

initialization
  { There is no need to include a token in the webhook URL, but the url must be secret!
If you do not change the token in future, you can insert your token directly in this code
instead of ":token". In this key no need to check token in overrided Post procedure }
  TMyAction.Register('/webhook/:token/');

end.
