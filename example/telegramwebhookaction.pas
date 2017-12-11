unit telegramwebhookaction;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, brooktelegramaction, BrookHttpDefs, tgtypes;

type

  { TMyAction }

  TMyAction = class(TWebhookAction)
  protected
    procedure TlgrmTestCmdHandler(ASender: TObject; const ACommand: String;
      AMessage: TTelegramMessageObj);
  public
    constructor Create(ARequest: TBrookRequest; AResponse: TBrookResponse); overload;
      override;
    procedure Post; override;
  end;

implementation

uses BrookException, sysutils, brokers;

{ Handler for the "TestCmd" telegram command (It is called through /TestCmd in chat with bot) }
procedure TMyAction.TlgrmTestCmdHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  Bot.RequestWhenAnswer:=True; // You can set to False if you want to see Telegram endpoint reply
  Bot.sendMessage('This is the response to the '+ACommand+
    ' command which processed by the TlgrmTestCmd procedure');
end;

constructor TMyAction.Create(ARequest: TBrookRequest; AResponse: TBrookResponse
  );
begin
  inherited Create(ARequest, AResponse);
  { This isnt real telegram token! Please get token from
  https://core.telegram.org/bots#botfather }
  Token:='123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11';
  StartText:='Hi! It is simplest HelloBot based on brookframework and fp-telegram!';
  HelpText:=
    'This help text for the bot... See code at https://github.com/Al-Muhandis/brook-telegram '+
    'and  https://github.com/Al-Muhandis/fp-telegram';
  { Custom command definition example }
  Bot.CommandHandlers['/testcmd']:=@TlgrmTestCmdHandler;

  { Please enter XXXXXX - [your ]user ID (integer value) for availabality of
  admin commands (/stat, /statf /terminate)}
  UserPermissions.Add('XXXXXX=a');  // You can comment out this if you do not need this functionality
  { You can do not create this log. If its value is nil,
  then the logging just will not be maintained }
  StatLogger.Paused:=False; // run statistics log
  Logger:=BLogger;
end;

procedure TMyAction.Post;
begin
  { If is is not a valid token passed from parameterized url then error raised }
  if AnsiSameStr({$IFNDEF bf30}Variables{$ELSE}Values{$ENDIF}.Values['token'], Token) then
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
