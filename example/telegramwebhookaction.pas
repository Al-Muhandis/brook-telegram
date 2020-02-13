unit telegramwebhookaction;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, brooktelegramaction, tgtypes;

type

  { TMyAction }

  TMyAction = class(TWebhookAction)
  protected
    procedure TlgrmTestCmdHandler({%H-}ASender: TObject; const ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    procedure TlgrmMyUserIDCmdHandler({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    procedure TlgrmMyHelpMarkdownHandler({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    procedure TlgrmReceiveMessage({%H-}ASender: TObject; AMessage: TTelegramMessageObj);
  public
    constructor Create; overload;
      override;
    procedure Post; override;
  end;

implementation

uses BrookException, sysutils, brokers, tgsendertypes, tgutils, tgstatlog;

{ Handler for the "TestCmd" telegram command (It is called through /TestCmd in chat with bot) }
procedure TMyAction.TlgrmTestCmdHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  Bot.RequestWhenAnswer:=True; // You can set to False if you want to see Telegram endpoint reply
  Bot.sendMessage('This is the response to the '+ACommand+
    ' command which processed by the TlgrmTestCmd procedure');
end;
{ A simple example getting ID of the message sender it is Yours }
procedure TMyAction.TlgrmMyUserIDCmdHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  Bot.EditOrSendMessage(MarkdownEscape(AMessage.From.First_name)+
    '! [Your](tg://user?id='+IntToStr(AMessage.From.ID)+') user ID is '+IntToStr(AMessage.From.ID),
    pmMarkdown);
end;

procedure TMyAction.TlgrmMyHelpMarkdownHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  Bot.EditOrSendMessage('markdown'+LineEnding+'**bold text**'+LineEnding+'__italic text__'+LineEnding+
    '`inline fixed-width code`'+LineEnding+'```block_language'+LineEnding+
    'pre-formatted fixed-width code block```', pmDefault);
end;

{ A simple example of a repeater of the text that creates a code formatting when the bot responds }
procedure TMyAction.TlgrmReceiveMessage(ASender: TObject;
  AMessage: TTelegramMessageObj);
begin
  if (AMessage.Text<>EmptyStr) and not AMessage.Text.StartsWith('/') then
    Bot.EditOrSendMessage('```'+LineEnding+AMessage.Text+LineEnding+'```', pmMarkdown, nil, True);
end;

constructor TMyAction.Create;
begin
  inherited Create;
  { This isnt real telegram token! Please get token from
  https://core.telegram.org/bots#botfather }
  Token:='123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11';
  Bot.StartText:='Hi! It is simplest HelloBot based on brookframework and fp-telegram!';
  Bot.HelpText:=
    'This help text for the bot... See code at https://github.com/Al-Muhandis/brook-telegram '+
    'and  https://github.com/Al-Muhandis/fp-telegram';
  { Custom command definition example }
  Bot.CommandHandlers['/testcmd']:=@TlgrmTestCmdHandler;
  Bot.CommandHandlers['/myid']:=@TlgrmMyUserIDCmdHandler;
  Bot.CommandHandlers['/markdown']:=@TlgrmMyHelpMarkdownHandler;
  Bot.OnReceiveMessage:=@TlgrmReceiveMessage;

  { Please enter 1234567890 - [your ]user ID (integer value) for availabality of
  admin commands (/stat, /statf /terminate)}
  Bot.UserStatus[1234567890]:=usAdmin;  // You can comment out this if you do not need this functionality
  { You can do not create this log. If its value is nil,
  then the logging just will not be maintained }
  Bot.StatLogger.Paused:=False; // run statistics log
  Logger:=BLogger;
  Bot.UpdateLogger:=TtgStatLog.Create(nil);
  Bot.UpdateLogger.FilePrefix:='stat';
  Bot.UpdateLogger.FilePostfix:='.log';
  Bot.UpdateLogger.Directory:=ExtractFileDir(ParamStr(0))+PathDelim+'stat'+PathDelim;
  Bot.UpdateLogger.Paused:=False;
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
