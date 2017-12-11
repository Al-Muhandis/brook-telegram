unit brooktelegramaction;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, tgtypes, tgsendertypes, sysutils, classes, tgstatlog, eventlog,
  ghashmap, fpjson;

type
  { TWebhookAction }

  TWebhookAction = class(TBrookAction)
  private
    FHelpText: String;
    FLogger: TEventLog;
    FOnCallbackQuery: TCallbackEvent;
    FOnUpdateMessage: TMessageEvent;
    FStartText: String;
    FStatLogger: TtgStatLog;
    FToken: String;
    FBot: TTelegramSender;
    FUserPermissions: TStringList; // namevalue pairs UserID=Character
    procedure BotStartHandler(ASender: TObject; const ACommand: String;
      AMessage: TTelegramMessageObj);
    procedure BotHelpHandler(ASender: TObject; const ACommand: String;
      AMessage: TTelegramMessageObj);
    procedure BotStatHandler(ASender: TObject; const ACommand: String;
      AMessage: TTelegramMessageObj);
    procedure BotStatFHandler(ASender: TObject; const ACommand: String;
      AMessage: TTelegramMessageObj);
    procedure BotTerminateHandler(ASender: TObject; const ACommand: String;
      AMessage: TTelegramMessageObj);
    procedure SetHelpText(AValue: String);
    procedure SetLogger(AValue: TEventLog);
    procedure SetOnCallbackQuery(AValue: TCallbackEvent);
    procedure SetOnUpdateMessage(AValue: TMessageEvent);
    procedure SetStartText(AValue: String);
    procedure SetStatLogger(AValue: TtgStatLog);
    procedure SetToken(AValue: String);
    procedure SetUpdateMessage(AValue: TTelegramUpdateObj);
    procedure DoCallbackQueryStat(ACallbackQuery: TCallbackQueryObj; SendFile: Boolean = False);
    procedure DoGetStat(ADate: TDate = 0; SendFile: Boolean = false);
    procedure DoStat(SDate: String = 'today'; SendFile: Boolean = false);
    procedure SendStatLog(ADate: TDate = 0; AReplyMarkup: TReplyMarkup = nil);
    procedure SendStatInlineKeyboard(SendFile: Boolean = false);
    procedure LogMessage(ASender: TObject; EventType: TEventType; const Msg: String);
    procedure StatLog(const AMessage: String; UpdateType: TUpdateType);
  protected
    function CreateInlineKeyboardStat(SendFile: Boolean): TJSONArray;
    procedure BotCallbackQuery(ASender: TObject; ACallback: TCallbackQueryObj);
    procedure BotMessageHandler(ASender: TObject; AMessage: TTelegramMessageObj);
    procedure EditOrSendMessage(const AMessage: String; AParseMode: TParseMode = pmDefault;
      ReplyMarkup: TReplyMarkup = nil; TryEdit: Boolean = False);
    function IsSimpleUser: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Post; override;
    property Token: String read FToken write SetToken;
    property OnCallbackQuery: TCallbackEvent read FOnCallbackQuery write SetOnCallbackQuery;
    property OnUpdateMessage: TMessageEvent read FOnUpdateMessage write SetOnUpdateMessage;
    property UserPermissions: TStringList read FUserPermissions write FUserPermissions;
    property StartText: String read FStartText write SetStartText; // Text for /start command reply
    property HelpText: String read FHelpText write SetHelpText;  // Text for /help command reply
    property StatLogger: TtgStatLog read FStatLogger write SetStatLogger;
    property Logger: TEventLog read FLogger write SetLogger;
    property Bot: TTelegramSender read FBot;
  end;

implementation

uses jsonparser, BrookHttpConsts, strutils, BrookApplication, jsonscanner;

const
  StatDateFormat = 'dd-mm-yyyy';

  { TWebhookAction }

procedure TWebhookAction.SetToken(AValue: String);
begin
  if FToken=AValue then Exit;
  FToken:=AValue;
  if Assigned(FBot) then
    FBot.Token:=FToken;
end;

procedure TWebhookAction.SetUpdateMessage(AValue: TTelegramUpdateObj);
begin

end;

procedure TWebhookAction.SetStartText(AValue: String);
begin
  if FStartText=AValue then Exit;
  FStartText:=AValue;
end;

procedure TWebhookAction.SetStatLogger(AValue: TtgStatLog);
begin
  if FStatLogger=AValue then Exit;
  FStatLogger:=AValue;
end;

procedure TWebhookAction.SetOnCallbackQuery(AValue: TCallbackEvent);
begin
  if FOnCallbackQuery=AValue then Exit;
  FOnCallbackQuery:=AValue;
end;

procedure TWebhookAction.SetOnUpdateMessage(AValue: TMessageEvent);
begin
  if FOnUpdateMessage=AValue then Exit;
  FOnUpdateMessage:=AValue;
end;

procedure TWebhookAction.BotStartHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  Bot.RequestWhenAnswer:=True;
  Bot.sendMessage(FStartText);
end;

procedure TWebhookAction.BotHelpHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  Bot.RequestWhenAnswer:=True;
  Bot.sendMessage(FHelpText);
end;

procedure TWebhookAction.BotStatHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
var
  S: String;
begin
  if IsSimpleUser then
    Exit;
  FBot.RequestWhenAnswer:=True;
  S:=ExtractDelimited(2, AMessage.Text, [' ']);
  if S<>EmptyStr then
    DoStat(S)
  else
    SendStatInlineKeyboard;
end;

procedure TWebhookAction.BotStatFHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
var
  S: String;
begin
  if IsSimpleUser then
    Exit;
  FBot.RequestWhenAnswer:=True;
  S:=ExtractDelimited(2, AMessage.Text, [' ']);
  if S<>EmptyStr then
     DoStat(S, True)
   else
     SendStatInlineKeyboard(True);
end;

procedure TWebhookAction.BotTerminateHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  if IsSimpleUser then
    Exit;
  FBot.RequestWhenAnswer:=True;
  FBot.sendMessage('Bot app is closed');
  BrookApp.Terminate;
end;

procedure TWebhookAction.SetHelpText(AValue: String);
begin
  if FHelpText=AValue then Exit;
  FHelpText:=AValue;
end;

procedure TWebhookAction.SetLogger(AValue: TEventLog);
begin
  if FLogger=AValue then Exit;
  FLogger:=AValue;
end;

function TWebhookAction.IsSimpleUser: Boolean;
begin
  if Assigned(FBot.CurrentUser) then
    Result:=FUserPermissions.Values[IntToStr(FBot.CurrentUser.ID)]=EmptyStr
  else
    Result:=True;
end;

procedure TWebhookAction.BotCallbackQuery(ASender: TObject;
  ACallback: TCallbackQueryObj);
begin
  if not IsSimpleUser then
  begin
    if AnsiStartsStr('GetStat ', ACallback.Data) then
      DoCallbackQueryStat(ACallback);
    if AnsiStartsStr('GetStatFile ', ACallback.Data) then
      DoCallbackQueryStat(ACallback, True);
  end;
  StatLog(ACallback.Data, utCallbackQuery);
  if Assigned(FOnCallbackQuery) then
    FOnCallbackQuery(Self, ACallback);
end;

procedure TWebhookAction.BotMessageHandler(ASender: TObject;
  AMessage: TTelegramMessageObj);
var
  lCommand, Txt, S: String;
  lMessageEntityObj: TTelegramMessageEntityObj;
  H: TCommandEvent;
begin
  StatLog(Txt, utMessage);
  if Assigned(FOnUpdateMessage) then
    FOnUpdateMessage(Self, AMessage);
end;

procedure TWebhookAction.DoCallbackQueryStat(ACallbackQuery: TCallbackQueryObj;
  SendFile: Boolean = False);
begin
  DoStat(ExtractDelimited(2, ACallbackQuery.Data, [' ']), SendFile);
end;

procedure TWebhookAction.DoGetStat(ADate: TDate = 0; SendFile: Boolean = false);
var
  StatFile: TStringList;
  Msg: String;
  AFileName: String;
  i: Integer;
  ReplyMarkup: TReplyMarkup;
begin
  if IsSimpleUser then
  Exit;
  ReplyMarkup:=TReplyMarkup.Create;
  try
    ReplyMarkup.InlineKeyBoard:=CreateInlineKeyboardStat(SendFile);
    if SendFile then
      SendStatLog(ADate, ReplyMarkup)
    else begin
      StatFile:=TStringList.Create;
      try
        AFileName:=StatLogger.GetFileNameFromDate(ADate);
        FBot.RequestWhenAnswer:=True;
        try
          if FileExists(AFileName) then
          begin
            StatFile.LoadFromFile(AFileName);
            Msg:='';
            for i:=StatFile.Count-1 downto StatFile.Count-20 do
            begin
              if i<0 then
                Break;
              Msg+=StatFile[i]+LineEnding;
            end;
            EditOrSendMessage(Msg, pmHTML, ReplyMarkup, True);
          end
          else
            EditOrSendMessage('Statistics for this date not found', pmDefault, ReplyMarkup, True);
        except
          EditOrSendMessage('Error: failed to load statistics file', pmDefault, ReplyMarkup);
        end;
      finally
        StatFile.Free;
      end;
    end;
  finally
    ReplyMarkup.Free;
  end;
end;

procedure TWebhookAction.DoStat(SDate: String = 'today'; SendFile: Boolean = false);
var
  FDate: TDate;
begin
  if not Assigned(FStatLogger) then
    Exit;
  SDate:=Trim(SDate);
  if (SDate='today') or (SDate=EmptyStr) then
    FDate:=Date
  else
    if SDate='yesterday' then
      FDate:=Date-1
    else
      if not TryStrToDate(SDate, FDate, StatDateFormat) then
      begin
        FBot.RequestWhenAnswer:=True;
        FBot.sendMessage('Please enter the date in format: dd-mm-yyyy');
        Exit;
      end;
  DoGetStat(FDate, SendFile);
end;

procedure TWebhookAction.SendStatLog(ADate: TDate = 0; AReplyMarkup: TReplyMarkup = nil);
var
  AFileName: String;
begin
  if ADate=0 then
    ADate:=sysutils.Date;
  AFileName:=StatLogger.GetFileNameFromDate(ADate);
  if FileExists(AFileName) then
  begin
    FBot.RequestWhenAnswer:=False;
    FBot.sendDocumentByFileName(Bot.CurrentChatId, AFileName, 'Statistics for '+DateToStr(ADate));
  end
  else
  begin
    FBot.RequestWhenAnswer:=True;
    EditOrSendMessage('Statistics for this date not found', pmDefault, AReplyMarkup, True);
  end;
end;

procedure TWebhookAction.SendStatInlineKeyboard(SendFile: Boolean);
var
  ReplyMarkup: TReplyMarkup;
begin
  ReplyMarkup:=TReplyMarkup.Create;
  try
    ReplyMarkup.InlineKeyBoard:=CreateInlineKeyboardStat(SendFile);
    FBot.RequestWhenAnswer:=True;
    FBot.sendMessage(
      'Select statistics by pressing the button. In addition, the available commands:'+
      LineEnding+'/stat <i>day</i> - the last records for a specified <i>date</i>, '+
      '/statf <i>date</i> - statistics file for the <i>date</i>,'+LineEnding+
      'where <i>date</i> is <i>today</i> or <i>yesterday</i> or in format <i>dd-mm-yyyy</i>',
      pmHTML, True, ReplyMarkup);
  finally
    ReplyMarkup.Free;
  end;
end;

procedure TWebhookAction.LogMessage(ASender: TObject; EventType: TEventType; const Msg: String);
begin
  if Assigned(FLogger) then
    Logger.Log(EventType, Msg);
end;

procedure TWebhookAction.StatLog(const AMessage: String; UpdateType: TUpdateType);
var
  EscMsg: String;
begin
  EscMsg:=StringReplace(AMessage, '"', '_', [rfReplaceAll]);
  EscMsg:=StringReplace(AMessage, LineEnding, '//', [rfReplaceAll]);
  if Length(EscMsg)>150 then
    SetLength(EscMsg, 150);
  if IsSimpleUser then
    if Assigned(Bot.CurrentUser)then
      StatLogger.Log(['@'+Bot.CurrentUser.Username, Bot.CurrentUser.First_name, Bot.CurrentUser.Last_name,
        Bot.CurrentUser.Language_code, UpdateTypeAliases[UpdateType], '"'+EscMsg+'"'])
    else
      StatLogger.Log(['', '', '', '', UpdateTypeAliases[UpdateType], '"'+EscMsg+'"'])
end;

{ Sometimes, if the message is sent to the result of the CallBack call,
it is desirable not to create a new message and edit the message from which the call came }
procedure TWebhookAction.EditOrSendMessage(const AMessage: String;
  AParseMode: TParseMode; ReplyMarkup: TReplyMarkup; TryEdit: Boolean);
begin
  if TryEdit then
  begin
    TryEdit:=False;
    if Bot.CurrentUpdate.UpdateType=utCallbackQuery then
      TryEdit:=True;
  end;
  if not TryEdit then
    Bot.sendMessage(AMessage, AParseMode, True, ReplyMarkup)
  else
    Bot.editMessageText(AMessage, AParseMode, False, '', ReplyMarkup);
end;

function TWebhookAction.CreateInlineKeyboardStat(SendFile: Boolean): TJSONArray;
var
  btns: TInlineKeyboardButtons;
  FileApp: String;
begin
  if SendFile then
    FileApp:='File'
  else
    FileApp:='';
  btns:=TInlineKeyboardButtons.Create;
  btns.AddButtons(['Today 🔃', 'GetStat'+FileApp+' today',
    'Yesterday', 'GetStat'+FileApp+' yesterday']);
  Result:=TJSONArray.Create;
  Result.Add(btns);
end;

constructor TWebhookAction.Create;
begin
  inherited Create;
  FUserPermissions:=TStringList.Create;
  FUserPermissions.Sorted:=True;
  FUserPermissions.Duplicates:=dupIgnore;
  FStatLogger:=TtgStatLog.Create(nil);
  FStatLogger.Active:=False;
  FBot:=TTelegramSender.Create(FToken);
  FBot.OnLogMessage:=@LogMessage;
  FBot.OnReceiveMessage:=@BotMessageHandler;
  FBot.OnReceiveCallbackQuery:=@BotCallbackQuery;
  FBot.CommandHandlers['/start']:=@BotStartHandler;
  FBot.CommandHandlers['/help']:=@BotHelpHandler;
end;

destructor TWebhookAction.Destroy;
begin
  FBot.Free;
  FStatLogger.Free;
  FUserPermissions.Free;
  inherited Destroy;
end;

procedure TWebhookAction.Post;
var
  Msg: String;
  lParser: TJSONParser;
  AnUpdate: TTelegramUpdateObj;
begin
  Msg:=TheRequest.Content;
  LogMessage(Self, etDebug, 'Recieve the update (Webhook): '+Msg);
  if Msg<>EmptyStr then
  begin
    lParser := TJSONParser.Create(Msg);
    try
      try
        AnUpdate :=
          TTelegramUpdateObj.CreateFromJSONObject(lParser.Parse as TJSONObject) as TTelegramUpdateObj;
      except
      end;
    finally
      lParser.Free;
    end;
    if Assigned(AnUpdate) then
    begin
      Bot.DoReceiveUpdate(AnUpdate);
      TheResponse.ContentType:=BROOK_HTTP_CONTENT_TYPE_APP_JSON;
      Write(FBot.RequestBody);
    end;
  end;
end;

end.
