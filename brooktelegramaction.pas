unit brooktelegramaction;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, tgtypes, BrookHttpDefs, tgsendertypes, sysutils, classes, tgstatlog, eventlog;

type

  { TWebhookAction }

  TWebhookAction = class(TBrookAction)
  private
    FCurrentChatID: Int64;
    FCurrentUser: TTelegramUserObj;
    FHelpText: String;
    FLogger: TEventLog;
    FOnCallbackQuery: TNotifyEvent;
    FOnUpdateMessage: TNotifyEvent;
    FRMsg: String;
    FStartText: String;
    FStatLogger: TtgStatLog;
    FToken: String;
    FtgSender: TTelegramSender;
    FUpdateMessage: TTelegramUpdateObj;
    FUserPermissions: TStringList; // namevalue pairs UserID=Character
    procedure SetHelpText(AValue: String);
    procedure SetLogger(AValue: TEventLog);
    procedure SetOnCallbackQuery(AValue: TNotifyEvent);
    procedure SetOnUpdateMessage(AValue: TNotifyEvent);
    procedure SetRMsg(AValue: String);
    procedure SetStartText(AValue: String);
    procedure SetStatLogger(AValue: TtgStatLog);
    procedure SetToken(AValue: String);
    procedure SetUpdateMessage(AValue: TTelegramUpdateObj);
    procedure DoCallbackQueryStat(SendFile: Boolean = False);
    procedure DoGetStat(ADate: TDate = 0; SendFile: Boolean = false);
    procedure DoStat(SDate: String = 'today'; SendFile: Boolean = false);
    procedure SendStatLog(ADate: TDate = 0);
    procedure SendStatInlineKeyboard(SendFile: Boolean = false);
    procedure LogMessage(Sender: TObject; EventType: TEventType; const Msg: String);
    procedure StatLog(const AMessage: String; UpdateType: TUpdateType);
  protected
    procedure DoCallbackQuery; virtual;
    procedure DoMessageHandler; virtual;
    function IsSimpleUser: Boolean;
  public
    constructor Create(ARequest: TBrookRequest; AResponse: TBrookResponse); overload;
      override;
    destructor Destroy; override;
    procedure Post; override;
    property Token: String read FToken write SetToken;
    property OnCallbackQuery: TNotifyEvent read FOnCallbackQuery write SetOnCallbackQuery;
    property OnUpdateMessage: TNotifyEvent read FOnUpdateMessage write SetOnUpdateMessage;
    property RMsg: String read FRMsg write SetRMsg;
    property UpdateObj: TTelegramUpdateObj read FUpdateMessage write SetUpdateMessage;
    property UserPermissions: TStringList read FUserPermissions write FUserPermissions;
    property StartText: String read FStartText write SetStartText; // Text for /start command reply
    property HelpText: String read FHelpText write SetHelpText;  // Text for /help command reply
    property StatLogger: TtgStatLog read FStatLogger write SetStatLogger;
    property Logger: TEventLog read FLogger write SetLogger;
    property CurrentUser: TTelegramUserObj read FCurrentUser;
    property CurrentChatID: Int64 read FCurrentChatID;
    property Sender: TTelegramSender read FtgSender;
  end;

implementation

uses jsonparser, fpjson, BrookHttpConsts, strutils, BrookApplication;

const
  UpdateTypeAliases: array[TUpdateType] of PChar = ('message', 'callback_query');
  StatDateFormat = 'dd-mm-yyyy';

procedure TWebhookAction.SetUpdateMessage(AValue: TTelegramUpdateObj);
begin
  if FUpdateMessage=AValue then Exit;
  FUpdateMessage:=AValue;
end;

procedure TWebhookAction.SetToken(AValue: String);
begin
  if FToken=AValue then Exit;
  FToken:=AValue;
  if Assigned(FtgSender) then
    FtgSender.Token:=FToken;
end;

procedure TWebhookAction.SetRMsg(AValue: String);
begin
  if FRMsg=AValue then Exit;
  FRMsg:=AValue;
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

procedure TWebhookAction.SetOnCallbackQuery(AValue: TNotifyEvent);
begin
  if FOnCallbackQuery=AValue then Exit;
  FOnCallbackQuery:=AValue;
end;

procedure TWebhookAction.SetOnUpdateMessage(AValue: TNotifyEvent);
begin
  if FOnUpdateMessage=AValue then Exit;
  FOnUpdateMessage:=AValue;
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
  if Assigned(FCurrentUser) then
    Result:=FUserPermissions.Values[IntToStr(FCurrentUser.ID)]=EmptyStr
  else
    Result:=True;
end;

procedure TWebhookAction.DoCallbackQuery;
begin
  if not IsSimpleUser then
  begin
    if AnsiStartsStr('GetStat ', UpdateObj.CallbackQuery.Data) then
      DoCallbackQueryStat;
    if AnsiStartsStr('GetStatFile ', UpdateObj.CallbackQuery.Data) then
      DoCallbackQueryStat(True);
  end;
  StatLog(UpdateObj.CallbackQuery.Data, utCallbackQuery);
  if Assigned(FOnCallbackQuery) then
    FOnCallbackQuery(Self);
end;

procedure TWebhookAction.DoCallbackQueryStat(SendFile: Boolean = False);
begin
  DoStat(ExtractDelimited(2, UpdateObj.CallbackQuery.Data, [' ']), SendFile);
end;

procedure TWebhookAction.DoGetStat(ADate: TDate = 0; SendFile: Boolean = false);
var
  StatFile: TStringList;
  Msg: String;
  AFileName: String;
  i: Integer;
begin
  if not IsSimpleUser then
    if SendFile then
      SendStatLog(ADate)
    else begin
      StatFile:=TStringList.Create;
      try
        AFileName:=StatLogger.GetFileNameFromDate(ADate);
        FtgSender.WebhookRequest:=True;
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
            FtgSender.sendMessage(FCurrentChatID, Msg, pmHTML, True);
          end
          else
            FtgSender.sendMessage(FCurrentChatID, 'Statistics for this date not found');
        except
          FtgSender.sendMessage(FCurrentChatID, 'Error: failed to load statistics file');
        end;
      finally
        StatFile.Free;
      end;
    end;
end;

procedure TWebhookAction.DoMessageHandler;
var
  lCommand, Txt, S: String;
  lMessageEntityObj: TTelegramMessageEntityObj;
begin
  Txt:=UpdateObj.Message.Text;
  StatLog(Txt, utMessage);
  for lMessageEntityObj in UpdateObj.Message.Entities do
  begin
    RMsg:='';
    if (lMessageEntityObj.TypeEntity = 'bot_command') and (lMessageEntityObj.Offset = 0) then
    begin
      lCommand := Copy(Txt, lMessageEntityObj.Offset, lMessageEntityObj.Length);
      FtgSender.WebhookRequest:=True;
      if lCommand = '/help' then
        FtgSender.sendMessage(FCurrentChatID, FHelpText);
      if lCommand = '/start' then
        FtgSender.sendMessage(FCurrentChatID, FStartText);
      if not IsSimpleUser then
      begin
        if lCommand = '/stat' then
        begin
          S:=RightStr(Txt, Length(Txt)-(lMessageEntityObj.Length-lMessageEntityObj.Offset));
          if S<>EmptyStr then
            DoStat(S)
          else
            SendStatInlineKeyboard;
        end;
        if lCommand = '/statf' then
        begin
          S:=RightStr(Txt, Length(Txt)-(lMessageEntityObj.Length-lMessageEntityObj.Offset));
          if S<>EmptyStr then
            DoStat(S, True)
          else
            SendStatInlineKeyboard(True);
        end;
        if lCommand = '/terminate' then
          if not IsSimpleUser then
          begin
            FtgSender.sendMessage(FCurrentChatID, 'Bot app is closed');
            BrookApp.Terminate;
          end;
      end;
    end;
  end;
  if Assigned(FOnUpdateMessage) then
    FOnUpdateMessage(Self);
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
        FtgSender.WebhookRequest:=True;
        FtgSender.sendMessage(FCurrentChatID, 'Please enter the date in format: dd-mm-yyyy');
        Exit;
      end;
  DoGetStat(FDate, SendFile);
end;

procedure TWebhookAction.SendStatLog(ADate: TDate = 0);
var
  AFileName: String;
begin
  if ADate=0 then
    ADate:=sysutils.Date;
  AFileName:=StatLogger.GetFileNameFromDate(ADate);
  if FileExists(AFileName) then
  begin
    FtgSender.WebhookRequest:=False;
    FtgSender.sendDocumentByFileName(FCurrentChatID, AFileName, 'Statistics for '+DateToStr(ADate));
  end
  else
  begin
    FtgSender.WebhookRequest:=True;
    FtgSender.sendMessage(FCurrentChatID, 'Statistics for this date not found');
  end;
end;

procedure TWebhookAction.SendStatInlineKeyboard(SendFile: Boolean);
var
  ReplyMarkup: TReplyMarkup;
  kybrd: TJSONArray;
  btns: TInlineKeyboardButtons;
  FileApp: String;
begin
  if SendFile then
    FileApp:='File'
  else
    FileApp:='';
  ReplyMarkup:=TReplyMarkup.Create;
  try
    btns:=TInlineKeyboardButtons.Create;
    btns.AddButton('Today', 'GetStat'+FileApp+' today');
    btns.AddButton('Yesterday', 'GetStat'+FileApp+' yesterday');
    kybrd:=TJSONArray.Create;
    kybrd.Add(btns);
    ReplyMarkup.InlineKeyBoard:=kybrd;
    FtgSender.WebhookRequest:=True;
    FtgSender.sendMessage(FCurrentChatID,
      'Select statistics by pressing the button. In addition, the available commands:'+
      LineEnding+'/stat dd-mm-yyyy - the last record for a specified date, '+
      '/stat today - ... today, /stat yesterday - ... yesterday'+LineEnding+
      '/statf dd-mm-yyyy - statistics file for a specified date'+LineEnding+
      '/statf today - ... today, /statf yesterday - ...yesterday',
      pmHTML, True, ReplyMarkup);
  finally
    ReplyMarkup.Free;
  end;
end;

procedure TWebhookAction.LogMessage(Sender: TObject; EventType: TEventType; const Msg: String);
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
    if Assigned(FCurrentUser)then
      StatLogger.Log(['@'+FCurrentUser.Username, FCurrentUser.First_name, FCurrentUser.Last_name,
        FCurrentUser.Language_code, UpdateTypeAliases[UpdateType], '"'+EscMsg+'"'])
    else
      StatLogger.Log(['', '', '', '', UpdateTypeAliases[UpdateType], '"'+EscMsg+'"'])
end;

constructor TWebhookAction.Create(ARequest: TBrookRequest;
  AResponse: TBrookResponse);
begin
  inherited Create(ARequest, AResponse);
  FCurrentUser:=nil;
  FUserPermissions:=TStringList.Create;
  FUserPermissions.Sorted:=True;
  FUserPermissions.Duplicates:=dupIgnore;
  FStatLogger:=TtgStatLog.Create(nil);
  FStatLogger.Active:=False;
  FtgSender:=TTelegramSender.Create(FToken);
  FtgSender.OnLogMessage:=@LogMessage;
end;

destructor TWebhookAction.Destroy;
begin
  FtgSender.Free;
  FStatLogger.Free;
  FUserPermissions.Free;
  if Assigned(UpdateObj) then
    UpdateObj.Free;
  inherited Destroy;
end;

procedure TWebhookAction.Post;
var
  Msg: String;
  lParser: TJSONParser;
begin
  Msg:=TheRequest.Content;
  LogMessage(Self, etDebug, 'Recieve the update (Webhook): '+Msg);
  if Msg<>EmptyStr then
  begin
    lParser := TJSONParser.Create(Msg);
    try
      try
        UpdateObj :=
          TTelegramUpdateObj.CreateFromJSONObject(lParser.Parse as TJSONObject) as TTelegramUpdateObj;
      except
      end;
    finally
      lParser.Free;
    end;
    if Assigned(UpdateObj) then
    begin
      if Assigned(UpdateObj.Message) then
      begin
        FCurrentChatID:=UpdateObj.Message.ChatId;
        FCurrentUser:=UpdateObj.Message.From;
        DoMessageHandler;
      end;
      if Assigned(UpdateObj.CallbackQuery) then
      begin
        FCurrentChatID:=UpdateObj.CallbackQuery.Message.ChatId;
        FCurrentUser:=UpdateObj.CallbackQuery.From;
        DoCallbackQuery;
      end;
      TheResponse.ContentType:=BROOK_HTTP_CONTENT_TYPE_APP_JSON;
      Write(FtgSender.RequestBody);
    end;
  end;
end;

end.
