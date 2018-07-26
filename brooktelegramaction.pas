unit brooktelegramaction;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, tgtypes, tgsendertypes, sysutils, classes, tgstatlog, eventlog,
  fpjson;

type

  TUserStatus = (usAdmin, usModerator, usDefault, usBanned);

  TWebhookBot = class;

  TRateEvent = procedure (var RateText: String; ReplyMarkup: TReplyMarkup) of object;
  TReceiveDeepLinkEvent = procedure (const AParameter: String) of object;

  { TWebhookAction }

  TWebhookAction = class(TBrookAction)
  private
    FLogger: TEventLog;
    FOnCallbackQuery: TCallbackEvent;
    FOnUpdateMessage: TMessageEvent;
    FStatLogger: TtgStatLog;
    FToken: String;
    FBot: TWebhookBot;
    procedure BotSetCommandReply({%H-}ASender: TObject; const ACommand: String;
      AMessage: TTelegramMessageObj);
    procedure BotTerminateHandler({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    procedure SetBot({%H-}AValue: TWebhookBot);
    procedure SetLogger(AValue: TEventLog);
    procedure SetOnCallbackQuery(AValue: TCallbackEvent);
    procedure SetOnUpdateMessage(AValue: TMessageEvent);
    procedure SetStatLogger(AValue: TtgStatLog);
    procedure SetToken(AValue: String);
    procedure SendStatLog(ADate: TDate = 0; AReplyMarkup: TReplyMarkup = nil);
    procedure LogMessage({%H-}ASender: TObject; EventType: TEventType; const Msg: String);
  protected
    procedure BotCallbackQuery(ACallback: TCallbackQueryObj); virtual;
    procedure BotMessageHandler(AMessage: TTelegramMessageObj); virtual;
    procedure EditOrSendMessage(const AMessage: String; AParseMode: TParseMode = pmDefault;
      ReplyMarkup: TReplyMarkup = nil; TryEdit: Boolean = False);
    function IsSimpleUser: Boolean;
    function IsBanned: Boolean;
    procedure SaveFeedback(AFrom: TTelegramUserObj; AMessage: String); virtual; abstract;
  public
    function AppDir: String;
    constructor Create; override;
    destructor Destroy; override;
    procedure Post; override;
    property Token: String read FToken write SetToken;
    property OnCallbackQuery: TCallbackEvent read FOnCallbackQuery write SetOnCallbackQuery;
    property OnUpdateMessage: TMessageEvent read FOnUpdateMessage write SetOnUpdateMessage;
    property StatLogger: TtgStatLog read FStatLogger write SetStatLogger;
    property Logger: TEventLog read FLogger write SetLogger;
    property Bot: TWebhookBot read FBot write SetBot;
  end;

  { TWebhookBot }
  TWebhookBot = class(TTelegramSender)
  private
    FHelpText: String;
    FBrookAction: TWebhookAction;
    FFeedbackText: String;
    FFeedbackThanks: String;
    FOnRate: TRateEvent;
    FOnReceiveDeepLinking: TReceiveDeepLinkEvent;
    FStartText: String;
    FUserPermissions: TStringList;
    procedure DoCallbackQueryStat(ACallbackQuery: TCallbackQueryObj; SendFile: Boolean = False);
    procedure DoGetStat(ADate: TDate = 0; Scroll: Boolean = False; Offset: Integer = 0);
    procedure DoGetStatFile(ADate: TDate = 0);
    procedure DoStat(const SDate: String; const SOffset: String = ''; SendFile: Boolean = false);
    function GetUserStatus(ID: Int64): TUserStatus;
    procedure SetBrookAction(AValue: TWebhookAction);
    procedure SetHelpText(AValue: String);
    procedure SetOnRate(AValue: TRateEvent);
    procedure SetOnReceiveDeepLinking(AValue: TReceiveDeepLinkEvent);
    procedure SetStartText(AValue: String);
    procedure SetUserStatus(ID: Int64; AValue: TUserStatus);
    procedure TlgrmStartHandler({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    procedure TlgrmHelpHandler({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    procedure TlgrmFeedback({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    procedure TlgrmRate({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    procedure TlgrmStatHandler({%H-}ASender: TObject;
      const {%H-}ACommand: String; AMessage: TTelegramMessageObj);
    procedure TlgrmStatFHandler({%H-}ASender: TObject;
      const {%H-}ACommand: String; AMessage: TTelegramMessageObj);
    procedure LangTranslate(const {%H-}ALang: String);
    procedure SendStatInlineKeyboard(SendFile: Boolean = False);
    procedure StatLog(const AMessage: String; UpdateType: TUpdateType);
  protected
    function CreateInlineKeyboardRate: TInlineKeyboard;
    function CreateInlineKeyboardStat(ADate: TDate): TInlineKeyboard;
    function CreateInlineKeyboardStat(ADate: TDate; SendFile: Boolean): TInlineKeyboard; overload;
    function CreateInlineKeyboardStatFile: TInlineKeyboard;
    function CreateInlineKeyboardStat(ADate: TDate; Len: Integer; Offset: Integer = 0;
      Step: Integer = 20): TInlineKeyboard; overload;
    procedure DoReceiveDeepLinking(const AParameter: String);
    procedure DoReceiveMessageUpdate(AMessage: TTelegramMessageObj); override;
    procedure DoReceiveCallbackQuery(ACallback: TCallbackQueryObj); override;
    procedure DoReceiveChannelPost(AChannelPost: TTelegramMessageObj); override;
    procedure DoReceiveChosenInlineResult(
      AChosenInlineResult: TTelegramChosenInlineResultObj); override;
    procedure DoReceiveInlineQuery(AnInlineQuery: TTelegramInlineQueryObj); override;
    procedure DebugMessage(const Msg: String); override;
    procedure InfoMessage(const Msg: String); override;
    procedure ErrorMessage(const Msg: String); override;
    property BrookAction: TWebhookAction read FBrookAction write SetBrookAction;
    function IsBanned(ChatID: Int64): Boolean; override;       
    function IsSimpleUser(ChatID: Int64): Boolean; override;
    procedure SetLanguage(const ALang: String); override;
  public
    constructor Create(const AToken: String; AWebhookAction: TWebhookAction);
    destructor Destroy; override;
    procedure LoadUserStatusValues(AStrings: TStrings);
    property StartText: String read FStartText write SetStartText; // Text for /start command reply
    property HelpText: String read FHelpText write SetHelpText;  // Text for /help command reply
    property FeedbackText: String read FFeedbackText write FFeedbackText;
    property FeedbackThanks: String read FFeedbackThanks write FFeedbackThanks;
    property UserStatus [ID: Int64]: TUserStatus read GetUserStatus write SetUserStatus;
    property OnRate: TRateEvent read FOnRate write SetOnRate;
    property OnReceiveDeepLinking: TReceiveDeepLinkEvent read FOnReceiveDeepLinking write SetOnReceiveDeepLinking;
  end;

function ExtractArgPart(const ASource, ACommand: String): String;
function FormatStatRec(const S: String): String;

implementation
{ Please define ni18n (No i18n) for excluding translate unit from uses and exclude i18n support }
uses jsonparser, BrookHttpConsts, strutils, BrookApplication, jsonscanner{$IFNDEF ni18n},
  Translations{$ENDIF}, tgutils;

// Please use i18n for localization *** Пожалуйста, используйте i18n для локализации
resourcestring
  str_FeedbackThanks='Thanks! %s Your message will be considered!'; // Спасибо, %s! Ваш сообщение будет обязательно рассмотрено!;
  str_FeedbackText='Send us a wish, recommendation or bug report';  // Отправьте разработчику бота пожелание, рекомендацию, информацию об ошибке
  str_TxtRplyIsScsChngd='Text reply for command is succesfully changed!'; // Текст сообщения для команды успешно изменен
  str_BtApIsClsd='Bot app is closed';  // Приложение закрыто
  str_SttstcsNtFnd='Statistics for this date not found'; // Статистика не найдено за указанный день
  str_ErrFldTLdSttstcsFl='Error: failed to load statistics file';
  str_EntrDtInFrmt='Please enter the date in format: dd-mm-yyyy';
  str_SttstcsFr='Statistics for ';
  str_SlctSttstcs_line1='Select statistics by pressing the button. In addition, the available commands:';
  str_SlctSttstcs_line2='/stat <i>day</i> - the last records for a specified <i>date</i>, ';
  str_SlctSttstcs_line3= '/statf <i>date</i> - statistics file for the <i>date</i>,';
  str_SlctSttstcs_line4='where <i>date</i> is <i>today</i> or <i>yesterday</i> or in format <i>dd-mm-yyyy</i>';
  str_Today='Today';
  str_Yesterday='Yesterday';
  str_PrevDay='Prev day';
  str_NextDay='Next day';
  str_Rate='Rate';
  str_RateText='Please leave feedback on "Storebot" if you like our bot!';
  str_Browse='Browse';


const
  StatDateFormat = 'dd-mm-yyyy';
  UserStatusChars: array [TUserStatus] of AnsiChar = ('a', 'm', 'b', '_');
  cmd_Start = '/start';
  cmd_Help = '/help';
  cmd_Stat = '/stat';
  cmd_StatF = '/statf';
  cmd_Terminate = '/terminate';
  cmd_Feedback = '/feedback';
  cmd_SetStart = '/setstart';
  cmd_SetHelp = '/sethelp';
  cmd_Rate = '/rate';
  s_Today = 'today';
  s_Yesterday = 'yesterday';
  s_GetStat='GetStat';
  s_File='File';
  s_StoreBotRate='https://telegram.me/storebot?start=';

function AnsiCharToUserStatus(const StatusChar: String): TUserStatus;
var
  a: AnsiChar;
begin
  if StatusChar=EmptyStr then
    a:=UserStatusChars[usDefault]
  else
    a:=PAnsiChar(StatusChar)[0];
  case a of
    'a': Result:=usAdmin;
    'm': Result:=usModerator;
    'b': Result:=usBanned;
  else
    Result:=usDefault;
  end;
end;

function ExtractArgPart(const ASource, ACommand: String): String;
begin
  Result:=Trim(RightStr(ASource, Length(ASource)-Length(ACommand)));
end;

function FormatStatRec(const S: String): String;
var
  Ss: TStringList;
  l: Integer;
begin
  Ss:=TStringList.Create;
  l:=ExtractStrings([';'], [' '], PChar(S), Ss, True);
  if l<>8 then
    Result:='Stat line parser error'
  else
    Result:=Ss[0]+'; '+'['+Ss[1]+'](tg://user?id='+Ss[1]+') '+
      MarkdownEscape(Ss[2]+' {'+Ss[3]+' '+Ss[4]+'}')+ ' '+
      MarkdownEscape(Ss[5])+' <'+MarkdownEscape(Ss[6])+'> '+MarkdownEscape(Ss[7]);
  Ss.Free;
end;

{ TWebhookBot }

procedure TWebhookBot.SetBrookAction(AValue: TWebhookAction);
begin
  if FBrookAction=AValue then Exit;
  FBrookAction:=AValue;
end;

procedure TWebhookBot.SetHelpText(AValue: String);
begin
  if FHelpText=AValue then Exit;
  FHelpText:=AValue;
end;

procedure TWebhookBot.SetOnRate(AValue: TRateEvent);
begin
  if FOnRate=AValue then Exit;
  FOnRate:=AValue;
end;

procedure TWebhookBot.SetOnReceiveDeepLinking(AValue: TReceiveDeepLinkEvent);
begin
  if FOnReceiveDeepLinking=AValue then Exit;
  FOnReceiveDeepLinking:=AValue;
end;

procedure TWebhookBot.SetStartText(AValue: String);
begin
  if FStartText=AValue then Exit;
  FStartText:=AValue;
end;

procedure TWebhookBot.DoCallbackQueryStat(ACallbackQuery: TCallbackQueryObj;
  SendFile: Boolean);
begin
  DoStat(ExtractDelimited(2, ACallbackQuery.Data, [' ']),
    ExtractDelimited(3, ACallbackQuery.Data, [' ']), SendFile);
  { After the user presses a callback button, Telegram clients will display a progress bar until
you call answerCallbackQuery. It is, therefore, necessary to react by calling answerCallbackQuery
even if no notification to the user is needed }
  RequestWhenAnswer:=False;
  answerCallbackQuery(ACallbackQuery.ID);
end;

procedure TWebhookBot.DoGetStat(ADate: TDate; Scroll: Boolean; Offset: Integer);
var
  StatFile: TStringList;
  Msg, SDate: String;
  AFileName: String;
  i: Integer;
  ReplyMarkup: TReplyMarkup;
const
  Step=20;
begin
  if CurrentIsSimpleUser then
    Exit;
  ReplyMarkup:=TReplyMarkup.Create;
  try
    StatFile:=TStringList.Create;
    try
      AFileName:=FBrookAction.StatLogger.GetFileNameFromDate(ADate);
      RequestWhenAnswer:=False;
      try
        if FileExists(AFileName) then
        begin
          DateTimeToString(SDate, 'dd-mm-yyyy', ADate);
          StatFile.LoadFromFile(AFileName);
          if not Scroll then
          begin
            ReplyMarkup.InlineKeyBoard:=CreateInlineKeyboardStat(ADate);
            Msg:='*Statistics for '+SDate+'*'+LineEnding;
            for i:=StatFile.Count-1 downto StatFile.Count-15 do
            begin
              if i<0 then
                Break;
              Msg+=FormatStatRec(StatFile[i])+LineEnding;
            end;
          end
          else begin
            ReplyMarkup.InlineKeyBoard:=CreateInlineKeyboardStat(ADate, StatFile.Count, Offset,
              Step);
            Msg:='*Statistics for '+SDate+'*'+LineEnding;
            for i:=Offset to Offset+Step-1 do
            begin
              if i>=StatFile.Count then
                Break;
              Msg+=FormatStatRec(StatFile[i])+LineEnding;
            end;
          end;
          editMessageText(Msg, pmMarkdown, True, ReplyMarkup);
        end
        else
          editMessageText(str_SttstcsNtFnd, pmDefault, True, ReplyMarkup);
      except
        editMessageText(str_ErrFldTLdSttstcsFl, pmDefault, True, ReplyMarkup);
      end;
    finally
      StatFile.Free;
    end;
  finally
    ReplyMarkup.Free;
  end;
end;

procedure TWebhookBot.DoGetStatFile(ADate: TDate);
var
  ReplyMarkup: TReplyMarkup;
begin
  if CurrentIsSimpleUser then
    Exit;
  ReplyMarkup:=TReplyMarkup.Create;
  try
    ReplyMarkup.InlineKeyBoard:=CreateInlineKeyboardStatFile;
    FBrookAction.SendStatLog(ADate, ReplyMarkup)
  finally
    ReplyMarkup.Free;
  end;
end;

procedure TWebhookBot.DoStat(const SDate: String; const SOffset: String;
  SendFile: Boolean);
var
  FDate: TDate;
  S: String;
begin
  if not Assigned(FBrookAction.StatLogger) then
    Exit;
  S:=Trim(SDate);
  if (S=s_Today) or (S=EmptyStr) then
    FDate:=Date
  else
    if S=s_Yesterday then
      FDate:=Date-1
    else
      if not TryStrToDate(S, FDate, StatDateFormat, '-') then
      begin
        RequestWhenAnswer:=True;
        sendMessage(str_EntrDtInFrmt);
        Exit;
      end;
  if SOffset=EmptyStr then
    if not SendFile then
      DoGetStat(FDate)
    else
      DoGetStatFile(FDate)
  else
    DoGetStat(FDate, True, StrToIntDef(SOffset, 0));
end;

function TWebhookBot.GetUserStatus(ID: Int64): TUserStatus;
begin
  Result:=AnsiCharToUserStatus(FUserPermissions.Values[IntToStr(ID)]);
end;

procedure TWebhookBot.SetUserStatus(ID: Int64; AValue: TUserStatus);
var
  i: Integer;
begin
  if AValue=usDefault then
  begin
    i:= FUserPermissions.IndexOf(IntToStr(ID));
    if i >-1 then
      FUserPermissions.Delete(i)
    else
      Exit;
  end;
  FUserPermissions.Values[IntToStr(ID)]:=UserStatusChars[AValue];
end;

procedure TWebhookBot.TlgrmStartHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  if not AMessage.Text.Contains(' ') then
  begin
    RequestWhenAnswer:=True;
    sendMessage(FStartText, pmMarkdown);
  end
  else
    DoReceiveDeepLinking(RightStr(AMessage.Text, AMessage.Text.Length-Length(ACommand)));
end;

procedure TWebhookBot.TlgrmHelpHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  RequestWhenAnswer:=True;
  sendMessage(FHelpText, pmMarkdown);
end;

procedure TWebhookBot.TlgrmFeedback(ASender: TObject; const ACommand: String;
  AMessage: TTelegramMessageObj);
var
  ReplyMarkup: TReplyMarkup;
begin
  ReplyMarkup:=TReplyMarkup.Create;
  try
    ReplyMarkup.ForceReply:=True;
    sendMessage(FFeedbackText, pmMarkdown, True, ReplyMarkup);
  finally
    ReplyMarkup.Free;
  end;
end;

procedure TWebhookBot.TlgrmRate(ASender: TObject; const ACommand: String;
  AMessage: TTelegramMessageObj);
var
  ReplyMarkup: TReplyMarkup;
  ATxt: String;
begin
  ReplyMarkup:=TReplyMarkup.Create;
  try
    { You must assign BotUsername before Create inline KeyboardRate for forming rate url for your bot
      except if FUsername already assinged (getMe or callbackquery before is called, for example) }
    ReplyMarkup.InlineKeyBoard:=CreateInlineKeyboardRate;
    RequestWhenAnswer:=True;
    ATxt:=str_RateText;
    if Assigned(FOnRate) then
      FOnRate(ATxt, ReplyMarkup);
    sendMessage(ATxt, pmMarkdown, True, ReplyMarkup);
  finally
    ReplyMarkup.Free;
  end;
end;

procedure TWebhookBot.TlgrmStatHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
var
  S, O: String;
begin
  if CurrentIsSimpleUser then
    Exit;
  RequestWhenAnswer:=True;
  S:=ExtractDelimited(2, AMessage.Text, [' ']);
  O:=ExtractDelimited(3, AMessage.Text, [' ']);
  if S<>EmptyStr then
    DoStat(S, O)
  else
    SendStatInlineKeyboard();
end;

procedure TWebhookBot.TlgrmStatFHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
var
  S: String;
begin
  if CurrentIsSimpleUser then
    Exit;
  RequestWhenAnswer:=True;
  S:=ExtractDelimited(2, AMessage.Text, [' ']);
  if S<>EmptyStr then
     DoStat(S, '', True)
  else
    SendStatInlineKeyboard(True);
end;

{ Please define ni18n (No i18n) for excluding translate unit from uses and exclude i18n support }
procedure TWebhookBot.LangTranslate(const ALang: String);{$IFNDEF ni18n}
var
  L, F: String;  {$ENDIF}
begin{$IFNDEF ni18n}
  if Length(ALang)>2 then
    L:=LeftStr(ALang, 2)
  else
    L:=ALang;
  F:='languages'+PathDelim+ApplicationName+'.%s.po';
  TranslateResourceStrings(F, L, '');{$ENDIF}
end;

procedure TWebhookBot.SendStatInlineKeyboard(SendFile: Boolean);
var
  ReplyMarkup: TReplyMarkup;
begin
  ReplyMarkup:=TReplyMarkup.Create;
  try
    ReplyMarkup.InlineKeyBoard:=CreateInlineKeyboardStat(Date, SendFile);
    RequestWhenAnswer:=True;
    sendMessage(str_SlctSttstcs_line1+LineEnding+str_SlctSttstcs_line2+LineEnding+
      str_SlctSttstcs_line3+LineEnding+str_SlctSttstcs_line4, pmHTML, True, ReplyMarkup);
  finally
    ReplyMarkup.Free;
  end;
end;

procedure TWebhookBot.StatLog(const AMessage: String; UpdateType: TUpdateType);
var
  EscMsg: String;
begin
  EscMsg:=AMessage;
  if Length(EscMsg)>150 then
    SetLength(EscMsg, 150);
  if CurrentIsSimpleUser then
    if Assigned(CurrentUser)then
      FBrookAction.StatLogger.Log([IntToStr(CurrentChatId), '@'+CurrentUser.Username, CurrentUser.First_name, CurrentUser.Last_name,
        CurrentUser.Language_code, UpdateTypeAliases[UpdateType], '"'+StringToJSONString(EscMsg)+'"'])
    else
      FBrookAction.StatLogger.Log(['', '', '', '', '', UpdateTypeAliases[UpdateType], '"'+EscMsg+'"'])
end;

function TWebhookBot.CreateInlineKeyboardRate: TInlineKeyboard;
var
  btns: TInlineKeyboardButtons;
  btn: TInlineKeyboardButton;
begin
  Result:=TInlineKeyboard.Create;
  btns:=Result.Add;
  btn:=TInlineKeyboardButton.Create(str_Rate);
  btn.url:=s_StoreBotRate+BotUsername;
  btns.Add(btn);
end;

function TWebhookBot.CreateInlineKeyboardStat(ADate: TDate): TInlineKeyboard;
var
  btns: TInlineKeyboardButtons;
  S: String;
begin
  Result:=TInlineKeyboard.Create;
  btns:=Result.Add;
  DateTimeToString(S, StatDateFormat, ADate);
  btns.AddButtons([str_Today+' 🔃', s_GetStat+' '+s_Today, str_Yesterday,
    s_GetStat+' '+s_Yesterday, str_Browse, s_GetStat+' '+S+' '+'0']);
end;

function TWebhookBot.CreateInlineKeyboardStat(ADate: TDate; SendFile: Boolean
  ): TInlineKeyboard;
begin
  if SendFile then
    Result:=CreateInlineKeyboardStatFile
  else
    Result:=CreateInlineKeyboardStat(ADate);
end;

function TWebhookBot.CreateInlineKeyboardStatFile: TInlineKeyboard;
var
  btns: TInlineKeyboardButtons;
begin
  Result:=TInlineKeyboard.Create;
  btns:=Result.Add;
  btns.AddButtons([str_Today+' 🔃', s_GetStat+'File'+' '+s_Today, str_Yesterday,
    s_GetStat+'File'+' '+s_Yesterday]);
end;

function TWebhookBot.CreateInlineKeyboardStat(ADate: TDate; Len: Integer;
  Offset: Integer; Step: Integer): TInlineKeyboard;
var
  PrevDate, NextDate, SDate: String;
  btns: TInlineKeyboardButtons;
  i: Integer;
begin
  Result:=TInlineKeyboard.Create;
  if Len=0 then
    Exit;
  DateTimeToString(PrevDate, StatDateFormat, ADate-1);
  DateTimeToString(NextDate, StatDateFormat, ADate+1);
  btns:=Result.Add;
  DateTimeToString(SDate, StatDateFormat, ADate);
  if Offset>0 then
  begin
    btns.AddButton('<<', s_GetStat+' '+ SDate+' '+IntToStr(0));
    i:=Offset-Step;
    if i<0 then
      i:=0;
    btns.AddButton('<', s_GetStat+' '+SDate+' '+IntToStr(i));
  end;
  if Offset+Step<Len then
  begin
    btns.AddButton('>', s_GetStat+' '+SDate+' '+IntToStr(Offset+Step));
    btns.AddButton('>>', s_GetStat+' '+SDate+' '+IntToStr(Len-Step));
  end;
  Result.Add.AddButtons([str_Today+' 🔃', s_GetStat+' '+s_Today,
    str_PrevDay, s_GetStat+' '+PrevDate+' '+'0', str_NextDay, s_GetStat+' '+NextDate+' '+'0']);
end;

procedure TWebhookBot.DoReceiveDeepLinking(const AParameter: String);
begin
  if Assigned(FOnReceiveDeepLinking) then
    FOnReceiveDeepLinking(AParameter);
end;

constructor TWebhookBot.Create(const AToken: String;
  AWebhookAction: TWebhookAction);
begin
  inherited Create(AToken);
  FBrookAction:=AWebhookAction;
  FUserPermissions:=TStringList.Create;
  FUserPermissions.Sorted:=True;
  FUserPermissions.Duplicates:=dupIgnore;
  FFeedbackThanks:=str_FeedbackThanks;
  FFeedbackText:=str_FeedbackText;
  CommandHandlers[cmd_Start]:=   @TlgrmStartHandler;
  CommandHandlers[cmd_Help]:=    @TlgrmHelpHandler;
  CommandHandlers[cmd_Feedback]:=@TlgrmFeedback;
  CommandHandlers[cmd_Rate]:=    @TlgrmRate;
  CommandHandlers[cmd_Stat]:=    @TlgrmStatHandler;
  CommandHandlers[cmd_StatF]:=   @TlgrmStatFHandler;
end;

destructor TWebhookBot.Destroy;
begin
  FUserPermissions.Free;
  inherited Destroy;
end;

procedure TWebhookBot.DoReceiveMessageUpdate(AMessage: TTelegramMessageObj);
begin
  inherited DoReceiveMessageUpdate(AMessage);
  if Assigned(AMessage.ReplyToMessage) then
  begin
    if Assigned(AMessage.ReplyToMessage.From) then
      if SameText(AMessage.ReplyToMessage.From.Username, BotUsername) then
      begin
        sendMessage(Format(FFeedbackThanks, [AMessage.From.First_name]));
        With AMessage do
          FBrookAction.SaveFeedback(From, Text);
      end;
    Exit;
  end;
  StatLog(AMessage.Text, utMessage);
  FBrookAction.BotMessageHandler(AMessage);
end;

procedure TWebhookBot.DoReceiveCallbackQuery(ACallback: TCallbackQueryObj);
var
  AHandled: Boolean;
begin
  AHandled:=False;
  inherited DoReceiveCallbackQuery(ACallback);
  if not CurrentIsSimpleUser then
  begin
    if AnsiStartsStr(s_GetStat+' ', ACallback.Data) then
    begin
      AHandled:=True;
      DoCallbackQueryStat(ACallback);
    end;
    if AnsiStartsStr(s_GetStat+s_File+' ', ACallback.Data) then
    begin
      AHandled:=True;
      DoCallbackQueryStat(ACallback, True);
    end;
  end
  else
    StatLog(ACallback.Data, utCallbackQuery);
  if not AHandled then
    FBrookAction.BotCallbackQuery(ACallback);
end;

procedure TWebhookBot.DoReceiveChannelPost(AChannelPost: TTelegramMessageObj);
begin
  inherited DoReceiveChannelPost(AChannelPost);
  StatLog(AChannelPost.Text, utChannelPost);
end;

procedure TWebhookBot.DoReceiveChosenInlineResult(
  AChosenInlineResult: TTelegramChosenInlineResultObj);
begin
  inherited DoReceiveChosenInlineResult(AChosenInlineResult);
  StatLog(AChosenInlineResult.Query, utChosenInlineResult);
end;

procedure TWebhookBot.DoReceiveInlineQuery(
  AnInlineQuery: TTelegramInlineQueryObj);
begin
  inherited DoReceiveInlineQuery(AnInlineQuery);
  StatLog(AnInlineQuery.Query, utInlineQuery);
end;

procedure TWebhookBot.DebugMessage(const Msg: String);
begin
  inherited DebugMessage(Msg);
  FBrookAction.LogMessage(Self, etDebug, Msg);
end;

procedure TWebhookBot.InfoMessage(const Msg: String);
begin
  inherited InfoMessage(Msg);                 
  FBrookAction.LogMessage(Self, etInfo, Msg);
end;

procedure TWebhookBot.ErrorMessage(const Msg: String);
begin
  inherited ErrorMessage(Msg);
  FBrookAction.LogMessage(Self, etError, Msg);
end;

function TWebhookBot.IsBanned(ChatID: Int64): Boolean;
begin
  Result:=FUserPermissions.Values[IntToStr(ChatID)]='b'
end;

function TWebhookBot.IsSimpleUser(ChatID: Int64): Boolean;
begin
  Result:=(FUserPermissions.Values[IntToStr(ChatID)]<>'a') and
    (FUserPermissions.Values[IntToStr(ChatID)]<>'m');
end;

procedure TWebhookBot.SetLanguage(const ALang: String);
begin
  inherited SetLanguage(ALang);
  LangTranslate(ALang);
end;

procedure TWebhookBot.LoadUserStatusValues(AStrings: TStrings);
begin
  FUserPermissions.AddStrings(AStrings);
end;

  { TWebhookAction }

procedure TWebhookAction.SetToken(AValue: String);
begin
  if FToken=AValue then Exit;
  FToken:=AValue;
  if Assigned(FBot) then
    FBot.Token:=FToken;
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

{ Caution! You must save this values in db or in configuration file! }
procedure TWebhookAction.BotSetCommandReply(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
var
  S: String;
begin
  if IsSimpleUser then
    Exit;
  S:=ExtractArgPart(AMessage.Text, ACommand);
  if ACommand=cmd_SetStart then
    Bot.StartText:=S
  else
    if ACommand=cmd_SetHelp then
      Bot.HelpText:=S;
  Bot.sendMessage(str_TxtRplyIsScsChngd);
end;

procedure TWebhookAction.BotTerminateHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  if IsSimpleUser then
    Exit;
  FBot.RequestWhenAnswer:=True;
  FBot.sendMessage(str_BtApIsClsd);
  BrookApp.Terminate;
end;

procedure TWebhookAction.SetBot(AValue: TWebhookBot);
begin
// todo Does It need a code here? Can it be so?
end;

procedure TWebhookAction.SetLogger(AValue: TEventLog);
begin
  if FLogger=AValue then Exit;
  FLogger:=AValue;
end;

function TWebhookAction.IsSimpleUser: Boolean;
begin
  Result:=FBot.CurrentIsSimpleUser;
end;

function TWebhookAction.IsBanned: Boolean;
begin
  Result:=Bot.CurrentIsBanned;
end;

function TWebhookAction.AppDir: String;
begin
  Result:=IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
end;

procedure TWebhookAction.BotCallbackQuery(ACallback: TCallbackQueryObj);
begin
  if Assigned(FOnCallbackQuery) then
    FOnCallbackQuery(Self, ACallback);
end;

procedure TWebhookAction.BotMessageHandler(AMessage: TTelegramMessageObj);
begin
  if Assigned(FOnUpdateMessage) then
    FOnUpdateMessage(Self, AMessage);
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
    FBot.sendDocumentByFileName(Bot.CurrentChatId, AFileName, str_SttstcsFr+DateToStr(ADate));
  end
  else
  begin
    FBot.RequestWhenAnswer:=True;
    EditOrSendMessage(str_SttstcsNtFnd, pmDefault, AReplyMarkup, True);
  end;
end;

procedure TWebhookAction.LogMessage(ASender: TObject; EventType: TEventType; const Msg: String);
begin
  if Assigned(FLogger) then
    Logger.Log(EventType, Msg);
end;

{ Sometimes, if the message is sent to the result of the CallBack call,
it is desirable not to create a new message and edit the message from which the call came }
procedure TWebhookAction.EditOrSendMessage(const AMessage: String;
  AParseMode: TParseMode; ReplyMarkup: TReplyMarkup; TryEdit: Boolean);
begin
  if not TryEdit or (Bot.CurrentUpdate.UpdateType<>utCallbackQuery) then
    Bot.sendMessage(AMessage, AParseMode, True, ReplyMarkup)
  else
    Bot.editMessageText(AMessage, AParseMode, True, ReplyMarkup);
end;

constructor TWebhookAction.Create;
begin
  inherited Create;
  FStatLogger:=TtgStatLog.Create(nil);
  FStatLogger.Active:=False;
  FStatLogger.TimeStampFormat:='hh:nn:ss';
  FBot:=TWebhookBot.Create(FToken, Self);
  FBot.CommandHandlers[cmd_Terminate]:=@BotTerminateHandler;
  FBot.CommandHandlers[cmd_SetStart]:= @BotSetCommandReply;
  FBot.CommandHandlers[cmd_SetHelp]:=  @BotSetCommandReply;
end;

destructor TWebhookAction.Destroy;
begin
  FBot.Free;
  FStatLogger.Free;
  inherited Destroy;
end;

procedure TWebhookAction.Post;
var
  Msg: String;
  lParser: TJSONParser;
  AnUpdate: TTelegramUpdateObj;
begin
  Msg:={$IFNDEF bf30}HttpRequest{$ELSE}TheRequest{$ENDIF}.Content;
  LogMessage(Self, etDebug, 'Recieve the update (Webhook): '+Msg);
  if Msg<>EmptyStr then
  begin
    lParser := TJSONParser.Create(Msg, DefaultOptions);
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
      {$IFNDEF bf30}HttpResponse{$ELSE}TheResponse{$ENDIF}.ContentType:=BROOK_HTTP_CONTENT_TYPE_APP_JSON;
      Write(FBot.RequestBody);
    end;
  end;
end;

end.
