unit brooktelegramaction;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, tgtypes, tgsendertypes, sysutils, classes, tgstatlog, eventlog,
  ghashmap, fpjson;

type

  TUserStatus = (usAdmin, usModerator, usDefault, usBanned);

  TWebhookBot = class;

  TRateEvent = procedure (var RateText: String; ReplyMarkup: TReplyMarkup) of object;
  TReceiveDeepLinkEvent = procedure (const AParameter: String) of object;

  TCallbackHandlersMap = specialize TStringHashMap<TCallbackEvent>;

  { TWebhookAction }

  TWebhookAction = class(TBrookAction)
  private
    FLogDebug: Boolean;
    FLogger: TEventLog;
    FOnCallbackQuery: TCallbackEvent;
    FOnUpdateMessage: TMessageEvent;
    FToken: String;
    FBot: TWebhookBot;
    procedure SetBot({%H-}AValue: TWebhookBot);
    procedure SetLogDebug(AValue: Boolean);
    procedure SetLogger(AValue: TEventLog);
    procedure SetOnCallbackQuery(AValue: TCallbackEvent);
    procedure SetOnUpdateMessage(AValue: TMessageEvent);
    procedure SetToken(AValue: String);
  protected
    procedure BotMessageHandler(AMessage: TTelegramMessageObj); virtual;
    { Use Bot.EditOrSendMessage instead... }
    procedure EditOrSendMessage(const AMessage: String; AParseMode: TParseMode = pmDefault;
      ReplyMarkup: TReplyMarkup = nil; TryEdit: Boolean = False); deprecated;
    function IsSimpleUser: Boolean; deprecated; // Use Bot.CurrentIsSimpleUser instead
    function IsBanned: Boolean; deprecated; // Use Bot.CurrentIsBanned instead
    procedure SaveFeedback(AFrom: TTelegramUserObj; AMessage: String); virtual; abstract; deprecated;
  public
    function AppDir: String;
    constructor Create; override;
    destructor Destroy; override;
    procedure Post; override;
    property Token: String read FToken write SetToken; deprecated; // Use Bot.Token instead
    { Use Bot.OnReceiveCallbackQuery instead }
    property OnCallbackQuery: TCallbackEvent read FOnCallbackQuery write SetOnCallbackQuery; deprecated;
    { Use Bot.OnReceiveMessageUpdate instead }
    property OnUpdateMessage: TMessageEvent read FOnUpdateMessage write SetOnUpdateMessage; deprecated;
    property Logger: TEventLog read FLogger write SetLogger; deprecated; // Use Bot.Logger instead
    property Bot: TWebhookBot read FBot write SetBot;
    property LogDebug: Boolean read FLogDebug write SetLogDebug; deprecated;
  end;

  { TWebhookBot }
  TWebhookBot = class(TTelegramSender)
  private
    FCallbackAnswered: Boolean;
    FCallbackHandlers: TCallbackHandlersMap;
    FHelpText: String;
    FBrookAction: TWebhookAction;
    FFeedbackText: String;
    FFeedbackThanks: String;
    FOnRate: TRateEvent;
    FOnReceiveDeepLinking: TReceiveDeepLinkEvent;
    FPublicStat: Boolean;
    FStartText: String;
    FStatLogger: TtgStatLog;
    FUserPermissions: TStringList;
    procedure BrowseStatFile(aDate: TDate; var Msg: String; ReplyMarkup: TReplyMarkup; Offset: Integer);
    procedure CalculateStat4Strings(aStatFile: TStrings; IDs: TIntegerHashSet;
      var aEvents: Integer);
    procedure DoCallbackQueryStat(ACallbackQuery: TCallbackQueryObj; SendFile: Boolean = False);
    procedure DoCallbackQueryGetUsers(ACallbackQuery: TCallbackQueryObj);
    procedure DoGetStat(aFromDate: TDate = 0; aToDate: TDate = 0; Scroll: Boolean = False; Offset: Integer = 0);
    procedure DoGetUsers(aFromDate: TDate = 0; aToDate: TDate = 0);
    procedure DoGetStatMonth(aYear, aMonth: Word);
    procedure DoGetStatFile(ADate: TDate = 0);
    procedure DoStat(const SDate: String; const SOffset: String = ''; SendFile: Boolean = false);
    procedure DoUsers(const SDate: String);
    function GetCallbackHandlers(const Command: String): TCallbackEvent;
    function GetUserStatus(ID: Int64): TUserStatus;
    procedure SendStatLog(ADate: TDate = 0; AReplyMarkup: TReplyMarkup = nil);
    procedure SetBrookAction(AValue: TWebhookAction);
    procedure SetCallbackHandlers(const Command: String; AValue: TCallbackEvent
      );
    procedure SetCommandReply({%H-}ASender: TObject; const ACommand: String;
      AMessage: TTelegramMessageObj);
    procedure SetHelpText(AValue: String);
    procedure SetOnRate(AValue: TRateEvent);
    procedure SetOnReceiveDeepLinking(AValue: TReceiveDeepLinkEvent);
    procedure SetPublicStat(AValue: Boolean);
    procedure SetStartText(AValue: String);
    procedure SetStatLogger(AValue: TtgStatLog);
    procedure SetUserStatus(ID: Int64; AValue: TUserStatus);
    procedure TerminateHandler({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
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
    function CreateInlineKeyboardStat(aFromDate, aToDate: TDate): TInlineKeyboard;
    function CreateInlineKeyboardStat(ADate: TDate; SendFile: Boolean): TInlineKeyboard; overload;
    function CreateInlineKeyboardStatFile: TInlineKeyboard;
    function CreateInlineKeyboardStatMonth(ADate: TDate): TInlineKeyboard;
    function CreateInlineKeyboardStat(ADate: TDate; Len: Integer; Offset: Integer = 0;
      Step: Integer = 20): TInlineKeyboard; overload;
    procedure DoReceiveMessageUpdate(AMessage: TTelegramMessageObj); override;
    procedure DoReceiveCallbackQuery(ACallback: TCallbackQueryObj); override;
    procedure DoReceiveChannelPost(AChannelPost: TTelegramMessageObj); override;
    procedure DoReceiveChosenInlineResult(
      AChosenInlineResult: TTelegramChosenInlineResultObj); override;
    procedure DoReceiveInlineQuery(AnInlineQuery: TTelegramInlineQueryObj); override;
    property BrookAction: TWebhookAction read FBrookAction write SetBrookAction;
    function IsAdminUser(ChatID: Int64): Boolean; override;
    function IsBanned(ChatID: Int64): Boolean; override;                        
    function IsBotUser(ChatID: Int64): Boolean; override;
    function IsSimpleUser(ChatID: Int64): Boolean; override;
    procedure SaveFeedback(AFrom: TTelegramUserObj; AMessage: String); virtual;
    procedure SetLanguage(const ALang: String); override;
  public
    function answerCallbackQuery(const CallbackQueryId: String; const Text: String=''; ShowAlert: Boolean=False;
      const Url: String=''; CacheTime: Integer=0): Boolean; override;
    procedure CalculateStat(aFromDate, aToDate: TDate; out aUsers, aEvents: Integer;
      aIDs: TStrings = nil);
    constructor Create(const AToken: String; AWebhookAction: TWebhookAction);
    destructor Destroy; override;
    procedure DoReceiveDeepLinking(const AParameter: String);
    function DoReceiveUpdate(const aUpdate: String): Boolean; overload;
    procedure EditOrSendMessage(const AMessage: String; AParseMode: TParseMode = pmDefault;
      ReplyMarkup: TReplyMarkup = nil; TryEdit: Boolean = False);
    procedure LoadUserStatusValues(AStrings: TStrings);
    property CallbackHandlers [const Command: String]: TCallbackEvent read GetCallbackHandlers
      write SetCallbackHandlers;  // It can create command handlers by assigning their to array elements
    property StartText: String read FStartText write SetStartText; // Text for /start command reply
    property HelpText: String read FHelpText write SetHelpText;  // Text for /help command reply
    property FeedbackText: String read FFeedbackText write FFeedbackText;
    property FeedbackThanks: String read FFeedbackThanks write FFeedbackThanks;
    property UserStatus [ID: Int64]: TUserStatus read GetUserStatus write SetUserStatus;
    property OnRate: TRateEvent read FOnRate write SetOnRate;
    property OnReceiveDeepLinking: TReceiveDeepLinkEvent read FOnReceiveDeepLinking write SetOnReceiveDeepLinking;
    property PublicStat: Boolean read FPublicStat write SetPublicStat;
    property StatLogger: TtgStatLog read FStatLogger write SetStatLogger;
  end;

function ExtractArgPart(const ASource, ACommand: String): String;
function FormatStatRec(const S: String): String;

implementation
{ Please define ni18n (No i18n) for excluding translate unit from uses and exclude i18n support }
uses jsonparser, BrookHttpConsts, strutils, BrookApplication, jsonscanner{$IFNDEF ni18n},
  Translations{$ENDIF}, tgutils, LazUTF8, dateutils;

// Please use i18n for localization *** Пожалуйста, используйте i18n для локализации
resourcestring
  str_FeedbackThanks='Thanks! %s Your message will be considered!'; // Спасибо, %s! Ваш сообщение будет обязательно рассмотрено!;
  str_FeedbackText='Send us your suggestions or bug reports please';  // Отправьте разработчику бота пожелание, рекомендацию, информацию об ошибке
  str_TxtRplyIsScsChngd='Text reply for command is succesfully changed!'; // Текст сообщения для команды успешно изменен
  str_BtApIsClsd='Bot app is closed';  // Приложение закрыто
  str_SttstcsNtFnd='Statistics for this date not found'; // Статистика не найдено за указанный день
//  str_ErrFldTLdSttstcsFl='Error: failed to load statistics file';
  str_EntrDtInFrmt='Please enter the date in format: dd-mm-yyyy';
  str_SttstcsFr='Statistics for ';
  str_SlctSttstcs_line1='Select statistics by pressing the button. In addition, the available commands:';
  str_SlctSttstcs_line2='/stat <i>day</i> - the last records for a specified <i>date</i>, ';
  str_SlctSttstcs_line3= '/statf <i>date</i> - statistics file for the <i>date</i>,';
  str_SlctSttstcs_line4='where <i>date</i> is <i>today</i> or <i>yesterday</i> or in format <i>dd-mm-yyyy</i>';
  str_Today='Today';
  str_Yesterday='Yesterday';
  str_Monthly='Monthly';
  str_PrevDay='Prev day';
  str_NextDay='Next day';
  str_Users='Users';
  str_Prev='<';
  str_Next='>';
  str_Rate='Rate';
  str_RateText='Please leave feedback on "Storebot" if you like our bot!';
  str_Browse='Browse';
  str_StatParseError='Stat line parser error';


const
  StatDateFormat = 'dd-mm-yyyy';
  StatMonthFormat = 'mm-yyyy';
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
  s_GetUsers='GetUsers';
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
  try
    l:=ExtractStrings([';'], [' '], PChar(S), Ss, True);
    if l<>8 then
      Result:=str_StatParseError
    else
      Result:=Ss[0]+'; '+'['+Ss[1]+'](tg://user?id='+Ss[1]+') '+
        MarkdownEscape(Ss[2]+' {'+TJSONUnicodeStringType(Ss[3]+' '+Ss[4])+'}')+ ' '+
        MarkdownEscape(Ss[5])+' <'+MarkdownEscape(Ss[6])+'> '+MarkdownEscape(JSONStringToString(Ss[7]));
  except
    Result:=str_StatParseError
  end;
  Ss.Free;
end;

function TryStrToMonth(const S: String; out aYear, aMonth: Word): Boolean;
var
  i: SizeInt;
  wMonth, wYear: DWord;
begin
  i:=Pos('-', S);
  if (i<2) or (i>3) then
    Exit(False);
  if TryStrToDWord(LeftStr(S, i-1), wMonth) then
  begin
    aMonth:=wMonth;
    if not TryStrToDWord(RightStr(S, Length(S)-i), wYear) then
      aYear:=CurrentYear
    else
      aYear:=wYear;
  end
  else
    Exit(False);
  Result:=True;
end;

{ TWebhookBot }

procedure TWebhookBot.SetBrookAction(AValue: TWebhookAction);
begin
  if FBrookAction=AValue then Exit;
  FBrookAction:=AValue;
end;

procedure TWebhookBot.SetCallbackHandlers(const Command: String;
  AValue: TCallbackEvent);
begin
  FCallbackHandlers.Items[Command]:=AValue;
end;


{ Caution! You must save this values in db or in configuration file! }
procedure TWebhookBot.SetCommandReply(ASender: TObject; const ACommand: String;
  AMessage: TTelegramMessageObj);
var
  S: String;
begin
  if not CurrentIsAdminUser then
    Exit;
  S:=ExtractArgPart(AMessage.Text, ACommand);
  if ACommand=cmd_SetStart then
    StartText:=S
  else
    if ACommand=cmd_SetHelp then
      HelpText:=S;
  sendMessage(str_TxtRplyIsScsChngd);
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

procedure TWebhookBot.SetPublicStat(AValue: Boolean);
begin
  if FPublicStat=AValue then Exit;
  FPublicStat:=AValue;
end;

procedure TWebhookBot.SetStartText(AValue: String);
begin
  if FStartText=AValue then Exit;
  FStartText:=AValue;
end;

procedure TWebhookBot.SetStatLogger(AValue: TtgStatLog);
begin
  if FStatLogger=AValue then Exit;
  FStatLogger:=AValue;
end;

procedure TWebhookBot.BrowseStatFile(aDate: TDate; var Msg: String;
  ReplyMarkup: TReplyMarkup; Offset: Integer);
var
  StatFile: TStringList;
  aFileName: String;
  i: Integer;
const
  Step=20;
begin
  aFileName:=FStatLogger.GetFileNameFromDate(aDate);
  if FileExists(aFileName) then
  begin
    StatFile:=TStringList.Create;
    try
      StatFile.LoadFromFile(AFileName);
      ReplyMarkup.InlineKeyBoard:=CreateInlineKeyboardStat(aDate, StatFile.Count, Offset,
        Step);
      for i:=Offset to Offset+Step-1 do
      begin
        if i>=StatFile.Count then
          Break;
        Msg+=FormatStatRec(StatFile[i])+LineEnding;
      end;
    finally
      StatFile.Free;
    end;
  end;
end;

procedure TWebhookBot.CalculateStat4Strings(aStatFile: TStrings;
  IDs: TIntegerHashSet; var aEvents: Integer);
var
  i: Integer;
  AnID: Int64;
begin
  for i:=aStatFile.Count-1 downto 0 do
  begin
    AnID:=StrToInt64Def(ExtractDelimited(2, aStatFile[i], [';']), 0);
    if AnID>0 then
    begin
      Inc(aEvents);
      IDs.insert(AnID);
    end;
  end;
end;

procedure TWebhookBot.CalculateStat(aFromDate, aToDate: TDate; out aUsers,
  aEvents: Integer; aIDs: TStrings);
var
  StatFile: TStringList;
  aDate: TDate;
  aFileName: String;
  IDs: TIntegerHashSet;
  aIterator: TIntegerHashSet.TIterator;
begin
  StatFile:=TStringList.Create;
  IDs:=TIntegerHashSet.create;
  aDate:=aFromDate;
  aEvents:=0;
  try
    repeat
      aFileName:=StatLogger.GetFileNameFromDate(aDate);
      if FileExists(aFileName) then
      begin
        StatFile.LoadFromFile(AFileName);
        { Simple calculation of statistics: the number of unique users per day and
          the number of received events from users (private chats) }
        CalculateStat4Strings(StatFile, IDs, aEvents);
      end;
      aDate+=1;
    until aDate>aToDate;
    aUsers:=IDs.size;
    if Assigned(aIDs) then
    begin
      aIDs.Clear;
      aIterator:=IDs.Iterator;
      try
        if IDs.IsEmpty then
          Exit;
        repeat
          aIDs.Add(aIterator.Data.ToString);
        until not aIterator.Next;
      finally
        aIterator.Free;
      end;
    end;
  finally
    StatFile.Free;
    IDs.Free;
  end;
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

procedure TWebhookBot.DoCallbackQueryGetUsers(ACallbackQuery: TCallbackQueryObj
  );
begin
  DoUsers(ExtractDelimited(2, ACallbackQuery.Data, [' ']));
  RequestWhenAnswer:=False;
  answerCallbackQuery(ACallbackQuery.ID);
end;

procedure TWebhookBot.DoGetStat(aFromDate: TDate; aToDate: TDate;
  Scroll: Boolean; Offset: Integer);
var
  Msg, SDate: String;
  SDate1: String;
  aEvents, aUsers: Integer;
  ReplyMarkup: TReplyMarkup;
begin
  if not CurrentIsAdminUser then
    if not PublicStat or Scroll then
      Exit;
  ReplyMarkup:=TReplyMarkup.Create;
  try
    RequestWhenAnswer:=False;
    try
      if aToDate<=aFromDate then
      begin
        DateTimeToString(SDate, 'dd-mm-yyyy', aFromDate);
        Msg:='*Statistics for '+SDate+'*'+LineEnding;
      end
      else begin
        DateTimeToString(SDate, 'dd-mm-yyyy', aFromDate);
        DateTimeToString(SDate1, 'dd-mm-yyyy', aToDate);
        Msg:='*Statistics from '+SDate+' to '+SDate1+'*'+LineEnding;
      end;
      if not Scroll then
      begin
        ReplyMarkup.InlineKeyBoard:=CreateInlineKeyboardStat(aFromDate, aToDate);
        aUsers:=0;
        aEvents:=0;
        CalculateStat(aFromDate, aToDate, aUsers, aEvents);
        Msg+=LineEnding+'Unique users: '+IntToStr(aUsers)+', user events: '+IntToStr(aEvents);
      end
      else
        BrowseStatFile(aFromDate, Msg, ReplyMarkup, Offset);
      editMessageText(Msg, pmMarkdown, True, ReplyMarkup);
    except
      on E: Exception do
        ErrorMessage('Error while get statistics: ['+E.ClassName+'] '+E.Message);
    end;
  finally
    ReplyMarkup.Free;
  end;
end;

procedure TWebhookBot.DoGetUsers(aFromDate: TDate; aToDate: TDate);
var
  Msg, SDate: String;
  SDate1: String;
  aEvents, aUsers: Integer;
  ReplyMarkup: TReplyMarkup;
  aIDs: TStringList;
  aStream: TStringStream;
begin
  if not CurrentIsAdminUser then
    Exit;
  ReplyMarkup:=TReplyMarkup.Create;
  try
    RequestWhenAnswer:=False;
    try
      if aToDate<=aFromDate then
      begin
        DateTimeToString(SDate, 'dd-mm-yyyy', aFromDate);
      end
      else begin
        DateTimeToString(SDate, 'dd-mm-yyyy', aFromDate);
        DateTimeToString(SDate1, 'dd-mm-yyyy', aToDate);
      end;
      ReplyMarkup.InlineKeyBoard:=CreateInlineKeyboardStat(aFromDate, aToDate);
      aUsers:=0;
      aEvents:=0;
      Msg:=EmptyStr;
      aIDs:=TStringList.Create;
      try
        CalculateStat(aFromDate, aToDate, aUsers, aEvents, aIDs);
        Msg+=LineEnding+'Unique users: '+IntToStr(aUsers)+', user events: '+IntToStr(aEvents);
        aStream:=TStringStream.Create(aIDs.Text);
        sendDocumentStream(CurrentChatId, 'users.txt', aStream, Msg);
      finally
        aStream.Free;
        aIDs.Free;
      end;
    except
      on E: Exception do
        ErrorMessage('Error while get statistics: ['+E.ClassName+'] '+E.Message);
    end;
  finally
    ReplyMarkup.Free;
  end;
end;

procedure TWebhookBot.DoGetStatMonth(aYear, aMonth: Word);
var
  Msg, SDate: String;
  aEvents, aUsers, sEvents, sUsers: Integer;
  ReplyMarkup: TReplyMarkup;
  aDate: TDate;
  aToDate, aFromDate: TDate;
begin
  if not CurrentIsAdminUser then
    if not PublicStat then
      Exit;
  ReplyMarkup:=TReplyMarkup.Create;
  try
    RequestWhenAnswer:=False;
    try
      SDate:=aMonth.ToString+'-'+aYear.ToString;
      aToDate:=EndOfAMonth(aYear, aMonth);
      aFromDate:=StartOfAMonth(aYear, aMonth);
      Msg:='*Statistics for '+SDate+'*'+LineEnding;
      ReplyMarkup.InlineKeyBoard:=CreateInlineKeyboardStatMonth(aFromDate);
      aUsers:=0;
      aEvents:=0;
      CalculateStat(aFromDate, aToDate, aUsers, aEvents);
      Msg+=LineEnding+'Unique users: '+IntToStr(aUsers)+', user events: '+IntToStr(aEvents);
      aDate:=aFromDate;
      sUsers:=0;
      sEvents:=0;
      repeat
        aUsers:=0;
        aEvents:=0;
        CalculateStat(aDate, aDate,  aUsers, aEvents);
        DateTimeToString(SDate, 'dd-mm-yyyy', aDate);
        Msg+=LineEnding+mdCode+SDate+mdCode+' - _users:_ '+IntToStr(aUsers)+'_, events:_ '+IntToStr(aEvents);
        sEvents+=aEvents;
        sUsers+=aUsers;
        aDate+=1;
      until (aDate>aToDate) or (aDate>Date);
      Msg+=LineEnding+mdCode+'Summary'+mdCode+' - _users:_ '+
        IntToStr(sUsers div DaysBetween(aFromDate, aToDate))+'_, events:_ '+IntToStr(sEvents);
      editMessageText(Msg, pmMarkdown, True, ReplyMarkup);
    except
      on E: Exception do
        ErrorMessage('Error while get month statistics: ['+E.ClassName+'] '+E.Message);
    end;
  finally
    ReplyMarkup.Free;
  end;
end;

procedure TWebhookBot.DoGetStatFile(ADate: TDate);
var
  ReplyMarkup: TReplyMarkup;
begin
  if not CurrentIsAdminUser then
    Exit;
  ReplyMarkup:=TReplyMarkup.Create;
  try
    ReplyMarkup.InlineKeyBoard:=CreateInlineKeyboardStatFile;
    SendStatLog(ADate, ReplyMarkup)
  finally
    ReplyMarkup.Free;
  end;
end;

procedure TWebhookBot.DoStat(const SDate: String; const SOffset: String;
  SendFile: Boolean);
var
  S: String;
  i: SizeInt;
  aToDate, aFromDate: TDate;
  aMonth, aYear: Word;
begin
  if not Assigned(FStatLogger) then
    Exit;
  aToDate:=0;
  S:=Trim(SDate);
  if (S=s_Today) or (S=EmptyStr) then
    aFromDate:=Date
  else
    if S=s_Yesterday then
      aFromDate:=Date-1
    else begin
      i:=Pos('/', S);
      if i>0 then
      begin
        if not (TryStrToDate(LeftStr(S, i-1), aFromDate, StatDateFormat, '-') and
          TryStrToDate(RightStr(S, Length(S)-i), aToDate, StatDateFormat, '-')) then
        begin
          RequestWhenAnswer:=True;
          sendMessage(str_EntrDtInFrmt);
          Exit;
        end
      end
      else
        if Length(S)>7 then
        begin
          if not TryStrToDate(S, aFromDate, StatDateFormat, '-') then
          begin
            RequestWhenAnswer:=True;
            sendMessage(str_EntrDtInFrmt);
            Exit;
          end
        end
        else begin
          if TryStrToMonth(S, aYear, aMonth) then
            DoGetStatMonth(aYear, aMonth)
          else begin
            RequestWhenAnswer:=True;
            sendMessage(str_EntrDtInFrmt);
          end;
          Exit;
        end;
     end;
  if SOffset=EmptyStr then
    if not SendFile then
      DoGetStat(aFromDate, aToDate)
    else
      DoGetStatFile(aFromDate)
  else
    DoGetStat(aFromDate, aToDate, True, StrToIntDef(SOffset, 0));
end;

procedure TWebhookBot.DoUsers(const SDate: String);
var
  S: String;
  i: SizeInt;
  aToDate, aFromDate: TDate;
begin
  if not Assigned(FStatLogger) then
    Exit;
  aToDate:=0;
  S:=Trim(SDate);
  if (S=s_Today) or (S=EmptyStr) then
    aFromDate:=Date
  else
    if S=s_Yesterday then
      aFromDate:=Date-1
    else begin
      i:=Pos('/', S);
      if i>0 then
      begin
        if not (TryStrToDate(LeftStr(S, i-1), aFromDate, StatDateFormat, '-') and
          TryStrToDate(RightStr(S, Length(S)-i), aToDate)) then
        begin
          RequestWhenAnswer:=True;
          sendMessage(str_EntrDtInFrmt);
          Exit;
        end
      end
      else
        if not TryStrToDate(S, aFromDate, StatDateFormat, '-') then
        begin
          RequestWhenAnswer:=True;
          sendMessage(str_EntrDtInFrmt);
        end
     end;
  DoGetUsers(aFromDate, aToDate)
end;

function TWebhookBot.GetCallbackHandlers(const Command: String): TCallbackEvent;
begin
    Result:=FCallbackHandlers.Items[Command];
end;

function TWebhookBot.GetUserStatus(ID: Int64): TUserStatus;
begin
  Result:=AnsiCharToUserStatus(FUserPermissions.Values[IntToStr(ID)]);
end;

procedure TWebhookBot.SendStatLog(ADate: TDate; AReplyMarkup: TReplyMarkup);
var
  AFileName: String;
begin
  if ADate=0 then
    ADate:=sysutils.Date;
  AFileName:=FStatLogger.GetFileNameFromDate(ADate);
  if FileExists(AFileName) then
  begin
    RequestWhenAnswer:=False;
    sendDocumentByFileName(CurrentChatId, AFileName, str_SttstcsFr+DateToStr(ADate));
  end
  else
  begin
    RequestWhenAnswer:=True;
    EditOrSendMessage(str_SttstcsNtFnd, pmDefault, AReplyMarkup, True);
  end;
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

procedure TWebhookBot.TerminateHandler(ASender: TObject; const ACommand: String;
  AMessage: TTelegramMessageObj);
begin
  if not CurrentIsAdminUser then
    Exit;
  RequestWhenAnswer:=True;
  sendMessage(str_BtApIsClsd);
  BrookApp.Terminate;
end;

procedure TWebhookBot.TlgrmStartHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  UpdateProcessed:=True;
  if not {%H-}AMessage.Text.Contains(' ') then
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
  UpdateProcessed:=True;
  RequestWhenAnswer:=True;
  sendMessage(FHelpText, pmMarkdown);
end;

procedure TWebhookBot.TlgrmFeedback(ASender: TObject; const ACommand: String;
  AMessage: TTelegramMessageObj);
var
  ReplyMarkup: TReplyMarkup;
begin
  UpdateProcessed:=True;
  ReplyMarkup:=TReplyMarkup.Create;
  try
    ReplyMarkup.ForceReply:=True;
    sendMessage(cmd_Feedback+LineEnding+FFeedbackText, pmMarkdown, True, ReplyMarkup);
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
  if not (PublicStat or CurrentIsAdminUser) then
    Exit;
  RequestWhenAnswer:=True;
  S:=ExtractDelimited(2, AMessage.Text, [' ']);
  O:=ExtractDelimited(3, AMessage.Text, [' ']);
  if S<>EmptyStr then
    DoStat(S, O)
  else
    SendStatInlineKeyboard();
  UpdateProcessed:=True;
end;

procedure TWebhookBot.TlgrmStatFHandler(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
var
  S: String;
begin
  if not CurrentIsAdminUser then
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
  if ALang=EmptyStr then
    Exit;
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
  if CurrentIsAdminUser or CurrentIsBotUser then
    Exit;
  if AMessage=EmptyStr then
    Exit;
  EscMsg:=UTF8LeftStr(AMessage, 150);
  try
    if Assigned(CurrentUser)then
    begin
      if (CurrentChatId>0) or UpdateProcessed then        // if message in the group and not processed then do not log
        StatLogger.Log([IntToStr(CurrentChatId), '@'+CurrentUser.Username,
          CurrentUser.First_name, CurrentUser.Last_name, CurrentUser.Language_code,
          UpdateTypeAliases[UpdateType], '"'+StringToJSONString(EscMsg)+'"'])
    end
    else begin
      if Assigned(CurrentChat) then
        FStatLogger.Log([IntToStr(CurrentChatId), '@'+CurrentChat.Username,
          CurrentChat.First_name, CurrentChat.Last_name, '-', UpdateTypeAliases[UpdateType],
          '"'+StringToJSONString(EscMsg)+'"'])
    end;
  except
    on E: Exception do
      ErrorMessage('Can''t write to log StatLogger. '+E.ClassName+': '+E.Message);
  end;
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

function TWebhookBot.CreateInlineKeyboardStat(aFromDate, aToDate: TDate): TInlineKeyboard;
var
  btns: TInlineKeyboardButtons;
  S, PrevDate, NextDate, FromDate, ToDate, SMonth: String;
begin
  DateTimeToString(PrevDate, StatDateFormat, aFromDate-1);
  DateTimeToString(NextDate, StatDateFormat, aFromDate+1);
  DateTimeToString(FromDate, StatDateFormat, aFromDate);
  DateTimeToString(ToDate, StatDateFormat, aToDate);
  Result:=TInlineKeyboard.Create;
  btns:=Result.Add;
  DateTimeToString(S, StatDateFormat, aFromDate);
  DateTimeToString(SMonth, StatMonthFormat, aFromDate);
  if CurrentIsAdminUser then
  begin
    btns.AddButtons([str_Today+' 🔃', s_GetStat+' '+s_Today, str_Monthly,
      s_GetStat+' '+SMonth, str_Browse, s_GetStat+' '+S+' '+'0']);
    btns:=Result.Add;
    btns.AddButtons([str_PrevDay, s_GetStat+' '+PrevDate, str_NextDay, s_GetStat+' '+NextDate,
      str_Users, s_GetUsers+' '+FromDate+'/'+ToDate]);
  end
  else
    btns.AddButtons([str_Today+' 🔃', s_GetStat+' '+s_Today,
      str_PrevDay, s_GetStat+' '+PrevDate, str_NextDay, s_GetStat+' '+NextDate])
end;

function TWebhookBot.CreateInlineKeyboardStat(ADate: TDate; SendFile: Boolean
  ): TInlineKeyboard;
begin
  if SendFile then
    Result:=CreateInlineKeyboardStatFile
  else
    Result:=CreateInlineKeyboardStat(ADate, ADate);
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

function TWebhookBot.CreateInlineKeyboardStatMonth(ADate: TDate
  ): TInlineKeyboard;
var
  btns: TInlineKeyboardButtons;
  S, PrevDate, NextDate: String;
begin
  DateTimeToString(PrevDate, StatMonthFormat, IncMonth(ADate, -1));
  DateTimeToString(NextDate, StatMonthFormat, IncMonth(ADate, 1));
  Result:=TInlineKeyboard.Create;
  btns:=Result.Add;
  DateTimeToString(S, StatDateFormat, ADate);
  if CurrentIsAdminUser then
  begin
    btns.AddButtons([str_Today+' 🔃', s_GetStat+' '+s_Yesterday, str_Browse+' '+S, s_GetStat+' '+S+' '+'0']);
    btns:=Result.Add;
    btns.AddButtons([str_Prev, s_GetStat+' '+PrevDate, str_Next, s_GetStat+' '+NextDate]);
  end
  else
    btns.AddButtons([str_Today+' 🔃', s_GetStat+' '+s_Today,
      str_Prev, s_GetStat+' '+PrevDate, str_Next, s_GetStat+' '+NextDate])
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

function TWebhookBot.DoReceiveUpdate(const aUpdate: String): Boolean;
var
  aJSON: TJSONObject;
begin
  Result:=False;
  if aUpdate.IsEmpty then
  begin
    ErrorMessage('POST data Content of HTTP request is empty!');
    Exit;
  end;
  try
    aJSON:=GetJSON(aUpdate) as TJSONObject;
    try
      DoReceiveUpdate(TTelegramUpdateObj.CreateFromJSONObject(aJSON) as TTelegramUpdateObj);
      Result:=True;
    finally
      aJSON.Free;
    end;
  except
    on E:Exception do
    begin
      ErrorMessage('Error while parse json string ('+E.ClassName+': '+E.Message+')');
      Exit;
    end;
  end;
end;

constructor TWebhookBot.Create(const AToken: String;
  AWebhookAction: TWebhookAction);
begin
  inherited Create(AToken);
  FBrookAction:=AWebhookAction;
  FStatLogger:=TtgStatLog.Create(nil);
  FStatLogger.Active:=False;
  FStatLogger.TimeStampFormat:='hh:nn:ss';
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
  CommandHandlers[cmd_Terminate]:=@TerminateHandler;
  CommandHandlers[cmd_SetStart]:= @SetCommandReply;
  CommandHandlers[cmd_SetHelp]:=  @SetCommandReply;
  FCallbackHandlers:=TCallbackHandlersMap.create;
  FPublicStat:=False;
  FCallbackAnswered:=False;
end;

destructor TWebhookBot.Destroy;
begin
  FCallbackHandlers.Free;
  FUserPermissions.Free;
  FStatLogger.Free;
  inherited Destroy;
end;

procedure TWebhookBot.DoReceiveMessageUpdate(AMessage: TTelegramMessageObj);
begin
  inherited DoReceiveMessageUpdate(AMessage);
  if Assigned(AMessage.ReplyToMessage) and not UpdateProcessed then
  begin
    if Assigned(AMessage.ReplyToMessage.From) then
      if SameText(AMessage.ReplyToMessage.From.Username, BotUsername) and
        AnsiStartsStr(cmd_Feedback, AMessage.ReplyToMessage.Text) then
      begin
        sendMessage(Format(FFeedbackThanks, [AMessage.From.First_name]));
        With AMessage do
          SaveFeedback(From, Text);
      end;
    Exit;
  end;
  StatLog(AMessage.Text, utMessage);
  FBrookAction.BotMessageHandler(AMessage);
end;

procedure TWebhookBot.DoReceiveCallbackQuery(ACallback: TCallbackQueryObj);
var
  AHandled, AFlag: Boolean;
  ACommand: String;
  H: TCallbackEvent;
begin
  AHandled:=False;
  FCallbackAnswered:=False;
  inherited DoReceiveCallbackQuery(ACallback);
  if CurrentIsAdminUser or PublicStat then
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
  end;
  if CurrentIsAdminUser and AnsiStartsStr(s_GetUsers+' ', ACallback.Data) then
  begin
    AHandled:=True;
    DoCallbackQueryGetUsers(ACallback);
  end;
  StatLog(ACallback.Data, utCallbackQuery);
  if not AHandled then
  begin
    AFlag:=RequestWhenAnswer;
    ACommand:=ExtractWord(1, ACallback.Data, [' ']);
    if FCallbackHandlers.contains(ACommand) then
    begin
      H:=FCallbackHandlers.Items[ACommand];
      H(Self, ACallback);
      RequestWhenAnswer:=False;
      if not FCallbackAnswered then
        answerCallbackQuery(ACallback.ID); // if user do not call this in callback procedure
      RequestWhenAnswer:=AFlag;
      AHandled:=True;
    end;
  end;
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

function TWebhookBot.IsAdminUser(ChatID: Int64): Boolean;
begin
  Result:=(FUserPermissions.Values[IntToStr(ChatID)]='a') or
    (FUserPermissions.Values[IntToStr(ChatID)]='m');
end;

procedure TWebhookBot.EditOrSendMessage(const AMessage: String;
  AParseMode: TParseMode; ReplyMarkup: TReplyMarkup; TryEdit: Boolean);
var
  aCanEdit: Boolean;
begin
  { Variable aCanEdit is to avoid telegram API error "Bad Request: there is no text in the message to edit" }
  aCanEdit:=False;
  if Assigned(CurrentUpdate.CallbackQuery) then
    if Assigned(CurrentUpdate.CallbackQuery.Message) then
      aCanEdit:=CurrentUpdate.CallbackQuery.Message.Text<>EmptyStr;
  if not (TryEdit and aCanEdit)  then
    sendMessage(AMessage, AParseMode, True, ReplyMarkup)
  else
    editMessageText(AMessage, AParseMode, True, ReplyMarkup);
end;

function TWebhookBot.IsBanned(ChatID: Int64): Boolean;
begin
  Result:=FUserPermissions.Values[IntToStr(ChatID)]='b'
end;

function TWebhookBot.IsBotUser(ChatID: Int64): Boolean;
begin
  Result:=FUserPermissions.Values[IntToStr(ChatID)]='r'; // robot
end;

function TWebhookBot.IsSimpleUser(ChatID: Int64): Boolean;
begin
  Result:=(FUserPermissions.Values[IntToStr(ChatID)]<>'a') and
    (FUserPermissions.Values[IntToStr(ChatID)]<>'m');
end;

procedure TWebhookBot.SaveFeedback(AFrom: TTelegramUserObj; AMessage: String);
begin
  FBrookAction.SaveFeedback(AFrom, AMessage);
end;

procedure TWebhookBot.SetLanguage(const ALang: String);
begin
  inherited SetLanguage(ALang);
  LangTranslate(ALang);
end;

function TWebhookBot.answerCallbackQuery(const CallbackQueryId: String; const Text: String; ShowAlert: Boolean;
  const Url: String; CacheTime: Integer): Boolean;
begin
  Result:=inherited;
  FCallbackAnswered:=True;
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

procedure TWebhookAction.SetBot(AValue: TWebhookBot);
begin
// todo Does It need a code here? Can it be so?
end;

procedure TWebhookAction.SetLogDebug(AValue: Boolean);
begin
  if FLogDebug=AValue then Exit;
  FLogDebug:=AValue;
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

procedure TWebhookAction.BotMessageHandler(AMessage: TTelegramMessageObj);
begin
  if Assigned(FOnUpdateMessage) then
    FOnUpdateMessage(Self, AMessage);
end;

{ Sometimes, if the message is sent to the result of the CallBack call,
it is desirable not to create a new message and edit the message from which the call came }
procedure TWebhookAction.EditOrSendMessage(const AMessage: String;
  AParseMode: TParseMode; ReplyMarkup: TReplyMarkup; TryEdit: Boolean);
begin
  FBot.EditOrSendMessage(AMessage, AParseMode, ReplyMarkup, TryEdit);
end;

constructor TWebhookAction.Create;
begin
  inherited Create;
  FLogDebug:=False;
  FBot:=TWebhookBot.Create(FToken, Self);
end;

destructor TWebhookAction.Destroy;
begin
  FreeAndNil(FBot);
  inherited Destroy;
end;

procedure TWebhookAction.Post;
var
  Msg: String;
begin
  Msg:=HttpRequest.Content;
  if Bot.DoReceiveUpdate(Msg) then
  begin
    HttpResponse.ContentType:=BROOK_HTTP_CONTENT_TYPE_APP_JSON;
    Write(FBot.RequestBody);
  end;
end;

end.
