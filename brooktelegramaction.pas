unit brooktelegramaction;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, tgtypes, tgsendertypes, sysutils, classes, tgstatlog, eventlog, ghashmap, fpjson, tgbot
  ;

type

  TWebhookBot = class;

  { TWebhookAction }

  TWebhookAction = class(TBrookAction)
  private
    FBot: TWebhookBot;
  protected
    { Use Bot.OnReceiveMessage or Bot.DoReceiveMessageUpdate instead }
    procedure BotMessageHandler({%H-}AMessage: TTelegramMessageObj); virtual; deprecated;
  public
    class function AppDir: String;
    constructor Create; override;
    destructor Destroy; override;
    procedure Post; override;
    property Bot: TWebhookBot read FBot;
  end;

  { TWebhookBot }
  TWebhookBot = class(TTelegramBot)
  private
    FBrookAction: TWebhookAction;
    function DoReceiveUpdate(const aUpdate: String): Boolean; overload;
  protected
    property BrookAction: TWebhookAction read FBrookAction;
  public
    constructor Create(const AToken: String; AWebhookAction: TWebhookAction);
  end;

implementation
{ Please define ni18n (No i18n) for excluding translate unit from uses and exclude i18n support }
uses jsonparser, BrookHttpConsts, strutils, BrookApplication, jsonscanner, LazUTF8, tgutils, dateutils;

{ TWebhookBot }

function TWebhookBot.DoReceiveUpdate(const aUpdate: String): Boolean;
var
  aJSON: TJSONObject;
  aUpdateObject: TTelegramUpdateObj;
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
      aUpdateObject:=TTelegramUpdateObj.CreateFromJSONObject(aJSON) as TTelegramUpdateObj;
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
  DoReceiveUpdate(aUpdateObject);
  Result:=True;
end;

constructor TWebhookBot.Create(const AToken: String;
  AWebhookAction: TWebhookAction);
begin
  inherited Create(AToken);
  FBrookAction:=AWebhookAction;
end;

  { TWebhookAction }

procedure TWebhookAction.BotMessageHandler(AMessage: TTelegramMessageObj);
begin
  // nothing
end;

class function TWebhookAction.AppDir: String;
begin
  Result:=IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
end;

constructor TWebhookAction.Create;
begin
  inherited Create;
  FBot:=TWebhookBot.Create(EmptyStr, Self);
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
