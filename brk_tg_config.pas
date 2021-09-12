unit brk_tg_config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson
  ;

type

  { TTelegramPaymentsConf }

  TTelegramPaymentsConf = class
  private
    FCurrency: String;
    FDisabled: Boolean;
    FPrice: Integer;
    FToken: String;
  published
    property Token: String read FToken write FToken;
    property Price: Integer read FPrice write FPrice;
    property Disabled: Boolean read FDisabled write FDisabled;
    property Currency: String read FCurrency write FCurrency;
  end;

  { TTelegramConf }

  TTelegramConf = class
  private
    FPayments: TTelegramPaymentsConf;
    FToken: String;
    FUsername: String;
    FUsers: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Token: String read FToken write FToken;
    property UserName: String read FUsername write FUserName;
    property Payments: TTelegramPaymentsConf read FPayments write FPayments;
    property Users: TStrings read FUsers write FUsers;
  end;

  { TDebugInfo }

  TDebugInfo = class
  private
    FEnabled: Boolean;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  { TLoggerConfig }

  TLoggerConf = class
  private
    FActive: Boolean;
    FFilename: String;
  published
    property Active: Boolean read FActive write FActive;
    property FileName: String read FFilename write FFilename;
  end;

  { TDBConf }

  TDBConf = class
  private
    FDatabase: String;
    FDriver: String;
    FHost: String;
    FLoggerConf: TLoggerConf;
    FPassword: String;
    FUser: String;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Driver: String read FDriver write FDriver;
    property Database: String read FDatabase write FDatabase;
    property User: String read FUser write FUser;
    property Password: String read FPassword write FPassword;
    property Host: String read FHost write FHost;
    property Logger: TLoggerConf read FLoggerConf write FLoggerConf;
  end;

  { TAppConf }

  TAppConf = class
  private
    FTitle: String;
  published
    property Title: String read FTitle write FTitle;
  end;

  { TBotConf }

  TBotConf = class
  private
    FAdverts: TStrings;
    FDebug: Boolean;
    FFreeLimit: Integer;
    FTelegram: TTelegramConf;
    procedure SetAdverts(AValue: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Telegram: TTelegramConf read FTelegram write FTelegram;
    property Adverts: TStrings read FAdverts write SetAdverts;
    property Debug: Boolean read FDebug write FDebug;
    property FreeLimit: Integer read FFreeLimit write FFreeLimit;
  end;

procedure LoadFromJSON(AObject: TObject; const AFileName: String);
procedure SaveToJSON(AObject: TObject; const AFileName: String);
function StringToJSONObject(const AString: String): TJSONObject;

implementation

uses
  jsonparser, fpjsonrtti, jsonscanner
  ;

var
  MyCriticalSection: TRTLCriticalSection;

procedure LoadFromJSON(AObject: TObject; const AFileName: String);
var
  ADeStreamer: TJSONDeStreamer;
  AJSON: TStringList;
begin
  if not FileExists(AFileName) then
    Exit;
  ADeStreamer:=TJSONDeStreamer.Create(nil);
  try
    AJSON:=TStringList.Create;
    try
      AJSON.LoadFromFile(AFileName);
      try
        ADeStreamer.JSONToObject(AJSON.Text, AObject);
      except
      end;
    finally
      AJSON.Free;
    end;
  finally
    ADeStreamer.Free;
  end;
end;

function StringToJSONObject(const AString: String): TJSONObject;
var
  lParser: TJSONParser;
begin
  Result := nil;
  if AString<>EmptyStr then
  begin
    lParser := TJSONParser.Create(AString, DefaultOptions);
    try
      try
        Result := lParser.Parse as TJSONObject
      except
        Result:=nil;
      end;
    finally
      lParser.Free;
    end;
  end;
end;

procedure SaveToJSON(AObject: TObject; const AFileName: String);
var
  AStreamer: TJSONStreamer;
  AJSON: TStringList;
  AJSONObject: TJSONObject;
begin
  AStreamer:=TJSONStreamer.Create(nil);
  AStreamer.Options:=[jsoTStringsAsArray];
  try
    AJSON:=TStringList.Create;
    try
      try
        AJSONObject:=AStreamer.ObjectToJSON(AObject);
        try
          AJSON.Text:=AJSONObject.FormatJSON();
          AJSON.SaveToFile(AFileName);
        finally
          AJSONObject.Free;
        end;
      except
      end;
    finally
      AJSON.Free;
    end;
  finally
    AStreamer.Free;
  end;
end;

{ TDBConf }

constructor TDBConf.Create;
begin
  inherited;
  FLoggerConf:=TLoggerConf.Create;
end;

destructor TDBConf.Destroy;
begin
  FLoggerConf.Free;
  inherited Destroy;
end;

{ TTelegramConf }

constructor TTelegramConf.Create;
begin
  FPayments:=TTelegramPaymentsConf.Create;
  FUsers:=TStringList.Create;
end;

destructor TTelegramConf.Destroy;
begin
  FUsers.Free;
  FPayments.Free;
  inherited Destroy;
end;

{ TBotConf }

procedure TBotConf.SetAdverts(AValue: TStrings);
begin
  if FAdverts=AValue then Exit;
  EnterCriticalsection(MyCriticalSection{%H-});
  FAdverts.Assign(AValue);
  LeaveCriticalsection(MyCriticalSection);
end;

constructor TBotConf.Create;
begin
  FTelegram:=TTelegramConf.Create;
  FAdverts:=TStringList.Create;
end;

destructor TBotConf.Destroy;
begin
  FAdverts.Free;
  FTelegram.Free;
  inherited Destroy;
end;

initialization

  InitCriticalSection(MyCriticalSection{%H-});

finalization

  DoneCriticalsection(MyCriticalSection);

end.

