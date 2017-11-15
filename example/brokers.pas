unit Brokers;

{$mode objfpc}{$H+}

interface

uses
  BrookFCLFCGIBroker, eventlog;

var
  BLogger: TEventLog;

implementation

uses
  sysutils;

initialization
  BLogger:=TEventLog.Create(nil);
  BLogger.LogType:=ltFile;
  BLogger.FileName:=ChangeFileExt(ParamStr(0),'.log');
  BLogger.Active:=True;
  BLogger.AppendContent:=True;
  BLogger.Log('Log is opened');

finalization
  BLogger.Log('Log is closed');
  BLogger.Free;


end.
