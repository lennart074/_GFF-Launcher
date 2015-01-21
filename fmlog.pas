unit fmLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, eventlog, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls,
  launcher;

type

  TLogLevel = Integer;

  { Tfm_Log }

  Tfm_Log = class(TForm)
    Memo_Log: TMemo;
    Timer_refreshLog: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer_refreshLogTimer(Sender: TObject);
  private
    { private declarations }
    function getStart: String;
    procedure updateLog;
  public
    { public declarations }
    LogList: TStringList;
    Property LineBegin: String read getStart;
  public
    procedure DoLog(Event: TEventType; const MSG: String; level: TLogLevel = 1);
    procedure DoLogAdv(Event: TEventType; const MSG: String; level: TLogLevel);
  end;

var
  fm_Log: Tfm_Log;

const
  StrInfo    = '[INFO]';
  StrDebug   = '[DEBG]';
  StrError   = '[ERRO]';
  StrCustom  = '';
  StrWarning = '[WARN]';

implementation

{$R *.lfm}

{ Tfm_Log }

procedure Tfm_Log.DoLog(Event: TEventType; const MSG: String; level: TLogLevel = 1);
var
  LineStart: String;
begin
  if (Assigned(fm_Log)) then
  begin
    LineStart := LineBegin;

    case Event of
      etCustom: LineStart := LineStart + StrCustom;
      etWarning: LineStart := LineStart + StrWarning;
      etError: LineStart := LineStart + StrError;
      etDebug: LineStart := LineStart + StrDebug;
      etInfo: LineStart := LineStart + StrInfo;
      else
        LineStart := LineStart + '[NULL]';
    end;

    if (Assigned(fm_Log.LogList)) then
    begin
      fm_Log.LogList.Add(LineStart + MSG);
      fm_Log.updateLog;
    end;
  end
  else
  begin
    Application.CreateForm(Tfm_Log, fm_Log);
    DoLog(Event, MSG);
  end;
end;

procedure Tfm_Log.DoLogAdv(Event: TEventType; const MSG: String; level: TLogLevel);
var
  LineStart: String;
begin
  if (level >= LSettings.logLevel) then
  begin
    LineStart := fm_Log.LineBegin;
    case Event of
      etCustom: LineStart := LineStart + StrCustom;
      etWarning: LineStart := LineStart + StrWarning;
      etError: LineStart := LineStart + StrError;
      etDebug: LineStart := LineStart + StrDebug;
      etInfo: LineStart := LineStart + StrInfo;
      else
        LineStart := LineStart + '[NULL]';
    end;
    LogList.Add(LineStart + MSG);
    updateLog;
  end
  else
  begin
    //Message hidden
  end;
end;

procedure Tfm_Log.FormCreate(Sender: TObject);
begin
  LogList := TStringList.Create;
  LogList.Clear;
  Memo_Log.Clear;
end;

procedure Tfm_Log.Button1Click(Sender: TObject);
begin
  ShowMessage(IntToStr(Memo_Log.Lines.Count));
end;

procedure Tfm_Log.Timer_refreshLogTimer(Sender: TObject);
begin
  if (Memo_Log.Lines.Count <> LogList.Count) then
    Memo_Log.Lines := LogList;
end;

function Tfm_Log.getStart: String;
begin
  Result := '[' + DateToStr(now) + '|' + TimeToStr(now) + ']';
end;

procedure Tfm_Log.updateLog;
begin
  Memo_Log.Lines := LogList;
end;

end.
