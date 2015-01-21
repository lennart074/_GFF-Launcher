unit obj_download;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsend, ssl_openssl, blcksock, zipper, appUtils,

  Dialogs, informations, launcher

  ;

type

  EDownloadError = class(Exception);

  { TDownloadObj }

  TDownloadObj = class(TObject)
  private
    unzipper: TUnZipper;
    pCurrDSize, pFullDSize: Real;
    Status: String;
    pMin, pMax, pStep: Integer;
    pPercentual: Integer;
    pDStatus: String;
    procedure SocketCallback(Sender: TObject; Reason: THookSocketReason;
      const Value: string);
    procedure Refresh;
    function DoDownload: Boolean;
    procedure Extract;
  public
    URL: String;
    Destination: String;
    ExtractTo: String;
    OnStatusUpdate:
    procedure(step, min, max: Integer; Msg: String) of Object;

    procedure Start;
  end;

implementation

{ TDownloadObj }

procedure TDownloadObj.SocketCallback(Sender: TObject; Reason: THookSocketReason;
  const Value: string);
var
  v, a, b: String;
begin
  if     //(Reason=hr_readcount) or
  (Reason = hr_writecount) or (Reason = hr_canread) then
    exit;
  if (Reason = HR_ReadCount) {or (Reason=HR_WriteCount) } then
  begin
    v := Value;
    pCurrDSize := (pCurrDSize + StrToFloat(v));
    a := FormatFloat('#.###', bTOmb(pCurrDSize));
    if (Pos(',', a) = 1) then
      a := '0' + a;
    b := FormatFloat('#.###', bTOmb(pFullDSize));
    if (Pos(',', b) = 1) then
      b := '0' + b;
    pDStatus := ((a) + 'MB of ' + (b) + 'MB');
    if (pCurrDSize <> 0) and (pFullDSize <> 0) then
    begin
      pPercentual := Round((pCurrDSize / pFullDSize) * 100);
    end
    else
    begin
      pPercentual := 0;
    end;
    pMax := 100;
    pMin := 0;
    Status := 'Downloading ' + ExtractFileName(Destination) + ' [' + pDStatus +
      '] (' + IntToStr(pPercentual) + '%)';
    pStep := pPercentual;
    Refresh;
    Exit;
  end;
  {
  v := getEnumName (typeinfo(THookSocketReason), integer(Reason)) + ' ' + Value;
  pCurrentAction := v;
  }
end;

procedure TDownloadObj.Refresh;
begin
  if Assigned(OnStatusUpdate) then
  begin
    OnStatusUpdate(pStep, pMin, pMax, Status);
  end;
end;

function TDownloadObj.DoDownload: Boolean;
Var
  HTTP: THTTPSend;
  s: String;
  i: Integer;
Begin
  pMax := 100;
  pMin := 0;
  HTTP := THTTPSend.Create;
  HTTP.Sock.OnStatus := @SocketCallback;
  if HTTP.HTTPMethod('HEAD', URL) = False then
  begin
    pFullDSize := (-1);
  end
  else
  begin
    // search for "Content-Length:" in header
    for i := 0 to http.Headers.Count - 1 do
    begin
      s := UpperCase(http.Headers[i]);
      if (pos('CONTENT-LENGTH:', s) > 0) then
        pFullDSize := StrToFloatDef(copy(s, pos(':', s) + 1, length(s)), 0);
    end;
  end;
  HTTP.Headers.Clear; //Clear Header for Download
  Status := '0MB/' + FloatToStr(pFullDSize) + 'MB';
  Refresh;

  If (HTTP.HTTPMethod('Get', URL)) Then
  Begin
    ForceDirectories(ExtractFileDir(Destination));
    HTTP.Document.SaveToFile(Destination);
    FreeAndNil(HTTP);
    Result := True;
  End
  Else
  Begin
    FreeAndNil(HTTP);
    Result := False;
  End;
end;

procedure TDownloadObj.Extract;
begin
try
Status := 'Download Finished: Creating Zipper';
Refresh;
unzipper := TUnZipper.Create;
unzipper.FileName := Destination;
unzipper.OutputPath := ExtractTo;

Status := 'Download Finished: Examining';
Refresh;
unzipper.Examine;

Status := 'Download Finished: unzipping...';
Refresh;
unzipper.UnZipAllFiles;
except
  ON E:Exception do begin
    Status := 'Exception has been raised while Unzipping the file:'+unzipper.FileName;
    Refresh;
    Log(etError,LineEnding+'Error occured while unzipping "'+unzipper.FileName+'"'+ LineEnding +
    'Class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding
    , LAUNCHER_MAXLOGLEVEL);
  end;
end;
end;

procedure TDownloadObj.Start;
begin

if not DoDownload then begin
   RAISE EDownloadError.Create('Unable to download '+URL);
end;
pStep := 0;
pMin := 0;
pMax := 1;
Refresh;
if ExtractTo <> '' then begin
   Extract;
end;

end;

end.
