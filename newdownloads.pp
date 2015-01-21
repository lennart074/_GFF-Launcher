unit newDownloads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  httpsend, blcksock,
  zipper,
  ComCtrls,
  ExtCtrls,
  BGRAFlashProgressBar,
  Forms,
  appUtils;

type

  EDownloadError = class(Exception);

  TProcedure = procedure;
  TProcedureOfObj = procedure of Object;
  TProcedureOfObj_ws = procedure (Sender : TObject) of object;

  { TNewDownloadThread }

  TNewDownloadThread = class(TThread)
  public
    CONSTRUCTOR Create;
  private
    pStatus: String;
    pAllowSwitch: Boolean;
    pStatusEdit: TLabeledEdit;
    pstatusFBar: TBGRAFlashProgressBar;
    pStatusBar: TProgressBar;
    pOwnerForm: TForm;
    maxVal: Integer;
    minVal: Integer;
    pFullSize: Real;
    pURL: String;
    pDest: String;
    pExtractFile: String;
    pExtractDir: String;
    pdoExtract: Boolean;
    pCurrSize: Real;
    pRealPerc: Real;
    pDStatus: String;
    pOnFin : TProcedureOfObj;
    procedure SockCallBack(Sender: TObject; Reason: THookSocketReason;
      const Value: string);
  private
      function GetTerm: Boolean;
    procedure SyncStatus;
    function pPercentual: Integer;
    procedure SetExtraction(TDir: String);
    procedure DoDownload;
    procedure DoExtract;
    procedure Finish;
  protected
    procedure Execute; override;
  public
    Property OnFinish : TProcedureOfObj read pOnFin write pOnFin;
    Property Status: String read pStatus;
    Property Percentual: Integer read pPercentual;
    Property statusEdit: TLabeledEdit read pStatusEdit write pStatusEdit;
    Property statusFBar: TBGRAFlashProgressBar read pstatusFBar write pStatusFBar;
    Property statusBar: TProgressBar read pStatusBar write pStatusBar;
    Property AllowSwitch: Boolean read pAllowSwitch write pAllowSwitch;
    Property OwnerForm: TForm read pOwnerForm write pOwnerForm;
    Property URL: String read pURL write pURL;
    Property Dest: String read pDest write pDest;
    Property ExtractTo: String read pExtractDir write SetExtraction;
    Property HasTerminated : Boolean read GetTerm;
  end;

implementation

{ TNewDownloadThread }

CONSTRUCTOR TNewDownloadThread.Create;
begin
  inherited Create(True);
  AllowSwitch := True;
end;

procedure TNewDownloadThread.SyncStatus;
begin

  if (Percentual <> 0) and (AllowSwitch = True) then
    if Assigned(StatusBar) then
      StatusBar.Visible := False;

  if (Percentual = 0) and (AllowSwitch = True) then
    if Assigned(StatusBar) then
      StatusBar.Visible := True;

  if Assigned(statusFBar) then
  begin
    statusFBar.MaxValue := maxVal;
    statusFBar.MinValue := minVal;
    statusFBar.Value := Percentual;
  end;

  if Assigned(statusEdit) then
    statusEdit.Text := Status;

  if Assigned(pOwnerForm) then
  begin
    pOwnerForm.Update;
  end;
end;

function TNewDownloadThread.pPercentual: Integer;
begin
  if (pFullSize <> 0) and (pCurrSize <> 0) then
  begin
    Result := Round(pCurrSize / pFullSize);
  end
  else
  begin
    Result := 0;
  end;
end;

procedure TNewDownloadThread.SetExtraction(TDir: String);
begin
  ForceDirectories(TDir);
  pExtractDir := TDir;
  pExtractFile := pDest;
  pdoExtract := True;
end;

procedure TNewDownloadThread.Execute;
begin
  pStatus := 'Initializing download';
  Synchronize(@SyncStatus);

  if (pURL = '') or (pDest = '') then
    Exit;
  DoDownload;

  pStatus := 'Checking Extraction';
  Synchronize(@SyncStatus);

  if (pdoExtract = True) then
     DoExtract;

  pStatus := 'Finished';
  Synchronize(@SyncStatus);

  Synchronize(@Finish);
end;

procedure TNewDownloadThread.SockCallBack(Sender: TObject;
  Reason: THookSocketReason; const Value: string);
var
  v, a, b: String;
begin
  if     //(Reason=hr_readcount) or
  (Reason = hr_writecount) or (Reason = hr_canread) then
    exit;
  if (Reason = HR_ReadCount) {or (Reason=HR_WriteCount) } then
  begin
    v := Value;
    pCurrSize := (pCurrSize + StrToFloat(v));
    a := FormatFloat('#.###', bTOmb(pCurrSize));
    if (Pos(',', a) = 1) then
      a := '0' + a;
    b := FormatFloat('#.###', bTOmb(pFullSize));
    if (Pos(',', b) = 1) then
      b := '0' + b;
    pDStatus := ((a) + 'MB of ' + (b) + 'MB');

    pStatus := 'Downloading ' + ExtractFileName(Dest) + ' ' + pDStatus + ' (' +
      IntToStr(pPercentual) + '%)';
    Synchronize(@SyncStatus);
    Exit;
  end;
  {
  v := getEnumName (typeinfo(THookSocketReason), integer(Reason)) + ' ' + Value;
  pCurrentAction := v;
  }
end;

function TNewDownloadThread.GetTerm: Boolean;
begin
    Result := Terminated;
end;

procedure TNewDownloadThread.DoDownload;
VAR
  HTTP: THTTPSend;
  I: Integer;
  s: String;
begin
  HTTP := THTTPSend.Create;
  HTTP.Sock.OnStatus := @SockCallBack;
  if HTTP.HTTPMethod('HEAD', pURL) = False then
  begin
    pFullSize := (-1);
  end
  else
  begin
    // search for "Content-Length:" in header
    for i := 0 to http.Headers.Count - 1 do
    begin
      s := UpperCase(http.Headers[i]);
      if pos('CONTENT-LENGTH:', s) > 0 then
        pFullSize := StrToFloatDef(copy(s, pos(':', s) + 1, length(s)), 0);
    end;
  end;
  HTTP.Headers.Clear;
  HTTP.HTTPMethod('Get', pURL);
  if HTTP.Document.Size = pFullSize then
  begin
    HTTP.Document.SaveToFile(pDest);
  end
  else
  begin
    RAISE EDownloadError.Create('Unable to download "' + pURL + '"');
  end;
end;

procedure TNewDownloadThread.DoExtract;
var
  unzipper: TUnZipper;
begin
  try
    pStatus := 'Creating unzipper';
    Synchronize(@SyncStatus);

    unzipper := TUnZipper.Create;
    unzipper.FileName := pExtractFile;
    unzipper.OutputPath := pExtractDir;

    pStatus := 'Unzipping '+ExtractFileName(unzipper.FileName);
    Synchronize(@SyncStatus);

    unzipper.Examine;
    unzipper.UnZipAllFiles;
  finally
    FreeAndNil(unzipper);
  end;
end;

procedure TNewDownloadThread.Finish;
begin
  if Assigned(pOnFin) then begin
    pOnFin;
  end;
end;

end.
