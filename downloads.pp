unit downloads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ComCtrls, ExtCtrls, BGRAFlashProgressBar,
  //{ lNet for TCP connections } lNetComponents, lNetSSL,
  { HTTP } httpsend, blcksock,
  //{ INIFiles } IniFiles,
  { Archives } zipper,
  { Utils } appUtils;

type

  EDownloadThreadError = class(Exception);

  { TDownLoadThread }

  TDownLoadThread = class(TThread)
  protected
    Procedure Execute; override;
    procedure DoTerminate; override;
  private
    ppCurrentAction: String;
    function DoDownload: Boolean;
    procedure OnTerminate;
    procedure SetAction(AValue: String);
    procedure setExtraction(dir: String);
    procedure ShowStatus;
    procedure SockCallBack(Sender: TObject; Reason: THookSocketReason;
      const Value: string);
    function pTerminated: Boolean;
    //destructor Destroy;
  private
    ACurrentAction: String;
    pExtractDir: String;
    pURL: String;
    pDest: String;
    pDoExtract: Boolean;
    pHasErrored: Boolean;
    pErrorMsg: String;
    pException: Exception;
    pCurrDSize: Real;
    pDSize: Real;
    pDStatus: String;
    pPercentual: Integer;

  public
    statusLEdit: TLabeledEdit;
    statusFBar: TBGRAFlashProgressBar;
    statusBar: TProgressBar;
    OnStatus:
    procedure of Object;
    property URL: String read pURL write pURL;
    property Destination: String read pDest write pDest;
    property ExtractTo: String read pExtractDir write setExtraction;
    property HasTerminated: Boolean read pTerminated;
    property Status: String read ACurrentAction;
    property pCurrentAction: String read ACurrentAction write SetAction;
    Property HasErrored: Boolean read pHasErrored;
    Property ErrorMsg: String read pErrorMsg;
    Property EX: Exception read pException;
    Property Downloadstatus: String read pDStatus;
    Property CurrDSize: Real read pCurrDSize;
    Property FileSize: Real read pDSize;
    Property Percentual: Integer read pPercentual;
  end;

implementation

{ TDownLoadThread }

Procedure TDownLoadThread.Execute;
var
  unzipper: TUnZipper;
begin
  try
    inherited Start;
    pPercentual := 0;
    pDStatus := '? of ?';

    FreeOnTerminate := False;
    pDest := StringReplace(pDest, '%S%', '/', [rfIgnoreCase, rfReplaceAll]);

    if (length(pDest) > 2) and (Length(pURL) > 2) then
    begin
      pCurrentAction := 'Force Directories';
      ForceDirectories(ExtractFileDir(pDest));
      pCurrentAction := 'Downloading:' + ExtractFileName(pDest);

      //Decode URL
      pURL := StringReplace(pURL, '%S%', '/', [rfIgnoreCase, rfReplaceAll]);

      if not DoDownload then
      begin
        RAISE EDownloadThreadError.Create('Unable to download:' + (pURL));
      end;
      if (pDoExtract) then
      begin
        pDStatus := 'Download Finished: Unzipping...';
        if Assigned(OnStatus) then
          Synchronize(OnStatus);

        pCurrentAction := 'Creating zipper';
        pDStatus := 'Download Finished: ' + 'Creating zipper';
        if Assigned(OnStatus) then
          Synchronize(OnStatus);

        try
          unzipper := TUnZipper.Create;
          unzipper.FileName := pDest;
          unzipper.OutputPath := pExtractDir;

          {pCurrentAction := 'Examining';
          pDStatus := 'Download Finished: ' + 'Examining';
          if Assigned(OnStatus) then
            Synchronize(OnStatus);
          unzipper.Examine;}

          pCurrentAction := 'Unzipping...';
          pDStatus := 'Download Finished: ' + 'Unzipping...';
          if Assigned(OnStatus) then
            Synchronize(OnStatus);
          unzipper.UnZipAllFiles;

          pCurrentAction := 'Unzipping finished!';
          pDStatus := 'Download Finished: ' + 'Unzipping finished!';
          if Assigned(OnStatus) then
            Synchronize(OnStatus);
        except
          on E: Exception do
          begin
            pDStatus := 'Excception: ' + E.ClassName + '||' + E.Message;
            if Assigned(OnStatus) then
              Synchronize(OnStatus);
            RAISE EDownloadThreadError.Create('ZipException:' +
              LineEnding + E.ClassName + LineEnding + E.Message);
          end;
        end;
        if (Assigned(unzipper)) then
        begin
          FreeAndNil(unzipper);
        end;
        pDStatus := 'Done';
        if Assigned(OnStatus) then
          Synchronize(OnStatus);
      end;
    end
    else
    begin
      RAISE EDownloadThreadError.Create('Either pDest and/or URL is nil-->No download!');
    end;

    pDStatus := 'Threadfinished';
    if Assigned(OnStatus) then
      Synchronize(OnStatus);
    pCurrentAction := 'Thread finished!';
  except
    ON E: Exception DO
    begin
      pHasErrored := True;
      pErrorMsg := E.Message;
      pException := E;
    end;
  end;
end;

procedure TDownLoadThread.DoTerminate;
begin
  FreeOnTerminate := False;
  Terminate;
  inherited DoTerminate;
end;

function TDownLoadThread.DoDownload: Boolean;
Var
  HTTP: THTTPSend;
  s: String;
  i: Integer;
Begin
  HTTP := THTTPSend.Create;
  HTTP.Sock.OnStatus := @SockCallBack;
  if HTTP.HTTPMethod('HEAD', pURL) = False then
  begin
    pDSize := (-1);
  end
  else
  begin
    // search for "Content-Length:" in header
    for i := 0 to http.Headers.Count - 1 do
    begin
      s := UpperCase(http.Headers[i]);
      if pos('CONTENT-LENGTH:', s) > 0 then
        pDSize := StrToFloatDef(copy(s, pos(':', s) + 1, length(s)), 0);
    end;
  end;
  HTTP.Headers.SaveToFile(ExtractFileDir(Destination) + '/headers.temp');
  HTTP.Headers.Clear;
  pDStatus := '0MB/' + FloatToStr(pDSize) + 'MB';

  If (HTTP.HTTPMethod('Get', pURL)) Then
  Begin
    ForceDirectories(ExtractFileDir(pDest));
    HTTP.Document.SaveToFile(pDest);
    FreeAndNil(HTTP);
    Result := True;
  End
  Else
  Begin
    FreeAndNil(HTTP);
    Result := False;
  End;
end;

procedure TDownLoadThread.setExtraction(dir: String);
begin
  if (dir = 'false') or (Length(dir) < 3) then
  begin
    pDoExtract := False;
    pExtractDir := '';
  end
  else
  begin
    ForceDirectories(dir);
    pExtractDir := dir;
    pDoExtract := True;
  end;
end;

procedure TDownLoadThread.SockCallBack(Sender: TObject; Reason: THookSocketReason;
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
    b := FormatFloat('#.###', bTOmb(pDSize));
    if (Pos(',', b) = 1) then
      b := '0' + b;
    pDStatus := ((a) + 'MB of ' + (b) + 'MB');
    if (pDSize <> 0) then
    begin
      pPercentual := Round((pCurrDSize / pDSize) * 100);
    end
    else
    begin
      pPercentual := 0;
    end;
    if Assigned(OnStatus) then
      Synchronize(OnStatus);
    Exit;
  end;
  {
  v := getEnumName (typeinfo(THookSocketReason), integer(Reason)) + ' ' + Value;
  pCurrentAction := v;
  }
end;

function TDownLoadThread.pTerminated: Boolean;
begin
  Result := Terminated;
end;

procedure TDownLoadThread.OnTerminate;
begin
  If (FileExists(ExtractFileDir(Destination) + '/headers.temp')) then
    DeleteFile(ExtractFileDir(Destination) + '/headers.temp');

  FreeOnTerminate := False;
end;

procedure TDownLoadThread.ShowStatus;
begin
  if Assigned(statusBar) then
    statusBar.Visible := True;
  if Assigned(statusLEdit) then
  begin
    statusLEdit.Text := pCurrentAction + ' (' + IntToStr(Percentual) + '%) ';
  end;
  if Assigned(statusFBar) then
  begin
    if Percentual <> 0 then
    begin
      if Assigned(statusBar) then
        statusBar.Visible := False;
      statusFBar.MinValue := 0;
      statusFBar.MaxValue := 100;
      statusFBar.Value := Percentual;
    end;
  end;
end;

procedure TDownLoadThread.SetAction(AValue: String);
begin
  if ppCurrentAction = AValue then
    Exit;
  ppCurrentAction := AValue;

  Synchronize(@ShowStatus);
end;

end.
