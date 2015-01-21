unit remProfiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, obj_remProfiles, obj_download, fpjson, jsonUtils, jsonparser, appUtils, contnrs, downloads;

type

  //########################################################################################

  { TRemoteIndex }

  TRemoteIndex = class(TObject)
  public
    CONSTRUCTOR Create(JSONString: TJSONStringType);
    procedure Parse;
    DESTRUCTOR Destroy;
  private
    pIndexString: TJSONStringType;
    P: TJSONParser;
    D: TJSONData;
    Ar: TJSONArray;
  public
    prf: TRemProfileCollection;
  end;

  //########################################################################################

  { TRemoteFile }

  TRemoteFile = class(TObject)
  public
    fileType: String;
    fileName: String;
    fileVers: String;
    fileDest: String;
    fileURL: String;
    ExtractTo: String;
    DoExtract: Boolean;
  end;

  //########################################################################################

  { TRemFileList }

  TRemFileList = class(TObjectList)
  private
    function GetObject(I: Integer): TRemoteFile;
    procedure SetObject(I: Integer; Obj: TRemoteFile);
  public
    property objects[I: Integer]: TRemoteFile read GetObject write SetObject;
  end;

  //########################################################################################

  { TRemoteProfileIndex }

  TRemoteProfileIndex = class(TObject)
  public
    CONSTRUCTOR Create(JSONString: TJSONStringType; Name: String);
    procedure Parse;
    function Count: Integer;
    DESTRUCTOR Destroy;
  private
    function GetFile(Index: Integer): TRemoteFile;
  private
    pName: String;
    pIndexString: TJSONStringType;
    P: TJSONParser; //json-level-1
    D: TJSONData; //json-level-1
    JSONProfileObj: TJSONObject; //json-level-2 (Profile)
    Ar: TJSONArray; //json-level-3 (array: files)
    FILES: TRemFileList;
  public
    Property member[Index: Integer]: TRemoteFile read GetFile;
  end;

  //########################################################################################

  TSyncProc = procedure(Step, min, max: Integer; CurrAct: String) of Object;

  { TRemoteProfileInstaller }

  TRemoteProfileInstaller = class(TThread)
  protected
    procedure Execute; override;
  public
    CONSTRUCTOR Create;
  private
    procedure SyncStat;
    procedure SyncStatus;
    procedure GetIndex;
    procedure OpenIndex;
    procedure ParseIndex;
    procedure GetFiles;
    procedure RefrStat;
  private
    ProfIndexPath: String;
    JSONCont: TJSONStringType;
    ProfIndex: TRemoteProfileIndex;
    DThread: TDownloadThread;
  public
    min, max, step: Integer;
    Status: String;
    Profile: TRemProfileObj;
    SyncProc: TSyncProc;
    OnFinish : Procedure of Object;
  end;

//########################################################################################
//########################################################################################

implementation

uses launcher, fmLogin, fmProfiles;

{ TRemoteProfileInstaller }

procedure TRemoteProfileInstaller.Execute;
begin
  max := 5;
  min := 0;
  step := 1;
  Status := 'Initializing...';
  SyncStatus;
  ProfIndexPath := (PC_CommonPaths.pathsBN['temp'].path + '/' +
    ExtractFileName(StringReplace(Profile.indexURL, '%S%', '/',
    [rfIgnoreCase, rfReplaceAll])));

  max := 5;
  min := 0;
  step := 2;
  Status := 'Retreving Index';
  SyncStatus;
  GetIndex;

  max := 5;
  min := 0;
  step := 3;
  Status := 'Opening Index';
  SyncStatus;
  OpenIndex;

  max := 5;
  min := 0;
  step := 4;
  Status := 'Parsing Index';
  SyncStatus;
  ParseIndex;

  max := 5;
  min := 0;
  step := 5;
  Status := 'Downloading Files';
  SyncStatus;
  GetFiles;

  if Assigned(OnFinish) then
     OnFinish;
end;

CONSTRUCTOR TRemoteProfileInstaller.Create;
begin
  inherited Create(True);
end;

procedure TRemoteProfileInstaller.SyncStat;
begin
  if Assigned(SyncProc) then
  begin
    SyncProc(Step, min, max, Status);
  end;
end;

procedure TRemoteProfileInstaller.SyncStatus;
begin
  Synchronize(@SyncStat);
end;

procedure TRemoteProfileInstaller.GetIndex;
var
  TempStr: String;
begin
  DThread := TDownLoadThread.Create(True);

  TempStr := StringReplace(Profile.indexURL, '%S%', '/', [rfIgnoreCase, rfReplaceAll]);
  DThread.URL := TempStr;
  TempStr := ProfIndexPath;
  TempStr := StringReplace(TempStr, '%profile_home%', LUser.ProfilePath +
    '/' + Profile.ProfName, [rfIgnoreCase, rfReplaceAll]);
  TempStr := StringReplace(TempStr, '%S%', '/', [rfIgnoreCase, rfReplaceAll]);
  DThread.Destination := TempStr;

  min := 0;
  step := 0;
  max := 100;


  DThread.Start;

  while (DThread.HasTerminated = False) and (Terminated = False) do
  begin
    Status := 'Retreving Profile Index (' + IntToStr(DThread.Percentual) + '%)';
    step := DThread.Percentual;
    SyncStatus;
    Sleep(40);
  end;

  FreeAndNil(DThread);
end;

procedure TRemoteProfileInstaller.OpenIndex;
var
  iContent: TStringList;
begin
  iContent := TStringList.Create;
  iContent.LoadFromFile(ProfIndexPath);

  JSONCont := iContent.Text;

  FreeAndNil(iContent);
end;

procedure TRemoteProfileInstaller.ParseIndex;
begin
  ProfIndex := TRemoteProfileIndex.Create(JSONCont, Profile.ProfName);
  ProfIndex.Parse;
end;

procedure TRemoteProfileInstaller.GetFiles;
var
  I: Integer;
  tempStr: String;
  stdRep: TReplaceFlags;
  Download : TDownloadObj;
begin
  stdRep := [rfIgnoreCase, rfReplaceAll];

  for I := 0 to (ProfIndex.Count - 1) do
  begin
    Status := 'Downloading Files';
    max := ProfIndex.Count;
    min := 1;
    step := I;
    SyncStat;

    Download := TDownloadObj.Create;

    tempStr := ProfIndex.member[I].fileURL;
    Download.URL := StringReplace(tempStr, '%S%', '/', stdRep);

    tempStr := ProfIndex.member[I].fileDest;
    tempStr := StringReplace(tempStr, '%profile_home%', LUser.ProfilePath +
      '/' + Profile.ProfName, stdRep);
    Download.Destination := StringReplace(tempStr, '%S%', '/', stdRep);

    if (ProfIndex.member[I].DoExtract = True) then
    begin
      tempStr := StringReplace(ProfIndex.member[I].ExtractTo,
        '%profile_home%', LUser.ProfilePath + '/' + Profile.ProfName, stdRep);
      tempStr := StringReplace(tempStr, '%S%', '/', stdRep);
      Download.ExtractTo := tempStr;
    end;

    max := 100;
    min := 0;
    step := 0;
    Download.OnStatusUpdate := SyncProc;

    Download.Start;

    {while (DThread.HasTerminated = False) and (Terminated = False) do
    begin
      SyncStatus;
      Sleep(100);
    end;}

    Status := 'Finished';
    SyncStatus;

    FreeAndNil(Download);

  end;

end;

procedure TRemoteProfileInstaller.RefrStat;
begin
  step := DThread.Percentual;
  Status := ' (Download:' +IntToStr(DThread.Percentual) + '%) | '+
  'Status: ' + DThread.Downloadstatus;
end;

//########################################################################################

{ TRemFileList }

function TRemFileList.GetObject(I: Integer): TRemoteFile;
begin
  Result := TRemoteFile(Items[I]);
end;

procedure TRemFileList.SetObject(I: Integer; Obj: TRemoteFile);
begin
  Items[I] := Obj;
end;

//########################################################################################

{ TRemoteProfileIndex }

CONSTRUCTOR TRemoteProfileIndex.Create(JSONString: TJSONStringType; Name: String);
begin
  pName := Name;
  pIndexString := JSONString;
  FILES := TRemFileList.Create(True);
  inherited Create;
end;

procedure TRemoteProfileIndex.Parse;
var
  I: Integer;
  lObj: TRemoteFile;
  jObj: TJSONObject;
begin
  P := TJSONParser.Create(pIndexString);
  D := P.Parse;
  JSONProfileObj := TJSONObject(TJSONObject(D).Extract(pName));
  Ar := TJSONArray(JSONProfileObj.Extract('files'));
  Log(etDebug,'Got FileArray:'+LineEnding+JSONToString(TJSONData(Ar)),1);

  for I := 0 to (Ar.Count - 1) do
  begin

    jObj := TJSONObject(Ar.Items[I]);

    lObj := TRemoteFile.Create;
    lObj.fileType := jObj.Get('type');
    lObj.fileName := jObj.Get('name');
    lObj.fileVers := jObj.Get('version');
    lObj.fileDest := jObj.Get('dest');
    lObj.fileURL := jObj.Get('url');
    lObj.ExtractTo := jObj.Get('ExtractTo');

    if (lObj.fileType = 'ARCHIVE') then
    begin
      lObj.DoExtract := True;
    end;

    FILES.Add(lObj);

    lObj := nil; //Do not Free : Is kept in List

    jObj.Free;
    jObj := nil;
  end;
end;

function TRemoteProfileIndex.Count: Integer;
begin
  Result := FILES.Count;
end;

DESTRUCTOR TRemoteProfileIndex.Destroy;
var
  I: Integer;
begin
  FILES.OwnsObjects := True;
  for I := FILES.Count - 1 downto 0 do
  begin
    FILES.Delete(I);
  end;
  FreeAndNil(FILES);
  FreeAndNil(P);
  FreeAndNil(D);
  FreeAndNil(JSONProfileObj);
  FreeAndNil(Ar);
  inherited Destroy;
end;

function TRemoteProfileIndex.GetFile(Index: Integer): TRemoteFile;
begin
  Result := FILES.GetObject(Index);
end;

//########################################################################################

{ TRemoteIndex }

CONSTRUCTOR TRemoteIndex.Create(JSONString: TJSONStringType);
begin
  pIndexString := JSONString;
  prf := TRemProfileCollection.Create;
  inherited Create;
end;

procedure TRemoteIndex.Parse;
var
  I: Integer;
  jObj: TJSONObject;
  pObj: TRemProfileObj;
begin

  P := TJSONParser.Create(pIndexString);
  D := P.Parse;
  Ar := TJSONArray(TJSONObject(D).Extract('profiles'));

  for I := 0 to (TJSONObject(Ar).Count - 1) do
  begin

    jObj := TJSONObject(Ar.Items[I]);

    pObj := TRemProfileObj.Create;
    pObj.ProfName := jObj.Get('name');
    pObj.version := jObj.Get('version');
    pObj.vCapt := jObj.Get('vCapt');
    pObj.branch := jObj.Get('branch');
    pObj.ProfOwner := jObj.Get('owner');
    pObj.indexURL := jObj.Get('indexURL');
    pObj.comment := jObj.Get('comment');
    pObj.Enabled := StrToBoolean(jObj.Get('enabled'), False);
    pObj.disableReason := jObj.Get('disableReason');
    pObj.minLauncherVer := jObj.Get('minLauncherVersion');

    prf.Add(pObj);

    pObj := nil;
    jObj.Free;
    jObj := nil;
  end;

end;

DESTRUCTOR TRemoteIndex.Destroy;
begin
  P.Free;
  D.Free;
  Ar.Free;
  inherited Destroy;
end;

//########################################################################################

end.
