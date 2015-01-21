unit Minecraft_Setup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, obj_settings, obj_profiles, launcher,
  downloads,
  fpjson, jsonparser, strutils, process, appUtils, FileUtil;

type

  { TMcSetup }

  TMcSetup = class(TThread)
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  private
    pVersion, pVanillaVer, pAssets, pMainClass, pMcArgs, pGameAssets: String;
    pStatus: String;
    pProfile: TProfileObj;
    pJarPath, pRelJarPath: String;
    pIndexPath: String;
    pAssetPath: String;
    WSet: TWebSettings;
    MSet: TProgramSettings;
    pprofileINI: TIniFile;
    pMaxPerc, pMinPerc: Integer;
    pPerc: Integer;
    pLibs: TStringList;
    pForceDownload, pExcludeAssets: Boolean;
    pLibURLs, pLibLoc: TStringList;
    pDenySwitch, pFReady: Boolean;
    procedure GetIndex;
    procedure GetLibs;
    procedure GetAssets;
    procedure StartMC;
    function GetTerminated: Boolean;
    function StreamToString(Stream: TStream): String;
    procedure SyncStatus;
    procedure ToLog;
  public
    RunOnTerminate:
    procedure of Object;
    CONSTRUCTOR Create(CreateSuspended: Boolean; Profile: TProfileObj;
      WebSet: TWebSettings; const StackSize: SizeUInt = DefaultStackSize);
    Property Status: String read pStatus;
    Property ForceDownload: Boolean read pForceDownload write pForceDownload;
    Property ExludeAssets: Boolean read pExcludeAssets write pExcludeAssets;
    Property HasTerminated: Boolean read GetTerminated;
  end;

const
  StdRep = [rfIgnoreCase, rfReplaceAll];

implementation

{ TMcSetup }

uses fmLauncher, fmLogin;

procedure TMcSetup.DoTerminate;
begin
  FreeOnTerminate := False;
  Terminate;
  inherited DoTerminate;
  if Assigned(RunOnTerminate) then
    Synchronize(RunOnTerminate);

  if Assigned(FatalException) then
  begin
    pStatus := 'Thread excepted!';
    Synchronize(@SyncStatus);
  end
  else
  begin

    pStatus := 'Terminated McSetup';
    Synchronize(@ToLog);
  end;

end;

CONSTRUCTOR TMcSetup.Create(CreateSuspended: Boolean; Profile: TProfileObj;
  WebSet: TWebSettings; const StackSize: SizeUInt);
begin
  pProfile := Profile;
  WSet := WebSet;
  inherited Create(CreateSuspended, StackSize);
end;

procedure TMcSetup.Execute;
var
  tempList: TStringList;
  fIndex: Integer;
begin
  try
    pStatus := ('Getting version');
    pprofileINI := TIniFile.Create(pProfile.indexFile);
    pVersion := pprofileINI.ReadString('Minecraft', 'version', 'xxx');
    if (pVersion = 'xxx') then
    begin
      pStatus := ('ERROR: Minecraft-Version invalid !');
      Synchronize(@SyncStatus);
      Abort;
    end;
    pVanillaVer := pVersion;

    pFReady := pProfile.indexINI.ReadBool('Profile', 'forge-Ready', False);

    If (pFReady) Then
    Begin
      tempList := TStringList.Create;
      GetSubDirs(pProfile.path + '/versions', tempList);
      If (tempList.Count > 1) Then
      Begin
        fIndex := FindMatchStr(tempList, 'forge');
        If (fIndex <> -1) Then
        Begin
          pVersion := tempList[fIndex];
        End;
      End;
    End;

    pJarPath := pProfile.path + '/versions/' + pVersion + '/' + pVersion + '.jar';
    pRelJarPath := './versions/' + pVersion + '/' + pVersion + '.jar';
    pIndexPath := pProfile.path + '/versions/' + pVersion + '/' + pVersion + '.json';

    GetIndex;

    pDenySwitch := False;
    pPerc := 0;
    pStatus := '';
    Synchronize(@SyncStatus);

    GetLibs;

    pDenySwitch := False;
    pPerc := 0;
    pStatus := '';
    Synchronize(@SyncStatus);

    GetAssets;

    pDenySwitch := False;
    pPerc := 0;
    pStatus := '';
    Synchronize(@SyncStatus);

    StartMC;

  except
    on E: Exception do
    begin
      pStatus := 'Exception ! ' + E.Message;
      Synchronize(@ToLog);
    end;
  end;
end;

procedure TMcSetup.GetIndex;
var
  IndexURL, JarURL: String;
  DThread: TDownLoadThread;
  tempStr: String;
begin
  IndexURL := WSet.download_subIndex;
  IndexURL := StringReplace(IndexURL, '%version%', pVersion, stdRep);
  JarURL := WSet.download_clientJar;
  JarURL := StringReplace(JarURL, '%version%', pVersion, StdRep);

  if (FileExists(pIndexPath) = False) or ((pForceDownload = True) and
    (Pos('forge', pVersion) < 0)) then
  begin
    pStatus := ('Retreving Index');

    DThread := TDownLoadThread.Create(True);
    DThread.URL := IndexURL;
    DThread.Destination := pIndexPath;

    pMaxPerc := 100;
    pMinPerc := 0;
    pPerc := 0;
    Synchronize(@SyncStatus);

    DThread.Start;

    while (DThread.HasTerminated = False) and (not Terminated) do
    begin
      pStatus := ('Retreving Index ' + IntToStr(DThread.Percentual) + '%');
      pPerc := DThread.Percentual;
      Synchronize(@SyncStatus);
    end;

    FreeAndNil(DThread);
  end
  else
  begin
    pStatus := 'Index skipped - exists or Forge';
    Synchronize(@SyncStatus);
    Synchronize(@ToLog);
  end;

  if (FileExists(pJarPath) = False) or ((pForceDownload = True) and
    (Pos('forge', pVersion) < 0)) then
  begin
    DThread := TDownLoadThread.Create(True);
    DThread.URL := JarURL;
    DThread.Destination := pJarPath;
    DThread.Start;

    while (DThread.HasTerminated = False) and (not Terminated) do
    begin
      pStatus := 'Retreving Jar (' + IntToStr(DThread.Percentual) + '%)';
      pPerc := DThread.Percentual;
      Synchronize(@SyncStatus);
    end;

    FreeAndNil(DThread);
  end
  else
  begin
    pStatus := 'Jar skipped - exists or Forge';
    Synchronize(@SyncStatus);
    Synchronize(@ToLog);
  end;
end;

procedure TMcSetup.GetLibs;
var
  P: TJSONParser;
  D: TJSONData;
  Ar: TJSONArray;
  iContent: TStringList;
  I: Integer;
  tempList: TStringList;
  pLibURL, LibURL, libStr, pkg, Name, ver, ntv: String;
  DThread: TDownLoadThread;
  AssetURL: String;
begin
  try
    pDenySwitch := True;
    pStatus := 'Reading index: libraries...';
    pPerc := 0;
    Synchronize(@SyncStatus);
    Synchronize(@ToLog);

    pLibURLs := TStringList.Create;
    pLibLoc := TStringList.Create;

    pLibURL := WSet.download_libs;

    iContent := TStringList.Create;
    iContent.LoadFromFile(pIndexPath);

    P := TJSONParser.Create(iContent.Text);
    D := P.Parse;
    Ar := TJSONArray(TJSONObject(D).Extract('libraries'));

    pStatus := 'Estimated LibCount: ' + IntToStr(TJSONObject(Ar).Count);
    Synchronize(@ToLog);

    for I := 0 to (TJSONObject(Ar).Count - 1) do
    begin
      tempList := TStringList.Create;
      tempList.Delimiter := ':';
      tempList.StrictDelimiter := True;
      tempList.DelimitedText := TJSONObject(Ar.Items[I]).Get('name');
      pkg := tempList[0];
      Name := tempList[1];
      ver := tempList[2];

      if (TJSONObject(Ar.Items[I]).IndexOfName('natives') >= 0) then
      begin
        ntv := ('-' + TJSONObject(TJSONObject(Ar.Items[I]).Extract('natives')).Get('windows'));
        ntv := StringReplace(ntv, '${arch}', IntToStr(appUtils.getSystemType()), StdRep);
      end
      else
      begin
        ntv := '';
      end;

      libStr := StringReplace(pkg, '.', '/', StdRep) + '/' + Name +
        '/' + ver + '/' + Name + '-' + ver + ntv + '.jar';

      pLibLoc.Add(libStr);

      if (TJSONObject(Ar.Items[I]).IndexOfName('url') >= 0) then
      begin
        LibURL := TJSONObject(Ar.Items[I]).get('url') + '%LibPath%';
      end
      else
      begin
        LibURL := pLibURL;
      end;

      pLibURLs.Add(StringReplace(LibURL, '%LibPath%', libStr,
        [rfIgnoreCase, rfReplaceAll]));

      FreeAndNil(tempList);
    end;

    pStatus := 'Downloading missing Libraries...';
    pPerc := 0;
    Synchronize(@SyncStatus);
    Synchronize(@ToLog);

    pStatus := 'Estimated download count: ' + IntToStr(pLibLoc.Count);
    Synchronize(@ToLog);

    for I := 0 to (pLibLoc.Count - 1) do
    begin
      Sleep(100);
      pMaxPerc := pLibLoc.Count;
      pPerc := I;
      if (FileExists(pProfile.path + '/libraries/' + pLibLoc[I]) = False) or
        (pForceDownload = True) then
      begin
        DThread := TDownLoadThread.Create(True);
        DThread.URL := pLibURLs[I];
        DThread.Destination := pProfile.path + '/libraries/' + pLibLoc[I];
        DThread.Start;

        pMaxPerc := 100;
        while (DThread.HasTerminated = False) do
        begin
          pStatus := pLibLoc[I] + ' (' + IntToStr(DThread.Percentual) + '%)';
          pPerc := DThread.Percentual;
          Synchronize(@SyncStatus);
        end;

      end
      else
      begin
        pStatus := 'Skipping ' + ExtractFileName(pLibLoc[I]) + ' - exists';
        Synchronize(@SyncStatus);
        //Synchronize(@ToLog);
      end;
    end;

    pMainClass := TJSONObject(D).Get('mainClass');
    pMcArgs := TJSONObject(D).Get('minecraftArguments');

    pStatus := 'Extracting Asset-Index';
    Synchronize(@SyncStatus);

    if (TJSONObject(D).IndexOfName('assets') >= 0) then
    begin
      pAssets := TJSONObject(D).Get('assets');
      AssetURL := StringReplace(WSet.download_assetIndex, '%index%', pAssets, StdRep);
    end
    else
    begin
      pAssets := 'legacy';
      AssetURL := StringReplace(WSet.download_assetIndex, '%index%', pAssets, StdRep);
    end;

    ForceDirectories(pProfile.path + '/assets/indexes');
    pAssetPath := 'GameAssets/' + pVanillaVer + '/assets/indexes/' + pAssets + '.json';

    DThread := TDownLoadThread.Create(True);
    DThread.URL := AssetURL;
    DThread.Destination := pAssetPath;
    DThread.Start;

    pStatus := 'Downloading AssetIndex : ' + pAssets;

    while (DThread.HasTerminated = False) do
    begin
      pStatus := 'AssetIndex [' + pAssets + '] (' + IntToStr(DThread.Percentual) + '%)';
      pPerc := DThread.Percentual;
      Synchronize(@SyncStatus);
    end;

    FreeAndNil(DThread);

    FreeAndNil(P);
    FreeAndNil(D);
    FreeAndNil(Ar);
    FreeAndNil(iContent);

  except
    ON E: Exception do
    begin
      pStatus := 'Exception! : ' + E.Message;
      Synchronize(@SyncStatus);
      Synchronize(@ToLog);
    end;
  end;
end;

procedure TMcSetup.GetAssets;
var
  P: TJSONParser;
  D: TJSONData;
  Ar: TJSONArray;
  iContent: TStringList;
  useHash: Boolean;
  Path: String;
  lURL, lPath, lHash, lName: String;
  DThread: TDownLoadThread;
  I: Integer;
begin
  try

    pStatus := 'Reading Index : Assets';
    Synchronize(@SyncStatus);

    iContent := TStringList.Create;
    iContent.LoadFromFile(pAssetPath);

    P := TJSONParser.Create(iContent.Text);
    D := P.Parse;

    {
    if (TJSONObject(D).IndexOfName('virtual') >= 0) then
    begin
      useHash := False;
      Path := pProfile.path + '/assets/virtual/legacy';
    end
    else
    begin
      useHash := True;
      Path := pProfile.path + '/assets/objects';
    end;
    }

    useHash := True;
    Path := 'GameAssets/' + pVanillaVer + '/assets/objects';

    if ( pAssets <> ('legacy') ) then
    begin
      pGameAssets := GetCurrentDir + '/GameAssets/' + pVanillaVer + '/assets';
    end
    else
    begin
      pGameAssets := GetCurrentDir + '/GameAssets/' + pVanillaVer +
        '/assets/virtual/legacy';
    end;

    ForceDirectories(path);

    Ar := TJSONArray(TJSONObject(D).Extract('objects'));
    pMaxPerc := (TJSONObject(Ar).Count);

    for I := 0 to (TJSONObject(Ar).Count - 1) do
    begin
      if (Terminated = True) then
        Exit;

      lName := TJSONObject(Ar).Names[I];
      lHash := TJSOnObject(Ar.Items[I]).Get('hash');
      if (useHash = True) then
      begin
        lPath := '/' + Copy(lHash, 1, 2) + '/' + lHash;
      end
      else
      begin
        lPath := '/' + lName;
      end;
      lURL := StringReplace(WSet.downloas_assets, '%assetPath%', lPath, StdRep);

      pPerc := I;

      if (FileExists(Path + lPath) = False) or
        ((pForceDownload = True) and (pExcludeAssets = False)) then
      begin
        pStatus := 'Downloading: ' + lPath + ' (0%)';
        Synchronize(@SyncStatus);

        DThread := TDownLoadThread.Create(True);
        DThread.URL := lURL;
        DThread.Destination := Path + lPath;
        DThread.Start;

        while (DThread.HasTerminated = False) and (Terminated = False) do
        begin
          pStatus := 'Downloading: ' + lPath + ' (' +
            IntToStr(DThread.Percentual) + '%)';
          Synchronize(@SyncStatus);
          Sleep(40);
        end;

        ForceDirectories('GameAssets/' + pVanillaVer + '/assets/virtual/legacy/' +
          ExtractFileDir(lName));
        CopyFile(DThread.Destination, 'GameAssets/' + pVanillaVer +
          '/assets/virtual/legacy/' + lName);

        FreeAndNil(DThread);
      end
      else
      begin
        pStatus := 'Skipping ' + lPath + ' - exists';
        Synchronize(@SyncStatus);
      end;
    end;

    FreeAndNil(P);
    FreeAndNil(D);
    FreeAndNil(Ar);
    FreeAndNil(iContent);

  except
    ON E: Exception do
    begin
      pStatus := 'Exception! :' + E.Message;
      Synchronize(@SyncStatus);
      Synchronize(@ToLog);
    end;
  end;
end;

procedure TMcSetup.StartMC;
var
  McProc: TProcess;
  tempStr: String;
  tempStr2: String;
  I: Integer;
  Strm: TMemoryStream;
  logline: String;
begin
  McProc := TProcess.Create(nil);

  McProc.Executable := LSettings.javaPath;
  McProc.CurrentDirectory := StringReplace(GetCurrentDir + '/' +
    pProfile.path, '/', '\', StdRep);

  tempStr := tempStr + ' ' + pProfile.AditArgs;

  tempStr := tempStr + ' ' + LSettings.BaseArgs + ' ';

  tempStr2 := '"' + './libraries/' + pLibLoc[0] + '";';
  for I := 1 to (pLibLoc.Count - 1) do
  begin
    tempStr2 := tempStr2 + '"' + './libraries/' + pLibLoc[I] + '";';
  end;

  tempStr := StringReplace(tempStr, '%CurrDir%', GetCurrentDir, StdRep);
  tempStr := StringReplace(tempStr, '%Libs%', tempStr2, StdRep);
  tempStr := StringReplace(tempStr, '%mainClass%', pMainClass, StdRep);
  tempStr := StringReplace(tempStr, '%jarPath%', pRelJarPath, StdRep);

  tempStr := tempStr + ' ' + pMcArgs;

  tempStr := StringReplace(tempStr, '${auth_player_name}', LUSer.username, StdRep);
  tempStr := StringReplace(tempStr, '${version_name}', pVersion, StdRep);
  tempStr := StringReplace(tempStr, '${game_directory}',
    McProc.CurrentDirectory, StdRep);

  tempStr := StringReplace(tempStr, '${assets_dir}', pGameAssets, StdRep);
  tempStr := StringReplace(tempStr, '${assets_root}', pGameAssets, StdRep);
  tempStr := StringReplace(tempStr, '${game_assets}', pGameAssets, StdRep);
  tempStr := StringReplace(tempStr, '${assets_index_name}', pAssets, StdRep);
  tempStr := StringReplace(tempStr, '${auth_uuid}', LUSer.profileID, StdRep);
  tempStr := StringReplace(tempStr, '${auth_access_token}', LUSer.accessToken, StdRep);
  tempStr := StringReplace(tempStr, '${auth_session}', LUSer.accessToken, StdRep);
  tempStr := StringReplace(tempStr, '${user_properties}', LUSer.userProperties, StdRep);
  tempStr := StringReplace(tempStr, '${user_type}', LUSer.userType, StdRep);
  //tempStr := StringReplace(tempStr, '${}', , StdRep);

  tempStr := StringReplace(tempStr, '/', '\', StdRep);
  McProc.Parameters.Add(tempStr);

  { TODO 70 -oL4YG -cMC-Console : Load console Output if wanted }
  //McProc.Options := [poUsePipes, poStderrToOutPut];

  //Strm := TMemoryStream.Create;

  pStatus :=
    ('Starting Minecraft with Settings:' + LineEnding + LineEnding +
    'Parameters: ' + LineEnding + McProc.Parameters[0] + LineEnding +
    'Directory:' + LineEnding + McProc.CurrentDirectory);
  Synchronize(@ToLog);

  McProc.Execute;

  While (mcProc.Running) and (Terminated = False) do

  Begin
    //logLine := StreamToString(mcProc.Output);
    //fm_Launcher.Memo1.Append(Logline);
    Sleep(100);
  end;

end;

function TMcSetup.GetTerminated: Boolean;
begin
  Result := Terminated;
end;

procedure TMcSetup.SyncStatus;
begin
  fm_Launcher.Label_setupStatus.Caption := Status;
  if (pPerc = 0) and (fm_Launcher.ProgressBar_setupStatus.Visible = False) and
    (pDenySwitch = False) then
  begin
    fm_Launcher.ProgressBar_setupStatus.Visible := True;
  end;
  if (pPerc <> 0) and (fm_Launcher.ProgressBar_setupStatus.Visible = True) then
  begin
    fm_Launcher.ProgressBar_setupStatus.Visible := False;
  end;
  fm_Launcher.FlashPBar_setup.MinValue := pMinPerc;
  fm_Launcher.FlashPBar_setup.MaxValue := pMaxPerc;
  fm_Launcher.FlashPBar_setup.Value := pPerc;

  fm_Launcher.Update;
end;

procedure TMcSetup.ToLog;
begin
  Log(etDebug, Status, 3);
end;

//-----Begin-Source:http://wiki.freepascal.org/Executing_External_Programs/de-----
function TMcSetup.StreamToString(Stream: TStream): String;
const
  READ_BYTES = 2048;

var
  OurCommand: String;
  OutputLines: TStringList;
  MemStream: TMemoryStream;
  NumBytes: LongInt;
  BytesRead: LongInt;

begin
  // A temp Memorystream is used to buffer the output
  MemStream := TMemoryStream.Create;
  BytesRead := 0;

  while True do
  begin
    // make sure we have room
    MemStream.SetSize(BytesRead + READ_BYTES);

    // try reading it
    NumBytes := Stream.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
    if NumBytes > 0 // All read() calls will block, except the final one.
    then
    begin
      Inc(BytesRead, NumBytes);
    end
    else
      BREAK; // Program has finished execution.
  end;

  MemStream.SetSize(BytesRead);

  OutputLines := TStringList.Create;
  OutputLines.LoadFromStream(MemStream);

  for NumBytes := 0 to OutputLines.Count - 1 do
  begin
    Result := Result + LineEnding + OutputLines[NumBytes];
  end;

  OutputLines.Free;
  MemStream.Free;
  //-----END-Source-----
end;

end.
