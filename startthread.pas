Unit StartThread;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, ComCtrls,
  { Online } authsystem,
  { Processes } process,
  { INIFiles } IniFiles,
  { JSON-Units } jsonUtils, fpjson, jsonparser,
  { Utils } unit_appUtils, FileUtil,
  { Log/Consoles }Log;

Type
  TStartThread = Class(TThread)
    // Protected {Make sure the following Procedures/Functions are protected}

    Procedure OnTerminate;
    Procedure Start; { Execution of the Thread }
    Destructor Destroy; { Termination of the Thread }

    Procedure GetInformation_type_Vanilla();
    Procedure StartMC_type_Vanilla();

    Procedure ShowStatus;
    Procedure PrintLog;
    Procedure SetBar;
    Procedure HandleTopLevel;
    Procedure Test;
  Public
    {Public Declarations}
    HasTerminated: Boolean;
    ProfilePath, version, assetIndex: String;
    HasErrored: Boolean;
    ErrorMsg: String;
    profileSettingsFile, webSettingsFile: String;
    webSettings: TIniFile;
    profileSettings: TIniFile;
    firstStart: Boolean;
    CurrentAction: String;
    Error: Exception;
    step, mstep: Integer;
    LibList: TStringList;
    subIndexFile, JarPath, logLine, logCons, mainClass: String;
    mcProc: TProcess;
  Private
    {Pivate Declarations}
  End;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

Implementation

Uses { SetupMC } setupMC,
  { Errors } errorhandler,
  { Settings } settings,
  { Console Access } consoles,
  { userdata } login;

Procedure TStartThread.ShowStatus;
Begin
  Form_setupMC.StaticText_threadAction.Caption := CurrentAction;
End;

Procedure TStartThread.Start;
Begin
  Try
    CurrentAction := 'Thread started';
    Synchronize(@ShowStatus);

    FreeOnTerminate := False; //Do NOT FreeOnTerminate! Have to read ErrorDetails First!
    If (Length(ProfilePath) < 5) Or (Not FileExists(webSettingsFile)) Or
      (Not FileExists(profileSettingsFile)) Then
    Begin
      HasErrored := True;
      ErrorMsg := 'Parameter init failed! One or more parameters were invalid';
      HasTerminated := True;
      Exit;
    End;
    LibList := TStringList.Create;

    GetInformation_type_Vanilla();



    If (HasErrored) Then
    Begin
      FreeOnTerminate := False;
      HasTerminated := True;
      Exit;
    End;

    StartMC_type_vanilla();



    If (HasErrored) Then
    Begin
      FreeOnTerminate := False;
      HasTerminated := True;
      Exit;
    End
    Else
    Begin
      CurrentAction := 'Thread finished!';
      mstep := step;
      Synchronize(@SetBar);
      Synchronize(@ShowStatus);
      FreeOnTerminate := True;
    End;

  Except
    on E: Exception Do
    Begin
      Error := E;
      Synchronize(@HandleTopLevel);
    End;
  End;
End;

Procedure TStartThread.HandleTopLevel;
Begin
  Form_error.Handle(Error,
    'Critical top-level Thread Exception! Please contact the developer as fast as possible!',
    True);
End;

Destructor TStartThread.Destroy;
Begin

  Inherited Destroy;
End;

Procedure TStartThread.OnTerminate;
Begin
  HasTerminated := True;
End;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//##############################################################################
//########################## BEGIN Vanilla Setup ###############################
//##############################################################################
//##############################################################################
Procedure TStartThread.GetInformation_type_Vanilla();
Var
  SubIndexURL, AssetIndexFile, AssetIndexURL, LibStr, currStr, tempURL, NNLibUrl: String;
  P: TJSONParser;
  D: TJSONData;
  items, names, content, tempList: TStringList;
  LibPkg, LibName, LibVersion, LibNativeStr, JarURL: String;
  ObjHash: String;
  I, I2, LibStrLoc, fIndex: Integer; //I2 = I(secondLevel)
  P2, P3: TJSONParser;
  D2, D3: TJSONData;
  assetPath, assetPathLegacy: String;
  tempAssetPath, tempAssetUrl: String;
Begin
  //Download All Indexes etc.
  CurrentAction := ('Started "GetInformation"');
  Synchronize(@ShowStatus);

  profileSettings := TIniFile.Create(profileSettingsFile);
  webSettings := TIniFile.Create(webSettingsFile);

  //Select Version:
  version := profileSettings.ReadString('Minecraft', 'version', 'XXX');
  If (version = 'XXX') Then
  Begin
    HasErrored := True;
    ErrorMsg := 'ProfileIndex: Version could not be read! This shouldn' +
      #39 + 't be able to happen here O.o! ';
    Exit;
  End;
  //Check if firstStart
  firstStart := (Not FileExists(ProfilePath + '/' + version + '.jar'));
  profileSettings.WriteBool('Profile', 'firstStart', firstStart);

  //Create needed Dirs
  ForceDirectories(ProfilePath + '/assets/indexes/');
  ForceDirectories(ProfilePath + '/libraries');
  ForceDirectories(ProfilePath + '/versions/' + version);
  ForceDirectories(ProfilePath + '/resourcepacks');
  If (profileSettings.ReadBool('Profile', 'Forge-Ready', False)) Then
  Begin
    tempList := TStringList.Create;
    tempList.Add('');
    tempList.SaveToFile(ProfilePath + '/launcher_profiles.json');
    FreeAndNil(tempList);
  End;

  If (profileSettings.ReadBool('Profile', 'forge-ready', False)) Then
  Begin
    tempList := TStringList.Create;
    GetSubDirs(ProfilePath + '/versions', tempList);
    fIndex := (FindMatchStr(tempList, 'forge'));
    If (fIndex >= 0) Then
    Begin
      DeleteDirectory(ProfilePath + '/versions/' + version, True);
      CopyFile(ProfilePath + '/versions/' + tempList[fIndex] + '/' +
        tempList[fIndex] + '.jar', ProfilePath + '/version/' + version + '/' + version + '.jar');
      CopyFile(ProfilePath + '/versions/' + tempList[fIndex] + '/' +
        tempList[fIndex] + '.json', ProfilePath + '/version/' + version + '/' + version + '.json');
    End;
  End;

  //Download SubIndex
  SubIndexURL := webSettings.ReadString('DownloadLocation', 'SubIndex',
    'http://s3.amazonaws.com/Minecraft.Download/versions/%version%/%version%.json');
  SubIndexURL := (StringReplace(SubIndexURL, '%version%', version, [rfReplaceAll]));
  SubIndexFile := (ProfilePath + '/versions/' + version + '/' +
    profileSettings.ReadString('Minecraft', 'SubIndexName', '%version%') + '.json');
  SubIndexFile := StringReplace(SubIndexFile, '%version%', version, [rfReplaceAll]);
  CurrentAction := ('Retreving :' + ExtractFileName(SubIndexURL));
  If (profileSettings.ReadBool('Profile', 'forge-ready', False) = False) And
    (FileExists(subIndexFile)) Then
  Begin
    If (authsystem.GetFile(SubIndexURL, SubIndexFile) = False) And
      (FileExists(SubIndexFile) = False) Then
    Begin
      HasErrored := True;
      ErrorMsg := 'Couldn' + #39 + 't load SubIndex! Aborting!';
      Terminate;
    End;
  End;
  CurrentAction := ('Retreving: ' + version + '.jar');
  Synchronize(@ShowStatus);

  JarPath := (ProfilePath + '/versions/' + version + '/' + version + '.jar');
  If (Not FileExists(JarPath)) Then
  Begin
    JarURL := webSettings.ReadString('DownloadLocations', 'ClientJar',
      'http://s3.amazonaws.com/Minecraft.Download/versions/%version%/%version%.jar');
    JarURL := StringReplace(JarURL, '%version%', version, [rfReplaceAll]);
    If (Not authsystem.GetBinFile(JarURL, JarPath)) Then
    Begin
      HasErrored := True;
      ErrorMsg := 'Couldn' + #39 + 't retreve ClientJar! Aborting!';
      Exit;
    End;
  End;

  Try
    CurrentAction := ('Extracting Informations...');
    Synchronize(@ShowStatus);

    content := TStringList.Create;
    content.LoadFromFile(SubIndexFile);
    P := TJSONParser.Create(content.Text);
    D := P.Parse;

    If (Not (TJSONObject(D).IndexOfName('assets') >= 0)) Then
    Begin
      assetIndex := 'legacy';
    End
    Else
    Begin
      assetIndex := TJSONObject(D).Get('assets');
    End;

    mainClass := TJSONObject(D).Get('mainClass');

    { DONE 100 -oL4YG -cDownload+Setup : Extract "Information" of SubIndex!}
    P.Free;
    //=========================================================================
    //We need "libraries" value (type:"JSONArray") as StringList[content]
    //or string[LibStr] converted to StringList[content]
    //to download all required libraries.
    //Has to work for legacy AND new system
    //Adit.:
    //Free(AndNil?)[D]
    //Clear [content]
    //=========================================================================
    tempList := TStringList.Create;
    LibStrLoc := (TJSONObject(D).IndexOfName('libraries'));
    ListItems(D, tempList);
    LibStr := tempList[LibStrLoc];
    content.Clear;
    D.Free;
    //Make String compatible to FPJSON
    currStr := StringReplace(LibStr, '\', '', [rfReplaceAll]);
    currStr := StringReplace(LibStr, ':true', ':"True" ', [rfReplaceAll, rfIgnoreCase]);

    P := TJSONParser.Create(currStr);
    D := P.Parse;
    For i := 0 To (D.Count - 1) Do
    Begin
      content.Add(JSONToString(D.Items[i]));
    End;
    D.Free;
    tempList.Free;
    //=========================================================================
  Except
    on E: Exception Do
    Begin
      HasErrored := True;
      If (E.ClassNameIs('TJSONParser')) {or (E.ClassNameIs('EScannerError'))} Then
      Begin
        content.SaveToFile('GFFLauncher/Logs/CrashDump.' + DateToStr(now) + '.txt');
        ErrorMsg := E.Message + LineEnding +
          'Dump written into: GFFLauncher/Logs/CrashDump.' + DateToStr(now) + '.txt';
      End
      Else
      Begin
        ErrorMsg := E.ClassName + LineEnding + E.Message + LineEnding + currStr;
      End;
      Exit;
    End;
  End;
  { DONE 100 -oL4YG -cDownload+Setup : Download Libraries}
  //Download Libraries
  Try
    CurrentAction := 'Preparing Lib-Download';
    Synchronize(@ShowStatus);

    If (content.Count < 2) Then
    Begin
      Raise Exception.Create(LineEnding +
        'LibString: Unknown error : content.count returned < 2!');
    End;
    mstep := content.Count;
    For i := 0 To content.Count - 1 Do
    Begin
      step := i;
      Synchronize(@SetBar);
      currStr := content[i];
      P := TJSONParser.Create(currStr);
      D := P.Parse;
      LibStr := TJSONObject(D).Get('name');
      NNLibUrl := '';
      If (TJSONObject(D).IndexOfName('url') >= 0) Then
      Begin
        NNLibUrl := TJSONObject(D).Get('url') + '%LibPath%';
      End;
      tempList := TStringList.Create;
      tempList.StrictDelimiter := True;
      tempList.Delimiter := ':';
      tempList.DelimitedText := LibStr;
      LibPkg := StringReplace(tempList[0], '.', '/', [rfReplaceAll]);
      LibName := tempList[1];
      LibVersion := tempList[2];
      If (TJSONObject(D).IndexOfName('"natives"', False) >= 0) Then
      Begin
        currStr := TJSONObject(D).Get('natives');
        P.Free;
        D.Free;
        P := TJSONParser.Create(currStr);
        D := P.Parse;
        LibNativeStr := StringReplace('-' + TJSONObject(D).Get('windows'),
          '${arch}', IntToStr(unit_appUtils.getSystemType()), [rfReplaceAll]);
      End
      Else
      Begin
        LibNativeStr := '';
      End;
      LibStr := (LibPkg + '/' + LibName + '/' + LibVersion + '/' +
        LibName + '-' + LibVersion + LibNativeStr + '.jar');

      LibList.Add('/libraries/' + LibStr);               //Hold List for launch!

      If (Not FileExists(ProfilePath + '/libraries/' + LibStr)) Then
      Begin
        CurrentAction := 'Retreving lib: ' + LibName;
        Synchronize(@ShowStatus);
        If (NNLibUrl <> '') Then
        Begin
          tempURL := StringReplace(NNLibUrl, '%LibPath%', LibStr,
            [rfReplaceAll]);
          ;
        End
        Else
        Begin
          tempURL := StringReplace(webSettings.ReadString(
            'DownloadLocation', 'Libs', 'https://libraries.minecraft.net/%LibPath%'),
            '%LibPath%', LibStr, [rfReplaceAll]);
        End;
        If (Not authsystem.GetBinFile(tempURL, ProfilePath + '/libraries/' +
          LibStr)) Then
        Begin
          HasErrored := True;
          ErrorMsg := 'Could not retreve missing lib. "' + LibName + '" aborting!';
          Exit;
        End;
      End;
      tempList.Free;
    End;
    content.Free;
    D.Free;
    P.Free;
  Except
    on E: Exception Do
    Begin
      HasErrored := True;
      ErrorMsg := 'LibParse-Error at element: "' + currStr + '"';
      ErrorMsg := ErrorMsg + LineEnding + 'MSG:' + E.Message;
      Exit;
    End;
  End;

  //Download AssetIndex
  CurrentAction := ('Retreving Assets-Index');
  Synchronize(@ShowStatus);

  AssetIndexURL := webSettings.ReadString('DownloadLocations',
    'AssetIndex', 'https://s3.amazonaws.com/Minecraft.Download/indexes/%index%.json');
  AssetIndexURL := (StringReplace(AssetIndexURL, '%index%', assetIndex, [rfReplaceAll]));
  AssetIndexFile := (ProfilePath + '/assets/indexes/' + assetIndex + '.json');
  CurrentAction := ('Retreving :' + ExtractFileName(AssetIndexURL));
  If (profileSettings.ReadBool('Profile', 'forge-ready', False) = False) And
    (FileExists(AssetIndexFile)) Then
  Begin
    If (authsystem.GetFile(AssetIndexURL, AssetIndexFile) = False) And
      (FileExists(AssetIndexFile) = False) Then
    Begin
      HasErrored := True;
      ErrorMsg := 'Couldn' + #39 + 't retreve/load AssetIndex! Aborting!';
      Exit;
    End;
  End;

  CurrentAction := ('Exctracting Asset-Info...');
  Synchronize(@ShowStatus);

  Try
    content := TStringList.Create;
    content.LoadFromFile(AssetIndexFile);
    currStr := content.Text;
    p := TJSONParser.Create(currStr);
    D := P.Parse;
    assetPath := (ProfilePath + '/assets');
    assetPathLegacy := (ProfilePath + '/assets/virtual/legacy');
    For i := 0 To (D.Count - 1) Do
    Begin
      If (TJSONObject(D).Items[i].JSONType = jtObject) Then
      Begin
        ForceDirectories(ProfilePath + '/assets/' + TJSONObject(D).Names[i]);
        currStr := JSONToString(TJSONObject(D).Items[i]);
        P2 := TJSONParser.Create(currStr);
        D2 := P2.Parse;
        mstep := D2.Count;
        Synchronize(@ShowStatus);
        For i2 := 0 To (D2.Count - 1) Do
        Begin
          If (TJSONObject(D2).Items[i2].JSONType = jtObject) Then
            step := i2;
          Synchronize(@SetBar);

          currStr := JSONToString(TJSONObject(D2).Items[i2]);
          P3 := TJSONParser.Create(currStr);
          D3 := P3.Parse;
          ForceDirectories(assetPathLegacy + '/' +
            ExtractFileDir(TJSONObject(D2).Names[i2]));

          ForceDirectories(assetPath + '/' + TJSONObject(D).Names[i] +
            '/' + Copy(TJSONObject(D3).Get('hash'), 1, 2));
          tempAssetPath := ('/' + Copy(TJSONObject(D3).Get('hash'), 1, 2) +
            '/' + TJSONObject(D3).Get('hash'));

          tempAssetUrl := webSettings.ReadString('DownloadLocations',
            'Asset', 'http://resources.download.minecraft.net%assetPath%');
          tempAssetUrl := StringReplace(tempAssetUrl, '%assetPath%',
            tempAssetPath, [rfReplaceAll]);

          If (Not FileExists(assetPath + '/' + TJSONObject(D).Names[i] +
            '/' + tempAssetPath)) Then
          Begin

            CurrentAction := ('Retreving :' + tempAssetPath);
            Synchronize(@ShowStatus);
            If (Not authsystem.GetBinFile(tempAssetUrl, assetPath +
              '/' + TJSONObject(D).Names[i] + '/' + tempAssetPath)) Then
            Begin
              HasErrored := True;
              ErrorMsg := ('Couldn' + #39 + 't retreve "' + tempAssetPath +
                '"! Aborting!');
              Exit;
            End
            Else
            Begin
              CopyFile((assetPath + '/' + TJSONObject(D).Names[i] + '/' + tempAssetPath),
                (assetPathLegacy + '/' + TJSONObject(D2).Names[i2]));
            End;

          End
          Else
          Begin
            CurrentAction := ('Skipping [exists] :' + tempAssetPath);
            Synchronize(@ShowStatus);
          End;
          //P3.Free; { Access-violation ?! I.o}
          //D3.Free;
          //P2.Free;
          //D2.Free;
        End;
      End;
    End;
    P.Free;
    D.Free;
    If (version = '1.7.2') Then
    Begin
      CopyDirTree(ProfilePath + '/assets/virtual/legacy/sound',
        ProfilePath + '/assets/virtual/legacy/sounds');
    End;
  Except
    on E: Exception Do
    Begin
      HasErrored := True;
      ErrorMsg :=
        ('An error occured while extracting/downloading assets/-information!' +
        LineEnding + 'Class: ' + E.ClassName + LineEnding + 'MSG: ' +
        E.Message + LineEnding + LineEnding + currStr);
      Exit;
    End;
  End;
  { DONE 100 -oL4YG -cDownload+Setup : Download Assets }
  { TODO 20 -oL4YG -cIdeas : Create startinfo-field in ProfileIndex.ini }

  step := 0;
  mstep := 0;
  Synchronize(@SetBar);
End;
//##############################################################################
//############################ END VANILLA SETUP ###############################
//############################ Start VANILLA ###################################
//##############################################################################

Procedure TStartThread.StartMC_type_Vanilla();
Var
  content, tempList: TStringList;
  P: TJSONParser;
  D: TJSONData;
  launchArgs, LibStr, startAct: String;
  i, argLoc: Integer;
Begin

  Try
    CurrentAction := 'Creating Process...';
    Synchronize(@ShowStatus);

    content := TStringList.Create;
    content.LoadFromFile(subIndexFile);

    startAct := 'Create TProcess';
    mcProc := TProcess.Create(nil);

    startAct := 'Set currentDir';
    mcProc.CurrentDirectory :=
      (SysUtils.GetEnvironmentVariable('appdata') + '\' + ProfilePath);
    startAct := 'Create launch parameters';

    P := TJSONParser.Create(content.Text);
    D := P.Parse;
    LibStr := '"' + mcProc.CurrentDirectory + LibList[0] + '"';
    LibList.Add('/versions/' + version + '/' + version + '.jar');
    For i := 1 To (LibList.Count - 1) Do
    Begin
      LibStr := LibStr + ';"' + mcProc.CurrentDirectory + LibList[i] + '"';
    End;
    LibStr := StringReplace(LibStr, '/', '\', [rfReplaceAll]);

    tempList := TStringList.Create;
    argLoc := TJSONObject(D).IndexOfName('minecraftArguments');
    jsonUtils.ListItems(D, tempList);

    launchArgs := StringReplace(settings.MainSettings.BaseArgs, '%Libs%',
      LibStr, [rfReplaceAll]);
    launchArgs := StringReplace(launchArgs, '%mainClass%', mainClass, [rfReplaceAll]);
    launchArgs := StringReplace(launchArgs, '%appdata%',
      SysUtils.GetEnvironmentVariable('appdata'), [rfReplaceAll]);
    startAct := 'Create launch parameters 2';
    launchArgs := launchArgs + ' ' + profileSettings.ReadString(
      'Minecraft', 'launchArgs', '-XX:PermSize=128M -Xmx1G -Xms512M');
    startAct := 'Create launch parameters 3';
    launchArgs := launchArgs + ' ' +
      (StringReplace(tempList[argLoc], '"', '', [rfReplaceAll]));
    FreeAndNil(tempList);
    { DONE 100 -oL4YG -cDownload+Setup : Filter/Replace Arg-Placeholder (Token, Nick, etc...) }
    launchArgs := StringReplace(launchArgs, '${auth_player_name}',
      UsrObj.username, [rfReplaceAll]);
    launchArgs := StringReplace(launchArgs, '${version_name}', version, [rfReplaceAll]);
    launchArgs := StringReplace(launchArgs, '${game_directory}', '"' +
      mcProc.CurrentDirectory + '"', [rfReplaceAll]);
    If (assetIndex <> 'legacy') Then
    Begin
      launchArgs := StringReplace(launchArgs, '${assets_root}', '"' +
        mcProc.CurrentDirectory + '/assets' + '"', [rfReplaceAll]);
    End
    Else
    Begin
      launchArgs := StringReplace(launchArgs, '${game_assets}', '"' +
        mcProc.CurrentDirectory + '/assets/virtual/legacy' + '"', [rfReplaceAll]);
    End;
    launchArgs := StringReplace(launchArgs, '${assets_index_name}',
      assetIndex, [rfReplaceAll]);
    launchArgs := StringReplace(launchArgs, '${auth_uuid}', UsrObj.profileID
      {UUID is per Profile! not the "clientToken" for whatever reason O.o}
      , [rfReplaceAll]);
    launchArgs := StringReplace(launchArgs, '${auth_access_token}',
      UsrObj.accessToken, [rfReplaceAll]);
    launchArgs := StringReplace(launchArgs, '${user_properties}',
      UsrObj.userProps, [rfReplaceAll]);
    launchArgs := StringReplace(launchArgs, '${user_type}', UsrObj.userType,
      [rfReplaceAll]);
    //launchArgs := StringReplace(launchArgs,,,[rfReplaceAll]);

    startAct := 'Create launch parameters 4';
    launchArgs := launchArgs + ' -jar ' + '/versions/' + version +
      '/' + version + '.jar';


    startAct := 'Set Executable';
    mcProc.Executable := settings.MainSettings.javaPath;

    If (MainSettings.showMCConsole = True) Or
      (profileSettings.ReadBool('Minecraft', 'showCons', False) = True) Then
    Begin
      startAct := 'Set options';
      mcProc.ShowWindow := swoShowMinimized;
      mcProc.Options := [poUsePipes, poStderrToOutPut];
      Form_consoles.Show;
    End;
    startAct := 'Adding Parameters';
    mcProc.Parameters.Add(launchArgs);
    startAct := 'Start Process...';

    CurrentAction := 'Starting Minecraft...';
    Synchronize(@ShowStatus);

    mcProc.Execute;

    While (mcProc.Running) And ((MainSettings.showMCConsole = True) Or
        (profileSettings.ReadBool('Minecraft', 'showCons', False) = True)) Do
    Begin
      logLine := StreamToString(mcProc.Output);
      If Length(LogLine) > 1 Then
      Begin
        logCons := Log.Consoles_MC;
        Synchronize(@PrintLog);
      End;
      Sleep(100);
    End;
  Except
    on E: Exception Do
    Begin
      HasErrored := True;
      ErrorMsg := ('An error occured while trying to start Minecraft!' +
        LineEnding + 'Action: ' + StartAct + LineEnding + 'Class: ' +
        E.ClassName + LineEnding + 'Msg: ' + E.Message);
      Exit;
    End;
  End;
End;

//##############################################################################
//############################ END Start VANILLA ###############################
//##############################################################################
//##############################################################################

Procedure TStartThread.PrintLog;
Begin
  Log.Print(logLine, logCons);
End;

Procedure TStartThread.Test;
Begin
  CurrentAction := 'Test' + TimeToStr(now);
  Synchronize(@ShowStatus);
End;

Procedure TStartThread.SetBar;
Begin
  If (step <> mstep) Then
  Begin
    If (Form_setupMC.ProgressBar_loading.Style = pbstMarquee) Then
    Begin
      Form_setupMC.ProgressBar_loading.Style := pbstNormal;
    End;
    Form_setupMC.ProgressBar_loading.Max := mstep;
    Form_setupMC.ProgressBar_loading.Position := step;
  End
  Else
  Begin
    Form_setupMC.ProgressBar_loading.Position := 0;
    Form_setupMC.ProgressBar_loading.Max := 0;
    Form_setupMC.ProgressBar_loading.Style := pbstMarquee;
  End;
End;

End.
