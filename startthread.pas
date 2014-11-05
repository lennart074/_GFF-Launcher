Unit StartThread;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, ComCtrls,
  { Online } authsystem,
  { Processes } process,
  { INIFiles } IniFiles,
  { JSON-Units } jsonUtils, fpjson, jsonparser,
  { Utils } unit_appUtils,
  FileUtil
  //{ Feedback } Dialogs
  { DONE 50 -oL4YG -cDebug : Remove Debug-Feedback ! };

Type
  TStartThread = Class(TThread)
    // Protected {Make sure the following Procedures/Functions are protected}

    Procedure OnTerminate;
    Procedure Start; { Execution of the Thread }
    Destructor Destroy; { Termination of the Thread }

    Procedure GetInformation_type_Vanilla();
    procedure StartMC_type_Vanilla();

    Procedure ShowStatus;
    Procedure SetBar;
    Procedure HandleTopLevel;
    Procedure Test;
  Public
    {Public Declarations}
    HasTerminated: Boolean;
    ProfilePath: String;
    HasErrored: Boolean;
    ErrorMsg: String;
    profileSettingsFile, webSettingsFile: String;
    webSettings: TIniFile;
    profileSettings: TIniFile;
    firstStart: Boolean;
    CurrentAction: String;
    Error: Exception;
    step, mstep: Integer;
  Private
    {Pivate Declarations}
  End;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

Implementation

Uses { SetupMC } setupMC,
  { Errors } errorhandler;

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

    GetInformation_type_Vanilla();
    StartMC_type_vanilla();



    If (HasErrored) Then
    Begin
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
  SubIndexFile, SubIndexURL, AssetIndexFile, AssetIndexURL, version,
  assetIndex, LibStr, currStr, tempURL: String;
  P: TJSONParser;
  D: TJSONData;
  items, names, content, tempList: TStringList;
  LibPkg, LibName, LibVersion, LibNativeStr, JarPath, JarURL: String;
  ObjHash: String;
  I, I2, LibStrLoc: Integer; //I2 = I(secondLevel)
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
    HasTerminated := True;
    Exit;
  End;
  //Check if firstStart
  firstStart := (Not FileExists(ProfilePath + '/' + version + '.jar'));
  profileSettings.WriteBool('Profile', 'firstStart', firstStart);

  //Create needed Dirs
  ForceDirectories(ProfilePath + '/assets/indexes/');
  ForceDirectories(ProfilePath + '/libraries');
  ForceDirectories(ProfilePath + '/versions/' + version);

  //Download SubIndex
  SubIndexURL := webSettings.ReadString('DownloadLocation', 'SubIndex',
    'http://s3.amazonaws.com/Minecraft.Download/versions/%version%/%version%.json');
  SubIndexURL := (StringReplace(SubIndexURL, '%version%', version, [rfReplaceAll]));
  SubIndexFile := (ProfilePath + '/versions/' + version + '/' + profileSettings.ReadString('Minecraft','SubIndexName','%version%') + '.json');
  SubIndexFile := StringReplace(SubIndexFile, '%version%', version, [rfReplaceAll]);
  CurrentAction := ('Retreving :' + ExtractFileName(SubIndexURL));
  If (authsystem.GetFile(SubIndexURL, SubIndexFile) = False) And
    (FileExists(SubIndexFile) = False) Then
  Begin
    HasErrored := True;
    ErrorMsg := 'Couldn' + #39 + 't load SubIndex! Aborting!';
    HasTerminated := True;
    Terminate;
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
      HasTerminated := True;
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
    currStr := StringReplace(LibStr, '\', '', [rfReplaceAll]);
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
        ErrorMsg := E.ClassName + LineEnding + E.Message;
      End;
      HasTerminated := True;
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
        LibNativeStr := StringReplace(TJSONObject(D).Get('windows'),
          '${arch}', IntToStr(unit_appUtils.getSystemType()), [rfReplaceAll]);
      End
      Else
      Begin
        LibNativeStr := '';
      End;
      LibStr := (LibPkg + '/' + LibName + '/' + LibVersion + '/' +
        LibName + '-' + LibVersion + '-' + LibNativeStr + '.jar');
      If (Not FileExists(ProfilePath + '/libraries/' + LibStr)) Then
      Begin
        CurrentAction := 'Retreving lib: ' + LibName;
        Synchronize(@ShowStatus);
        tempURL := StringReplace(webSettings.ReadString(
          'DownloadLocation', 'Libs', 'https://libraries.minecraft.net/%LibPath%'),
          '%LibPath%', LibStr, [rfReplaceAll]);
        If (Not authsystem.GetBinFile(tempURL, ProfilePath + '/libraries/' +
          LibStr)) Then
        Begin
          HasErrored := True;
          ErrorMsg := 'Could not retreve missing lib. "' + LibName + '" aborting!';
          HasTerminated := True;
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
      HasTerminated := True;
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
  If (authsystem.GetFile(AssetIndexURL, AssetIndexFile) = False) And
    (FileExists(AssetIndexFile) = False) Then
  Begin
    HasErrored := True;
    ErrorMsg := 'Couldn' + #39 + 't retreve/load AssetIndex! Aborting!';
    HasTerminated := True;
    Exit;
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
      ForceDirectories(ProfilePath + '/assets/' + TJSONObject(D).Names[i]);
      P2 := TJSONParser.Create(JSONToString(TJSONObject(D).Items[i]));
      D2 := P2.Parse;
      mstep:=D2.Count;
      Synchronize(@ShowStatus);
      For i2 := 0 To (D2.Count - 1) Do
      Begin
        step := i2;
        Synchronize(@SetBar);
      {//---------------------------------------------------
      //TEST
        HasErrored := True;
        ErrorMsg := TJSONObject(D2).Names[0];
        HasTerminated := True;
        Exit;
        //---------------------------------------------------}
        P3 := TJSONParser.Create(JSONToString(TJSONObject(D2).Items[i2]));
        D3 := P3.Parse;
        //CurrentAction := ('Dir :' + Copy(TJSONObject(D3).Get('hash'), 1, 2));
        //Synchronize(@ShowStatus);
        ForceDirectories(assetPathLegacy + '/' +
          ExtractFileDir(TJSONObject(D2).Names[i2]));

        ForceDirectories(assetPath + '/' + TJSONObject(D).Names[i] +
          '/' + Copy(TJSONObject(D3).Get('hash'), 1, 2));
        tempAssetPath := ('/' + Copy(TJSONObject(D3).Get('hash'), 1, 2) +
          '/' + TJSONObject(D3).Get('hash'));

        //CurrentAction := ('File :' + tempAssetPath);
        //Synchronize(@ShowStatus);

        tempAssetUrl := webSettings.ReadString('DownloadLocations',
          'Asset', 'http://resources.download.minecraft.net%assetPath%');
        tempAssetUrl := StringReplace(tempAssetUrl, '%assetPath%',
          tempAssetPath, [rfReplaceAll]);

        If (Not FileExists(assetPath +
            '/' + TJSONObject(D).Names[i] + '/' + tempAssetPath)) Then
        Begin

          CurrentAction := ('Retreving :' + tempAssetPath);
          Synchronize(@ShowStatus);
          If (Not authsystem.GetBinFile(tempAssetUrl, assetPath +
            '/' + TJSONObject(D).Names[i] + '/' + tempAssetPath)) Then
          Begin
            HasErrored := True;
            ErrorMsg := ('Couldn' + #39 + 't retreve "' + tempAssetPath +
              '"! Aborting!');
            HasTerminated := True;
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
        //P3.Free;
        //D3.Free;
        //P2.Free;
        //D2.Free;
      End;
    End;
    P.Free;
    D.Free;
  Except
    on E: Exception Do
    Begin
      HasErrored := True;
      ErrorMsg :=
        ('An error occured while extracting/downloading assets/-information!' +
        LineEnding + 'Class: ' + E.ClassName + LineEnding + 'MSG: ' + E.Message);
      HasTerminated := True;
      Exit;
    End;
  End;
  { DONE 100 -oL4YG -cDownload+Setup : Download Objects }
  { TODO 100 -oL4YG -cDownload+Setup : Create startinfo-field in ProfileIndex.ini }

  step:=0;
  mstep:=0;
  Synchronize(@SetBar);
End;
//##############################################################################
//############################ END VANILLA SETUP ###############################
//############################ Start VANILLA ###################################
//##############################################################################

procedure TStartThread.StartMC_type_Vanilla();
var
  content : TStringList;
begin
  { TODO 100 -oL4YG -cDownload+Setup : Start Minecraft }
end;

//##############################################################################
//############################ END Start VANILLA ###############################
//##############################################################################
//##############################################################################

Procedure TStartThread.Test;
Begin
  CurrentAction := 'Test' + TimeToStr(now);
  Synchronize(@ShowStatus);
End;

procedure TStartThread.SetBar;
begin
  if (step<>mstep) then begin
    if (Form_setupMC.ProgressBar_loading.Style=pbstMarquee) then begin
       Form_setupMC.ProgressBar_loading.Style := pbstNormal;
    end;
    Form_setupMC.ProgressBar_loading.Max := mstep;
    Form_setupMC.ProgressBar_loading.Position := step;
  end else begin
    Form_setupMC.ProgressBar_loading.Position := 0;
    Form_setupMC.ProgressBar_loading.Max := 0;
    Form_setupMC.ProgressBar_loading.Style := pbstMarquee;
  end;
end;

End.
