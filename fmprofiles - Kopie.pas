unit fmProfiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, BGRAFlashProgressBar, downloads, obj_versions, json,
  IniFiles,
  { JSON } jsonparser, fpjson, jsonUtils,
  { Profiles } obj_remProfiles,
  { Utils } appUtils;

type

  { Tfm_Profiles }

  Tfm_Profiles = class(TForm)
    Button_cancelPred: TButton;
    Button_install: TButton;
    Button_ok: TButton;
    Button_cancel: TButton;
    CheckBox_isFav: TCheckBox;
    CheckBox_fReady: TCheckBox;
    ComboBox_selVersion: TComboBox;
    FProgBar_progress: TBGRAFlashProgressBar;
    GroupBox_info: TGroupBox;
    GroupBox_profiles: TGroupBox;
    GroupBox_crPredef: TGroupBox;
    GroupBox_options: TGroupBox;
    GroupBox_relChannel: TGroupBox;
    GroupBox_createMC_Forge: TGroupBox;
    Image_background_crMC_Forge: TImage;
    Label_Comment: TLabel;
    LabeledEdit_owner: TLabeledEdit;
    LabeledEdit_version: TLabeledEdit;
    LabeledEdit_branch: TLabeledEdit;
    LabeledEdit_name: TLabeledEdit;
    LabeledEdit_javaArgs: TLabeledEdit;
    LabeledEdit_profilename: TLabeledEdit;
    ListBox_profiles: TListBox;
    Memo_descr: TMemo;
    ProgressBar_progress: TProgressBar;
    RadioButton_release: TRadioButton;
    RadioButton_beta: TRadioButton;
    RadioButton_alpha: TRadioButton;
    RadioButton_snapshot: TRadioButton;
    PageControl_main: TPageControl;
    status: TLabeledEdit;
    TabSheet1: TTabSheet;
    TabSheet_crPredef: TTabSheet;
    TabSheet_crMC_Forge: TTabSheet;
    procedure Button_cancelClick(Sender: TObject);
    procedure Button_cancelPredClick(Sender: TObject);
    procedure Button_installClick(Sender: TObject);
    procedure Button_okClick(Sender: TObject);
    procedure ComboBox_selVersionChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ListBox_profilesSelectionChange(Sender: TObject; User: boolean);
    procedure RadioButton_alphaChange(Sender: TObject);
    procedure RadioButton_betaChange(Sender: TObject);
    procedure RadioButton_releaseChange(Sender: TObject);
    procedure RadioButton_snapshotChange(Sender: TObject);
    procedure UpdateSelector(vList: TVersionList; Box: TComboBox; Channel: String);
    procedure crMc_Forge;
    procedure crPredef;
    procedure InstallProfile(Profile: TRemProfileObj);
  private
    { private declarations }
    versions: TVersionList;
    PredProfiles: TRemProfileCollection;
  public
    { public declarations }
  end;

var
  fm_Profiles: Tfm_Profiles;

implementation

{$R *.lfm}

uses { Forms } launcher, fmMain, fmLogin, fmLauncher, fmSettings, fmError, informations;

{ Tfm_Profiles }

procedure Tfm_Profiles.FormShow(Sender: TObject);
begin
  self.Height := 300;
  self.Width := 500;

  Image_background_crMC_Forge.Picture := fm_Launcher.Image_background.Picture;
end;

procedure Tfm_Profiles.ListBox_profilesSelectionChange(Sender: TObject;
  User: boolean);
var
  lProfile: TRemProfileObj;
begin
  if (ListBox_profiles.ItemIndex <> -1) then
  begin
    lProfile := PredProfiles.profiles[ListBox_profiles.ItemIndex];
    LabeledEdit_name.Text := lProfile.ProfName;
    LabeledEdit_branch.Text := lProfile.branch;
    LabeledEdit_version.Text := lProfile.vCapt;
    LabeledEdit_owner.Text := lProfile.ProfOwner;
    Memo_descr.Lines.Clear;
    Memo_descr.Lines.Delimiter := '^';
    Memo_descr.Lines.StrictDelimiter := True;
    Memo_descr.Lines.DelimitedText := lProfile.comment;
  end;
end;

procedure Tfm_Profiles.Button_okClick(Sender: TObject);
var
  INI: TINIFile;
  DoCr: Boolean;
  pPath: String;
  lList: TStringList;
begin
  GroupBox_createMC_Forge.Enabled := False;
  DoCr := True;
  if (LabeledEdit_profilename.Text = '') then
  begin
    DoCr := False;
  end;
  if (DirectoryExists(LabeledEdit_profilename.Text)) then
  begin
    DoCr := False;
  end;
  if (ComboBox_selVersion.Text = '') then
  begin
    DoCr := False;
  end;

  if (DoCr = True) then
  begin
    Log(etInfo, 'Attempting to create Profile "' +
      LabeledEdit_profilename.Text + '"', 4);
    pPath := (LUser.ProfilePath + '/' + LabeledEdit_profilename.Text);
    Log(etDebug, 'Path:' + LineEnding + pPath, 2);
    ForceDirectories(pPath);
    INI := TIniFile.Create(pPath + '/profileIndex.ini');
    INI.WriteString('Profile', 'creator', LUser.username);
    INI.WriteString('Profile', 'crDate', DateToStr(now));
    INI.WriteString('Profile', 'crTime', TimeToStr(now));
    INI.WriteString('Profile', 'name', LabeledEdit_profilename.Text);
    INI.WriteBool('Profile', 'Forge-Ready', CheckBox_fReady.Checked);
    INI.WriteBool('Profile', 'Favourite', CheckBox_isFav.Checked);
    INI.WriteString('Minecraft', 'launchArgs', LabeledEdit_javaArgs.Text);
    INI.WriteString('Minecraft', 'version', ComboBox_selVersion.Text);
    INI.UpdateFile;
    FreeAndNil(INI);

    if (CheckBox_fReady.Checked = True) then
    begin
      Log(etDebug, 'Forge-Ready selected', 2);
      lList := TStringList.Create;
      lList.Add('{' + LineEnding + '"profiles": {' + LineEnding +
        '}' + LineEnding + '}');
      lList.SaveToFile(pPath + '/launcher_profiles.json');
      FreeAndNil(lList);
    end;

    Log(etDebug, 'Done creating Profile', 4);
    ShowMessage('Profile Created');
    fm_Profiles.Close;
  end
  else
  begin
    ShowMessage('Unable to create Profile, please check your entries');
  end;
end;

procedure Tfm_Profiles.ComboBox_selVersionChange(Sender: TObject);
begin
  LabeledEdit_profilename.Text := ComboBox_selVersion.Text;
end;

procedure Tfm_Profiles.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CheckBox_fReady.Checked := False;
  if Assigned(versions) then
    FreeAndNil(versions);
end;

procedure Tfm_Profiles.Button_cancelClick(Sender: TObject);
begin
  Close;
end;

procedure Tfm_Profiles.Button_cancelPredClick(Sender: TObject);
begin
  if Assigned(PredProfiles) then
    FreeAndNil(PredProfiles);

  ListBox_profiles.Clear;
  LabeledEdit_branch.Clear;
  LabeledEdit_name.Clear;
  LabeledEdit_owner.Clear;
  LabeledEdit_version.Clear;

  Close;
end;

procedure Tfm_Profiles.Button_installClick(Sender: TObject);
var
  DThread: TDownLoadThread;
  lProfile: TRemProfileObj;
begin
  if (ListBox_profiles.ItemIndex <> -1) then
  begin
    lProfile := PredProfiles.profiles[ListBox_profiles.ItemIndex];
    if (lProfile.Enabled = True) then
    begin
      InstallProfile(lProfile);
    end
    else
    begin
      ShowMessage('Profile disabled:' + LineEnding + lProfile.disableReason);
    end;
    lProfile := nil;
  end;
end;

procedure Tfm_Profiles.RadioButton_alphaChange(Sender: TObject);
begin
  if RadioButton_alpha.Checked then
  begin
    UpdateSelector(versions, ComboBox_selVersion, 'old_alpha');
  end;
end;

procedure Tfm_Profiles.RadioButton_betaChange(Sender: TObject);
begin
  if RadioButton_beta.Checked then
  begin
    UpdateSelector(versions, ComboBox_selVersion, 'old_beta');
  end;
end;

procedure Tfm_Profiles.RadioButton_releaseChange(Sender: TObject);
begin
  if RadioButton_release.Checked then
  begin
    UpdateSelector(versions, ComboBox_selVersion, 'release');
  end;
end;

procedure Tfm_Profiles.RadioButton_snapshotChange(Sender: TObject);
begin
  if RadioButton_snapshot.Checked then
  begin
    UpdateSelector(versions, ComboBox_selVersion, 'snapshot');
  end;
end;

procedure Tfm_Profiles.UpdateSelector(vList: TVersionList; Box: TComboBox;
  Channel: String);
var
  I: Integer;
begin
  Box.Enabled := False;
  Box.Items.BeginUpdate;
  Box.ItemIndex := -1;
  Box.Text := 'select Version';
  Box.Items.Clear;
  for I := 0 to vList.Count - 1 do
  begin
    if vList.v[I].vType = Channel then
    begin
      Box.Items.Add(vList.v[I].id);
    end;
  end;
  Box.Items.EndUpdate;
  Box.Enabled := True;
end;

procedure Tfm_Profiles.crMc_Forge;
var
  Download: TDownLoadThread;
  Strs: TStringList;
begin
  Log(etDebug, 'Creating new Vanilla profile', 1);
  PageControl_main.ActivePage := TabSheet_crMC_Forge;
  GroupBox_createMC_Forge.Enabled := False;
  self.Show;

  Log(etDebug, 'Realoc. prog.bar', 1);
  ProgressBar_progress.Height := FProgBar_progress.Height;
  ProgressBar_progress.Width := FProgBar_progress.Width;
  ProgressBar_progress.Top := FProgBar_progress.Top;
  ProgressBar_progress.Left := FProgBar_progress.Left;
  ProgressBar_progress.Style := pbstMarquee;

  Log(etInfo, 'Retreving Index', 3);
  status.Text := 'Retreving version index...';
  Download := TDownLoadThread.Create(True);
  Download.URL := LWebSettings.download_index;
  Download.Destination := PC_CommonPaths.pathsBN['temp'].path + '/Index_Vanilla.json';

  FProgBar_progress.MaxValue := 100;
  FProgBar_progress.MinValue := 0;
  ProgressBar_progress.Hide;

  Download.Start;
  while not Download.HasTerminated do
  begin
    FProgBar_progress.Value := Download.Percentual;
    Self.Update;
  end;
  FreeAndNil(Download);
  ProgressBar_progress.Show;
  FProgBar_progress.Value := 0;

  Strs := TStringList.Create;
  Strs.LoadFromFile(PC_CommonPaths.pathsBN['temp'].path + '/Index_Vanilla.json');
  versions := TVersionList.Create(False);

  Log(etInfo, 'Extracting versions...', 3);
  status.Text := 'Extracting versions';
  json.DecodeVersions(Strs.Text, versions);
  Log(etInfo, 'Done', 3);

  RadioButton_release.Checked := True;
  status.Text := 'Please create your Profile :)';
  GroupBox_createMC_Forge.Enabled := True;

end;

procedure Tfm_Profiles.crPredef;
var
  ConType: String;
  URL: String;
  lPath: String;
  DThread: TDownLoadThread;
  P: TJSONParser; //Parser
  D: TJSONData; //Data : level 1
  Ar: TJSONArray; //Array : level 2
  Ar2: TJSONArray; //Array : level 3
  iContent: TStringList; //indexContent
  I: Integer;
  //Profile Data
  //pName, pVCapt, pBranch, pOwner, pIndexURL, pComment : String;
  //pVersion : Integer;
  lObj: TRemProfileObj;
begin

  Log(etInfo, 'Creating new Predefined profile', 2);
  PageControl_main.ActivePage := TabSheet_crPredef;
  GroupBox_crPredef.Enabled := False;
  self.Show;

  Log(etDebug, 'Realoc. prog.bar', 1);
  ProgressBar_progress.Height := FProgBar_progress.Height;
  ProgressBar_progress.Width := FProgBar_progress.Width;
  ProgressBar_progress.Top := FProgBar_progress.Top;
  ProgressBar_progress.Left := FProgBar_progress.Left;
  ProgressBar_progress.Style := pbstMarquee;

  ConType := LWebSettings.INI.ReadString('Webservice', 'type', 'indexFile');
  URL := LWebSettings.INI.ReadString('Webservice', 'url',
    'http://gfflauncher.life4yourgames.de/profiles/premadeProfiles.json');
  lPath := LWebSettings.INI.ReadString('Webservice', 'localPath',
    '%temp%/premadeProfiles.json');
  lPath := StringReplace(lPath, '%temp%', PC_CommonPaths.pathsBN['temp'].path,
    [rfIgnoreCase, rfReplaceAll]);
  if (FileExists(lPath)) then
    DeleteFile(lPath);
  Log(etInfo, 'Type: ' + ConType, 2);

  if (ConType = 'indexFile') then
  begin
    DThread := TDownLoadThread.Create(True);
    DThread.URL := URL;
    DThread.Destination := lPath;

    FProgBar_progress.MaxValue := 100;
    FProgBar_progress.MinValue := 0;
    FProgBar_progress.Value := 0;
    ProgressBar_progress.Visible := False;
    status.Text := 'Downloading Index';

    DThread.Start;

    while (DThread.HasTerminated = False) do
    begin
      status.Text := DThread.Status;
      FProgBar_progress.Value := DThread.Percentual;
      Update;
      //Sleep(400);
    end;

    FreeAndNil(DThread);
    //ProgressBar_progress.Show;
  end
  else
  begin
    ShowMessage('Invalid Index-Type !');
  end;

  { TODO 88 -oL4YG -cPredefProf : Type - IndexServer }

  if (FileExists(lPath)) then
  begin
    try

      PredProfiles := TRemProfileCollection.Create;

      iContent := TStringList.Create;
      iContent.LoadFromFile(lPath);
      Log(etDebug, 'String: ' + LineEnding + iContent.Text, 0);

      P := TJSONParser.Create(iContent.Text);
      D := P.Parse;
      Ar := TJSONArray(TJSONObject(D).Extract('profiles'));

      ProgressBar_progress.Hide;
      status.Text := 'Reading Profiles';
      FProgBar_progress.Value := 0;
      FProgBar_progress.MaxValue := TJSONObject(Ar).Count;

      for I := 0 to (Ar.Count - 1) do
      begin

        Log(etDebug, 'JSONString: ' + LineEnding + JSONToString(D), 0);

        status.Text := 'Reading Profiles.. (' +
          (TJSONObject(Ar.Items[I]).Get('name')) + ')';

        lObj := TRemProfileObj.Create;
        lObj.ProfName := (TJSONObject(Ar.Items[I]).Get('name'));
        lObj.version := (TJSONObject(Ar.Items[I]).Get('version'));
        lObj.vCapt := (TJSONObject(Ar.Items[I]).Get('vCapt'));
        lObj.branch := (TJSONObject(Ar.Items[I]).Get('branch'));
        lObj.ProfOwner := (TJSONObject(Ar.Items[I]).Get('owner'));
        lObj.indexURL := (TJSONObject(Ar.Items[I]).Get('indexURL'));
        lObj.comment := (TJSONObject(Ar.Items[I]).Get('comment'));
        lObj.Enabled := StrToBoolean(TJSONObject(Ar.Items[I]).Get('enabled'), False);
        lObj.disableReason := (TJSONObject(Ar.Items[I]).Get('disableReason'));

        PredProfiles.Add(lObj);

        Log(etInfo, 'Added profile "' + lObj.ProfName + '"', 3);

        lObj := nil;

        FProgBar_progress.Value := I + 1;

      end;

      FreeAndNil(P);
      FreeAndNil(D);
      FreeAndNil(Ar);
      FreeAndNil(iContent);

      PredProfiles.getNames(ListBox_profiles.Items);
      Memo_descr.Clear;

      GroupBox_crPredef.Enabled := True;
      ProgressBar_progress.Show;

    except
      ON E: Exception do
      begin
        ShowMessage('An Exception occured while reading the index!' +
          LineEnding + E.ClassName + LineEnding + E.Message);
        if Assigned(lObj) then
          FreeAndNil(lObj);
        if Assigned(iContent) then
          FreeAndNil(iContent);
        fm_Profiles.Hide;
      end;
    end;
  end
  else
  begin
    ShowMessage('Sorry - Couldn' + #39 + 't find Index-File ! D:');
    fm_Profiles.Hide;
  end;

end;

procedure Tfm_Profiles.InstallProfile(Profile: TRemProfileObj);
var
  DThread: TDownLoadThread;
  P: TJSONParser; //Parser
  D: TJSONData; //Data : level 1
  jObj: TJSONObject; //Object : level 2
  Ar: TJSONArray; //Array : level 3
  iContent: TStringList; //indexContent
  I: Integer;
  lURL, lDest, lExtr, lName: String;
  lIndex: String;
  stdRep: TReplaceFlags;
begin

  stdRep := [rfIgnoreCase, rfReplaceAll];
  lIndex := LUser.ProfilePath + '/' + Profile.ProfName + '/' + ExtractFileName(
    StringReplace(Profile.indexURL, '%S%', '/', stdRep));

  DThread := TDownLoadThread.Create(True);
  DThread.URL := StringReplace(Profile.indexURL, '%S%', '/', stdRep);
  DThread.Destination := lIndex;

  status.Text := 'Downloading index of ' + Profile.ProfName;
  ProgressBar_progress.Hide;
  FProgBar_progress.Value := 0;
  FProgBar_progress.MaxValue := 100;
  FProgBar_progress.MinValue := 0;

  DThread.Start;

  while (DThread.HasTerminated = False) do
  begin
    status.Text := 'Downloading index of ' + Profile.ProfName + ' (' +
      IntToStr(DThread.Percentual) + '%)';
    Update;
    Sleep(40);
  end;

  FreeAndNil(DThread);

  if (FileExists(lIndex) = True) then
  begin
    try
      status.Text := 'Reading Index';
      iContent := TStringList.Create;
      iContent.LoadFromFile(lIndex);

      P := TJSONParser.Create(iContent.Text);
      D := P.Parse;
      jObj := TJSONObject(TJSONObject(D).Extract(Profile.ProfName));
      Ar := (TJSONArray((TJSONObject(jObj).Extract('files'))));

      for I := 0 to (TJSONObject(Ar).Count - 1) do
      begin

        lExtr := '';

        if (TJSONObject(Ar.Items[I]).Get('type') = 'ARCHIVE') then
        begin
          lExtr := (TJSONObject(Ar.Items[I]).Get('ExtractTo'));
        end;
        lExtr := StringReplace(lExtr, '%profile_home%',
          LUser.ProfilePath + '/' + Profile.ProfName, stdRep);
        lExtr := StringReplace(lExtr, '%S%', '/', stdRep);

        lDest := (TJSONObject(Ar.Items[I]).Get('dest'));
        lDest := StringReplace(lDest, '%profile_home%',
          LUser.ProfilePath + '/' + Profile.ProfName, stdRep);
        lDest := StringReplace(lDest, '%S%', '/', stdRep);

        lURL := (TJSONObject(Ar.Items[I]).Get('url'));
        lURL := StringReplace(lURL, '%S%', '/', stdRep);

        lName := (TJSONObject(Ar.Items[I]).Get('name'));
        status.Text := 'File: '+lName;

        Log(etDebug, 'New File:'+lName+LineEnding+
        lExtr+LineEnding+
        lDest+LineEnding+
        lURL+LineEnding
        ,2);

        DThread := TDownLoadThread.Create(True);
        DThread.URL := lURL;
        DThread.Destination := lDest;
        if (lExtr <> '') then
          DThread.ExtractTo := lExtr;

        ProgressBar_progress.Visible := False;
        FProgBar_progress.MinValue := 0;
        FProgBar_progress.Value := 0;
        FProgBar_progress.MaxValue := 100;

        DThread.Start;

        while (DThread.HasTerminated = False) do
        begin
          status.Text := DThread.Status+' ('+IntToStr(DThread.Percentual)+'%)';
          FProgBar_progress.Value := DThread.Percentual;
          Update;
          Sleep(80);
        end;
        status.Text := 'Done';
      end;

      FreeAndNil(P);
      FreeAndNil(D);
      FreeAndNil(jObj);
      FreeAndNil(Ar);
      FreeAndNil(iContent);

      status.Text := 'not set';
      fm_Launcher.IButton_refrProfilesClick(fm_Profiles);
      Close;
    except
          ON E:Exception do begin
            ShowMessage('Exception '+LineEnding+E.ClassName+LineEnding+E.Message);
          end;
    end;
  end
  else
  begin
    ShowMessage('Unable to download index :(');
    Button_cancelClick(Button_install);
  end;

end;

end.
