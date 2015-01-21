unit fmProfiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, BGRAFlashProgressBar, downloads, obj_versions, json,
  IniFiles,
  { JSON } jsonparser, fpjson, jsonUtils,
  { Profiles } obj_remProfiles,
  { Utils } appUtils,
  remProfiles,
  obj_download
  , types;

type

  { Tfm_Profiles }

  Tfm_Profiles = class(TForm)
    Button_cancelPred: TButton;
    Button_install: TButton;
    Button_ok: TButton;
    Button_cancel: TButton;
    CheckBox_fupdateAllowed: TCheckBox;
    CheckBox_frdy: TCheckBox;
    CheckBox_enabled: TCheckBox;
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
    LabeledEdit_ppath: TLabeledEdit;
    LabeledEdit_pName: TLabeledEdit;
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
    TabSheet_editProfile: TTabSheet;
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
    procedure RefrStatus(step, min, max: Integer; sStatus: String);
    procedure EnablePredef;
  private
    { private declarations }
    versions: TVersionList;
    PredProfiles: TRemProfileCollection;
    AllowSwitch: Boolean;
    Setup: TRemoteProfileInstaller;
    index: TRemoteIndex;
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
  AllowSwitch := True;

  Image_background_crMC_Forge.Picture := fm_Launcher.Image_background.Picture;
end;

procedure Tfm_Profiles.ListBox_profilesSelectionChange(Sender: TObject; User: boolean);
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
    INI.WriteBool('Profile', 'updateDisabled', False);
    //INI.WriteString('Minecraft', 'launchArgs', LabeledEdit_javaArgs.Text);
    INI.WriteString('Minecraft', 'launchArgs_64',LabeledEdit_javaArgs.Text);
    INI.WriteString('Minecraft', 'launchArgs_32',LabeledEdit_javaArgs.Text);
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
  if Assigned(Setup) then
    FreeAndNil(Setup);


  if Assigned(index) then
    FreeAndNil(index);

  fm_Launcher.IButton_refrProfilesClick(fm_Profiles);
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
  Download: TDownloadObj;
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
  Download := TDownloadObj.Create;
  Download.URL := LWebSettings.download_index;
  Download.Destination := PC_CommonPaths.pathsBN['temp'].path + '/Index_Vanilla.json';

  FProgBar_progress.MaxValue := 100;
  FProgBar_progress.MinValue := 0;
  ProgressBar_progress.Hide;

  Download.OnStatusUpdate := @RefrStatus;

  Download.Start;

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
  DThread: TDownLoadThread;
  Download : TDownloadObj;
  url, dest: String;
  iContent: TStringList;
begin
  Log(etDebug, 'Creating new Predefined profile', 1);
  PageControl_main.ActivePage := TabSheet_crPredef;
  GroupBox_crPredef.Enabled := False;
  self.Show;

  Log(etDebug, 'Realoc. prog.bar', 1);
  ProgressBar_progress.Height := FProgBar_progress.Height;
  ProgressBar_progress.Width := FProgBar_progress.Width;
  ProgressBar_progress.Top := FProgBar_progress.Top;
  ProgressBar_progress.Left := FProgBar_progress.Left;
  ProgressBar_progress.Style := pbstMarquee;

  Log(etInfo, 'Retreving Index', 3);
  url := LWebSettings.INI.ReadString('Webservice', 'url',
    'http://gfflauncher.life4yourgames.de/profiles/premadeProfiles.json');
  dest := PC_CommonPaths.pathsBN['temp'].path + '/predefIndex.json';

  DeleteFile(dest);

  Download := TDownloadObj.Create;
  Download.URL := url;
  Download.Destination := dest;

  RefrStatus(0, 0, 1, 'Retreving Index');

  Download.OnStatusUpdate := @RefrStatus;

  Download.Start;

  FreeAndNil(Download);

  iContent := TStringList.Create;
  iContent.LoadFromFile(dest);

  index := TRemoteIndex.Create(iContent.Text);

  FreeAndNil(iContent);

  index.Parse;

  PredProfiles := index.prf;

  PredProfiles.getNames(ListBox_profiles.Items);

  GroupBox_crPredef.Enabled := True;
  RefrStatus(0, 0, 1, 'Select a profile');
end;

procedure Tfm_Profiles.InstallProfile(Profile: TRemProfileObj);
begin
  GroupBox_crPredef.Enabled := False;

  Setup := TRemoteProfileInstaller.Create;
  Setup.Profile := Profile;
  Setup.SyncProc := @RefrStatus;
  Setup.OnFinish := @EnablePredef;
  Setup.Start;
end;

procedure Tfm_Profiles.RefrStatus(step, min, max: Integer; sStatus: String);
begin
  if (AllowSwitch = True) then
    if (step = 0) then
      ProgressBar_progress.Visible := True;

  if (AllowSwitch = True) then
    if (step <> 0) then
      ProgressBar_progress.Visible := False;

  FProgBar_progress.MinValue := min;
  FProgBar_progress.MaxValue := max;
  FProgBar_progress.Value := step;

  status.Text := sStatus;

  Update;
end;

procedure Tfm_Profiles.EnablePredef;
begin
  GroupBox_crPredef.Enabled := True;
end;

end.
