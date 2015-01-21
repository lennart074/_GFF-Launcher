unit fmLauncher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, BCImageButton, BGRAFlashProgressBar, BGRASpriteAnimation,
  obj_profiles, appUtils, Minecraft_Setup, syncobjs, LCLType;

type

  { Tfm_Launcher }

  Tfm_Launcher = class(TForm)
    BGRASpriteAnimation1: TBGRASpriteAnimation;
    Button_delete: TButton;
    CheckBox_excludeAssets: TCheckBox;
    CheckBox_fUpdate: TCheckBox;
    ComboBox_profiles: TComboBox;
    FlashPBar_setup: TBGRAFlashProgressBar;
    Button_start: TButton;
    Button_open: TButton;
    Button_edit: TButton;
    Button_profiles: TButton;
    Button_changelog: TButton;
    Button_galaxycraft: TButton;
    Button_info: TButton;
    Button_lic: TButton;
    Button_create: TButton;
    GroupBox_setup: TGroupBox;
    GroupBox_selSheet: TGroupBox;
    GroupBox_info: TGroupBox;
    GroupBox_create: TGroupBox;
    IButton_settings: TBCImageButton;
    IButton_logout: TBCImageButton;
    IButton_refrProfiles: TBCImageButton;
    Image_background_selSheet: TImage;
    Image_background_info: TImage;
    Image_backgroundTop: TImage;
    Image_background: TImage;
    Image_backgroundBot: TImage;
    Label_unfinished1: TLabel;
    Label_running: TLabel;
    Label_profName: TLabel;
    Label_setupStatus: TLabel;
    LabeledEdit_copyRight: TLabeledEdit;
    LabeledEdit_username: TLabeledEdit;
    LabeledEdit_version: TLabeledEdit;
    Label_unfinished2: TLabel;
    Label_unfinished3: TLabel;
    Memo1: TMemo;
    Memo_license: TMemo;
    PageControl_main: TPageControl;
    Panel_topBar: TPanel;
    Panel_botBar: TPanel;
    ProgressBar_setupStatus: TProgressBar;
    RadioButton_vanilla: TRadioButton;
    RadioButton_predefined: TRadioButton;
    TabSheet_galaxycraft: TTabSheet;
    TabSheet_info: TTabSheet;
    TabSheet_lic: TTabSheet;
    TabSheet_changelog: TTabSheet;
    TabSheet_profiles: TTabSheet;
    ToggleBox_cats: TToggleBox;
    ToggleBox_proDescr: TToggleBox;
    ToggleBox_create: TToggleBox;
    procedure Button_deleteClick(Sender: TObject);
    procedure Button_editClick(Sender: TObject);
    procedure Button_galaxycraftClick(Sender: TObject);
    procedure Button_startClick(Sender: TObject);
    procedure Button_changelogClick(Sender: TObject);
    procedure Button_createClick(Sender: TObject);
    procedure Button_infoClick(Sender: TObject);
    procedure Button_licClick(Sender: TObject);
    procedure Button_profilesClick(Sender: TObject);
    procedure CheckBox_fUpdateChange(Sender: TObject);
    procedure ComboBox_profilesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IButton_logoutClick(Sender: TObject);
    procedure IButton_refrProfilesClick(Sender: TObject);
    procedure IButton_settingsClick(Sender: TObject);
    procedure Image_backgroundDblClick(Sender: TObject);
    procedure PageControl_mainChange(Sender: TObject);
    procedure PageControl_mainEnter(Sender: TObject);
    procedure TabSheet_infoShow(Sender: TObject);
    procedure TabSheet_licShow(Sender: TObject);
    procedure ToggleBox_catsChange(Sender: TObject);
    procedure ToggleBox_createChange(Sender: TObject);
    procedure ToggleBox_proDescrChange(Sender: TObject);
    procedure UpdateProfiles;
    procedure Disable;
    procedure Enable;
    procedure FreeSetupThread;
  private
    { private declarations }
    StartThread: TMcSetup;
  public
    { public declarations }
  end;

var
  fm_Launcher: Tfm_Launcher;
  Profiles: TProfileCollection;

implementation

{$R *.lfm}

uses { Forms } launcher, fmMain, fmLogin, fmProfiles, fmSettings, fmError, informations;

{ Tfm_Launcher }

procedure Tfm_Launcher.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  fm_Main.Close;
end;

procedure Tfm_Launcher.FormCreate(Sender: TObject);
begin
  PageControl_main.ActivePage := TabSheet_profiles;
  Button_profiles.Enabled := False;
  GroupBox_setup.Visible := False;
end;

procedure Tfm_Launcher.Button_createClick(Sender: TObject);
begin
  Log(etDebug, 'Calling profile creator', 1);
  if (RadioButton_vanilla.Checked) then
  begin
    fm_Profiles.crMc_Forge;
  end;
  if (RadioButton_predefined.Checked) then
  begin
    fm_Profiles.crPredef;
  end;
  ToggleBox_create.Checked := False;
end;

procedure Tfm_Launcher.Button_infoClick(Sender: TObject);
begin
  PageControl_main.ActivePage := TabSheet_info;
  Button_info.Enabled := False;
end;

procedure Tfm_Launcher.Button_licClick(Sender: TObject);
begin
  PageControl_main.ActivePage := TabSheet_lic;
  Button_lic.Enabled := False;
end;

procedure Tfm_Launcher.Button_changelogClick(Sender: TObject);
begin
  ShowMessage('Sorry but currently this Page is just a placeholder' +
    LineEnding + 'We' + #39 +
    're planning on including the feature during the next updates.' +
    LineEnding + 'It will be enabled, as soon as it is finished.' +
    LineEnding + LineEnding + 'Please excuse the possible Inconvience :('
    );
  {PageControl_main.ActivePage := TabSheet_changelog;
  Button_changelog.Enabled := False;}
end;

procedure Tfm_Launcher.Button_startClick(Sender: TObject);
var
  lProfile: TProfileObj;
begin
  if ComboBox_profiles.ItemIndex >= 0 then
  begin

    if (appUtils.getSystemType(True) = 64) and (Pos('x86', LSettings.javaPath) > 0) then
    begin
      ShowMessage('Please remember to set the correct Java-Binary Version');
      Log(etWarning, 'Java and OS Bin.-Type does not match!', 3);
    end;

    lProfile := Profiles.profiles[ComboBox_profiles.ItemIndex];

    GroupBox_setup.Visible := True;
    StartThread := TMcSetup.Create(True, lprofile, LWebSettings);
    StartThread.ForceDownload := CheckBox_fUpdate.Checked;
    StartThread.ExludeAssets := CheckBox_excludeAssets.Checked;
    StartThread.RunOnTerminate := @FreeSetupThread;
    Log(etInfo, 'Starting MC-Setup', 3);
    StartThread.Start;

    ComboBox_profilesChange(Button_start);
  end;
end;

procedure Tfm_Launcher.Button_deleteClick(Sender: TObject);
begin
  DeleteDirectory(Profiles.profiles[ComboBox_profiles.ItemIndex].path, False);
  IButton_refrProfilesClick(Button_delete);
end;

procedure Tfm_Launcher.Button_editClick(Sender: TObject);
begin

end;

procedure Tfm_Launcher.Button_galaxycraftClick(Sender: TObject);
begin
  ShowMessage('Sorry but currently this Page is just a placeholder' +
    LineEnding + 'We' + #39 +
    're planning on including the feature during the next updates.' +
    LineEnding + 'It will be enabled, as soon as it is finished.' +
    LineEnding + LineEnding + 'Please excuse the possible Inconvience :('
    );
end;

procedure Tfm_Launcher.Button_profilesClick(Sender: TObject);
begin
  PageControl_main.ActivePage := TabSheet_profiles;
  PageControl_mainChange(Button_profiles);
  Button_profiles.Enabled := False;
end;

procedure Tfm_Launcher.CheckBox_fUpdateChange(Sender: TObject);
var
  lProf: TProfileObj;
  Checked: Boolean;
begin
  Checked := CheckBox_fUpdate.Checked;
  CheckBox_fUpdate.Checked := False;
  if (ComboBox_profiles.ItemIndex >= 0) and (Checked = True) then
  begin
    lProf := Profiles.profiles[ComboBox_profiles.ItemIndex];

    if (lProf.UpdateDisabled = False) then
    begin
      CheckBox_fUpdate.Checked := Checked;
      CheckBox_excludeAssets.Visible := CheckBox_fUpdate.Checked;
      CheckBox_excludeAssets.Checked := CheckBox_fUpdate.Checked;
    end
    else
    begin
      ShowMessage(
        'Update is disabled for this profile :(' + LineEnding +
        'Please update it manually, if you need to update.' +
        LineEnding + LineEnding +
        'A better update functionallity will follow during future updates' +
        LineEnding + 'Please excuse the inconvience.'
        );
    end;
  end;
  CheckBox_excludeAssets.Visible := CheckBox_fUpdate.Checked;
  if (CheckBox_excludeAssets.Visible = False) then
  begin
    CheckBox_excludeAssets.Checked := False;
  end;
end;

procedure Tfm_Launcher.ComboBox_profilesChange(Sender: TObject);
begin
  if (ComboBox_profiles.ItemIndex <> -1) then
  begin
    Label_profName.Caption :=
      'Name: ' + Profiles.profiles[ComboBox_profiles.ItemIndex].ProfName;
    Label_running.Caption := 'Running: ' + BoolToString(GroupBox_setup.Visible, False);
    Button_delete.Enabled := True;
  end
  else
  begin
    Label_profName.Caption := 'Name: <select a profile>';
    Label_running.Caption := 'Running: <select a profile>';
    Button_delete.Enabled := False;
  end;
end;

procedure Tfm_Launcher.FormShow(Sender: TObject);
var
  lPath: String;
begin
  if (LUser.isLoggedIn = False) and (LUser.isOffline = False) then
  begin
    fm_Launcher.Hide;
    Exit;
  end;

  Log(etDebug, 'Correcting bounds', 1);
  self.Enabled := False;
  self.Height := 400;
  self.Width := 700;

  GroupBox_selSheet.Top := 50;
  GroupBox_selSheet.Left := 2;

  GroupBox_create.Top := 248;
  GroupBox_create.Left := 8;

  GroupBox_info.Top := 248;
  GroupBox_info.Left := 290;

  ProgressBar_setupStatus.Top := FlashPBar_setup.Top;
  ProgressBar_setupStatus.Left := FlashPBar_setup.Left;
  ProgressBar_setupStatus.Height := FlashPBar_setup.Height;
  ProgressBar_setupStatus.Width := FlashPBar_setup.Width;

  GroupBox_setup.Top := Panel_botBar.Top;
  GroupBox_setup.Left := Panel_botBar.Left;
  GroupBox_setup.Height := Panel_botBar.Height;
  GroupBox_setup.Width := Panel_botBar.Width;

  CheckBox_fUpdateChange(fm_Launcher);

  Log(etDebug, 'Updating Objects', 1);
  LabeledEdit_username.Text := LUser.username;

  LabeledEdit_username.Color := clBlack;
  LabeledEdit_username.Font.Color := clLime;
  LabeledEdit_username.EditLabel.Font.Color := clLime;

  Panel_topBar.Color := clBlack;
  Panel_botBar.Color := clBlack;

  ToggleBox_createChange(fm_Launcher);
  ToggleBox_proDescrChange(fm_Launcher);
  ToggleBox_catsChange(fm_Launcher);

  Panel_topBar.Caption := '';
  Panel_botBar.Caption := '';

  Label_profName.Caption := 'Name: <select a profile>';
  Label_running.Caption := 'Running: <select a profile>';
  Button_delete.Enabled := False;
  Button_edit.Enabled := False;
  Button_open.Enabled := False;
  CheckBox_excludeAssets.Visible := False;

  ToggleBox_cats.Left := Round((Panel_topBar.Width / 2) - (ToggleBox_cats.Width / 2));
  ToggleBox_cats.Top := (Panel_topBar.Height - ToggleBox_cats.Height);

  LabeledEdit_version.Text := LAUNCHER_VERSION;
  LabeledEdit_copyRight.Text := COPYRIGHT;

  Log(etDebug, 'Checking image-path "bars"', 1);
  if (PC_Images.IndexOfName['bars'] >= 0) then
  begin
    Log(etDebug, 'Image-entry exists', 1);
    lPath := PC_Images.pathsBN['bars'].path;
    if (FileExists(lPath) = True) then
    begin
      Log(etDebug, 'File exists', 1);
      try
        Image_backgroundTop.Picture.LoadFromFile(lPath);
        Image_backgroundBot.Picture.LoadFromFile(lPath);
        //Image_background_selSheet.Picture.LoadFromFile(lPath);
        //Do not load this, 'cause Image doesn't really fit currently
        GroupBox_selSheet.Caption := '';
        //Remove Caption

        Log(etDebug, 'Images loaded', 2);
      except
        ON E: Exception do
        begin
          Log(etError, 'Unable to load images' + LineEnding +
            E.ClassName + LineEnding + E.Message, 5);
          ShowMessage('Unable to load images:' + LineEnding +
            E.ClassName + LineEnding + E.Message);
        end;
      end;
    end
    else
    begin
      Log(etError, 'Unable to find: ' + lPath, 5);
      ShowMessage('Unable to find: ' + lPath);
    end;
  end
  else
  begin
    Log(etWarning, 'Image-path doesnt exists, skipping', 3);
  end;

  Log(etDebug, 'Checking image-path "settings"', 1);
  if (PC_Images.IndexOfName['settings_x4'] >= 0) then
  begin
    Log(etDebug, 'Image-entry exists', 1);
    lPath := PC_Images.pathsBN['settings_x4'].path;
    if (FileExists(lPath) = True) then
    begin
      Log(etDebug, 'File exists', 1);
      try
        IButton_settings.BitmapFile := lPath;
        IButton_settings.LoadFromBitmapFile;
        Log(etDebug, 'Image loaded', 2);
      except
        ON E: Exception do
        begin
          Log(etError, 'Unable to load images' + LineEnding +
            E.ClassName + LineEnding + E.Message, 5);
          ShowMessage('Unable to load images:' + LineEnding +
            E.ClassName + LineEnding + E.Message);
        end;
      end;
    end
    else
    begin
      Log(etError, 'Unable to find: ' + lPath, 5);
      ShowMessage('Unable to find: ' + lPath);
    end;
  end
  else
  begin
    Log(etWarning, 'Image-path doesnt exists, skipping', 3);
  end;

  Log(etDebug, 'Checking image-path "logout_x4"', 1);
  if (PC_Images.IndexOfName['logout_x4'] >= 0) then
  begin
    Log(etDebug, 'Image-entry exists', 1);
    lPath := PC_Images.pathsBN['logout_x4'].path;
    if (FileExists(lPath) = True) then
    begin
      Log(etDebug, 'File exists', 1);
      try
        IButton_logout.BitmapFile := lPath;
        IButton_logout.LoadFromBitmapFile;
        Log(etDebug, 'Image loaded', 2);
      except
        ON E: Exception do
        begin
          Log(etError, 'Unable to load images' + LineEnding +
            E.ClassName + LineEnding + E.Message, 5);
          ShowMessage('Unable to load images:' + LineEnding +
            E.ClassName + LineEnding + E.Message);
        end;
      end;
    end
    else
    begin
      Log(etError, 'Unable to find: ' + lPath, 5);
      ShowMessage('Unable to find: ' + lPath);
    end;
  end
  else
  begin
    Log(etWarning, 'Image-path doesnt exists, skipping', 3);
  end;

  Log(etDebug, 'Checking image-path "refr_x4"', 1);
  if (PC_Images.IndexOfName['refr_x4'] >= 0) then
  begin
    Log(etDebug, 'Image-entry exists', 1);
    lPath := PC_Images.pathsBN['refr_x4'].path;
    if (FileExists(lPath) = True) then
    begin
      Log(etDebug, 'File exists', 1);
      try
        IButton_refrProfiles.BitmapFile := lPath;
        IButton_refrProfiles.LoadFromBitmapFile;
        Log(etDebug, 'Image loaded', 2);
      except
        ON E: Exception do
        begin
          Log(etError, 'Unable to load images' + LineEnding +
            E.ClassName + LineEnding + E.Message, 5);
          ShowMessage('Unable to load images:' + LineEnding +
            E.ClassName + LineEnding + E.Message);
        end;
      end;
    end
    else
    begin
      Log(etError, 'Unable to find: ' + lPath, 5);
      ShowMessage('Unable to find: ' + lPath);
    end;
  end
  else
  begin
    Log(etWarning, 'Image-path doesnt exists, skipping', 3);
  end;

  Memo_license.Clear;

  Log(etInfo, 'Searching Profiles', 3);
  UpdateProfiles;

  Self.Enabled := True;
  Log(etDebug, 'Updating form', 1);
  self.Update;
  Log(etInfo, 'Done', 1);
end;

procedure Tfm_Launcher.IButton_logoutClick(Sender: TObject);
begin
  Self.Hide;
  FreeAndNil(LUser);
  fm_Login.Show;
end;

procedure Tfm_Launcher.IButton_refrProfilesClick(Sender: TObject);
begin
  IButton_refrProfiles.Enabled := False;
  UpdateProfiles;
  IButton_refrProfiles.Enabled := True;
end;

procedure Tfm_Launcher.IButton_settingsClick(Sender: TObject);
begin
  if not (Assigned(fm_Settings)) then
  begin
    Log(etInfo, 'Creating Form: fm_Settings', 1);
    Application.CreateForm(Tfm_Settings, fm_Settings);
  end;
  fm_Settings.ShowModal;
end;

procedure Tfm_Launcher.Image_backgroundDblClick(Sender: TObject);
begin
  ToggleBox_create.Checked := False;
  ToggleBox_proDescr.Checked := False;
end;

procedure Tfm_Launcher.PageControl_mainChange(Sender: TObject);
begin
  Button_profiles.Enabled := True;
  Button_changelog.Enabled := True;
  Button_galaxycraft.Enabled := True;
  Button_info.Enabled := True;
  Button_lic.Enabled := True;
end;

procedure Tfm_Launcher.PageControl_mainEnter(Sender: TObject);
begin
  ToggleBox_cats.Checked := False;
end;

procedure Tfm_Launcher.TabSheet_infoShow(Sender: TObject);
begin
  ToggleBox_create.Checked := False;
  ToggleBox_proDescr.Checked := False;
  ToggleBox_cats.Checked := False;
end;

procedure Tfm_Launcher.TabSheet_licShow(Sender: TObject);
var
  path: String;
begin
  path := PC_CommonPaths.pathsBN['lic'].path;
  if FileExists(path) then
  begin
    Memo_license.Lines.LoadFromFile(path);
  end;
  ToggleBox_create.Checked := False;
  ToggleBox_proDescr.Checked := False;
  ToggleBox_cats.Checked := False;
end;

procedure Tfm_Launcher.ToggleBox_catsChange(Sender: TObject);
begin
  GroupBox_selSheet.Visible := ToggleBox_cats.Checked;
  with ToggleBox_cats do
  begin
    Caption := 'Categories';
    if Checked then
      Caption := 'Select Category';
  end;
end;

procedure Tfm_Launcher.ToggleBox_createChange(Sender: TObject);
begin
  GroupBox_create.Visible := ToggleBox_create.Checked;
end;

procedure Tfm_Launcher.ToggleBox_proDescrChange(Sender: TObject);
begin
  GroupBox_info.Visible := ToggleBox_proDescr.Checked;
end;

procedure Tfm_Launcher.UpdateProfiles;
var
  DirList: TStringList;
  I: Integer;
begin
  if Assigned(Profiles) then
    FreeAndNil(Profiles);
  Profiles := TProfileCollection.Create;

  if (not DirectoryExists(LUser.ProfilePath)) then
    Exit;

  DirList := TStringList.Create;
  GetSubDirs(LUser.ProfilePath, DirList);

  for I := 0 to (DirList.Count - 1) do
  begin
    if (FileExists(LUser.ProfilePath + '/' + DirList[I] + '/profileIndex.ini')) then
    begin
      Profiles.Add(TProfileObj.Create(LUser.ProfilePath + '/' +
        DirList[I] + '/profileIndex.ini'));
    end;
  end;
  Profiles.getNames(ComboBox_profiles.Items);
end;

procedure Tfm_Launcher.Disable;
begin
  Self.Enabled := False;
end;

procedure Tfm_Launcher.Enable;
begin
  self.Enabled := True;
end;

procedure Tfm_Launcher.FreeSetupThread;
begin
  GroupBox_setup.Hide;
  FreeAndNil(StartThread);
  ComboBox_profilesChange(Button_start);
end;

end.
