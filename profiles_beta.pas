Unit profiles_BETA;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Fileutil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, BCImageButton, types,
  { L.Screen } setupMC,
  { JSON } jsonWork, jsonUtils,
  { appUtils } unit_appUtils,
  { Errors } errorhandler,
  { ObjCollection } objcollection,
  { Userdata } login,
  { Self-Explaining } Log,
  { GFFLauncher } gfflauncher,
  { Paths } startup,
  { Web } authsystem,
  { Settings } settings,
  { INIFiles } IniFiles;

Type

  { ProfileList }

  { TForm_profilesBETA }

  TForm_profilesBETA = Class(TForm)
    BCImageButton_crMC_Forge1: TBCImageButton;
    BCImageButton_crMC_Forge2: TBCImageButton;
    BCImageButton_crMC_Forge3: TBCImageButton;
    BCImageButton_crVanilla: TBCImageButton;
    CheckBox_forgeReady: Tcheckbox;
    ComboBox_channel: Tcombobox;
    Edit_profileName: TEdit;
    Image_crCat_custom: TImage;
    Image_crCat_predefined: TImage;
    Image_background: TImage;
    Label_crMC_Forge1: TLabel;
    Label_crMC_Forge2: TLabel;
    Label_crMC_Forge3: TLabel;
    PageControl_Profiles: TPageControl;
    StaticText_channel: Tstatictext;
    TabSheet_choose: TTabSheet;
    TabSheet_crMC_Forge: TTabSheet;
    TabSheet_crPredef: TTabSheet;
    TabSheet_profileType: TTabSheet;
    TrackBar_current: Ttrackbar;
    procedure BCImageButton_crMC_Forge1Click(Sender: TObject);
    procedure BCImageButton_crMC_Forge2Click(Sender: TObject);
    procedure BCImageButton_crMC_Forge3Click(Sender: TObject);
    procedure BCImageButton_crVanillaClick(Sender: TObject);
    procedure ComboBox_channelChange(Sender: TObject);
    Procedure Formcreate(Sender: TObject);
    Procedure Image_closeclick(Sender: TObject);
    Procedure Pagecontrol_profileschange(Sender: TObject);
    Procedure Pagecontrol_profileschanging(Sender: TObject;
      Var Allowchange: Boolean);
    procedure TabSheet_crMC_ForgeHide(Sender: TObject);
    procedure TrackBar_currentChange(Sender: TObject);
    Procedure WriteProfile(version: String);
    Procedure CreateProfile();
    Procedure LoadProfiles(Directory: String);
    function CheckForProfileChanges(Directory: String): Boolean;
    procedure TabSheet_crMC_ForgeShow(Sender: TObject);
    procedure TabSheet_profileTypeShow(Sender: TObject);
  Private
    { private declarations }
    Favourites, betaVersions, alphaVersions, preAlphaVersions, Versions,
    snapshots, CurrentVList: TStringList;
    CreateNewP: Boolean;
  Public
    { public declarations }
  End;

Var
  Form_profilesBETA: TForm_profilesBETA;
  ProfileList: TProfileCollection;

Implementation

{$R *.lfm}

{ TForm_profilesBETA }

Procedure TForm_profilesBETA.Pagecontrol_profileschange(Sender: TObject);
Begin
  PageControl_Profiles.ActivePage.TabVisible := True;
End;

Procedure TForm_profilesBETA.Formcreate(Sender: TObject);
Var
  i: Integer;
Begin
  For i := 0 To PageControl_Profiles.PageCount - 1 Do
    PageControl_Profiles.Pages[i].TabVisible := False;

  ProfileList := TProfileCollection.Create;
  if (Length(UsrObj.GFFProfilePath) > 1) then
  begin
    LoadProfiles(UsrObj.GFFProfilePath);
  end;

  betaVersions := TStringList.Create;
  alphaVersions := TStringList.Create;
  preAlphaVersions := TStringList.Create;
  Versions := TStringList.Create;
  snapshots := TStringList.Create;
  Favourites := TStringList.Create;
  CurrentVList := TStringList.Create;

  Favourites.Delimiter := ('|');
  Favourites.StrictDelimiter := True;
  Favourites.DelimitedText :=
    MainSettings.INI.ReadString(UsrObj.username, 'Favourites', '');
End;

procedure TForm_profilesBETA.BCImageButton_crMC_Forge1Click(Sender: TObject);
begin
  WriteProfile(Label_crMC_Forge1.Caption);
end;

procedure TForm_profilesBETA.BCImageButton_crMC_Forge2Click(Sender: TObject);
begin
  WriteProfile(Label_crMC_Forge2.Caption);
end;

procedure TForm_profilesBETA.BCImageButton_crMC_Forge3Click(Sender: TObject);
begin
  WriteProfile(Label_crMC_Forge3.Caption);
end;

procedure TForm_profilesBETA.BCImageButton_crVanillaClick(Sender: TObject);
begin
  PageControl_Profiles.ActivePage := TabSheet_crMC_Forge;
end;

procedure TForm_profilesBETA.ComboBox_channelChange(Sender: TObject);
begin
  case ComboBox_channel.Text of
    'Release': CurrentVList := Versions;
    'Snapshot': CurrentVList := snapshots;
    'Beta': CurrentVList := betaVersions;
    'Alpha': CurrentVList := alphaVersions;
    'Pre-Alpha': CurrentVList := preAlphaVersions;
    else
    begin
      ComboBox_channel.ItemIndex := 0;
    end;
  end;
  TrackBar_current.Min := 1;
  TrackBar_current.Position := 1;
  TrackBar_current.Max := CurrentVList.Count;
  TrackBar_currentChange(ComboBox_channel);
end;

Procedure TForm_profilesBETA.Image_closeclick(Sender: TObject);
Begin
  Form_profilesBETA.Close;
End;

Procedure TForm_profilesBETA.Pagecontrol_profileschanging(Sender: TObject;
  Var Allowchange: Boolean);
Begin
  PageControl_Profiles.ActivePage.TabVisible := False;
End;

procedure TForm_profilesBETA.TabSheet_crMC_ForgeHide(Sender: TObject);
begin
  TrackBar_current.Enabled := False;
  TrackBar_current.Visible := False;
end;

procedure TForm_profilesBETA.TrackBar_currentChange(Sender: TObject);
begin
  if (PageControl_Profiles.ActivePage = TabSheet_crMC_Forge) then
  begin
    if (TrackBar_current.Position - 1 >= 1) then
    begin
      Label_crMC_Forge1.Caption := CurrentVList[TrackBar_current.Position - 1];
    end
    else
    begin
      Label_crMC_Forge1.Caption := 'OUT OF BOUNDS';
    end;
    if (CurrentVList.Count - TrackBar_current.Position > 1) then
    begin
      Label_crMC_Forge2.Caption := CurrentVList[TrackBar_current.Position];
    end
    else
    begin
      Label_crMC_Forge2.Caption := 'OUT OF BOUNDS';
    end;
    if (CurrentVList.Count - TrackBar_current.Position > 2) then
    begin
      Label_crMC_Forge3.Caption := CurrentVList[TrackBar_current.Position + 1];
    end
    else
      Label_crMC_Forge3.Caption := 'OUT OF BOUNDS';
  end;
end;

Procedure TForm_profilesBETA.WriteProfile(version: String);
var
  f: TextFile;
  INI: TINIFile;
begin
  If ((Edit_profileName.Text <> '') And (Edit_profileName.Text <> ' ')) And
    (Edit_profileName.Caption <> '<Profilename>') and
    (Not DirectoryExists(UsrObj.GFFProfilePath + '/' + Edit_profileName.Text)) And
    (version <> 'OUT OF BOUNDS') Then
  Begin
    ForceDirectories(UsrObj.GFFProfilePath + '/' + Edit_profileName.Text);
    ini := TIniFile.Create(UsrObj.GFFProfilePath + '/' +
      Edit_profileName.Text + '/profileIndex.ini');
    ini.WriteString('Profile', 'creator', UsrObj.username);
    ini.WriteString('Profile', 'crDate', DateToStr(now));
    ini.WriteString('Profile', 'crTime', TimeToStr(now));
    ini.WriteString('Profile', 'name', Edit_profileName.Text);
    ini.WriteBool('Profile', 'Forge-Ready', CheckBox_forgeReady.Checked);

    ini.WriteString('Minecraft', 'version', version);
    ini.WriteString('Minecraft', 'mc-type', 'MC_Forge');
    FreeAndNil(ini);
    If (CheckBox_forgeReady.Checked) Then
    Begin
      AssignFile(f, UsrObj.GFFProfilePath + '/' + Edit_profileName.Text +
        '/launcher_profiles.json');
      Rewrite(f);
      CloseFile(f);
    End;
    Edit_profileName.Text := '<Profilename>';
    ComboBox_channel.ItemIndex := 0;
    CheckBox_forgeReady.Checked := True;
    Form_profilesBETA.Close;

  end;
end;

Procedure TForm_profilesBETA.CreateProfile;
//Modify this to fit new system !
Var
  IndexURL: String;
  i, i1, i2, i3, i4, i5: Integer;
  sorted: Boolean;
Begin
  If Not Assigned(Form_ProfilesBETA) Then
  Begin
    Application.CreateForm(TForm_ProfilesBETA, Form_ProfilesBETA);
  End;
  PageControl_Profiles.ActivePage := TabSheet_profileType;
  if (Form_profilesBETA.Visible = False) then
  begin
    Form_profilesBETA.Show;
  end;
End;

Procedure TForm_profilesBETA.LoadProfiles(Directory: String);
Var
  DirList: TStringList;
  I: Integer;
  tObj: TProfileObj;
Begin
  DirList := TStringList.Create;
  gfflauncher.GetSubDirectories(Directory, DirList);
  if (ProfileList.Count > 0) then
  begin
    ProfileList.Clear;
  end;

  For I := 0 To (DirList.Count - 1) Do
  Begin
    If (FileExists(DirList[I] + '/ProfileIndex.ini')) Then
    Begin
      ProfileList.Add(TProfileObj.Create(DirList[I] + '/ProfileIndex.ini'));
    End;
  End;
  FreeAndNil(DirList);
End;

function TForm_profilesBETA.CheckForProfileChanges(Directory: String): Boolean;
Var
  DirList: TStringList;
  I: Integer;
  tObj: TProfileObj;
  ProfileListTemp: TProfileCollection;
Begin
  DirList := TStringList.Create;
  gfflauncher.GetSubDirectories(Directory, DirList);
  ProfileListTemp := TProfileCollection.Create;
  For I := 0 To (DirList.Count - 1) Do
  Begin
    If (FileExists(DirList[I] + '/ProfileIndex.ini')) Then
    Begin
      ProfileListTemp.Add(TProfileObj.Create(DirList[I] + '/ProfileIndex.ini'));
    End;
  End;

  if (ProfileListTemp.CollectionIsDifferent(ProfileList, False) = True) then
  begin
    LoadProfiles(UsrObj.GFFProfilePath);
    Result := True;
  end
  else
  begin
    Result := False;
  end;
  FreeAndNil(ProfileListTemp);
  FreeAndNil(DirList);
end;

procedure TForm_profilesBETA.TabSheet_crMC_ForgeShow(Sender: TObject);
Var
  IndexURL: String;
  i, i1, i2, i3, i4, i5: Integer;
  sorted: Boolean;
begin
  if (PC_Images.IndexOfName['crMC_Forge'] <> -1) then
  begin
    if (PC_Images.pathsBN['crMC_Forge'].isExistent) then
    begin
      BCImageButton_crMC_Forge1.BitmapFile := (PC_Images.pathsBN['crMC_Forge'].path);
      BCImageButton_crMC_Forge2.BitmapFile := (PC_Images.pathsBN['crMC_Forge'].path);
      BCImageButton_crMC_Forge3.BitmapFile := (PC_Images.pathsBN['crMC_Forge'].path);
      BCImageButton_crMC_Forge1.LoadFromBitmapFile;
      BCImageButton_crMC_Forge2.LoadFromBitmapFile;
      BCImageButton_crMC_Forge3.LoadFromBitmapFile;
    end;
  end;
  Form_profilesBETA.Enabled := False;
  Form_setupMC.Show;

  IndexURL := startup.WebSettings.INI.ReadString('DownloadLocations',
    'Index', 'http://s3.amazonaws.com/Minecraft.Download/versions/versions.json');
  authsystem.GetFile(IndexURL, 'GFFLauncher/temp/index.json');

  jsonwork.listVersions('GFFLauncher/temp/index.json', versions);

  { TODO 80 -oL4YG -cCrProfile : Split Array into lists using JSON correctly! }
  Try
    For i := (Versions.Count - 1) Downto 0 Do
    Begin
      If (Pos('a', Versions[i]) = 1) And (sorted <> True) Then
      Begin
        alphaVersions.Insert(0, Versions[i]);
        Versions.Delete(i);
        Sorted := True;
      End;
      i1 := i;
      If (Pos('b', Versions[i]) = 1) And (sorted <> True) Then
      Begin
        betaVersions.Insert(0, Versions[i]);
        Versions.Delete(i);
        sorted := True;
      End;
      i2 := i;
      If (Pos('w', Versions[i]) > 0) And (sorted <> True) Then
      Begin
        snapshots.Insert(0, Versions[i]);
        Versions.Delete(i);
        sorted := True;
      End;
      i3 := i;
      If ((Pos('inf', Versions[i]) = 1) Or (Pos('c', Versions[i]) = 1) Or
        (Pos('r', Versions[i]) = 1)) And (sorted <> True) Then
      Begin
        preAlphaVersions.Insert(0, Versions[i]);
        Versions.Delete(i);
        sorted := True;
      End;
      i4 := i;
      If (Pos('pre', Versions[i]) > 0) And (sorted <> True) Then
      Begin
        snapshots.Insert(0, Versions[i]);
        Versions.Delete(i);
        sorted := True;
      End;
      i5 := i;
      sorted := False;
    End;
    //preAlphaVersions.CustomSort(@unit_appUtils.SortDown);
    //alphaVersions.CustomSort(@unit_appUtils.SortDown);
    //betaVersions.CustomSort(@unit_appUtils.SortDown);
    Versions.CustomSort(@unit_appUtils.SortDown);
    //snapshots.CustomSort(@unit_appUtils.SortDown);

    //preAlphaVersions.CustomSort(unit_appUtils.StringListSortDownwards());
    //alphaVersions.CustomSort(unit_appUtils.StringListSortDownwards());
    //betaVersions.CustomSort(unit_appUtils.StringListSortDownwards());
    //Versions.CustomSort(unit_appUtils.StringListSortDownwards());
    //snapshots.CustomSort(unit_appUtils.StringListSortDownwards());

  Except
    on E: Exception Do
    Begin
      Form_profilesBETA.Enabled := True;
      Form_error.Handle(E, 'C:' + IntToStr(i) + '|M:' +
        IntToStr(Versions.Count - 1) + LineEnding + 'Logpoints:' +
        IntToStr(i1) + '|' + IntToStr(i2) + '|' + IntToStr(i3) +
        '|' + IntToStr(i4) + '|' + IntToStr(i5)
        , False);
    End;
  End;

  CurrentVList := Versions;
  Form_profilesBETA.Enabled := True;
  TrackBar_current.Visible := True;
  TrackBar_current.Enabled := True;
  TrackBar_current.Min := 1;
  TrackBar_current.Max := CurrentVList.Count;
  TrackBar_currentChange(TabSheet_crMC_Forge);
  Form_setupMC.Hide;

end;

procedure TForm_profilesBETA.TabSheet_profileTypeShow(Sender: TObject);
begin
  if (PC_Images.IndexOfName['cat_crMC_Forge'] <> -1) then
  begin
    if (PC_Images.pathsBN['cat_crMC_Forge'].isExistent) then
    begin
      BCImageButton_crVanilla.BitmapFile :=
        (PC_Images.pathsBN['cat_crMC_Forge'].path);
      BCImageButton_crVanilla.LoadFromBitmapFile;
    end
    else
    begin
      ShowMessage('Cannot find file p:"cat_crMC_Forge"');
    end;
  end
  else
  begin
    ShowMessage('Cannot find path "cat_crMC_Forge"');
  end;
end;

End.
