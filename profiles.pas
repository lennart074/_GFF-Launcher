Unit Profiles;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Fileutil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls,
  { Loading } setupMC,
  { Online } authsystem,
  { Access WebSettings } startup,
  { JSON }jsonUtils, jsonWork {,jsonparser,fpjson},
  { Errors }errorhandler,
  { INIFiles }IniFiles,
  { JPEG-Support }JPEGLib
  ;

{ DONE 100 -oL4YG -cProfiles : Adding Profiles }
{ TODO 50 -oL4YG -cProfiles : Editing Profiles }
{ TODO 10 -oL4YG -cWish_Profiles : Favourite Profiles }

Type

  { TForm_Profiles }

  TForm_Profiles = Class(TForm)
    Button_editProfile: TButton;
    Button_refresh: TButton;
    Button_delete: TButton;
    Button_cancel: TButton;
    Button_ok: TButton;
    Button_create: TButton;
    CheckBox_forgeReady: Tcheckbox;
    ComboBox_version: Tcombobox;
    Edit_profileName: TEdit;
    GroupBox_channel: Tgroupbox;
    Image_backGround: TImage;
    Label_name: Tlabel;
    Label_soon: TLabel;
    Label_info1: TLabel;
    ListBox_profiles: Tlistbox;
    PageControl_profiles: TPageControl;
    RadioButton_preAlpha: Tradiobutton;
    RadioButton_alpha: Tradiobutton;
    RadioButton_release: Tradiobutton;
    RadioButton_snapshot: Tradiobutton;
    RadioButton_beta: Tradiobutton;
    TabSheet_create: TTabSheet;
    TabSheet_profiles: TTabSheet;
    TabSheet_fav: TTabSheet;
    Procedure Button_refreshClick(Sender: TObject);
    Procedure Button_cancelclick(Sender: TObject);
    Procedure Button_createclick(Sender: TObject);
    Procedure Button_deleteclick(Sender: TObject);
    Procedure Button_okclick(Sender: TObject);
    Procedure CreateProfile();
    Procedure FormClose(Sender: TObject; Var Closeaction: Tcloseaction);
    Procedure FormCreate(Sender: TObject);
    Procedure Listbox_profilesselectionchange(Sender: TObject; User: Boolean);
    Procedure ListProfiles(Directory: String; ToList: TStrings);
    Procedure Radiobutton_alphachange(Sender: TObject);
    Procedure RadioButton_betaChange(Sender: TObject);
    Procedure Radiobutton_prealphachange(Sender: TObject);
    Procedure RadioButton_snapshotChange(Sender: TObject);
    Procedure Radiobutton_releasechange(Sender: TObject);
    Procedure Tabsheet_profilesenter(Sender: TObject);
  Private
    { private declarations }
    betaVersions, alphaVersions, preAlphaVersions, Versions, snapshots: TStringList;
  Public
    { public declarations }
  End;

Var
  Form_Profiles: TForm_Profiles;

Implementation

{$R *.lfm}

Uses { Main Form } launcher,
  { Utils }gfflauncher,
  { Userdata }login;

Procedure TForm_Profiles.CreateProfile();
Var
  IndexURL: String;
  i, i1, i2, i3, i4, i5: Integer;
  sorted: Boolean;
Begin
  If Not Assigned(Form_Profiles) Then
  Begin
    Application.CreateForm(TForm_Profiles, Form_Profiles);
  End;

  Form_setupMC.Show;


  { Load Profiles(\) | Retreve Indexes(X) | etc( ) }

  { BEGIN LOAD PROFILES }
  Form_Profiles.ListProfiles(UsrObj.GFFProfilePath, ListBox_profiles.Items);
  { END LOAD PROFILES }// UNFINISHED?

  { BEGIN RETREVE INDEX }
  IndexURL := startup.WebSettings.INI.ReadString('DownloadLocations',
    'Index', 'http://s3.amazonaws.com/Minecraft.Download/versions/versions.json');
  authsystem.GetFile(IndexURL, 'GFFLauncher/temp/index.json');
  { END RETREVE INDEX }
  jsonwork.listVersions('GFFLauncher/temp/index.json', versions);
  //Split List in Versions
  Try
    For i := (Versions.Count - 1) Downto 0 Do
    Begin
      If (Pos('a', Versions[i]) = 1) And (sorted <> True) Then
      Begin
        alphaVersions.Add(Versions[i]);
        Versions.Delete(i);
        Sorted := True;
      End;
      i1 := i;
      If (Pos('b', Versions[i]) = 1) And (sorted <> True) Then
      Begin
        betaVersions.Add(Versions[i]);
        Versions.Delete(i);
        sorted := True;
      End;
      i2 := i;
      If (Pos('w', Versions[i]) > 0) And (sorted <> True) Then
      Begin
        snapshots.Add(Versions[i]);
        Versions.Delete(i);
        sorted := True;
      End;
      i3 := i;
      If ((Pos('inf', Versions[i]) = 1) Or (Pos('c', Versions[i]) = 1) Or
        (Pos('r', Versions[i]) = 1)) And (sorted <> True) Then
      Begin
        preAlphaVersions.Add(Versions[i]);
        Versions.Delete(i);
        sorted := True;
      End;
      i4 := i;
      If (Pos('pre', Versions[i]) > 0) And (sorted <> True) Then
      Begin
        snapshots.Add(Versions[i]);
        Versions.Delete(i);
        sorted := True;
      End;
      i5 := i;
      sorted := False;
    End;
    //preAlphaVersions.Sort;
    //alphaVersions.Sort;
    //betaVersions.Sort;
    Versions.Sort;
    //snapshots.Sort;
    ComboBox_version.Items := Versions;
  Except
    on E: Exception Do
    Begin
      Form_error.Handle(E, 'C:' + IntToStr(i) + '|M:' +
        IntToStr(Versions.Count - 1) + LineEnding + 'Logpoints:' +
        IntToStr(i1) + '|' + IntToStr(i2) + '|' + IntToStr(i3) + '|' +
        IntToStr(i4) + '|' + IntToStr(i5)
        , False);
    End;
  End;
  Form_setupMC.Hide;
  PageControl_profiles.ActivePage := TabSheet_create;
  TabSheet_create.Show;
  If Form_Profiles.Visible = False Then
  Begin
    Form_Profiles.Show;
  End;
End;

Procedure Tform_profiles.Button_createclick(Sender: TObject);
Begin
  Form_Profiles.CreateProfile();
End;

Procedure Tform_profiles.Button_deleteclick(Sender: TObject);
Begin
  If (ListBox_profiles.SelCount > 0) And (ListBox_profiles.SelCount < 2) Then
  Begin
    DeleteDirectory(UsrObj.GFFProfilePath + '/' +
      ListBox_profiles.GetSelectedText, False);
  End;
End;

Procedure Tform_profiles.Button_okclick(Sender: TObject);
Var
  ini: TIniFile;
Begin

  If ((Edit_profileName.Text <> '') And (Edit_profileName.Text <> ' ')) And
    (Not DirectoryExists(UsrObj.GFFProfilePath + '/' + Edit_profileName.Text)) And
    (ComboBox_version.ItemIndex <> -1) Then
  Begin
    ForceDirectories(UsrObj.GFFProfilePath + '/' + Edit_profileName.Text);
    ini := TIniFile.Create(UsrObj.GFFProfilePath + '/' +
      Edit_profileName.Text + '/profileIndex.ini');
    ini.WriteString('Profile', 'creator', UsrObj.username);
    ini.WriteString('Profile', 'crDate', DateToStr(now));
    ini.WriteString('Profile', 'crTime', TimeToStr(now));
    ini.WriteBool('Profile', 'Forge-Ready', CheckBox_forgeReady.Checked);
    ini.WriteString('Minecraft', 'version', ComboBox_version.Items[
      ComboBox_version.ItemIndex]);
    ini.WriteBool('Minecraft', 'mc-type', True);
    FreeAndNil(ini);

    TabSheet_create.Hide;
    Tabsheet_profilesenter(TabSheet_create);
  End;
End;

Procedure Tform_profiles.Button_cancelclick(Sender: TObject);
Begin
  Form_Profiles.Close;
End;

Procedure Tform_profiles.Button_refreshClick(Sender: TObject);
Begin
  Tabsheet_profilesenter(Button_refresh);
End;

Procedure TForm_Profiles.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  TabSheet_create.Hide;
  DeleteDirectory('GFFLauncher/temp', True);
  Form_launcher.Show;
End;

Procedure TForm_Profiles.FormCreate(Sender: TObject);
Begin
  { Load Profiles(X) | Send them to launcher(X) }
  Form_Profiles.ListProfiles(UsrObj.GFFProfilePath,
    Form_launcher.ComboBox_selectProfile.Items);

  betaVersions := TStringList.Create;
  alphaVersions := TStringList.Create;
  preAlphaVersions := TStringList.Create;
  Versions := TStringList.Create;
  snapshots := TStringList.Create;
End;

Procedure Tform_profiles.Listbox_profilesselectionchange(Sender: TObject;
  User: Boolean);
var
  jpeg : TJPEGImage;
Begin
  Try
    Button_delete.Enabled := Not (ListBox_profiles.GetSelectedText =
      '<your Profiles will appear here>');
    Button_editProfile.Enabled :=
      Not (ListBox_profiles.GetSelectedText = '<your Profiles will appear here>');

    If FileExists(UsrObj.GFFProfilePath + '/' + ListBox_profiles.GetSelectedText +
      '/logo.jpg') Then
    Begin
      { TODO 90 -oL4YG -cWish_Support : Include Feature to load multiple formats}
      Jpeg := TJpegImage.Create;
      Jpeg.LoadFromFile(UsrObj.GFFProfilePath + '/' +
      ListBox_profiles.GetSelectedText+ '/logo.jpg');
      Image_backGround.Picture.Bitmap.Assign(jpeg);
      jpeg.Free;
    End
    Else
    Begin
      Image_backGround.Picture.Clear;
    End;

  Except
    On E: Exception Do
    Begin
      Form_error.Handle(E, 'Module: ListBox_Profiles@SelChange', False);
    End;
  End;
End;

Procedure TForm_Profiles.ListProfiles(Directory: String; ToList: TStrings);
Var
  DirList, TempToList: TStringList;
  I: Integer;
Begin
  DirList := TStringList.Create;
  gfflauncher.GetSubDirectories(Directory, DirList);
  TempToList := TStringList.Create;
  For I := 0 To (DirList.Count - 1) Do
  Begin
    If (FileExists(DirList[I] + '/ProfileIndex.ini')) Then
    Begin
      TempToList.Add(ExtractFileName(DirList[I]));
    End;
  End;
  If (TempToList.Count <> ToList.Count) Then
  Begin
    ToList.Assign(TempToList);
  End;

  TempToList.Free;
  DirList.Free;
End;

Procedure Tform_profiles.Radiobutton_alphachange(Sender: TObject);
Begin
  If (RadioButton_alpha.Checked) Then
  Begin
    ComboBox_version.ItemIndex := -1;
    ComboBox_version.Text := '<select Version>';
    ComboBox_version.Items := alphaVersions;
  End;
End;

Procedure Tform_profiles.RadioButton_betaChange(Sender: TObject);
Begin
  If (RadioButton_beta.Checked) Then
  Begin
    ComboBox_version.ItemIndex := -1;
    ComboBox_version.Text := '<select Version>';
    ComboBox_version.Items := betaVersions;
  End;
End;

Procedure Tform_profiles.Radiobutton_prealphachange(Sender: TObject);
Begin
  If (RadioButton_preAlpha.Checked) Then
  Begin
    ComboBox_version.ItemIndex := -1;
    ComboBox_version.Text := '<select Version>';
    ComboBox_version.Items := preAlphaVersions;
  End;
End;

Procedure Tform_profiles.RadioButton_snapshotChange(Sender: TObject);
Begin
  If (RadioButton_snapshot.Checked) Then
  Begin
    ComboBox_version.ItemIndex := -1;
    ComboBox_version.Text := '<select Version>';
    ComboBox_version.Items := snapshots;
  End;
End;

Procedure Tform_profiles.Radiobutton_releasechange(Sender: TObject);
Begin
  If (RadioButton_release.Checked) Then
  Begin
    ComboBox_version.ItemIndex := -1;
    ComboBox_version.Text := '<select Version>';
    ComboBox_version.Items := Versions;
  End;
End;

Procedure Tform_profiles.Tabsheet_profilesenter(Sender: TObject);
Begin
  Form_Profiles.ListProfiles(UsrObj.GFFProfilePath, ListBox_profiles.Items);
End;

End.
