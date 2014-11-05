Unit launcher;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus,
  { Main Unit } gfflauncher,
  { Settings } settings,
  { Profiles } Profiles,
  { Web } authsystem;

Type

  { TForm_launcher }

  TForm_launcher = Class(TForm)
    Button_moreProfiles: TButton;
    Button_addProfile: TButton;
    Button_options: TButton;
    Button_logout: TButton;
    Button_start: TButton;
    ComboBox_selectProfile: Tcombobox;
    Image_back: TImage;
    Label_username1: TLabel;
    Label_username: TLabel;
    Timer_synchProfiles: Ttimer;
    Timer_checkLogin: TTimer;
    Procedure Button_addprofileclick(Sender: TObject);
    Procedure Button_logoutClick(Sender: TObject);
    Procedure Button_moreprofilesclick(Sender: TObject);
    Procedure Button_optionsClick(Sender: TObject);
    Procedure Button_startClick(Sender: TObject);
    Procedure Combobox_selectprofilechange(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure Timer_checkLoginTimer(Sender: TObject);
    Procedure Timer_synchprofilestimer(Sender: TObject);
  Private
    { private declarations }
  Public
    { public declarations }
    loggedIn: Boolean;
  End;

Var
  Form_launcher: TForm_launcher;

Implementation

{ Additional forms }Uses startup, login, setupMC;

{$R *.lfm}

{ TForm_launcher }

Procedure TForm_launcher.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  // STILL INCOMPLETE : authsystem.LogOut();
  Form_startup.Close;
End;

Procedure TForm_launcher.Button_logoutClick(Sender: TObject);
Begin
  LoggedIn := False;
  FreeAndNil(Form_Profiles);
  If (Assigned(Form_setupMC)) Then
  Begin
    FreeAndNil(Form_setupMC);
  End;
  UsrObj.Free;
End;

Procedure Tform_launcher.Button_moreprofilesclick(Sender: TObject);
Begin
  Form_Profiles.Show;
End;

Procedure Tform_launcher.Button_addprofileclick(Sender: TObject);
Begin
  Form_launcher.Hide;
  Form_Profiles.CreateProfile;
End;

Procedure TForm_launcher.Button_optionsClick(Sender: TObject);
Begin
  Form_settings.Show;
End;

Procedure Tform_launcher.Button_startClick(Sender: TObject);
Begin
  If (ComboBox_selectProfile.ItemIndex <> -1) Then
  Begin
    Button_start.Enabled := False;
    If (Assigned(Form_setupMC) = False) Then
    Begin
      Application.Createform(Tform_setupmc, Form_setupmc);
    End;
    Form_setupMC.Show;
    Form_setupMC.setupMC(ComboBox_selectProfile.Items[ComboBox_selectProfile.ItemIndex]);
  End;
End;

Procedure Tform_launcher.Combobox_selectprofilechange(Sender: TObject);
Begin
  If (ComboBox_selectProfile.Items[ComboBox_selectProfile.ItemIndex] <>
    '<select Profile>') Then
  Begin
    Button_start.Enabled := True;
  End
  Else
  Begin
    Button_start.Enabled := False;
  End;
End;

Procedure TForm_launcher.FormCreate(Sender: TObject);
Begin
  Form_launcher.Show;
  If (UsrObj.username = 'baerchen1966') Then
  Begin
    Button_start.Caption := 'Starte Apfelkuchen :D';
  End;
End;

Procedure TForm_launcher.FormShow(Sender: TObject);
Begin
  Label_username.Caption := UsrObj.username;
End;

Procedure TForm_launcher.Timer_checkLoginTimer(Sender: TObject);
Begin
  If (loggedIn = False) Then
  Begin
    Button_start.Enabled := False;
    Button_logout.Enabled := False;
    Label_username.Caption := '{missing!}';
    Form_launcher.Hide;
    Timer_checkLogin.Enabled := False;
    Form_login.Show;
  End
  Else
  Begin
    Button_logout.Enabled := True;
  End;
End;

Procedure Tform_launcher.Timer_synchprofilestimer(Sender: TObject);
Begin
  Form_Profiles.ListProfiles(UsrObj.GFFProfilePath,
    ComboBox_selectProfile.Items);
End;

End.
