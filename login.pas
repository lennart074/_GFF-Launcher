Unit login;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  { Settings } settings,
  { Authsystem } authsystem,
  { Launcher } launcher,
  { Objects } objcollection,
  { Startup } startup,
  { Profiles } Profiles;

Type

  { TForm_login }

  TForm_login = Class(TForm)
    Button_login: TButton;
    Edit_password: TEdit;
    Edit_username: TEdit;
    Image_back: TImage;
    StaticText_info_github: Tstatictext;
    StaticText_info: Tstatictext;
    Procedure Button_loginClick(Sender: TObject);
    Procedure Edit_passwordKeyPress(Sender: TObject; Var Key: Char);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure Formshow(Sender: TObject);
  Private
    { private declarations }
  Public
    { public declarations }
  End;

Var
  Form_login: TForm_login;
  UsrObj: TUserObject;

Implementation

{$R *.lfm}

{ TForm_login }

Procedure TForm_login.FormCreate(Sender: TObject);
Begin

  Form_login.Show;
End;

Procedure Tform_login.Formshow(Sender: TObject);
Begin
  If Assigned(Form_Launcher) Then
  Begin
    FreeAndNil(Form_launcher);
  End;
End;

Procedure TForm_login.Button_loginClick(Sender: TObject);
Var
  url, syntax: String;
Begin
  url := MainSettings.INI.ReadString('Authentication',
    'url', 'https://authserver.mojang.com/authenticate');
  syntax := startup.webSettings.INI.ReadString('Authentication',
    'SendSyntax',
    '{"agent":{"name": "Minecraft","version": 1},"username": "%username%","password": "%password%",}');
  UsrObj := TUserObject.Create;
  If (authsystem.Authenticate(Edit_username.Text, Edit_password.Text,
    syntax, url, authsystem.DIA_ALL) = True) Then
  Begin
    UsrObj.loginName := Edit_username.Text;
    UsrObj.GFFProfilePath := 'GFFLauncher/Profiles/' + UsrObj.username;
    If Not Assigned(Form_launcher) Then
    Begin
      Application.CreateForm(TForm_launcher, Form_launcher);
    End;
    If Not Assigned(Form_Profiles) Then
    Begin
      Application.CreateForm(TForm_Profiles, Form_Profiles);
    End;
    Form_launcher.Timer_checkLogin.Enabled := True;
    Form_launcher.loggedIn := True;
    Form_launcher.Show;
    Form_login.Hide;
    Form_login.Edit_password.Clear;
  End;
End;

Procedure TForm_login.Edit_passwordKeyPress(Sender: TObject; Var Key: Char);
Begin
  If (key = #13) Then
  Begin
    Button_loginClick(Edit_password);
  End;
End;

Procedure TForm_login.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  Form_startup.Close;
End;


End.
