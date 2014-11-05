Unit startup;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,

  { Main unit } gfflauncher,
  { Objects }objcollection;

Type

  TWebSettings = Class(gfflauncher.TSettings)

    //procedure ReadFile;
  End;

  { TForm_startup }

  TForm_startup = Class(TForm)
    Image_background: TImage;
    Image_loading: TImage;
    Timer_hideForm: TTimer;
    Procedure Formclose(Sender: TObject; Var Closeaction: Tcloseaction);
    Procedure FormShow(Sender: TObject);
    Procedure Timer_hideFormTimer(Sender: TObject);
  Private
    { private declarations }
    StartupCompleted: Boolean;
  Public
    { public declarations }
    CloseOption: String;
  End;

Var
  Form_startup: TForm_startup;
  WebSettings: TWebSettings;

Implementation

Uses settings, launcher, login, errorhandler;
//Statements positioned like those are to avoid circular unit references!

{$R *.lfm}

{ TForm_startup }

Procedure TForm_startup.FormShow(Sender: TObject);
Begin
  SetCurrentDir(SysUtils.GetEnvironmentVariable('appdata') + '\');
  //Use AppData (<drive>:\Users\<user>\AppData\Roaming\)

  If (DirectoryExists('GFFLauncher/Binaries') = False) Or
    (FileExists('GFFLauncher/settings/webSettings.ini') = False) Then
  Begin
    Raise MissingFileException.Create('Important files are missing! ' +
      LineEnding + 'Please check your installation!');
  End;

  If (FileExists('GFFLauncher/Images/Loading/loading.png') = True) Then
    Image_loading.Picture.LoadFromFile('GFFLauncher/Images/Loading/loading.png');

  If (FileExists('GFFLauncher/Images/Loading/back.png') = True) Then
    Image_background.Picture.LoadFromFile('GFFLauncher/Images/Loading/back.png');

  If (DirectoryExists('GFFLauncher') = False) Then
    CreateDir('GFFLauncher');
  If (DirectoryExists('GFFLauncher/settings') = False) Then
    CreateDir('GFFLauncher/settings');
  If (DirectoryExists('GFFLauncher/Logs') = False) then
    CreateDir('GFFLauncher/Logs');
  if (DirectoryExists('GFFLauncher/temp')) then
    CreateDir('GFFLauncher/temp');

  WebSettings := TWebSettings.Create('GFFLauncher/settings/webSettings.ini');

  Application.CreateForm(TForm_settings, Form_settings);
  Application.CreateForm(TForm_login, Form_login);
  Timer_hideForm.Enabled := True;
  StartupCompleted := True;
End;

Procedure Tform_startup.Formclose(Sender: TObject;
  Var Closeaction: Tcloseaction);
Begin
  If Not (Pos('NoDEL', CloseOption)>0) Then
  Begin
    DeleteDirectory('GFFLauncher/temp', True);
  End;
End;

Procedure TForm_startup.Timer_hideFormTimer(Sender: TObject);
Begin
  If (StartupCompleted = True) Then
  Begin
    Form_startup.Hide;
    Timer_hideForm.Enabled := False;
  End;
End;

End.
