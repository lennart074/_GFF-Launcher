Unit startup;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,

  { Main unit } gfflauncher,
  { Objects }objcollection,
  { Dynamics }dynamics;

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
    Procedure ReloadDynamics;
    Procedure FormShow(Sender: TObject);
    Procedure Timer_hideFormTimer(Sender: TObject);
  Private
    { private declarations }
    StartupCompleted: Boolean;
  Public
    { public declarations }
    CloseOption: String;
    tempStr: String;
  End;

Var
  Form_startup: TForm_startup;
  WebSettings: TWebSettings;
  PC_ParentalDirs, PC_ParentalFiles, PC_Settings, PC_CommonPaths,
  PC_Images: TPathCollection;

Implementation

Uses settings, launcher, login, errorhandler;
//Statements positioned here to avoid circular unit references!

{$R *.lfm}

{ TForm_startup }

Procedure TForm_startup.FormShow(Sender: TObject);
var
  CurrentDir: String;
  i: Integer;
Begin
  try
    tempStr := 'CurrentDir';
    CurrentDir := Application.GetOptionValue('-GFFUseDir');
    if ((Length(CurrentDir) <= 3) or (DirectoryExists(CurrentDir) = False)) then
    begin
      SetCurrentDir(SysUtils.GetEnvironmentVariable('appdata') + '\');
    end
    else
    begin
      SetCurrentDir(CurrentDir);
    end;
    //Use AppData (<drive>:\Users\<user>\AppData\Roaming\)
    //if not specified otherwise

    ReloadDynamics;

    tempStr := 'Forms';
    Application.CreateForm(TForm_settings, Form_settings);
    Application.CreateForm(TForm_login, Form_login);
    Timer_hideForm.Enabled := True;
    StartupCompleted := True;
  except
    on E: Exception do
    begin
      Form_error.Handle(E, 'Fatal Startup Error ! Please check your installation!' +
        LineEnding + 'Please contact the developer(s) if you think this shouldn' +
        #39 + 't happen!' + LineEnding + 'Debug info: ' + tempStr, True);
    end;
  end;
End;

Procedure TForm_startup.Formclose(Sender: TObject; Var Closeaction: Tcloseaction);
Begin
  If Not (Pos('NoDEL', CloseOption) > 0) Then
  Begin
    DeleteDirectory(PC_ParentalDirs.paths[PC_ParentalDirs.IndexOfName['temp']].path, True);
  End;
End;

Procedure TForm_startup.ReloadDynamics;
var
  I: Integer;
begin
  try
    tempStr := 'ParentalDirs';
    PC_ParentalDirs := TPathCollection.CreateByINISec(
      'GFFLauncher/settings/paths.ini', 'ParentalDirs');
    for i := 0 to (PC_ParentalDirs.Count - 1) do
    begin
      if not (DirectoryExists(PC_ParentalDirs.paths[i].path)) then
      begin
        CreateDir(PC_ParentalDirs.paths[i].path);
      end;
    end;

    tempStr := 'ParentalFiles';
    PC_ParentalFiles := TPathCollection.CreateByINISec(
      'GFFLauncher/settings/paths.ini', 'PC_ParentalFiles');
    for i := 0 to (PC_ParentalFiles.Count - 1) do
    begin
      if not (FileExists(PC_ParentalFiles.paths[i].path)) then
      begin
        raise (EMissingFileException.Create('File "' +
          PC_ParentalFiles.paths[i].Name + '" is marked as parental but missing!' +
          LineEnding + 'Please check your installation and/or Path-Settings!' +
          LineEnding + 'Path: ' + PC_ParentalFiles.paths[i].path));
      end;
    end;

    tempStr := 'Images';
    PC_Images := TPathCollection.CreateByINISec(
      'GFFLauncher/settings/paths.ini', 'Images');

    tempStr := 'CommonPaths';
    PC_CommonPaths := TPathCollection.CreateByINISec(
      'GFFLauncher/settings/paths.ini', 'CommonPaths');

    tempStr := 'Settings';
    PC_Settings := TPathCollection.CreateByINISec(
      'GFFLauncher/settings/paths.ini', 'Settings');

    tempStr := 'Websettings';
    WebSettings := TWebSettings.Create(
      PC_Settings.paths[PC_Settings.IndexOfName['webSettings']].path);
  except
    on E: Exception do
    begin
      Form_error.Handle(E, 'Fatal Recreate/Startup Error ! Please check your installation!'
        + LineEnding + 'Please contact the developer(s) if you think this shouldn' +
        #39 + 't happen!' + LineEnding + 'Debug info: ' + tempStr, True);
    end;
  end;

end;

Procedure TForm_startup.Timer_hideFormTimer(Sender: TObject);
Begin
  If (StartupCompleted = True) Then
  Begin
    Form_startup.Hide;
    Timer_hideForm.Enabled := False;
  End;
End;

End.
