unit launcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  { Objects } obj_settings, obj_paths,
  { Application } Forms,
  { WinRegistry } registry;

procedure Initialize;
procedure ChangeFocus(form:TForm);

var
  LSettings: TProgramSettings;
  LWebSettings: TWebSettings;
  PC_ParentalDirs, PC_ParentalFiles, PC_Settings, PC_CommonPaths,
  PC_Images: TPathCollection;
  Log: procedure(Event: TEventType; const MSG: String; LogLevel: Integer) of object;


implementation

uses fmMain, fmLogin, fmLauncher, fmProfiles, fmSettings, fmLog, fmError, informations;

Procedure ReloadDynamics;
var
  I: Integer;
  DoWrite: Boolean;
  tempStr: String;
begin
  PC_ParentalDirs := TPathCollection.CreateByINISec(
    'settings/paths.ini', 'ParentalDirs');
  for I := 0 to (PC_ParentalDirs.Count - 1) do
  begin
    if not (DirectoryExists(PC_ParentalDirs.paths[I].path)) then
    begin
      CreateDir(PC_ParentalDirs.paths[I].path);
    end;
  end;

  PC_ParentalFiles := TPathCollection.CreateByINISec(
    'settings/paths.ini', 'PC_ParentalFiles');
  for i := 0 to (PC_ParentalFiles.Count - 1) do
  begin
    if not (FileExists(PC_ParentalFiles.paths[I].path)) then
    begin
      raise (EMissingFileException.Create('File "' +
        PC_ParentalFiles.paths[I].Name + '" is marked as parental but missing!' +
        LineEnding + 'Please check your installation and/or Path-Settings!' +
        LineEnding + 'Path: ' + PC_ParentalFiles.paths[i].path));
    end;
  end;

  PC_Images := TPathCollection.CreateByINISec('settings/paths.ini', 'Images');

  PC_CommonPaths := TPathCollection.CreateByINISec(
    'settings/paths.ini', 'CommonPaths');

  PC_Settings := TPathCollection.CreateByINISec('settings/paths.ini', 'Settings');

  LWebSettings := TWebSettings.Create(PC_Settings.pathsBN['webSettings'].path);
  LWebSettings.ReadFile;

  DoWrite := not (FileExists(PC_Settings.pathsBN['MainSettings'].path));
  LSettings := TProgramSettings.Create(PC_Settings.pathsBN['MainSettings'].path);
  if (DoWrite) then
  begin
    LSettings.WriteFile;
  end;
  LSettings.ReadFile;
end;

procedure Initialize;
var
  tempStr: String;
begin
  ReloadDynamics;
  fm_Log.Show;
  Log := nil;
  Log := @fm_Log.DoLogAdv;

  Log(etInfo, 'Creating Form: fm_Login', 1);
  Application.CreateForm(Tfm_Login, fm_Login);
  Log(etInfo, 'Creating Form: fm_Launcher', 1);
  Application.CreateForm(Tfm_Launcher, fm_Launcher);
  Log(etDebug, 'Creating Form: fm_Profiles',1);
  Application.CreateForm(Tfm_Profiles, fm_Profiles);

  fm_Main.NextForm := fm_Login;

  if (LSettings.showConsole = False) then
  begin
    Log(etInfo, 'Hiding Console', 1);
    fm_Log.Hide;
  end;
end;

procedure ChangeFocus(form: TForm);
//Set Focus to form by hiding and reshowing
begin
    form.Hide;
    form.Show;
    form:=nil;
end;

end.








