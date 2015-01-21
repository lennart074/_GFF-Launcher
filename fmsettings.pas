unit fmSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin;

type

  { Tfm_Settings }

  Tfm_Settings = class(TForm)
    Button_cancel: TButton;
    Button_ok: TButton;
    Button_reset: TButton;
    Button_browseJava: TButton;
    Label_loglevel: TLabel;
    LabeledEdit_java: TLabeledEdit;
    OpenDialog_java: TOpenDialog;
    SpinEdit_loglevel: TSpinEdit;
    procedure Button_browseJavaClick(Sender: TObject);
    procedure Button_cancelClick(Sender: TObject);
    procedure Button_okClick(Sender: TObject);
    procedure Button_resetClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fm_Settings: Tfm_Settings;

implementation

{$R *.lfm}

uses { Forms } fmMain, fmLog, fmLauncher, fmProfiles, fmLogin, fmError,
  launcher, informations;

{ Tfm_Settings }

procedure Tfm_Settings.FormShow(Sender: TObject);
begin
  if (Pos('_NotFound_', LSettings.javaPath) > 0) then
  begin
    LabeledEdit_java.Text := '<Unable to locate Java - Path!>';
  end
  else
  begin
    LabeledEdit_java.Text := LSettings.javaPath;
  end;
  SpinEdit_loglevel.MaxValue := LAUNCHER_MAXLOGLEVEL;
  SpinEdit_loglevel.Value := LSettings.logLevel;
end;

procedure Tfm_Settings.Button_resetClick(Sender: TObject);
begin
  LSettings.WriteFile;
  LSettings.INI.UpdateFile;
  LSettings.ReadFile;
  FormShow(fm_Settings);
end;

procedure Tfm_Settings.Button_browseJavaClick(Sender: TObject);
begin
  if (Pos('_NotFound_', LSettings.javaPath) <= 0) then
    OpenDialog_java.InitialDir := ExtractFileDir(LSettings.javaPath);
  if (OpenDialog_java.Execute) then
  begin
    LabeledEdit_java.Text := OpenDialog_java.FileName;
    LSettings.INI.WriteString('GFFLauncher', 'javaPath', OpenDialog_java.FileName);
  end;
end;

procedure Tfm_Settings.Button_cancelClick(Sender: TObject);
begin
  LSettings.ReadFile;
  Close;
end;

procedure Tfm_Settings.Button_okClick(Sender: TObject);
begin
  LSettings.INI.WriteInteger('GFFLauncher', 'logLevel', SpinEdit_loglevel.Value);
  LSettings.INI.UpdateFile;
  LSettings.ReadFile;

  if (LSettings.showConsole = True) then
  begin
    fm_log.Show;
    launcher.ChangeFocus(fm_Launcher);
  end
  else
  begin
    fm_Log.Hide;
  end;
  Close;
end;

end.
