unit fmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,

  { Main Code-"Thread" } launcher,
  { Informations for the Program } informations;

type

  { Tfm_Main }

  Tfm_Main = class(TForm)
      Timer_hideform: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer_hideformTimer(Sender: TObject);
  private
    { private declarations }
    pStartupFinished : Boolean;
    pNextForm : TForm;
  public
    { public declarations }
    Property NextForm : TForm read pNextForm write pNextForm;
  end;

var
  fm_Main: Tfm_Main;

implementation

{$R *.lfm}

uses { Forms } fmLogin, fmLauncher, fmProfiles, fmSettings, fmError;

{ Tfm_Main }

procedure Tfm_Main.FormCreate(Sender: TObject);
begin
  Log(etInfo,'Setting Directory',1);

  Application.CreateForm(Tfm_Error, fm_Error);

  if (Application.HasOption('UseDir')) then
  //Check if launcher should use other path than "%appData%+'\'+Launcher_Name"
  begin
    SetCurrentDir(Application.GetOptionValue('UseDir'));
  end
  else
  begin
    SetCurrentDir(SysUtils.GetEnvironmentVariable('appdata'));
  end;
  if not (Pos(LAUNCHER_NAME, GetCurrentDir) > 0) then
  //Create Directory for the launcher, if not given
  begin
    ForceDirectories(GetCurrentDir + '/' + LAUNCHER_NAME);
    SetCurrentDir(GetCurrentDir + '/' + LAUNCHER_NAME);
  end;
  Log(etInfo,'Dir:'+GetCurrentDir,1);

  Log(etInfo,'Starting Initialization',1);
  launcher.Initialize;
  Log(etInfo,'Startup finished',1);
  pStartupFinished := True;
end;

procedure Tfm_Main.Timer_hideformTimer(Sender: TObject);
begin
    if (pStartupFinished=True) then begin
      Timer_hideform.Enabled := False;
      fm_Main.Hide;
      if (Assigned(pNextForm)) then begin
        pNextForm.Show;
        pNextForm:=Nil;
      end;
    end;
end;

end.
