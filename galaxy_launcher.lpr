program galaxy_launcher;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, fmMain, fmLogin, fmLauncher, fmProfiles, fmSettings,
  fmError, launcher, informations, obj_settings, obj_paths, fmLog, mojang,
  obj_user, Transparency, downloads, json, obj_versions, Minecraft_Setup,
obj_profiles, obj_remProfiles, remProfiles, obj_download;

{$R *.res}

begin
  Application.OnException := @fm_Error.CatchError;
  Log := @fm_Log.DoLog;

  RequireDerivedFormResource := True;
  Application.Initialize;
  //Application.ShowMainForm := False;
  Application.CreateForm(Tfm_Main, fm_Main);
  Application.Run;
end.
