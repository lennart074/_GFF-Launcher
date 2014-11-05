program project_gfflauncher;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, bgrabitmappack, startup, gfflauncher, settings, launcher, login,
  authsystem, errorhandler, objcollection, StartThread,
setupMC, Profiles, jsonWork { you can add units after this };

{$R *.res}

begin
  Application.Title := 'GFF-Launcher';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm_startup, Form_startup);
  Application.CreateForm(TForm_error, Form_error);
  Application.Run;
end.
