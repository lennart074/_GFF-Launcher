Program project_gfflauncher;

{$mode objfpc}{$H+}

Uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  bgrabitmappack,
  startup,
  gfflauncher,
  settings,
  launcher,
  login,
  authsystem,
  errorhandler,
  objcollection,
  StartThread,
  setupMC,
  Profiles,
  jsonWork,
  consoles,
  Log { you can add units after this };

{$R *.res}

Begin
  Application.Title := 'GFF-Launcher';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm_startup, Form_startup);
  Application.CreateForm(TForm_error, Form_error);
  Application.Createform(Tform_consoles, Form_consoles);
  Application.Run;
End.
