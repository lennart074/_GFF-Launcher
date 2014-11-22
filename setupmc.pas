Unit setupMC;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Fileutil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls,
  { Threads for setting up mc }StartThread,
  { INIFiles } IniFiles,
  { Utils } unit_appUtils,
  { SelfExplaining} log;


Type

  { TForm_setupMC }

  TForm_setupMC = Class(TForm)
    Image_background: TImage;
    Label_loading: TLabel;
    ProgressBar_loading: TProgressbar;
    StaticText_threadAction: Tstatictext;
    Timer_threadWatch: Ttimer;
    Procedure Formcreate(Sender: TObject);
    Procedure Show;
    Procedure FormClose(Sender: TObject; Var Closeaction: TCloseaction);

    Procedure SetupMC(Profile: String);
    Procedure Timer_threadwatchtimer(Sender: TObject);
  Private
    { private declarations }
  Public
    { public declarations }
  End;

Var
  Form_setupMC: TForm_setupMC;
  SetupThread: TStartThread;

Implementation

{$R *.lfm}

Uses { UserInformation }login,
  { import webSettings }startup;

{ TForm_setupMC }

Procedure TForm_setupmc.FormClose(Sender: TObject; Var Closeaction: TCloseAction);
Begin
  If (Assigned(SetupThread) = True) Then
  Begin
    If (SetupThread.HasTerminated <> True) Then
    Begin
      SetupThread.Terminate;
    End;
  End;
End;

Procedure TForm_setupMC.Show;
Begin
  If Not Assigned(Form_setupMC) Then
  Begin
    Application.CreateForm(TForm_setupMC, Form_setupMC);
  End;
  Form_setupMC.Visible := True;
  Form_setupMC.BringToFront;
End;

Procedure TForm_setupmc.Formcreate(Sender: TObject);
Begin
  If (UsrObj.username = 'baerchen1966') Then
  Begin
    Form_setupMC.Caption := 'Installiere und starte Apfelkuchen ;D ';
  End;
End;

Procedure TForm_setupMC.SetupMC(Profile: String);
Begin
  If FileExists(UsrObj.GFFProfilePath + '/' + Profile + '/profileIndex.ini') Then
  Begin
    SetupThread := TStartThread.Create(True);
    SetupThread.ProfilePath := UsrObj.GFFProfilePath + '/' + Profile;
    SetupThread.profileSettingsFile :=
      (UsrObj.GFFProfilePath + '/' + Profile + '/profileIndex.ini');
    SetupThread.webSettingsFile := WebSettings.settingsFile;
    Timer_threadWatch.Enabled := True;
    Application.ProcessMessages;
    log.ClearMC;
    SetupThread.Start;
  End
  Else
  Begin
    ShowMessage('Profile:' + Profile + ': not found!' + LineEnding +
      'How did you manage to get this ?!');
  End;
End;

Procedure Tform_setupmc.Timer_threadwatchtimer(Sender: TObject);
Begin
  If (Assigned(SetupThread)) Then
  Begin
    If (SetupThread.HasErrored <> False) And (SetupThread.HasTerminated <> False) Then
    Begin
      Timer_threadWatch.Enabled := False;
      ShowMessage(SetupThread.ErrorMsg);
      Form_setupMC.Close;
      //FreeAndNil(SetupThread);
    End;
    If (SetupThread.CurrentAction = 'Thread finished!') Then
    Begin
      Form_setupMC.Close;
      FreeAndNil(SetupThread);
    End;
    Application.ProcessMessages;
  End;
End;

End.
