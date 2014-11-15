Unit setupMC;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Fileutil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls,
  { Thread for setting up mc }StartThread,
  { Utils } unit_appUtils
  ;

Type

  { TForm_setupMC }

  TForm_setupMC = Class(TForm)
    Image_background: TImage;
    Label_loading: TLabel;
    ProgressBar_loading: TProgressbar;
    StaticText_threadAction: Tstatictext;
    Timer_threadWatch: Ttimer;
    procedure Formcreate(Sender: Tobject);
    Procedure Show;
    Procedure FormClose(Sender: TObject; Var Closeaction: TCloseaction);

    Procedure SetupMC(Profile: String);
    procedure Timer_threadwatchtimer(Sender: Tobject);
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

uses { UserInformation }login,
  { import webSettings }startup
;

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

procedure TForm_setupmc.Formcreate(Sender: Tobject);
begin
  if (UsrObj.username='baerchen1966') then begin
    Form_setupMC.Caption := 'Installiere und starte Apfelkuchen ;D ';
  end;
End;

procedure TForm_setupMC.SetupMC(Profile:String);
begin
  if FileExists(UsrObj.GFFProfilePath+'/'+Profile+'/profileIndex.ini') then begin
     SetupThread:=TStartThread.Create(True);
     SetupThread.ProfilePath := UsrObj.GFFProfilePath+'/'+Profile;
     SetupThread.profileSettingsFile := (UsrObj.GFFProfilePath+'/'+Profile+'/profileIndex.ini');
     SetupThread.webSettingsFile := WebSettings.settingsFile;
     Timer_threadWatch.Enabled := True;
     Application.ProcessMessages;
     SetupThread.Start;
  end else begin
    ShowMessage('Profile:'+Profile+': not found!'+LineEnding+'How did you manage to get this ?!');
  end;
end;

procedure Tform_setupmc.Timer_threadwatchtimer(Sender: Tobject);
begin
    if (Assigned(SetupThread)) then begin
      if (SetupThread.HasErrored<>False) and (SetupThread.HasTerminated<>False) then begin
        Timer_threadWatch.Enabled := False;
        ShowMessage(SetupThread.ErrorMsg);
        Form_setupMC.Close;
        //FreeAndNil(SetupThread);
      end;
      if (SetupThread.CurrentAction='Thread finished!') then begin
         Form_setupMC.Close;
         FreeAndNil(SetupThread);
      end;
      Application.ProcessMessages;
    end;
End;

End.

