Unit errorhandler;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm_error }

  TForm_error = Class(TForm)
    Button_kill: TButton;
    Label_detCaption: TLabel;
    Label_caption: TLabel;
    Memo_short: TMemo;
    Memo_details: TMemo;
    ToggleBox_details: TToggleBox;
    Procedure Button_killClick(Sender: TObject);
    Procedure Handle(E: Exception; Details: String = 'X';
      critical: Boolean = False); Overload;
    Procedure Handle(ErrClass, ErrMsg: String; Details: String;
      critical: Boolean = False); Overload;
    Procedure ToggleBox_detailsChange(Sender: TObject);
  Private
    { private declarations }
  Public
    { public declarations }
  End;

Var
  Form_error: TForm_error;

Implementation

Uses { Forms }startup, launcher, settings, login;

{$R *.lfm}

Procedure TForm_error.Handle(E: Exception; Details: String = 'X';
  critical: Boolean = False); Overload;
Begin
  Form_error.Height := 150;
  ToggleBox_details.Checked := False;
  With Memo_short Do
  Begin
    Clear;
    Append('Class:' + E.ClassName);
    Append('');
    Append('Message:' + E.Message);
  End;
  If Details <> 'X' Then
  Begin
    Label_detCaption.Caption :=
      'The following details have been refered by the errored-function:';
    With Memo_details Do
    Begin
      Clear;
      Lines.Delimiter := #13;
      Lines.StrictDelimiter := True;
      Lines.DelimitedText := Details;
    End;
  End
  Else
  Begin
    Label_detCaption.Caption := 'There were no details refered!';
    With Memo_details Do
    Begin
      Clear;
      Append('No Details avaliable');
    End;
  End;
  Memo_details.Enabled := False;
  Memo_short.Enabled := False;
  Memo_details.ReadOnly := True;
  Memo_short.ReadOnly := True;

  If Assigned(Form_startup) Then
  Begin
    Form_startup.Hide;
    Form_startup.Enabled := False;
  End;
  If Assigned(Form_settings) Then
  Begin
    Form_settings.Hide;
    Form_settings.Enabled := False;
  End;
  If Assigned(Form_login) Then
  Begin
    Form_login.Hide;
    Form_login.Enabled := False;
    ShowMessage('');
  End;
  If Assigned(Form_launcher) Then
  Begin
    Form_launcher.Hide;
    Form_launcher.Enabled := False;
  End;

  Form_error.Show;
End;

Procedure TForm_error.Button_killClick(Sender: TObject);
Begin
  Try
    Form_startup.CloseOption:='NoDEL';
    Form_startup.Close;
  Except
    on E: Exception Do
    Begin
      Application.Terminate;
    End;
  End;
End;

Procedure TForm_error.Handle(ErrClass, ErrMsg: String; Details: String = 'X';
  critical: Boolean = False); Overload;
Begin

End;

Procedure TForm_error.ToggleBox_detailsChange(Sender: TObject);
Begin
  If (ToggleBox_details.Checked = True) Then
  Begin
    Form_error.Height := 450;
  End
  Else
  Begin
    Form_error.Height := 150;
  End;
End;

End.
