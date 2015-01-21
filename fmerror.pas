unit fmError;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,

  { Main Code-"Thread" } launcher,
  { Informations for the Program } informations;

type

  EFatalExceptionObj = class(TObject);
  EFatalException = class(Exception);
  EMissingFileException = class(Exception);
  EMissingDirectoryException = class(Exception);
  EDynamicsError = class(Exception);
  EPathObjError = class(Exception);
  EPathCollError = class(Exception);
  EDynamicsObjError = class(Exception);
  EDynamicsListError = class(Exception);

  { Tfm_Error }

  Tfm_Error = class(TForm)
    GroupBox_details: TGroupBox;
    GroupBox_short: TGroupBox;
    LabeledEdit_classname: TLabeledEdit;
    LabeledEdit_message: TLabeledEdit;
    Memo_details: TMemo;
    Splitter1: TSplitter;
    ToggleBox_details: TToggleBox;
    procedure CatchError(Sender: TObject; E: Exception);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HandleException(E: Exception; Details: String = '';
      Fatal: Boolean = True; Sender: TObject = nil);
    procedure ResetStatus;
    procedure ToggleBox_detailsChange(Sender: TObject);
  private
    { private declarations }
    pIsFatal: Boolean;
  public
    { public declarations }
  end;

var
  fm_Error: Tfm_Error;

implementation

{$R *.lfm}

uses { Forms } fmMain, fmLogin, fmLauncher, fmProfiles, fmSettings;

{ Tfm_Error }

procedure Tfm_Error.CatchError(Sender: TObject; E: Exception);
var
  Debug: String;
  Frames: PPointer;
  I: Integer;
begin
  //Collect data
  Debug := 'A toplevel-exception has been caught while executing "' +
    LAUNCHER_NAME + '"';
  Debug := Debug + LineEnding + LineEnding + 'Class:"' + E.ClassName + '"';
  Debug := Debug + LineEnding + 'Message:"' + E.Message + '"';
  Debug := Debug + LineEnding + 'Module:"' + E.UnitName + '"';
  Debug := Debug + LineEnding + 'TraceBack-StackTrace:' + LineEnding +
    BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to (ExceptFrameCount - 1) do
    Debug := Debug + LineEnding + BackTraceStrFunc(Frames[I]);

  if not Assigned(fm_Error) then
  begin
    try
      Application.CreateForm(Tfm_Error, fm_Error);
      fm_Error.HandleException(E, Debug, True, fm_Error);
    except
      on E: Exception do
      begin
        Debug := Debug + LineEnding + LineEnding + 'Unable to create Error-Formular!';
        Debug := Debug + LineEnding + 'Cannot create nothing! ;' + #39;
        ShowMessage(Debug);
        halt;
      end;
    end;
  end
  //Throw errors as a dialog, if ErrorHandler isn't assigned AND cannot be created!
  else
  begin
    //Handle Exception as normal
    fm_Error.HandleException(E, Debug, True, fm_Error);
  end;

end;

procedure Tfm_Error.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  { TODO 90 -oL4YG -cExceptions : Ignore non-fatal exceptions }
  if pIsFatal then
  begin
    fm_Main.Close;
  end
  else
  begin
    PopupParent.Enabled := True;
  end;
end;

procedure Tfm_Error.FormCreate(Sender: TObject);
begin
  Self.Caption := LAUNCHER_NAME + ': An Error has occured!';
end;

procedure Tfm_Error.HandleException(E: Exception; Details: String = '';
  Fatal: Boolean = True; Sender: TObject = nil);
begin
  if not Assigned(fm_Error) then
  begin
    Application.CreateForm(Tfm_Error, fm_Error);
  end;
  ResetStatus;
  LabeledEdit_classname.Text := E.ClassName;
  if (Pos(LineEnding, E.Message) > 0) then
  begin
    LabeledEdit_message.Text := Copy(E.Message, 1, Pos(LineEnding, E.Message));
  end
  else
  begin
    LabeledEdit_message.Text := E.Message;
  end;

  if (Details <> '') then
  begin
    Memo_details.Lines.Delimiter := #10;
    Memo_details.Lines.StrictDelimiter := True;
    Memo_details.Lines.DelimitedText := Details;
  end;

  PopupMode := pmExplicit;
  if (Sender.ClassType = TForm) then
  begin
    PopupParent := TForm(Sender);
  end
  else
  begin
    PopupParent := fm_Main;
  end;
  PopupParent.Enabled := False;
  Top := PopupParent.Top;
  Left := PopupParent.Left;
  Show;

  pIsFatal := Fatal;
end;

procedure Tfm_Error.ResetStatus;
begin
  LabeledEdit_classname.Text := '<CLASSNAME>';
  LabeledEdit_message.Text := '<MESSAGE>';

  Memo_details.Clear;
  Memo_details.Append('<NO DETAILS SUBMITTED>');

  LabeledEdit_classname.EditLabel.Enabled := True;
  LabeledEdit_message.EditLabel.Enabled := True;

  ToggleBox_details.Checked := False;
  ToggleBox_detailsChange(fm_Error);
end;

procedure Tfm_Error.ToggleBox_detailsChange(Sender: TObject);
begin
  if (ToggleBox_details.Checked) then
  begin
    Self.Height := (GroupBox_short.Height + Splitter1.Height +
      GroupBox_details.Height + 5);
  end
  else
  begin
    Self.Height := (GroupBox_short.Height + Splitter1.Height + 0);
  end;
end;

end.
