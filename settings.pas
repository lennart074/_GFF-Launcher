Unit settings;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, eventlog, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditBtn, ValEdit, ComCtrls, Spin, IniPropStorage,
  BGRAFlashProgressBar,

  { Main Unit } gfflauncher;

Type
  { Main Program Settings }
  TProgramSettings = Class(gfflauncher.TSettings)
  Public
    javaPath, BaseArgs: String;
    Memory, BaseMem: Integer;
    getUpdates, customMem, showConsole, showMCConsole: Boolean;
    Procedure WriteFile;
    Procedure ReadFile;
  End;


  { TForm_settings }
  TForm_settings = Class(TForm)
    Button_cancel: TButton;
    Button_reset: TButton;
    Button_OK: TButton;
    Button_setMem: TButton;
    Button_browseJava: TButton;
    CheckBox_mcConsole: Tcheckbox;
    CheckBox_gffConsole: Tcheckbox;
    CheckBox_customMemory: TCheckBox;
    CheckBox_customJava: TCheckBox;
    Edit_javaPath: TEdit;
    Label_mb: TLabel;
    OpenDialog: TOpenDialog;
    SpinEdit_ram: TSpinEdit;
    Procedure Button_browseJavaClick(Sender: TObject);
    Procedure Button_cancelClick(Sender: TObject);
    Procedure Button_OKClick(Sender: TObject);
    Procedure Button_resetClick(Sender: TObject);
    Procedure Button_setMemClick(Sender: TObject);
    Procedure CheckBox_customJavaChange(Sender: TObject);
    Procedure CheckBox_customMemoryChange(Sender: TObject);
    procedure Checkbox_mcconsolechange(Sender: Tobject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure SpinEdit_ramChange(Sender: TObject);
  Private
    { private declarations }
  Public
    { public declarations }
  End;



Var
  Form_settings: TForm_settings;
  MainSettings: TProgramSettings;

Implementation

{$R *.lfm}

{ TForm_settings }

Procedure TForm_settings.FormCreate(Sender: TObject);
Var
  DoWrite: Boolean;
Begin
  If (FileExists('GFFLauncher/settings/settings.ini') = False) Then
  Begin
    DoWrite := True;
  End
  Else
  Begin
    DoWrite := False;
  End;

  MainSettings := TProgramSettings.Create('GFFLauncher/settings/settings.ini');
  If (DoWrite = True) Then
    MainSettings.WriteFile;
  MainSettings.ReadFile;
End;

Procedure TForm_settings.FormShow(Sender: TObject);
Begin
  MainSettings.ReadFile;

  If (MainSettings.javaPath <> 'java') Then
  Begin
    Edit_javaPath.Caption := MainSettings.javaPath;
    CheckBox_customJava.Checked := True;
    CheckBox_customJavaChange(Form_settings);
  End
  Else
  Begin
    Edit_javaPath.Caption := 'Please set your Java-Path!';
    CheckBox_customJava.Checked := False;
    CheckBox_customJavaChange(Form_settings);
  End;

  If (MainSettings.customMem = False) Then
  Begin
    CheckBox_customMemory.Checked := False;
    SpinEdit_ram.Enabled := False;
  End
  Else
  Begin
    CheckBox_customMemory.Checked := True;
    SpinEdit_ram.Enabled := True;
  End;
  Button_setMem.Enabled := False;
  SpinEdit_ram.Value := MainSettings.Memory;

  CheckBox_mcConsole.Checked := MainSettings.INI.ReadBool('Minecraft','showCons',False);
End;

Procedure TForm_settings.SpinEdit_ramChange(Sender: TObject);
Begin
  If (Button_setMem.Enabled = False) Then
  Begin
    Button_setMem.Enabled := True;
  End;
End;

Procedure TForm_settings.Button_browseJavaClick(Sender: TObject);
Begin
  If (OpenDialog.Execute = True) Then
  Begin
    Edit_javaPath.Text := OpenDialog.FileName;
    MainSettings.javaPath := OpenDialog.FileName;
    MainSettings.INI.WriteString('GFFLauncher', 'javaPath', OpenDialog.FileName);
  End
  Else
  Begin
    MainSettings.INI.WriteString('GFFLauncher', 'javaPath', 'java');
    CheckBox_customJava.Checked := False;
  End;
End;

Procedure TForm_settings.Button_cancelClick(Sender: TObject);
Begin
  Form_settings.Close;
End;

Procedure TForm_settings.Button_OKClick(Sender: TObject);
Var
  CanClose: Boolean;
Begin
  If ((CheckBox_customJava.Checked = True) And
    (Edit_javaPath.Text = 'Please set your Java-Path!')) Then
  Begin
    ShowMessage('Please set you javapath !');
    CanClose := False;
  End
  Else
  Begin
    CanClose := True;
  End;

  If (CanClose = True) Then
  Begin
    Close;
  End;
End;

Procedure TForm_settings.Button_resetClick(Sender: TObject);
Begin
  MainSettings.WriteFile;
  //DeleteFile(MainSettings.settingsFile);
  Form_settings.Hide;
  //FormCreate(Form_settings);
  MainSettings.ReadFile;
  Form_settings.Show;
End;

Procedure TForm_settings.Button_setMemClick(Sender: TObject);
Begin
  MainSettings.Memory := SpinEdit_ram.Value;
  MainSettings.INI.WriteInteger('Minecraft', 'Memory', SpinEdit_ram.Value);
  Button_setMem.Enabled := False;
End;

Procedure TForm_settings.CheckBox_customJavaChange(Sender: TObject);
Begin
  Button_browseJava.Enabled := CheckBox_customJava.Checked;
  Edit_javaPath.Enabled := CheckBox_customJava.Checked;

  If (CheckBox_customJava.Checked = False) Then
  Begin
    MainSettings.javaPath := 'java';
    MainSettings.INI.WriteString('GFFLauncher', 'javaPath', 'java');
  End;
End;

Procedure TForm_settings.CheckBox_customMemoryChange(Sender: TObject);
Begin
  MainSettings.INI.WriteBool('Minecraft', 'customMem', CheckBox_customMemory.Checked);
  SpinEdit_ram.Enabled := CheckBox_customMemory.Checked;

  If (CheckBox_customMemory.Checked = False) Then
  Begin
    SpinEdit_ram.Value := 1024;
    Button_setMemClick(Form_settings);
  End;
End;

procedure Tform_settings.Checkbox_mcconsolechange(Sender: Tobject);
begin
    MainSettings.INI.WriteBool('Minecraft', 'showCons',CheckBox_mcConsole.Checked);
End;

////////////////////////////////////////////////////////////////////////////////
//############################################################################//
////////////////////////////////////////////////////////////////////////////////

Procedure TProgramSettings.WriteFile;
Begin
  INI.WriteString('GFFLauncher', 'javaPath', 'java');
  INI.WriteBool('GFFLauncher', 'getUpdates', False);
  INI.WriteBool('Minecraft', 'customMem', False);
  INI.WriteInteger('Minecraft', 'Memory', 1024);
  INI.WriteBool('GFFLauncher','showCons',False);
  INI.WriteBool('Minecraft','showCons',False);
End;

Procedure TProgramSettings.ReadFile;
Begin
  javaPath := INI.ReadString('GFFLauncher', 'javaPath', 'java');
  getUpdates := INI.ReadBool('GFFLauncher', 'getUpdates', True);
  customMem := INI.ReadBool('Minecraft', 'customMem', False);
  Memory := INI.ReadInteger('Minecraft', 'Memory', 1024);
  BaseArgs := INI.ReadString('Minecraft','BaseArgs','-Djava.library.path=%appdata%\GFFLauncher\Binaries\LWJGL\windows\natives -cp %Libs% net.minecraft.client.main.Main');
  showConsole:=INI.ReadBool('GFFLauncher','showCons',False);
  showMCConsole:= INI.ReadBool('Minecraft','showCons',False);

End;

End.
