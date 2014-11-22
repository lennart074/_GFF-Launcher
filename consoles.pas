unit consoles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Fileutil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls;

type

  { TForm_consoles }

  TForm_consoles = class(TForm)
    console_main: Tmemo;
    console_mc: Tmemo;
    PageControl_Consoles: TPageControl;
    TabSheet_GFFLauncher: TTabSheet;
    TabSheet_Minecraft: TTabSheet;
    procedure Formclosequery(Sender: TObject; var Canclose: Boolean);
    procedure Formshow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form_consoles: TForm_consoles;

implementation

{$R *.lfm}

{ TForm_consoles }

procedure Tform_consoles.Formshow(Sender: TObject);
begin
  console_main.Font.Color := clBlack;
  console_mc.Font.Color := clBlack;
End;

procedure Tform_consoles.Formclosequery(Sender: TObject; var Canclose: Boolean);
begin
  Form_consoles.Hide;
End;

end.

