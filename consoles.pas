unit consoles;

{$mode objfpc}{$H+}

interface

uses
    Classes, Sysutils, Fileutil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ComCtrls;

type

    { TForm_consoles }

    TForm_consoles = class(Tform)
        console_main: Tmemo;
        console_mc: Tmemo;
        PageControl_Consoles: Tpagecontrol;
        TabSheet_GFFLauncher: Ttabsheet;
        TabSheet_Minecraft: Ttabsheet;
        procedure Formclosequery(Sender: Tobject; var Canclose: Boolean);
        procedure Formshow(Sender: Tobject);
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

procedure Tform_consoles.Formshow(Sender: Tobject);
begin
    console_main.Font.Color := clBlack;
    console_mc.Font.Color := clBlack;
End;

procedure Tform_consoles.Formclosequery(Sender: Tobject; var Canclose: Boolean);
begin
    Form_consoles.Hide;
End;

end.

