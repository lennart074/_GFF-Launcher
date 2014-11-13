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
    private
        { private declarations }
    public
        { public declarations }
    end;

var
    Form_consoles: TForm_consoles;

implementation

{$R *.lfm}

end.

