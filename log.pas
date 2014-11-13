unit Log;

{$mode objfpc}{$H+}

interface

uses
    Classes, Sysutils,
    { Console-Formular }consoles
    ;

{ DONE 90 -oL4YG -cDebug : Create Log-Api/Output to console }

procedure Print(msg: String; targetConsole : String = 'Main');

const
  consoles_mc        : String = 'MC';
  consoles_main      : String = 'Main';

implementation

procedure Print(msg: String; targetConsole : String = 'Main');
begin
case targetConsole of
                     'Main':with Form_consoles.console_main do begin Append(msg) end;
                     'MC'  :with Form_consoles.console_mc do begin Append(msg) end;
otherwise begin
          with Form_consoles.console_main do begin
               Append('|UNASSIGNED!|'+msg);
               end;
          end;
end;
end;

procedure SwitchTo(targetConsole: String = 'Main');
begin
case targetConsole of
                     'Main':with Form_consoles.PageControl_Consoles do begin ActivePage:=Form_consoles.TabSheet_GFFLauncher; end;
                     'MC'  :with Form_consoles.PageControl_Consoles do begin ActivePage:=Form_consoles.TabSheet_Minecraft; end;
otherwise begin
          with Form_consoles.PageControl_Consoles do begin
               ActivePage:=Form_consoles.TabSheet_GFFLauncher;
               end;
          end;
end;
end;

end.

