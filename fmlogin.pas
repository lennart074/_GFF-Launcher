unit fmLogin;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    StdCtrls,
    { Mojang } mojang,
    { User } obj_user
    ;

type

    { Tfm_Login }

    Tfm_Login = class(TForm)
        Button_login: TButton;
        Image_background: TImage;
        LabeledEdit_username: TLabeledEdit;
        LabeledEdit_pass: TLabeledEdit;
        procedure Button_loginClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure LabeledEdit_passKeyPress(Sender: TObject; var Key: char);
    private
        { private declarations }
    public
        { public declarations }
    end;

var
    fm_Login: Tfm_Login;
    LUser : TUserObject;

implementation

{$R *.lfm}

uses { Forms } fmMain, fmLauncher, fmProfiles, fmSettings, fmError, launcher;

{ Tfm_Login }

procedure Tfm_Login.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    fm_Main.Close;
end;

procedure Tfm_Login.LabeledEdit_passKeyPress(Sender: TObject; var Key: char);
begin
    if (Key=#13) then
       Button_loginClick(LabeledEdit_pass);
end;

procedure Tfm_Login.Button_loginClick(Sender: TObject);
var
    syntax, url : String;
begin
     Log(etDebug,'Authentication started',0);
     syntax := LWebSettings.authSyntax;
     url := LWebSettings.authURL;

     LUser := TUserObject.Create;

     if (mojang.Authenticate(LUser,LabeledEdit_username.Text,LabeledEdit_pass.Text,syntax,url,DIA_ALL)) then begin
        { TODO 100 -oL4Yg -cContinue : Successful login }
        LUser.isLoggedIn := True;
        LUser.isOffline := False;
        LUser.ProfilePath := PC_CommonPaths.pathsBN['profiles'].path+'/'+LUser.username;
        LabeledEdit_pass.Text := '';
        Log(etDebug, 'Showing Form: fmLauncher', 0);
        fm_Launcher.Show;
        fm_Login.Hide;
     end else begin
       FreeAndNil(LUser);
     end;
end;

end.

