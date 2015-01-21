unit obj_user;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TUserObject }

  TUserObject = class(TObject)
  public
    CONSTRUCTOR Create;
  public
    username: String;
    loginName: String;
    accessToken: String;
    UUID: String;
    profileID: String;
    ProfilePath: String;
    userType: String;
    userProperties: String;
    isLoggedIn: Boolean;
    isOffline: Boolean;
    isLegacy: Boolean;
  end;

implementation

{ TUserObject }

CONSTRUCTOR TUserObject.Create;
begin
    inherited Create;
    isOffline := True;
    isLoggedIn := False;
end;

end.

