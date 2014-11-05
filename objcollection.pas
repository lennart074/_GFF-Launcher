Unit objcollection;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Type
  TUserObject = Class(TObject)
    username, loginName, UUID, profileID, accessToken, GFFProfilePath: String;
    isOffline, isLegacy: Boolean;
  End;


Implementation

End.



