unit json;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, jsonparser, jsonUtils, fpjson, obj_versions,
    Dialogs
    ;

procedure DecodeVersions(JSONStr : String;out ToList : TVersionList);

implementation

procedure DecodeVersions(JSONStr : String;out ToList: TVersionList);
var
  P : TJSONParser;
  D : TJSONData;
  Ar : TJSONArray;
  I : Integer;
  tObj : TVersionObj;
begin
     P := TJSONParser.Create(JSONStr);
     D := P.Parse;
     Ar := TJSONArray(TJSONObject(D).Extract('versions'));

     for I:=0 to (TJSONObject(Ar).Count-1) do begin
       tObj := TVersionObj.Create;
       tObj.id := TJSONObject(Ar.Items[I]).Get('id');
       tObj.time := TJSONObject(Ar.Items[I]).Get('time');
       tObj.releaseTime := TJSONObject(Ar.Items[I]).Get('releaseTime');
       tObj.vType := TJSONObject(Ar.Items[I]).Get('type');

       ToList.Add(tObj);
       tObj := nil;
     end;
     FreeAndNil(P);
     FreeAndNil(D);
     FreeAndNil(Ar);
end;

end.

