unit obj_versions;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, contnrs;

type

  { TVersionObj }

  TVersionObj = class(TObject)
  public
    id : String;
    time : String;
    releaseTime : String;
    vType : String;
  end;

  { TVersionList }

  TVersionList = class(TObjectList)
  private
      function GetObject(Index: Integer): TVersionObj;
      procedure SetObject(Index: Integer; AValue: TVersionObj);
  public
    Property v[Index:Integer] : TVersionObj read GetObject write SetObject;
  end;

implementation

{ TVersionList }

function TVersionList.GetObject(Index: Integer): TVersionObj;
begin
     Result := TVersionObj(Items[Index]);
end;

procedure TVersionList.SetObject(Index: Integer; AValue: TVersionObj);
begin
     Items[Index] := AValue;
end;

end.

