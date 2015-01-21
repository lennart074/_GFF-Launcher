unit obj_remProfiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, informations;

type
  { TProfileObj }

  { TRemProfileObj }

  TRemProfileObj = Class(TObject)
  private
    pminLauncherVer: Integer;
    procedure setMinLauncherVer(AValue: Integer);
  Public
    ProfName: String;
    version: Integer;
    vCapt: String;
    branch: String;
    ProfOwner: String;
    indexURL: String;
    comment: String;
    Enabled: Boolean;
    disableReason: String;
    Property minLauncherVer: Integer read pminLauncherVer write setMinLauncherVer;
  End;

  { TProfileList }

  TRemProfileList = class(TObjectList)
  private
    function GetObject(I: Integer): TRemProfileObj;
    procedure SetObject(I: Integer; Obj: TRemProfileObj);
  public
    property objects[I: Integer]: TRemProfileObj read GetObject write SetObject;
  end;

  { TProfileCollection }

  TRemProfileCollection = Class(TObject)
  public
    Procedure Clear;
    Procedure Add(ProfObj: TRemProfileObj);
    Function IndexOf(str: String): Integer;
    Procedure getNames(ToList: TStrings);
    Function getNameByIndex(Index: Integer): String;
    Function GetCount: Integer;
    Function GetObject(Index: Integer): TRemProfileObj;
    Function CollectionIsDifferent(OtherCollection: TRemProfileCollection;
      CompareCounts: Boolean = True): Boolean;
    Constructor Create;
    Destructor Destroy;
  Private
    pProfiles: TRemProfileList;
    function GetObjectByName(Name: String): TRemProfileObj;
  Public
    Property profiles[Index: Integer]: TRemProfileObj read GetObject;
    Property profilesBN[Name: String]: TRemProfileObj read GetObjectByName;
    Property Count: Integer read GetCount;
    Property Names[Index: Integer]: String read GetNameByIndex;
  End;


implementation

{ TRemProfileObj }

procedure TRemProfileObj.setMinLauncherVer(AValue: Integer);
begin
  pminLauncherVer := AValue;
  if pminLauncherVer > INOFF_LAUNCHER_VERSION_NUM then
  begin
    Enabled := False;
    disableReason := 'Launcher outdated !';
  end;
end;


function TRemProfileList.GetObject(I: Integer): TRemProfileObj;
begin
  Result := TRemProfileObj(Items[I]);
end;

procedure TRemProfileList.SetObject(I: Integer; Obj: TRemProfileObj);
begin
  Items[I] := Obj;
end;

Procedure TRemProfileCollection.Add(ProfObj: TRemProfileObj);
Begin
  pProfiles.Add(ProfObj);
End;

Procedure TRemProfileCollection.Clear;
Begin
  pProfiles.Clear;
End;

Function TRemProfileCollection.IndexOf(str: String): Integer;
Var
  I: Integer;
Begin
  For I := 0 To (pProfiles.Count - 1) Do
  Begin
    If (pProfiles.objects[i].ProfName = str) Then
    Begin
      Result := i;
      Exit;
    End;
  End;
  Result := (-1);
End;

Procedure TRemProfileCollection.getNames(ToList: TStrings);
Var
  i: Integer;
  NameStr: String;
Begin
  ToList.Clear;
  For I := 0 To (pProfiles.Count - 1) Do
  Begin
    NameStr := pProfiles.objects[I].ProfName;
    ToList.Add(NameStr);
  End;
  if pProfiles.Count = 0 then
  begin
    ToList.Clear;
  end;
End;

Function TRemProfileCollection.getNameByIndex(Index: Integer): String;
Begin
  Result := pProfiles.objects[index].ProfName;
End;

Function TRemProfileCollection.GetCount: Integer;
Begin
  Result := pProfiles.Count;
End;

Function TRemProfileCollection.GetObject(Index: Integer): TRemProfileObj;
begin
  Result := pProfiles.objects[Index];
end;

Function TRemProfileCollection.CollectionIsDifferent(OtherCollection:
  TRemProfileCollection;
  CompareCounts: Boolean = True): Boolean;
var
  I: Integer;
begin
  if ((Count <> OtherCollection.Count) and (CompareCounts = True)) then
  begin
    Result := True;
  end
  else
  begin
    if OtherCollection.Count >= Count then
    begin
      for I := 0 to (Count - 1) do
      begin
        if (pProfiles.objects[I].ProfName <> OtherCollection.profiles[I].ProfName) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end
    else
    begin
      Result := True;
    end;
  end;
  Result := False;
end;

Constructor TRemProfileCollection.Create;
Begin
  pProfiles := TRemProfileList.Create(True);
  Inherited Create;
End;

Destructor TRemProfileCollection.Destroy;
Var
  I: Integer;
Begin
  Clear;
  Inherited Destroy;
End;

function TRemProfileCollection.GetObjectByName(Name: String): TRemProfileObj;
begin
  Result := pProfiles.objects[IndexOf(Name)];
end;

end.
