Unit objcollection;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil,
  { INIFiles }IniFiles,
  { Debug } Dialogs,
  { TObjectList } contnrs
  ;

Type

  TUserObject = Class(TObject)
    username, loginName, UUID, profileID, accessToken, GFFProfilePath,
    userType, userProps: String;
    isOffline, isLegacy: Boolean;
  End;

  TProfileObj = Class(TObject)
  public
    Procedure OpenINI(INIFile: String);
    Procedure DeleteProfile;
    Constructor Create; Overload;
    Constructor Create(indexFilePath: String); Overload;
  Private
    pName: String;
    pPath: String;
    pIndexFile: String;
    pIndexINI: TIniFile;
    pRelChannel: String;
    pMinLauncherVer: Integer;
  Public
    Property ProfName: String read pName;
    Property path: String read pPath;
    Property indexFile: String read pIndexFile write OpenINI;
    Property indexINI: TINIFile read pIndexINI;
    Property RelChannel: String read pRelChannel;
    Property minLauncherVer: Integer read pMinLauncherVer;
  End;

    TProfileList = class(TObjectList)
  private
    function GetObject(I:Integer):TProfileObj;
    procedure SetObject(I:Integer; Obj:TProfileObj);
  public
    property objects[I:Integer]: TProfileObj read GetObject write SetObject;
  end;

  { TProfileCollection }

  TProfileCollection = Class(TObject)
  public
    Procedure Clear;
    Procedure Add(ProfObj: TProfileObj);
    Function IndexOf(str: String): Integer;
    Procedure getNames(ToList:TStrings);
    Function getNameByIndex(Index: Integer): String;
    Function GetCount: Integer;
    Function GetObject(Index: Integer): TProfileObj;
    Function CollectionIsDifferent(OtherCollection :TProfileCollection):Boolean;
    Constructor Create;
    Destructor Destroy;
  Private
    pProfiles: TProfileList;
  Public
    Property profiles[Index: Integer]: TProfileObj read GetObject;
    Property Count: Integer read GetCount;
    Property Names[Index: Integer]: String read GetNameByIndex;
  End;

Implementation

//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################

Procedure TProfileObj.DeleteProfile;
Begin
  FreeAndNil(pIndexINI);
  If (Not DeleteDirectory(pPath, False)) Then
  Begin
    Raise Exception.Create('Cannot delete Profile(Obj) "' + pName + '"');
    Exit;
  End;
  Destroy;
End;

Constructor TProfileObj.Create; Overload;
Begin
  pName := '*not assigned*';
  pPath := '*not assigned*';
End;

Constructor TProfileObj.Create(indexFilePath: String); Overload;
Begin
  pName := '*not assigned*';
  pPath := '*not assigned*';
  OpenINI(indexFilePath);
End;

Procedure TProfileObj.OpenINI(INIFile: String);
Begin
  pIndexFile := INIFile;
  If (Assigned(pIndexINI)) Then
  Begin
    FreeAndNil(pIndexINI);
  End;
  pIndexINI := TIniFile.Create(INIFile);
  pPath := ExtractFileDir(IniFile);
  pName := pIndexINI.ReadString('Profile', 'name', '**unknown**');
  pRelChannel := pIndexINI.ReadString('Profile', 'relChannel', '*undefinded*');
  pMinLauncherVer := pIndexINI.ReadInteger('Profile', 'minLauncherVer', (-1));
End;

//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################

function TProfileList.GetObject(I:Integer):TProfileObj;
begin
  Result := TProfileObj(Items[I]);
end;

procedure TProfileList.SetObject(I:Integer; Obj:TProfileObj);
begin
  Items[I]:=Obj;
end;

//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################

Procedure TProfileCollection.Add(ProfObj: TProfileObj);
Begin
  pProfiles.Add(ProfObj);
End;

Procedure TProfileCollection.Clear;
Begin
  pProfiles.Clear;
End;

Function TProfileCollection.IndexOf(str: String): Integer;
Var
  I: Integer;
Begin
  For I:=0 To (pProfiles.Count-1) Do
  Begin
    If (pProfiles.objects[i].ProfName = str) Then
    Begin
      Result := i;
      Exit;
    End;
  End;
  Result := (-1);
End;

Procedure TProfileCollection.getNames(ToList: TStrings);
Var
  i: Integer;
  NameStr: String;
Begin
  ToList.Clear;
  For I:=0 To (pProfiles.Count-1) Do
  Begin
    NameStr := pProfiles.objects[i].ProfName;
    ToList.Add(NameStr);
  End;
End;

Function TProfileCollection.getNameByIndex(Index: Integer): String;
Begin
  Result := pProfiles.objects[index].ProfName;
End;

Function TProfileCollection.GetCount: Integer;
Begin
  Result := pProfiles.Count;
End;

Function TProfileCollection.GetObject(Index: Integer): TProfileObj;
begin
  Result:=pProfiles.objects[Index];
end;

Function TProfileCollection.CollectionIsDifferent(OtherCollection: TProfileCollection): Boolean;
var
  I:Integer;
begin
    if (Count<>OtherCollection.Count) then begin
      Result:=True;
    end else begin
      for I:=0 to (Count-1) do begin
          if (pProfiles.objects[I].ProfName<>OtherCollection.profiles[I].ProfName) then begin
            Result:=True;
            Exit;
          end;
      end;
    end;
    Result:=False;
end;

Constructor TProfileCollection.Create;
Begin
  pProfiles:=TProfileList.create(True);
  Inherited Create;
End;

Destructor TProfileCollection.Destroy;
Var
  I: Integer;
Begin
  Clear;
  Inherited Destroy;
End;

//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################


End.
