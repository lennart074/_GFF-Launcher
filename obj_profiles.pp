unit obj_profiles;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil,
  { INIFiles }IniFiles,
  { Debug } Dialogs,
  { TObjectList } contnrs,
  { obj_settings } obj_settings,
  { Informations } informations
  ;

Type

  TINILocation = String;

  { TProfileObj }

  TProfileObj = Class(TObject)
  public
    Procedure OpenINI(INIFile: TINILocation);
    Procedure DeleteProfile;
    function ImageExists: Boolean;
    Constructor Create; Overload;
    Constructor Create(lname, lpath, limage, lindexFile, lRelChannel,
      lMibLauncherVer: String); overload;
    Constructor Create(iString: String; useiString: Boolean); overload;
    Constructor Create(indexFilePath: TINILocation); Overload;
  Private
    pEnabled: Boolean;
    pDisableReason: String;
    pImage: String;
    pName: String;
    pPath: String;
    pIndexFile: String;
    pIndexINI: TIniFile;
    pRelChannel: String;
    pMinLauncherVer: Integer; //FORCES adaption to launcher!
    pUpdateDisabled: Boolean;
    function GetAditOptions : String;
  Public
    Property ProfName: String read pName;
    Property path: String read pPath;
    Property indexFile: TINILocation read pIndexFile write OpenINI;
    Property indexINI: TINIFile read pIndexINI;
    Property HasImage: Boolean read ImageExists;
    Property Image: String read pImage;
    Property RelChannel: String read pRelChannel;
    Property minLauncherVer: integer read pMinLauncherVer;
    Property Enabled : Boolean read pEnabled;
    Property DisableReason : String read pDisableReason;
    Property AditArgs : String read GetAditOptions;
    Property UpdateDisabled : Boolean read pUpdateDisabled;
  End;

  { TProfileList }

  TProfileList = class(TObjectList)
  private
    function GetObject(I: Integer): TProfileObj;
    procedure SetObject(I: Integer; Obj: TProfileObj);
  public
    property objects[I: Integer]: TProfileObj read GetObject write SetObject;
  end;

  { TProfileCollection }

  TProfileCollection = Class(TObject)
  public
    Procedure Clear;
    Procedure Add(ProfObj: TProfileObj);
    Function IndexOf(str: String): Integer;
    Procedure getNames(ToList: TStrings);
    Function getNameByIndex(Index: Integer): String;
    Function GetCount: Integer;
    Function GetObject(Index: Integer): TProfileObj;
    Function CollectionIsDifferent(OtherCollection: TProfileCollection;
      CompareCounts: Boolean = True): Boolean;
    Constructor Create;
    Destructor Destroy;
  Private
    pProfiles: TProfileList;
    function GetObjectByName(Name: String): TProfileObj;
  Public
    Property profiles[Index: Integer]: TProfileObj read GetObject;
    Property profilesBN[Name: String]: TProfileObj read GetObjectByName;
    Property Count: Integer read GetCount;
    Property Names[Index: Integer]: String read GetNameByIndex;
  End;

Implementation

//------------------------------------------------------------------------------------------------
uses launcher, appUtils;
//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################

Procedure TProfileObj.DeleteProfile;
Begin
  if Assigned(pIndexINI) then
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
  pImage := '*not assigned*';
  pEnabled := False;
  pDisableReason := '**Information_incomplete**';
End;

Constructor TProfileObj.Create(lname, lpath, limage, lindexFile,
  lRelChannel, lMibLauncherVer: String); overload;
begin
  Create;
  {pName := lname;
  pPath := lpath;
  pImage := limage;
  pIndexFile := lindexFile;
  pRelChannel := lRelChannel;
  pMinLauncherVer := lMibLauncherVer;
  if (pMinLauncherVer>INOFF_LAUNCHER_VERSION) then begin
    pEnabled := FALSE;
    pDisableReason := 'Launcher outdated!';
  end;}
end;

Constructor TProfileObj.Create(iString: String; useiString: Boolean);
  //Create Profile by Information-containing String;
  //Current format:
  //<launcherVer>|<RelChannel>|<name>|<path>|<image/urlToImage>|<index/urlToIndex>|<aditOptions>|<enabled:true/false>|<disableReason>
const
  index_minLVer = 0;
  index_RelChannel = index_minLVer + 1;
  index_pName = index_RelChannel + 1;
  index_pPath = index_pName + 1;
  index_pImage = index_pPath + 1;
  index_pIndex = index_pImage + 1;
  index_AditOpt = index_pIndex + 1;

var
  tempList: TStringList;
  index_enabled: Integer;
  index_disReas: Integer;
begin
  Create;
  tempList := TStringList.Create;
  tempList.Delimiter := '|';
  tempList.StrictDelimiter := True;
  tempList.DelimitedText := iString;

  pMinLauncherVer := StrToInt(tempList[index_minLVer]);
  index_enabled := tempList.Count - 2;
  index_disReas := tempList.Count - 1;
  pEnabled := StrToBoolean(tempList[index_enabled]);
  pDisableReason := tempList[index_disReas];

  pName := tempList[index_pName];
  pPath := tempList[index_pPath];
  pImage := tempList[index_pImage];
  pIndexFile := tempList[index_pIndex];
  pRelChannel := tempList[index_RelChannel];
  if (pMinLauncherVer > INOFF_LAUNCHER_VERSION_NUM) then
  begin
    pEnabled := False;
    pDisableReason := pDisableReason + '' + 'Launcher outdated!';
  end;
end;

Constructor TProfileObj.Create(indexFilePath: TINILocation);
Begin
  Create;
  OpenINI(indexFilePath);
End;

function TProfileObj.GetAditOptions: String;
begin
    Result := pIndexINI.ReadString('Minecraft','launchArgs_'+IntToStr(appUtils.getSystemType(True)),'');
end;

function TProfileObj.ImageExists: Boolean;
begin
  Result := FileExists(pImage);
end;

Procedure TProfileObj.OpenINI(INIFile: TINILocation);
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
  pMinLauncherVer := pIndexINI.ReadInteger('Profile', 'minLauncherVer', -1);
  pUpdateDisabled := pIndexINI.ReadBool('Profile','updateDisabled', True);
  if (pMinLauncherVer > INOFF_LAUNCHER_VERSION_NUM) then
  begin
    pEnabled := False;
    pDisableReason := 'Launcher outdated!';
  end;
  pImage := pIndexINI.ReadString('Profile', 'image', '*not assigned*');
  if (pImage = '*not assigned*') and (FileExists(pPath + '/logo.png')) then
  begin
    pImage := pPath + '/logo.png';
  end;
End;

//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################

function TProfileList.GetObject(I: Integer): TProfileObj;
begin
  Result := TProfileObj(Items[I]);
end;

procedure TProfileList.SetObject(I: Integer; Obj: TProfileObj);
begin
  Items[I] := Obj;
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

Procedure TProfileCollection.getNames(ToList: TStrings);
Var
  i: Integer;
  NameStr: String;
Begin
  ToList.Clear;
  For I := 0 To (pProfiles.Count - 1) Do
  Begin
    NameStr := pProfiles.objects[i].ProfName;
    ToList.Add(NameStr);
  End;
  if pProfiles.Count = 0 then
  begin
    ToList.Clear;
  end;
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
  Result := pProfiles.objects[Index];
end;

Function TProfileCollection.CollectionIsDifferent(OtherCollection: TProfileCollection;
  CompareCounts: Boolean = True): Boolean;
var
  I: Integer;
begin
  if ((Count <> OtherCollection.Count) and (CompareCounts = True)) then
  begin
    ShowMessage(IntToStr(Count) + '<->' + IntToStr(OtherCollection.Count));
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

Constructor TProfileCollection.Create;
Begin
  pProfiles := TProfileList.Create(True);
  Inherited Create;
End;

Destructor TProfileCollection.Destroy;
Var
  I: Integer;
Begin
  Clear;
  Inherited Destroy;
End;

function TProfileCollection.GetObjectByName(Name: String): TProfileObj;
begin
  Result := pProfiles.objects[IndexOf(Name)];
end;

//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################


End.

