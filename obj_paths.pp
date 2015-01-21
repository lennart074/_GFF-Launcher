unit obj_paths;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  { TObjectList } contnrs,
  { INI } IniFiles,
  { Utils } appUtils,
  { Objects } obj_settings,
  typinfo;

type

  TPathString = String;
  TListOfPaths = TStringList;

  { TPathObject }

  TPathObject = class(TObject)
  public
    constructor Create(PathInfo: TPathString);
    constructor CreateByINIVal(Inifile: TINILocation; Section, key: String;
      default: String = 'INVALID');
      overload;
    constructor CreateByINIVal(Inifile: Tinifile; Section, key: String;
      default: String = 'INVALID');
  private
    pName: String;
    pPath: String;
    pIsRelative: Boolean;
    pPSettings : TProfileSettings;
    function pIsExisting: Boolean;
  public
    property path: String read pPath;
    property Name: String read pName;
    property isExistent: Boolean read pIsExisting;
    property isRelative: Boolean read pIsRelative;
  end;

  TPathList = class(TObjectList)
  private
    function GetObject(I: Integer): TPathObject;
    procedure SetObject(I: Integer; Obj: TPathObject);
  public
    property objects[I: Integer]: TPathObject read GetObject write SetObject;
  end;

  { TPathCollection }

  TPathCollection = class(TObject)
  public
    constructor Create(ListOfPaths: TListOfPaths);
    constructor CreateByINISec(iniFile: TINIFile; Section: String); overload;
    constructor CreateByINISec(iniFile: TINILocation; Section: String); overload;
  private
    pPaths: TPathList;
    function GetCount: Integer;
    function GetPaths(Index: Integer): TPathObject; overload;
    function GetPaths(Str: String): TPathObject; overload;
    function SearchName(Str: String): Integer;
    function SearchVal(Str: String): Integer;
  public
    property paths[Index: Integer]: TPathObject read GetPaths;
    property pathsBN[Str: String]: TPathObject read GetPaths;
    property Count: Integer read GetCount;
    property IndexOfName[Str: String]: Integer read SearchName;
    property IndexOfValue[Str: String]: Integer read SearchVal;
  end;

implementation

uses fmError;

{ TPathCollection }

constructor TPathCollection.Create(ListOfPaths: TListOfPaths);
var
  I: Integer;
begin
  pPaths := TPathList.Create(True);
  for i := 0 to (ListOfPaths.Count - 1) do
  begin
    pPaths.Add(TPathObject.Create(ListOfPaths[i]));
  end;
  inherited Create;
end;

constructor TPathCollection.CreateByINISec(iniFile: TINIFile; Section: String);
var
  tempKeys, tempVals: TStringList;
  INI: TIniFile;
  tempStr: String;
  I: Integer;
begin
  { DONE 75 -oL4YG -cBug_OfUnit : Comments withing read Section will break the creation! (arent't ignored!) }
  tempKeys := TStringList.Create;
  tempVals := TStringList.Create;
  iniFile.ReadSection(Section, tempKeys);
  for i := 0 to tempKeys.Count - 1 do
  begin
    tempStr := iniFile.ReadString(Section, tempKeys[i], ';*INVALID*');
    if not (Pos(';', tempStr) = 1) then
    begin
      tempVals.Add(tempStr);
    end;
  end;
  Create(tempVals);
  FreeAndNil(tempVals);
  FreeAndNil(tempKeys);
end;

constructor TPathCollection.CreateByINISec(iniFile, Section: String);
var
  tempKeys, tempVals: TStringList;
  INI: TIniFile;
  tempStr: String;
  I: Integer;
begin
  { DONE 75 -oL4YG -cBug_OfUnit : Comments withing read Section will break the creation! (arent't ignored!) }
  tempKeys := TStringList.Create;
  tempVals := TStringList.Create;
  INI := TIniFile.Create(iniFile);
  INI.ReadSection(Section, tempKeys);
  for i := 0 to tempKeys.Count - 1 do
  begin
    tempStr := INI.ReadString(Section, tempKeys[i], ';*INVALID*');
    if not (Pos(';', tempStr) = 1) then
    begin
      tempVals.Add(tempStr);
    end;
  end;
  Create(tempVals);
  FreeAndNil(INI);
  FreeAndNil(tempVals);
  FreeAndNil(tempKeys);
end;

function TPathCollection.GetPaths(Index: Integer): TPathObject;
begin
  Result := pPaths.objects[Index];
end;

function TPathCollection.GetPaths(Str: String): TPathObject;
var
  Index: Integer;
  dump: String;
begin
  Index := IndexOfName[Str];
  if (Index <> (-1)) then
  begin
    Result := pPaths.objects[Index];
  end
  else
  begin
    dump := 'Path: >>' + Str + '<< was not found in >>' + (Self.ToString) + '<<' + LineEnding;
    dump := dump+'ListDump:>>';
    for Index := 0 to self.Count - 1 do
      dump := dump + self.pPaths.objects[Index].Name + ';;';
    dump := dump+'<<';
    raise EListError.Create(dump);
  end;
end;

function TPathCollection.GetCount: Integer;
begin
  Result := pPaths.Count;
end;

function TPathCollection.SearchName(Str: String): Integer;
var
  I: Integer;
begin
  for I := 0 to pPaths.Count - 1 do
  begin
    if (UpperCase(pPaths.objects[i].pName) = UpperCase(Str)) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := (-1);
end;

function TPathCollection.SearchVal(Str: String): Integer;
var
  I: Integer;
begin
  for I := 0 to pPaths.Count - 1 do
  begin
    if (pPaths.objects[i].pPath = Str) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := (-1);
end;

{ TPathObject }

constructor Tpathobject.Create(Pathinfo: Tpathstring);
var
  SplitList: TStringList;
begin
  SplitList := TStringList.Create;
  SplitList.Delimiter := ':';
  SplitList.DelimitedText := Pathinfo;

  if (SplitList.Count < 3) then
  begin
    raise (EPathObjError.Create('Cannot create Path without valid pathString!' +
      LineEnding + 'Str: "' + Pathinfo + '"'));
  end
  else
  begin
    pPath := SplitList[0];
    pIsRelative := StrToBoolean(SplitList[1]);
    pName := SplitList[2];
    pPSettings := TProfileSettings.Create(pPath+'/profileIndex.ini');
  end;
  inherited Create;
end;

constructor TPathObject.CreateByINIVal(Inifile, Section, key: String;
  default: String = 'INVALID');
var
  INI: TIniFile;
begin
  INI := TIniFile.Create(Inifile);
  Create(INI.ReadString(Section, key, default));
  FreeAndNil(INI);
end;

constructor TPathObject.CreateByINIVal(Inifile: Tinifile; Section, key: String;
  default: String = 'INVALID');
begin
  Create(Inifile.ReadString(Section, key, default));
end;

function TPathObject.pIsExisting: Boolean;
begin
  Result := (FileExists(pPath+'/profileIndex.ini') and DirectoryExists(pPath));
end;

//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################

function TPathList.GetObject(I: Integer): TPathObject;
begin
  Result := TPathObject(Items[I]);
end;

procedure TPathList.SetObject(I: Integer; Obj: TPathObject);
begin
  Items[I] := Obj;
end;

//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################
//################################################################################################


end.
