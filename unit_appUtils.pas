{All funtions and procedures (except of "Like","MemoryStreamToString","DirectoryIsEmpty") are written by Life4YourGames!}


unit unit_appUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ShellApi, StrUtils, Process;

function getSystemType(Binary: Boolean = False): Integer;
function Like(const AString, APattern: string): boolean;
procedure CopyDir(fromDir, toDir: string);
procedure GetSubDirs(const directory: string; list: TStrings);
function run(executable, attribs, PUseDir: string; NewGroup: boolean = False): boolean;
function StrToBoolean(str: string; Strict: Boolean = False): boolean;
function BoolToString(str: boolean; Strict: Boolean = False): string;
function MemoryStreamToString(M: TMemoryStream): string;
function FindStringInList(list: TStringList; str: String): Integer;
function DirectoryIsEmpty(Directory: string): boolean;
function countString(str, char: String): Integer;
function FindMatchStr(Strings: TStringList; const SubStr: string): Integer;
function SortDown(List: TStringList; I1, I2: Integer): Integer;

implementation

//==============================================================================
//==============================================================================
//==============================================================================
{ Like prüft die Übereinstimmung eines Strings mit einem Muster.
  So liefert Like('Delphi', 'D*p?i') true.
  Der Vergleich berücksichtigt Klein- und Großschreibung.
  Ist das nicht gewünscht, muss statt dessen
  Like(AnsiUpperCase(AString), AnsiUpperCase(APattern)) benutzt werden: }

function Like(const AString, APattern: string): boolean;
var
  StringPtr, PatternPtr: PChar;
  StringRes, PatternRes: PChar;
begin
  Result := False;
  StringPtr := PChar(AString);
  PatternPtr := PChar(APattern);
  StringRes := nil;
  PatternRes := nil;
  repeat
    repeat // ohne vorangegangenes "*"
      case PatternPtr^ of
        #0:
        begin
          Result := StringPtr^ = #0;
          if Result or (StringRes = nil) or (PatternRes = nil) then
            Exit;
          StringPtr := StringRes;
          PatternPtr := PatternRes;
          Break;
        end;
        '*':
        begin
          Inc(PatternPtr);
          PatternRes := PatternPtr;
          Break;
        end;
        '?':
        begin
          if StringPtr^ = #0 then
            Exit;
          Inc(StringPtr);
          Inc(PatternPtr);
        end;
        else
        begin
          if StringPtr^ = #0 then
            Exit;
          if StringPtr^ <> PatternPtr^ then
          begin
            if (StringRes = nil) or (PatternRes = nil) then
              Exit;
            StringPtr := StringRes;
            PatternPtr := PatternRes;
            Break;
          end
          else
          begin
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        end;
      end;
    until False;
    repeat // mit vorangegangenem "*"
      case PatternPtr^ of
        #0:
        begin
          Result := True;
          Exit;
        end;
        '*':
        begin
          Inc(PatternPtr);
          PatternRes := PatternPtr;
        end;
        '?':
        begin
          if StringPtr^ = #0 then
            Exit;
          Inc(StringPtr);
          Inc(PatternPtr);
        end;
        else
        begin
          repeat
            if StringPtr^ = #0 then
              Exit;
            if StringPtr^ = PatternPtr^ then
              Break;
            Inc(StringPtr);
          until False;
          Inc(StringPtr);
          StringRes := StringPtr;
          Inc(PatternPtr);
          Break;
        end;
      end;
    until False;
  until False;
end; {Michael Winter}

function MemoryStreamToString(M: TMemoryStream): ansistring;
begin
  SetString(Result, PAnsiChar(M.Memory), M.Size);
end;

function DirectoryIsEmpty(Directory: string): boolean;
var
  SR: TSearchRec;
  i: integer;
begin
  Result := False;
  FindFirst(IncludeTrailingPathDelimiter(Directory) + '*', faAnyFile, SR);
  for i := 1 to 2 do
    if (SR.Name = '.') or (SR.Name = '..') then
      Result := FindNext(SR) <> 0;
  FindClose(SR);
end;


//==============================================================================
//==============================================================================
//==============================================================================

function run(executable, attribs, PUseDir: string; NewGroup: boolean = False): boolean;
var
  AProcess: TProcess;
begin
  Result := False;
  if (executable <> ' ') and (executable <> '') then
  begin
    AProcess := TProcess.Create(nil);
    AProcess.Executable := executable;

    if (attribs <> ' ') and (attribs <> '') then
    begin
      AProcess.Parameters.Add(attribs);
    end;

    if (PUseDir <> ' ') and (PUSeDir <> '') then
    begin
      AProcess.CurrentDirectory := PUseDir;
    end;

    if (NewGroup = True) then
    begin
      AProcess.Options := AProcess.Options + [poNewProcessGroup];
    end;

    AProcess.Execute;
    Result := True;
    AProcess.Free;

  end
  else
  begin
    Result := False;
  end;
end;

function FindStringInList(list: TStringList; str: String): Integer;
var
  I, Found: Integer;
begin
  i := 0;
  repeat
    Found := (Pos(str, list[i]));
    Inc(i);
  until (Found >= 1) or (i = list.Count);
  if (Found >= 1) then
  begin
    Result := Found;
  end
  else
  begin
    Result := -1;
  end;
end;

procedure GetSubDirs(const directory: string; list: TStrings);
var
  sr: TSearchRec;
begin
  try
    if FindFirst(IncludeTrailingPathDelimiter(directory) + '*.*',
      faDirectory, sr) < 0 then
      Exit
    else
      repeat
        if ((sr.Attr and faDirectory <> 0) AND (sr.Name <> '.') AND
          (sr.Name <> '..')) then
          List.Add({IncludeTrailingPathDelimiter(directory) +} sr.Name);
      until FindNext(sr) <> 0;
  finally
    SysUtils.FindClose(sr);
  end;
end;

procedure CopyDir(fromDir, toDir: string);
var
  OpStruct: TSHFileOpStruct;
begin
  AppendStr(fromDir, #0);
  AppendStr(toDir, #0);
  with OpStruct do
  begin
    Wnd := 0;
    wFunc := FO_COPY;
    pFrom := @fromDir[1];
    pTo := @toDir[1];
    fFlags := FOF_ALLOWUNDO or FOF_SILENT or FOF_NOCONFIRMATION;
  end;
  if not DirectoryExists(toDir) then
    ForceDirectories(toDir);
  SHFileOperation(OpStruct);
end;

function StrToBoolean(str: string; Strict: Boolean = False): boolean;
var
  Res: boolean;
begin
  if (Strict = True) then
  begin
    if str = 'false' then
      Res := False;
    if str = 'true' then
      Res := True;
    Result := Res;
  end
  else
  begin
    if (Pos('false', str) > 0) or (Pos('False', str) > 0) then
      Res := False;
    if (Pos('true', str) > 0) or (Pos('True', str) > 0) then
      Res := True;
    Result := Res;
  end;
end;

function BoolToString(str: boolean; Strict: Boolean = False): string;
var
  Res: string;
begin
  if str = False then
    Res := 'false';
  if str = True then
    Res := 'true';
  Result := Res;
end;

//-----BEGIN-Source:https://stackoverflow.com/questions/6341449/tstringlist-indexof-wildcard-within-indexof-----
function FindMatchStr(Strings: TStringList; const SubStr: string): Integer;
begin
  for Result := 0 to Strings.Count - 1 do
  begin
    if (Pos(UpperCase(SubStr), UpperCase(Strings[Result])) > 0) then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;
//-----END-Source-----


function SortDown(List: TStringList; I1, I2: Integer): Integer;
begin
  Result := CompareStr(List.Strings[I2], List.Strings[I1]);
end;

function getSystemType(Binary: Boolean = False): Integer;
var
  windir: String;
begin
  windir := SysUtils.GetEnvironmentVariable('windir');
  if DirectoryExists(windir + '\SysWOW64') and
    DirectoryExists(ExtractFileDrive(windir) + '\Program Files (x86)') then
  begin
    Result := 64;
  end
  else
  begin
    if Binary = True then
    begin
      Result := 86;
    end
    else
    begin
      Result := 32;
    end;
  end;
end;

function countString(str, char: String): Integer;
var
  i: Integer;
begin
  Result := 0;
  i := Length(Str);
  for i := 1 to i do
  begin
    if Str[i] = char then
      Inc(Result);
  end;
end;

end.
