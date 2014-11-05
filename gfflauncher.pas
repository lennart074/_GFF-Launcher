Unit gfflauncher;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, IniFiles;

//Additional classes

Type
  MissingFileException = Class(Exception)
  End;

  ////////////////////////////////////////////////////////////////////////////////
  //############################################################################//
  ////////////////////////////////////////////////////////////////////////////////


  //Additional Objects
  TSettings = Class(TObject)
  Public
    settingsFile: String;
    INI: TIniFile;
    Constructor Create(INIFile: String); Overload;
    Constructor Create(INIFile: TIniFile); Overload;
    Destructor Destroy;
  End;

Procedure GetSubDirectories(Const directory: String; list: TStrings);
////////////////////////////////////////////////////////////////////////////////
//############################################################################//
////////////////////////////////////////////////////////////////////////////////

Implementation

Uses errorhandler;

////////////////////////////////////////////////////////////////////////////////
//############################################################################//
////////////////////////////////////////////////////////////////////////////////

Constructor TSettings.Create(INIFile: String); Overload;
Begin
  settingsFile := INIFile;
  INI := TIniFile.Create(INIFile);
  Inherited Create;
End;

Constructor TSettings.Create(INIFile: TINIFile); Overload;
Begin
  INI := INIFile;
  Inherited Create;
End;

Destructor TSettings.Destroy;
Begin
  INI.Free;
  Inherited Destroy;
End;

////////////////////////////////////////////////////////////////////////////////
//############################################################################//
////////////////////////////////////////////////////////////////////////////////

Procedure GetSubDirectories(Const directory: String; list: TStrings);
Var
  sr: TSearchRec;
Begin
  Try
    If FindFirst(IncludeTrailingPathDelimiter(directory) + '*.*',
      faDirectory, sr) < 0 Then
      Exit
    Else
      Repeat
        If ((sr.Attr And faDirectory <> 0) And (sr.Name <> '.') And
          (sr.Name <> '..')) Then
          List.Add(IncludeTrailingPathDelimiter(directory) + sr.Name);
      Until FindNext(sr) <> 0;
  Finally
    SysUtils.FindClose(sr);
  End;
End;      { FROM DELPHI EXAMPLES }

End.
