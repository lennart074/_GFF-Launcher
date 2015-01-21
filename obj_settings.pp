unit obj_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, registry;

type

  TINILocation = String;

  { TSettings }

  TSettings = Class(TObject)
  Public
    settingsFile: String;
    INI: TIniFile;
    Constructor Create(INIFile: String); Overload;
    Constructor Create(INIFile: TIniFile); Overload;
    Destructor Destroy;
  End;

  TProfileSettings = class(TSettings)
  Public
    Property ProfileIndex : TINILocation read settingsFile write settingsFile;
  end;

  { TProgramSettings }

  TProgramSettings = Class(TSettings)
  Public
    javaPath: String;
    BaseArgs: String;
    Memory: Integer;
    BaseMem: Integer;
    logLevel: Integer;
    getUpdates, customMem, showConsole, showMCConsole: Boolean;
    Procedure WriteFile;
    Procedure ReadFile;
    function DetectJava: String;
  End;

  { TWebSettings }

  TWebSettings = Class(TSettings)
  Public
    authSyntax: String;
    authURL: String;
    download_index: String;
    download_subIndex: String;
    download_assetIndex: String;
    download_clientJar: String;
    download_serverJar: String;
    download_serverEXE: String;
    download_libs: String;
    downloas_assets: String;
    Procedure ReadFile;
  End;

implementation

{ TWebSettings }

uses launcher;

Procedure TWebSettings.ReadFile;
begin
  authSyntax := INI.ReadString('Authentication', 'SendSyntax',
    '{"agent":{"name": "Minecraft","version": 1},"username": "%username%","password": "%password%"}');
  authURL := INI.ReadString('Authentication', 'server',
    'https://authserver.mojang.com/authenticate');

  download_index := INI.ReadString('DownloadLocations', 'Index',
  'http://s3.amazonaws.com/Minecraft.Download/versions/versions.json');
  download_subIndex := INI.ReadString('DownloadLocations', 'SubIndex',
  'http://s3.amazonaws.com/Minecraft.Download/versions/%version%/%version%.json');
  download_assetIndex := INI.ReadString('DownloadLocations', 'AssetIndex',
  'https://s3.amazonaws.com/Minecraft.Download/indexes/%index%.json');
  download_clientJar := INI.ReadString('DownloadLocations', 'ClientJar',
  'http://s3.amazonaws.com/Minecraft.Download/versions/%version%/%version%.jar');
  download_serverJar := INI.ReadString('DownloadLocations', 'ServerJar',
  'http://s3.amazonaws.com/Minecraft.Download/versions/%version%/minecraft_server.%version%.jar');
  download_serverEXE := INI.ReadString('DownloadLocations', 'ServerExe',
  'http://s3.amazonaws.com/Minecraft.Download/versions/%version%/minecraft_server.%version%.exe');
  download_libs := INI.ReadString('DownloadLocations', 'Libs',
  'https://libraries.minecraft.net/%LibPath%');
  downloas_assets := INI.ReadString('DownloadLocations', 'Assets',
  'http://resources.download.minecraft.net%assetPath%');
end;

{ TProgramSettings }

Procedure TProgramSettings.WriteFile;
Begin
  INI.WriteString('GFFLauncher', 'javaPath', 'auto');
  INI.WriteBool('GFFLauncher', 'getUpdates', False);
  INI.WriteBool('GFFLauncher', 'showCons', True);
  INI.WriteBool('Minecraft', 'showCons', False);
  INI.WriteString('Minecraft','BaseArgs',
  '-Djava.library.path=%currDir%\Binaries\LWJGL\windows\natives '+
  '-cp %Libs%%JarPath% %mainClass% -jar %jarPath%');
  INI.WriteInteger('GFFLauncher', 'loglevel', 0);
  INI.UpdateFile; //Make sure that changes are really written
End;

Procedure TProgramSettings.ReadFile;
var
  tempStr : String;
Begin
  logLevel := INI.ReadInteger('GFFLauncher', 'loglevel', 0);
  showConsole := (logLevel <> -1);
  javaPath := INI.ReadString('GFFLauncher', 'javaPath', 'auto');
  getUpdates := INI.ReadBool('GFFLauncher', 'getUpdates', True);
  BaseArgs := INI.ReadString('Minecraft', 'BaseArgs',
      '-Djava.library.path=%currDir%\Binaries\LWJGL\windows\natives '+
  '-cp %Libs%"%jarPath%" %mainClass% -jar %jarPath%');
  //showConsole := INI.ReadBool('GFFLauncher', 'showCons', True);
  showMCConsole := INI.ReadBool('Minecraft', 'showCons', False);

  if (javaPath = 'auto') or (javaPath = 'java') or
    (FileExists(javaPath) = False) then
  begin
    Log(etInfo, 'Detecting Java', 1);
    tempStr := DetectJava;
    Log(etDebug, 'Java Home: ' + tempStr, 0);
    javaPath := tempStr + '\bin\javaw.exe';
    Log(etInfo, 'Full path:' + javaPath, 1);
  end;

End;

function TProgramSettings.DetectJava: String;
var
  jrePath: String = '';
  RegAcc: TRegistry;
begin
  jrePath := '_NotFound_';
  RegAcc := TRegistry.Create;
  RegAcc.RootKey := HKEY_LOCAL_MACHINE;
  if RegAcc.OpenKeyReadOnly('\SOFTWARE\JavaSoft\Java Runtime Environment\1.7') then
  begin
    jrePath := RegAcc.ReadString('JavaHome');
  end;
  FreeAndNil(RegAcc);
  Result := jrePath;
end;


{ TSettings }

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

end.
