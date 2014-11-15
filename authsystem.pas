Unit authsystem;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils,
  { Online } httpsend, ssl_openssl,
  { SynaUtil } synautil,
  { Responses } fpjson, jsonparser, jsonUtils,
  { Main Unit } gfflauncher,
  { Errors } errorhandler,
  { Dialogs } Dialogs,
  { AppUtils }unit_appUtils;

Const
  DIA_DEN = 'DENY DIALOGS';

Const
  DIA_ALL = 'ALLOW DIALOGS';

Function Authenticate(Const username, password, syntax, adress: String;
  Const arg: String = DIA_DEN): Boolean;
Function LogOut(url, accessToken, clientToken: String): Boolean;
Function GetFile(url, dest: String): Boolean;
Function GetBinFile(url, dest: String): Boolean;
function StreamToString(Stream: TStream): String;

Implementation

Uses { Userdata }login;


//Override the HttpPostURL -Function of Synapse, to fit it into Mojangs systems ;)
Function HttpPostURL(Const URL, contenttype, URLData: String;
  Const Data: TStream): Boolean;
Var
  HTTP: THTTPSend;
Begin
  HTTP := THTTPSend.Create;
  Try
    WriteStrToStream(HTTP.Document, URLData);
    //HTTP.MimeType := 'application/x-www-form-urlencoded';
    //Exchanged prev. line with next line, as we need 'application/json' as contenttype!
    HTTP.MimeType := contenttype;
    Result := HTTP.HTTPMethod('POST', URL);
    If Result Then
      Data.CopyFrom(HTTP.Document, 0);
  Finally
    HTTP.Free;
  End;
End;

//-----Begin-Source:http://wiki.freepascal.org/Executing_External_Programs/de-----
function StreamToString(Stream: TStream): String;
const
  READ_BYTES = 2048;

var
  OurCommand: String;
  OutputLines: TStringList;
  MemStream: TMemoryStream;
  NumBytes: LongInt;
  BytesRead: LongInt;

begin
  // A temp Memorystream is used to buffer the output
  MemStream := TMemoryStream.Create;
  BytesRead := 0;

  while True do
  begin
    // make sure we have room
    MemStream.SetSize(BytesRead + READ_BYTES);

    // try reading it
    NumBytes := Stream.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
    if NumBytes > 0 // All read() calls will block, except the final one.
    then begin
      Inc(BytesRead, NumBytes);
    end else
      BREAK // Program has finished execution.
  end;

  MemStream.SetSize(BytesRead);

  OutputLines := TStringList.Create;
  OutputLines.LoadFromStream(MemStream);

  for NumBytes := 0 to OutputLines.Count - 1 do
  begin
    Result:=Result+LineEnding+OutputLines[NumBytes];
  end;

  OutputLines.Free;
  MemStream.Free;
//-----END-Source-----
end;

Function MemoryStreamToString(M: TMemoryStream): Ansistring;
Begin
  SetString(Result, PAnsiChar(M.Memory), M.Size);
End;

Procedure StringToMemStream(AString: Ansistring; Strm: TMemoryStream);
Var
  Len: Integer;
Begin
  If Strm <> nil Then
  Begin
    Len := Length(AString);
    Strm.Size := Len;
    Strm.Position := 0;
    Strm.Write(PChar(AString)^, Len);
    Strm.Position := 0;
  End;
End;

Procedure POSTFile(Const URL, PostValue, contenttype: String; OutStrm: TMemoryStream);
Begin
  If (authsystem.HttpPostURL(URL, contenttype, PostValue, OutStrm) = True) Then
  Begin

  End
  Else
  Begin
    StringToMemStream('{"error":"PNSError","errorMessage":"Post not successful! ' +
      'Please check if the program is able to connect to the internet.' + '"}', OutStrm);
  End;
End;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

Function Authenticate(Const username, password, syntax, adress: String;
  Const arg: String = DIA_DEN): Boolean;
Var
  RespStream: TMemoryStream;
  str, Resp, PString: String;
  P: TJSONParser;
  D: TJSONData;
  Names, Items: TStringList;
  errorIndex, tempIndex, parse: Integer;
Begin
  str := StringReplace(syntax, '%username%', username, [rfReplaceAll]);
  str := StringReplace(str, '%password%', password, [rfReplaceAll]);
  RespStream := TMemoryStream.Create;
  POSTFile(adress, str, 'application/json', RespStream);
  Resp := MemoryStreamToString(RespStream);
  RespStream.SaveToFile('GFFLauncher/temp/loginDump.json');
  { TODO 10 -oL4YG -cDebug : Remove Dump File }
  RespStream.Free;
  If Length(Resp) > 5 Then
  Begin
    PString:=Resp;
    P := TJSONParser.Create(PString);
    Try
      Names := TStringList.Create;
      Items := TStringList.Create;
      parse:=1;
      D := P.Parse;
      jsonUtils.ListNames(D, Names);
      jsonUtils.ListItems(D, Items);
      errorIndex := Names.IndexOf('errorMessage');
      If ((errorIndex >= 0) And (arg <> DIA_DEN)) Then
      Begin
        ShowMessage('Error occured!'+ LineEnding +'MSG:'+ Items[errorIndex]);
        Result := False;
      End
      Else
      Begin
        //tempIndex := names.IndexOf('clientToken');
        //UsrObj.UUID := Items[tempIndex];
        UsrObj.UUID := TJSONObject(D).Get('clientToken');
        //tempIndex := names.IndexOf('accessToken');
        //UsrObj.accessToken := items[tempIndex];
        UsrObj.accessToken := TJSONObject(D).Get('accessToken');
        tempIndex := Names.IndexOf('selectedProfile');
        P.Free;
        D.Free;

        //Prevent '"legacy":True' - Bug!
        items[tempIndex]:=StringReplace(items[tempIndex],'True','"True"',[rfReplaceAll]);

        PString:=items[tempIndex];
        P := TJSONParser.Create(PString);
        Parse:=2;
        D := P.Parse;
        Begin
          jsonUtils.ListNames(D, Names);
          jsonUtils.ListItems(D, Items);
          {tempIndex := names.IndexOf('name');
          UsrObj.username :=
            StringReplace(Items[tempIndex], '"', '', [rfReplaceAll]);}
          UsrObj.username := TJSONObject(D).Get('name');
          //tempIndex := Names.IndexOf('id');
          //UsrObj.profileID := Items[tempIndex];
          UsrObj.profileID := TJSONObject(D).Get('id');

          tempIndex := Names.IndexOf('legacy');
          If (tempIndex >= 0) Then
          Begin
            UsrObj.isLegacy := StrToBoolean(items[tempIndex]);
            UsrObj.userType := 'legacy';
          End else begin
            UsrObj.isLegacy := False;
            UsrObj.userType := 'mojang';
          end;
        End;

        { TODO 05 -oL4YG -cUsrObject : Define Userproperties correctly! }
        UsrObj.userProps := '{}';

        FreeAndNil(P);
        FreeAndNil(D);
        FreeAndNil(names);
        FreeAndNil(items);
        Result := True;
        //Authenticated
      End;

    Except
      on E: Exception Do
      Begin
        Result := False;
        Form_error.Handle(E, 'JSONParsing failed at string:' + LineEnding + PString + LineEnding + 'Parse:'+IntToStr(parse));
      End;
    End;
  End;
End;

Function LogOut(url, accessToken, clientToken: String): Boolean;
Var
  RespStream: TMemoryStream;
Begin
  RespStream := TMemoryStream.Create;
  If (HttpPostURL(url, 'application/json', '{"accessToken": "' +
    accessToken + '","clientToken":"' + clientToken + '"}', RespStream)) Then
  Begin
    If (Length(MemoryStreamToString(RespStream)) > 0) Then
    Begin
      Result := False;
    End
    Else
    Begin
      Result := True;
    End;
  End
  Else
  Begin
    Result := False;
  End;
End;

Function GetFile(url, dest: String): Boolean;
Var
  RespList: TStringList;
Begin
  RespList := TStringList.Create;
  If (HttpGetText(url, RespList)) Then
  Begin
    ForceDirectories(ExtractFileDir(dest));
    RespList.SaveToFile(dest);
    FreeAndNil(RespList);
    Result := True;
  End
  Else
  Begin
    FreeAndNil(RespList);
    Result := False;
  End;

End;

Function GetBinFile(url, dest: String): Boolean;
Var
  RespStream: TMemoryStream;
Begin
  RespStream := TMemoryStream.Create;
  If (HttpGetBinary(url, RespStream)) Then
  Begin
    ForceDirectories(ExtractFileDir(dest));
    RespStream.SaveToFile(dest);
    FreeAndNil(RespStream);
    Result := True;
  End
  Else
  Begin
    FreeAndNil(RespStream);
    Result := False;
  End;

End;

End.
