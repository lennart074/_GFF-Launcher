unit mojang;

{$mode objfpc}{$H+}

interface

Uses
  Classes, SysUtils,
  { Online } httpsend, ssl_openssl,
  { SynaUtil } synautil,
  { Responses } fpjson, jsonparser, jsonUtils,
  { Feedback } Dialogs,
  { AppUtils } appUtils,
  { UserObject } obj_user
  ;

Const
  DIA_DEN = 'DENY DIALOGS';
  DIA_ALL = 'ALLOW DIALOGS';

Function Authenticate(lUsrObj: TUserObject;
  Const username, password, syntax, adress: String;
  Const arg: String = DIA_DEN): Boolean;
Function LogOut(url, accessToken, clientToken: String): Boolean;
Function GetFile(url, dest: String): Boolean;
Function GetBinFile(url, dest: String): Boolean;
function StreamToString(Stream: TStream): String;

implementation

uses launcher, fmLog;

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
    then
    begin
      Inc(BytesRead, NumBytes);
    end
    else
      BREAK; // Program has finished execution.
  end;

  MemStream.SetSize(BytesRead);

  OutputLines := TStringList.Create;
  OutputLines.LoadFromStream(MemStream);

  for NumBytes := 0 to OutputLines.Count - 1 do
  begin
    Result := Result + LineEnding + OutputLines[NumBytes];
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
  If (mojang.HttpPostURL(URL, contenttype, PostValue, OutStrm) = True) Then
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

Function Authenticate(lUsrObj: TUserObject;
  Const username, password, syntax, adress: String;
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

  Log(etInfo,'Trying auth',5);
  Log(etInfo,'with :'+LineEnding+
  StringReplace(str,password,'**REDACTED**',[rfReplaceAll])
  ,4);

  POSTFile(adress, str, 'application/json', RespStream);
  Resp := MemoryStreamToString(RespStream);

  RespStream.Free;
  If Length(Resp) > 5 Then
  Begin
    PString := Resp;
    P := TJSONParser.Create(PString);
    Try
      Names := TStringList.Create;
      Items := TStringList.Create;
      parse := 1;
      D := P.Parse;
      jsonUtils.ListNames(D, Names);
      jsonUtils.ListItems(D, Items);
      errorIndex := Names.IndexOf('errorMessage');
      If ((errorIndex >= 0) And (arg <> DIA_DEN)) Then
      Begin
        Log(etError,'Unable to login!',5);
        Log(etError,'Response:'+LineEnding+Resp,3);
        ShowMessage('Error occured!' + LineEnding + 'MSG:' + Items[errorIndex]);

        Result := False;
      End
      Else
      Begin
        //tempIndex := names.IndexOf('clientToken');
        //lUsrObj.UUID := Items[tempIndex];
        lUsrObj.UUID := TJSONObject(D).Get('clientToken');
        Log(etDebug,'UUID:'+lUsrObj.UUID,3);

        //tempIndex := names.IndexOf('accessToken');
        //lUsrObj.accessToken := items[tempIndex];
        lUsrObj.accessToken := TJSONObject(D).Get('accessToken');
        Log(etDebug,'AToken:'+lUsrObj.accessToken,3);

        tempIndex := Names.IndexOf('selectedProfile');
        P.Free;
        D.Free;

        //Prevent '"legacy":True' - Bug! (will crash parser otherwise)
        items[tempIndex] := StringReplace(items[tempIndex], 'True',
          '"True"', [rfReplaceAll]);

        PString := items[tempIndex];
        P := TJSONParser.Create(PString);
        Parse := 2;
        D := P.Parse;
        Begin
          jsonUtils.ListNames(D, Names);
          jsonUtils.ListItems(D, Items);
          {tempIndex := names.IndexOf('name');
          lUsrObj.username :=
            StringReplace(Items[tempIndex], '"', '', [rfReplaceAll]);}
          lUsrObj.username := TJSONObject(D).Get('name');
          Log(etDebug,'Username:'+lUsrObj.username,4);

          //tempIndex := Names.IndexOf('id');
          //lUsrObj.profileID := Items[tempIndex];
          lUsrObj.profileID := TJSONObject(D).Get('id');
          Log(etDebug,'ProfileID:'+lUsrObj.profileID,3);

          tempIndex := Names.IndexOf('legacy');
          If (tempIndex >= 0) Then
          Begin
            lUsrObj.isLegacy := StrToBoolean(items[tempIndex]);
            lUsrObj.userType := 'legacy';
          End
          else
          begin
            lUsrObj.isLegacy := False;
            lUsrObj.userType := 'mojang';
          end;
          Log(etDebug,'userType:'+lUsrObj.userType,4);
        End;

        { TODO 05 -oL4YG -clUsrObject : Define Userproperties correctly! }
        lUsrObj.userProperties := '{}';
        Log(etDebug,'userProps:'+lUsrObj.userProperties,4);

        FreeAndNil(P);
        FreeAndNil(D);
        FreeAndNil(names);
        FreeAndNil(items);

        Log(etInfo,'Successful login',5);

        Result := True;
        //Authenticated
      End;

    Except
      on E: Exception Do
      Begin
        Result := False;
        {Form_error.HandleException(E, 'JSONParsing failed at string:' + LineEnding +
          PString + LineEnding + 'Parse:' + IntToStr(parse));}
        { TODO 100 -oL4YG -cRelease_port : Catch Exceptions }
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
  HTTP: THTTPSend;
Begin
  HTTP := THTTPSend.Create;
  If (HTTP.HTTPMethod('Get', url)) Then
  Begin
    ForceDirectories(ExtractFileDir(dest));
    HTTP.Document.SaveToFile(dest);
    FreeAndNil(HTTP);
    Result := True;
  End
  Else
  Begin
    FreeAndNil(HTTP);
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

end.
