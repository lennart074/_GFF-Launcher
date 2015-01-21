Unit jsonUtils;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, StrUtils, fpjson, jsonparser;

Function JSONToString(J: TJSonData): String;
Procedure ArrayToList(J: TJSONData; ToList: TStringList; Advanced: Boolean = False);
Procedure ListStringToList(str: String; ToList: TStringList);
Function getValue(Const pair: String; Const name0value1: Integer = 1): String;
Procedure ListNames(Data: TJSONData; GoalList: TStringList);
Procedure ListItems(Data: TJSONData; GoalList: TStringList);

Implementation

Function JSONToString(J: TJSONData): String;

Var
  I: Integer;
  Res: String;
Begin
  Res := '';
  // JSONType property determines kind of value.
  Case J.jsontype Of
    jtNull: Res := Res + 'Null';
    jtBoolean: If J.AsBoolean Then
        Res := Res + 'True'
      Else
        Res := Res + 'False';
    jtNumber: {JSONNumber has extra NumberType property
                which determines kind of value (int/float).}
      Case TJSONNumber(J).NumberType Of
        ntInteger: Res := Res + IntToStr(J.AsInteger);
        ntFloat: Res := Res + FloatToStr(J.AsFloat / 10 / 2);
      End;
    jtString: Res := Res + '"' + J.AsString + '"';
    jtArray:
    Begin
      Res := Res + '[';
      For I := 0 To J.Count - 1 Do
      Begin
        Res := Res + JSONToString(J.Items[I]);
        If I < J.Count - 1 Then
          Res := Res + ', ';
      End;
      Res := Res + ']';
    End;
    jtObject:
    Begin
      Res := Res + '{';
      For I := 0 To J.Count - 1 Do
      Begin
        Res := Res + '"' + TJSONObject(J).Names[i] + '":';
        Res := Res + JSonToString(J.Items[I]);
        If I < J.Count - 1 Then
          Res := Res + ',';
      End;
      Res := Res + '}';
    End;
  End;
  Result := Res;
End;

Procedure ArrayToList(J: TJSONData; ToList: TStringList; Advanced: Boolean = False);
Var
  //Res: TStringList;
  sPos, I: Integer;
  r1, r2, r3: Integer;
  closed, quit: Boolean;
  str: String;
  Par: TJSONParser;
  Data: TJSONData;
Begin
  str := JSONToString(J);
  sPos := 1;
  Try
    //Res := TStringList.Create;
    //--------------------------------------------
    If (Advanced = True) Then
    Begin
      Par := TJSONParser.Create(str);
      Data := Par.Parse;
      For i := 0 To Data.Count - 1 Do
      Begin
        ToList.Add(JSONToString(Data.Items[i]));
      End;
      //---------------------------------------------
    End
    Else
    Begin
      //---------------------------------------------
      If (Pos('[', str) = 1) And (PosEx(']', str, length(str)) = length(str)) Then
        str := Copy(str, 2, length(str) - 2);

      ToList.Add(str);
      str := StringReplace(str, '[', '{', [rfReplaceAll]);
      str := StringReplace(str, ']', '}', [rfReplaceAll]);
      sPos := 1;
      Repeat
        r1 := Pos('{', str);
        If r1 < sPos Then
        Begin
          r1 := Pos('[', str);
        End;
        If r1 < sPos Then
          quit := True;

        r2 := Pos('}', str);
        If r2 < sPos Then
        Begin

          r2 := Pos(']', str);
        End;
        If r2 < sPos Then
          quit := True;

        If (r1 >= sPos) And (r2 > sPos) Then
        Begin
          //Res.Add('"'+InttoStr(r1)+'" : "'+IntToStr(r2)+'",'+Copy(str,r1+1,(r2-r1)-1)); //FOR DEBUG-USES
          ToList.Add(Copy(str, r1 + 1, (r2 - r1) - 1));
          str := Copy(str, r2 + 1, length(str));
        End;
      Until quit = True;
    End;
    //---------------------------------------------
  Finally
    //ToList.Assign(Res);
    //FreeAndNil(Res);
  End;

End;

Procedure ListStringToList(str: String; ToList: TStringList);
Begin
  ToList.Clear;
  ToList.Delimiter := ',';
  ToList.StrictDelimiter := True;
  ToList.DelimitedText := str;
End;

Function getValue(Const pair: String; Const name0value1: Integer = 1): String;
Var
  Res: TStringList;
Begin
  Res := TStringList.Create;
  Res.Delimiter := (':');
  Res.StrictDelimiter := False;
  Res.DelimitedText := pair;
  Result := Res[name0value1];
  Res.Free;
End;

Procedure ListNames(Data: TJSONData; GoalList: TStringList);
Var
  i: Integer;
Begin
  GoalList.Clear;
  For i := 0 To Data.Count - 1 Do
  Begin
    GoalList.Add(TJSONObject(Data).Names[i]);
  End;
End;

Procedure ListItems(Data: TJSONData; GoalList: TStringList);
Var
  i: Integer;
Begin
  GoalList.Clear;
  For i := 0 To Data.Count - 1 Do
  Begin
    GoalList.Add(JSONToString(TJSONObject(Data).Items[i]));
  End;
End;

End.














