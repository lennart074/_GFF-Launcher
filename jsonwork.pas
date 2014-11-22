Unit jsonWork;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, jsonparser, fpjson, jsonUtils, errorhandler,
  Dialogs;

Procedure listVersions(indexFile: String; OutList: TStrings);

Implementation

Procedure listVersions(indexFile: String; OutList: TStrings);
Var
  P: TJSONParser;
  D: TJSONData;
  Content, names, items: TStringList;
  i, index: Integer;
  str: String;
Begin
  Try
    Content := TStringList.Create;
    Content.LoadFromFile(indexFile);
    str := Content.Text;
    P := TJSONParser.Create(str);
    D := P.Parse;
    names := TStringList.Create;
    items := TStringList.Create;
    jsonUtils.ListNames(D, names);
    jsonUtils.ListItems(D, items);
    index := names.IndexOf('versions');

    Content.Clear;
    P.Free;
    str := items[index];
    //str := StringReplace(str,'[','{',[rfReplaceAll]);
    //str := StringReplace(str,']','}',[rfReplaceAll]);
    P := TJSONParser.Create(str);
    names.Clear;
    items.Clear;

    D := P.Parse;
    jsonUtils.ListItems(D, content);
    P.Free;
    D.Free;

    For i := 0 To (content.Count - 1) Do
    Begin
      P := TJSONParser.Create(content[i]);
      D := P.Parse;
      jsonUtils.ListNames(D, names);
      jsonUtils.ListItems(D, Items);

      OutList.Add(StringReplace(Items[names.IndexOf('id')], '"', '', [rfReplaceAll]));
    End;

  Except
    on E: Exception Do
    Begin
      Form_error.Handle(E, str, False);
    End;
  End;
  FreeAndNil(names);
  FreeAndNil(P);
  FreeAndNil(D);
  FreeAndNil(items);
  FreeAndNil(Content);
End;

End.
