Unit profiles_BETA;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Fileutil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, types,
  { L.Screen } setupMC,
  { JSON } jsonWork, jsonUtils,
  { appUtils } unit_appUtils,
  { Errors } errorhandler,
  { ObjCollection } objcollection,
  { Userdata } login,
  { Self-Explaining } Log,
  { GFFLauncher } gfflauncher
  ;

Type

  { ProfileList }

  { TForm_profilesBETA }

  TForm_profilesBETA = Class(TForm)
    CheckBox_forgeReady: Tcheckbox;
    ComboBox_channel: Tcombobox;
    Image_crMC_Forge1: TImage;
    Image_crMC_Forge2: TImage;
    Image_crMC_Forge3: TImage;
    Image_crCat_vanilla: TImage;
    Image_crCat_custom: TImage;
    Image_crCat_predefined: TImage;
    Image_background: TImage;
    Label_crMC_Forge1: TLabel;
    Label_crmc_forge2: Tlabel;
    Label_crmc_forge3: Tlabel;
    PageControl_Profiles: TPageControl;
    StaticText_channel: Tstatictext;
    TabSheet_choose: TTabSheet;
    TabSheet_crMC_Forge: TTabSheet;
    Tabsheet3: TTabSheet;
    TabSheet_profileType: TTabSheet;
    Trackbar1: Ttrackbar;
    Procedure Formcreate(Sender: TObject);
    Procedure Image_closeclick(Sender: TObject);
    Procedure Pagecontrol_profileschange(Sender: TObject);
    Procedure Pagecontrol_profileschanging(Sender: TObject;
      Var Allowchange: Boolean);
    Procedure CreateProfile();
    Procedure LoadProfiles(Directory:String);
    function CheckForProfileChanges(Directory:String):Boolean;
  Private
    { private declarations }
    Favourites, betaVersions, alphaVersions, preAlphaVersions,
    Versions, snapshots: TStringList;
    CreateNewP: Boolean;
  Public
    { public declarations }
  End;

Var
  Form_profilesBETA: TForm_profilesBETA;
  ProfileList: TProfileCollection;

Implementation

{$R *.lfm}

{ TForm_profilesBETA }

Procedure Tform_profilesbeta.Pagecontrol_profileschange(Sender: TObject);
Begin
  PageControl_Profiles.ActivePage.TabVisible := True;
End;

Procedure Tform_profilesbeta.Formcreate(Sender: TObject);
Var
  i: Integer;
Begin
  For i := 0 To PageControl_Profiles.PageCount - 1 Do
    PageControl_Profiles.Pages[i].TabVisible := False;

  ProfileList:=TProfileCollection.Create;
  if (Length(UsrObj.GFFProfilePath)>1) then begin
     LoadProfiles(UsrObj.GFFProfilePath);
  end;

End;

Procedure Tform_profilesbeta.Image_closeclick(Sender: TObject);
Begin
  Form_profilesBETA.Close;
End;

Procedure Tform_profilesbeta.Pagecontrol_profileschanging(Sender: TObject;
  Var Allowchange: Boolean);
Begin
  PageControl_Profiles.ActivePage.TabVisible := False;
End;


Procedure TForm_ProfilesBETA.CreateProfile();
Var
  IndexURL: String;
  i, i1, i2, i3, i4, i5: Integer;
  sorted: Boolean;
Begin
  {CreateNewP := True;
  If Not Assigned(Form_ProfilesBETA) Then
  Begin
    Application.CreateForm(TForm_ProfilesBETA, Form_ProfilesBETA);
  End;

  Form_setupMC.Show;


  { Load Profiles(\) | Retreve Indexes(X) | etc( ) }

  { BEGIN LOAD PROFILES }
  Form_ProfilesBeta.ListProfiles(UsrObj.GFFProfilePath, ListBox_profiles.Items);
  { END LOAD PROFILES }// UNFINISHED?

  { BEGIN RETREVE INDEX }
  IndexURL := startup.WebSettings.INI.ReadString('DownloadLocations',
    'Index', 'http://s3.amazonaws.com/Minecraft.Download/versions/versions.json');
  authsystem.GetFile(IndexURL, 'GFFLauncher/temp/index.json');
  { END RETREVE INDEX }
  jsonwork.listVersions('GFFLauncher/temp/index.json', versions);
  //Split List in Versions
  Try
    For i := (Versions.Count - 1) Downto 0 Do
    Begin
      If (Pos('a', Versions[i]) = 1) And (sorted <> True) Then
      Begin
        alphaVersions.Add(Versions[i]);
        Versions.Delete(i);
        Sorted := True;
      End;
      i1 := i;
      If (Pos('b', Versions[i]) = 1) And (sorted <> True) Then
      Begin
        betaVersions.Add(Versions[i]);
        Versions.Delete(i);
        sorted := True;
      End;
      i2 := i;
      If (Pos('w', Versions[i]) > 0) And (sorted <> True) Then
      Begin
        snapshots.Add(Versions[i]);
        Versions.Delete(i);
        sorted := True;
      End;
      i3 := i;
      If ((Pos('inf', Versions[i]) = 1) Or (Pos('c', Versions[i]) = 1) Or
        (Pos('r', Versions[i]) = 1)) And (sorted <> True) Then
      Begin
        preAlphaVersions.Add(Versions[i]);
        Versions.Delete(i);
        sorted := True;
      End;
      i4 := i;
      If (Pos('pre', Versions[i]) > 0) And (sorted <> True) Then
      Begin
        snapshots.Add(Versions[i]);
        Versions.Delete(i);
        sorted := True;
      End;
      i5 := i;
      sorted := False;
    End;
    preAlphaVersions.CustomSort(@unit_appUtils.SortDown);
    alphaVersions.CustomSort(@unit_appUtils.SortDown);
    betaVersions.CustomSort(@unit_appUtils.SortDown);
    Versions.CustomSort(@unit_appUtils.SortDown);
    snapshots.CustomSort(@unit_appUtils.SortDown);

    //preAlphaVersions.CustomSort(unit_appUtils.StringListSortDownwards());
    //alphaVersions.CustomSort(unit_appUtils.StringListSortDownwards());
    //betaVersions.CustomSort(unit_appUtils.StringListSortDownwards());
    //Versions.CustomSort(unit_appUtils.StringListSortDownwards());
    //snapshots.CustomSort(unit_appUtils.StringListSortDownwards());

  Except
    on E: Exception Do
    Begin
      Form_error.Handle(E, 'C:' + IntToStr(i) + '|M:' +
        IntToStr(Versions.Count - 1) + LineEnding + 'Logpoints:' +
        IntToStr(i1) + '|' + IntToStr(i2) + '|' + IntToStr(i3) + '|' +
        IntToStr(i4) + '|' + IntToStr(i5)
        , False);
    End;
  End;
  Form_setupMC.Hide;
  If Form_ProfilesBETA.Visible = False Then
  Begin
    Form_Profiles.BETAShow;
  End;
  TabSheet_crMC_Forge.TabVisible := True;
  PageControl_profiles.ActivePage := TabSheet_crMC_Forge;}
End;

Procedure TForm_ProfilesBETA.LoadProfiles(Directory: String);
Var
  DirList: TStringList;
  I: Integer;
  tObj: TProfileObj;
Begin
  DirList := TStringList.Create;
  gfflauncher.GetSubDirectories(Directory, DirList);
  if (ProfileList.Count>0) then begin
    ProfileList.Clear;
  end;

  For I := 0 To (DirList.Count - 1) Do
  Begin
    If (FileExists(DirList[I] + '/ProfileIndex.ini')) Then
    Begin
      ProfileList.Add(TProfileObj.Create(DirList[I] + '/ProfileIndex.ini'));
    End;
  End;
  FreeAndNil(DirList);
End;

function TForm_profilesBETA.CheckForProfileChanges(Directory:String):Boolean;
Var
  DirList: TStringList;
  I: Integer;
  tObj: TProfileObj;
  ProfileListTemp:TProfileCollection;
Begin
  DirList := TStringList.Create;
  gfflauncher.GetSubDirectories(Directory, DirList);
  ProfileListTemp:=TProfileCollection.Create;

  For I := 0 To (DirList.Count - 1) Do
  Begin
    If (FileExists(DirList[I] + '/ProfileIndex.ini')) Then
    Begin
      ProfileListTemp.Add(TProfileObj.Create(DirList[I] + '/ProfileIndex.ini'));
    End;
  End;

  if (ProfileListTemp.CollectionIsDifferent(ProfileList)=True) then begin
     LoadProfiles(UsrObj.GFFProfilePath);
     Result:=True;
  end else begin
      Result:=False;
  end;
  FreeAndNil(ProfileListTemp);
  FreeAndNil(DirList);
end;

End.


