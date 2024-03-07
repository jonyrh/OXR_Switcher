unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComboEx, StdCtrls, LazUTF8, lclintf, Registry, Windows,
  fpJSON, JSONParser, JSONScanner, StrUtils,
  uDarkStyleParams, uWin32WidgetSetDark, uDarkStyleSchemes, uMetaDarkStyle;

type

  TOXR_Item = record
    oi_active: Boolean;
    oi_json,
    oi_name: String;
    oi_icon: Integer;
  end;

  TAvailableRuntimes = array of TOXR_Item;

  { TForm1 }

  TForm1 = class(TForm)
    ComboBoxEx1: TComboBoxEx;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure ComboBoxEx1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private

  public

  end;

const
  icon_oculus = 0;
  icon_steam  = 1;
  icon_vd     = 2;
  icon_wmr    = 3;
  icon_vive   = 4;
  icon_varjo  = 5;
  icon_meta   = 6;

var
  Form1: TForm1;

  OXR_List: TAvailableRuntimes;

implementation

{$R *.lfm}

procedure SetDarkStyle;
begin
 try
  if not IsDarkModeEnabled then
   begin
   uDarkStyleParams.PreferredAppMode:=pamAllowDark;
   uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
   end;
  except
  end;
end;

procedure AddParseRuntimeFromJSON(const aRuntimeJsonFile, aActiveRuntime: String);
var
  aSL: TStringList;
  jsonData: TJSONData;
  aName,
  aDll: String;
begin
 if (aRuntimeJsonFile.IsEmpty) or
    (not FileExists(aRuntimeJsonFile)) then Exit;

 aSL:= TStringList.Create;

 try
  aSL.LoadFromFile(aRuntimeJsonFile);

  jsonData:= GetJSON(aSL.Text, True);
  aName:= Trim(jsonData.FindPath('runtime.name').AsString);
  aDll:=  Trim(jsonData.FindPath('runtime.library_path').AsString);

  if aName.IsEmpty then aName:= aRuntimeJsonFile;

  aDll:= UTF8StringReplace(aDll, '/', DirectorySeparator, [rfReplaceAll]).Trim;

  if UTF8Copy(aDll, 1, 2)=('.'+DirectorySeparator)
   then aDll:= UTF8Copy(aDll, 3, UTF8Length(aDll)).Trim;

  if not FileExists(aDll) then aDll:= ExtractFilePath(aRuntimeJsonFile) + aDll;

  if FileExists(aDll) then
   begin
   SetLength(OXR_List, Length(OXR_List)+1);

   OXR_List[Length(OXR_List)-1].oi_name:= aName;
   OXR_List[Length(OXR_List)-1].oi_json:= aRuntimeJsonFile;

   if aActiveRuntime= UTF8LowerCase(aRuntimeJsonFile)
    then OXR_List[Length(OXR_List)-1].oi_active:= True
      else OXR_List[Length(OXR_List)-1].oi_active:= False;

   aName:= UTF8LowerCase(aName);

        if ContainsText( aName, 'oculus' )  then OXR_List[Length(OXR_List)-1].oi_icon:= icon_oculus
   else if ContainsText( aName, 'meta'   )  then OXR_List[Length(OXR_List)-1].oi_icon:= icon_meta
   else if ContainsText( aName, 'steam'  )  then OXR_List[Length(OXR_List)-1].oi_icon:= icon_steam
   else if (ContainsText(aName, 'virtual')) and
           (ContainsText(aName, 'desktop')) then OXR_List[Length(OXR_List)-1].oi_icon:= icon_vd
   else if (ContainsText(aName, 'mixed'  )) and
           (ContainsText(aName, 'reality')) then OXR_List[Length(OXR_List)-1].oi_icon:= icon_wmr
   else if ContainsText( aName, 'vive'   )  then OXR_List[Length(OXR_List)-1].oi_icon:= icon_vive
   else if ContainsText( aName, 'varjo'  )  then OXR_List[Length(OXR_List)-1].oi_icon:= icon_varjo
   else OXR_List[Length(OXR_List)-1].oi_icon:= -1;
   end;

 finally
  if jsonData<>nil then jsonData.Free;
  aSL.Free;
 end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
 Registry: TRegistry;
 i: Integer;
 aSL: TStringList;
 aActiveRuntime: String;
begin
 ComboBoxEx1.ItemsEx.Clear;

 OXR_List:= nil;
 SetLength(OXR_List, 0);

 aActiveRuntime:= 'none';

 aSL:= TStringList.Create;
 aSL.Clear;

 Registry:= TRegistry.Create;

  try
   Registry.RootKey:= HKEY_LOCAL_MACHINE;

   if Registry.OpenKeyReadOnly('Software\Khronos\OpenXR\1') then aActiveRuntime:= UTF8LowerCase(Registry.ReadString('ActiveRuntime')).Trim;
   Registry.CloseKey;

   if Registry.OpenKeyReadOnly('Software\Khronos\OpenXR\1\AvailableRuntimes') then Registry.GetValueNames(aSL);
   Registry.CloseKey;

   for i:=0 to aSL.Count-1 do
    AddParseRuntimeFromJSON(aSL[i].Trim, aActiveRuntime);

   finally
    Registry.Free;
    aSL.Free;
   end;

  // test icons
 { SetLength(OXR_List, 7);
   OXR_List[0].oi_name:='Oculus OpenXR';
   OXR_List[0].oi_icon:= icon_oculus;
   OXR_List[1].oi_name:='Meta OpenXR';
   OXR_List[1].oi_icon:= icon_meta;
   OXR_List[2].oi_name:='SteamVR';
   OXR_List[2].oi_icon:= icon_steam;
   OXR_List[3].oi_name:='VirtualDesktopXR (Bundled)';
   OXR_List[3].oi_icon:= icon_vd;
   OXR_List[4].oi_name:='Windows Mixed Reality';
   OXR_List[4].oi_icon:= icon_wmr;
   OXR_List[5].oi_name:='Vive Runtime';
   OXR_List[5].oi_icon:= icon_vive;
   OXR_List[6].oi_name:='Varjo OpenXR';
   OXR_List[6].oi_icon:= icon_varjo; }

  for i:=0 to Length(OXR_List)-1 do
   begin
   ComboBoxEx1.ItemsEx.AddItem(OXR_List[i].oi_name, OXR_List[i].oi_icon);
   if OXR_List[i].oi_active then ComboBoxEx1.ItemIndex:=i;
   end;
end;

procedure TForm1.ComboBoxEx1Change(Sender: TObject);
var
 Registry: TRegistry;
begin
 Registry:= TRegistry.Create;
  try
   Registry.RootKey:= HKEY_LOCAL_MACHINE;
   if Registry.OpenKey('Software\Khronos\OpenXR\1', False) then
    Registry.WriteString('ActiveRuntime', OXR_List[ComboBoxEx1.ItemIndex].oi_json);
   Registry.CloseKey;
  finally
   Registry.Free;
  end;
end;

procedure TForm1.Label2Click(Sender: TObject);
begin
  OpenURL('http://www.jonyrh.ru');
end;

initialization

SetDarkStyle;

end.

