unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComboEx, StdCtrls, LazUTF8, lclintf, Registry, Windows,
  fpJSON, JSONParser, JSONScanner,
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
  version     = '2023.12.10.09.30';

  icon_oculus = 0;
  icon_steam  = 1;
  icon_vd     = 2;
  icon_wmr    = 3;
  icon_vive   = 4;
  icon_varjo  = 5;

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

function GetRuntimeNameFromJSON(const aFile: String): String;
var
  aSL: TStringList;
  jsonData: TJSONData;
begin
 Result:= aFile;

 if not FileExists(aFile) then Exit;

 aSL:= TStringList.Create;

 try
  aSL.LoadFromFile(aFile);
  jsonData:= GetJSON(aSL.Text, True);
  Result:= jsonData.FindPath('runtime.name').AsString;
 finally
  jsonData.Free;
  aSL.Free;
 end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
 Registry: TRegistry;
 i: Integer;
 aSL: TStringList;
 aActive: String;
begin
 Label3.Caption:= version;

 ComboBoxEx1.ItemsEx.Clear;

 OXR_List:= nil;
 SetLength(OXR_List, 0);

 aActive:= 'none';

 aSL:= TStringList.Create;
 aSL.Clear;

 Registry:= TRegistry.Create;

  try
   Registry.RootKey:= HKEY_LOCAL_MACHINE;

   if Registry.OpenKeyReadOnly('Software\Khronos\OpenXR\1') then aActive:= Registry.ReadString('ActiveRuntime');

   Registry.CloseKey;

   if Registry.OpenKeyReadOnly('Software\Khronos\OpenXR\1\AvailableRuntimes') then Registry.GetValueNames(aSL);

   Registry.CloseKey;

   for i:=0 to aSL.Count-1 do
    begin
    SetLength(OXR_List, Length(OXR_List)+1);

    OXR_List[Length(OXR_List)-1].oi_active:= False;
    if UTF8LowerCase(aSL[i])= UTF8LowerCase(aActive) then OXR_List[Length(OXR_List)-1].oi_active:= True;

    OXR_List[Length(OXR_List)-1].oi_json:= aSL[i];
    OXR_List[Length(OXR_List)-1].oi_name:= GetRuntimeNameFromJSON(OXR_List[Length(OXR_List)-1].oi_json);

    OXR_List[Length(OXR_List)-1].oi_icon:= -1;
    if UTF8Pos( 'steam',    UTF8LowerCase(OXR_List[Length(OXR_List)-1].oi_name))<>0 then OXR_List[Length(OXR_List)-1].oi_icon:=  icon_steam;
    if UTF8Pos( 'oculus',   UTF8LowerCase(OXR_List[Length(OXR_List)-1].oi_name))<>0 then OXR_List[Length(OXR_List)-1].oi_icon:=  icon_oculus;
    if (UTF8Pos('virtual',  UTF8LowerCase(OXR_List[Length(OXR_List)-1].oi_name))<>0) and
       (UTF8Pos('desktop',  UTF8LowerCase(OXR_List[Length(OXR_List)-1].oi_name))<>0) then OXR_List[Length(OXR_List)-1].oi_icon:= icon_vd;
    if (UTF8Pos('mixed',    UTF8LowerCase(OXR_List[Length(OXR_List)-1].oi_name))<>0) and
       (UTF8Pos('reality',  UTF8LowerCase(OXR_List[Length(OXR_List)-1].oi_name))<>0) then OXR_List[Length(OXR_List)-1].oi_icon:= icon_wmr;
    if UTF8Pos( 'vive',     UTF8LowerCase(OXR_List[Length(OXR_List)-1].oi_name))<>0 then OXR_List[Length(OXR_List)-1].oi_icon:=  icon_vive;
    if UTF8Pos( 'varjo',    UTF8LowerCase(OXR_List[Length(OXR_List)-1].oi_name))<>0 then OXR_List[Length(OXR_List)-1].oi_icon:=  icon_varjo;
    end;

   finally
    Registry.Free;
    aSL.Free;
   end;

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

   if Registry.OpenKey('Software\Khronos\OpenXR\1', False) then Registry.WriteString('ActiveRuntime', OXR_List[ComboBoxEx1.ItemIndex].oi_json);

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

