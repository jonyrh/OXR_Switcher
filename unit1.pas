unit Unit1;

{$mode objfpc}{$H+}

{$DEFINE ALLOW_DARK}
//{$DEFINE TEST_ICONS}
//{$DEFINE TEST_LIST}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComboEx, StdCtrls, LazUTF8, lclintf, Registry, Windows,
  fpJSON, JSONParser, JSONScanner, StrUtils
  {$IFDEF TEST_LIST}
  ,IniFiles
  {$ENDIF}
  {$IFDEF ALLOW_DARK}
  ,uDarkStyleParams, uWin32WidgetSetDark, uDarkStyleSchemes, uMetaDarkStyle
  {$ENDIF}
  ;

type

  TOXR_Item = record
    oi_active: Boolean;
    oi_json32,
    oi_json64,
    oi_name: String;
    oi_icon: Integer;
  end;

  TAvailableRuntimes = array of TOXR_Item;

  { TForm1 }

  TForm1 = class(TForm)
    ComboBoxEx1: TComboBoxEx;
    ImageList1: TImageList;
    Label1: TLabel;
    procedure ComboBoxEx1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SetActiveOpenXR(const idXR: Integer);
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
  private

  public

  end;

const
  app_version = 'OXR_Switcher v.2025.11.20.13';
  app_copy    = '(c) Jony Rh, 2025';
  app_url     = 'http://www.jonyrh.ru';

  icon_oculus = 0;
  icon_steam  = 1;
  icon_vd     = 2;
  icon_wmr    = 3;
  icon_vive   = 4;
  icon_varjo  = 5;
  icon_meta   = 6;
  icon_pimax  = 7;
  icon_monado = 8;
  icon_none   = 9;

  SC_MyMenuItem = WM_USER + 1;

var
  Form1: TForm1;

  mode: DWORD = 0;
  hOut: HANDLE;

  OXR_List: TAvailableRuntimes;

implementation

{$R *.lfm}

procedure TForm1.WMSysCommand(var Msg: TWMSysCommand);
begin
  if Msg.CmdType = SC_MyMenuItem then OpenURL(app_url)
  else
    inherited;
end;

{$IFDEF ALLOW_DARK}
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
{$ENDIF}

procedure SortArrayRuntimesAndSetBits;
var
  tmp_List: TAvailableRuntimes;
  aSL: TStringList;
  i, j: Integer;
begin
  if Length(OXR_List)=0 then Exit;

  tmp_List:= nil;
  SetLength(tmp_List, 0);

  aSL:= TStringList.Create;
  aSL.Sorted:= True;
  aSL.Duplicates:= dupIgnore;

  for i:=0 to Length(OXR_List)-1 do
   aSL.Add(OXR_List[i].oi_name);

  for i:=0 to aSL.Count-1 do
   begin
    for j:=0 to Length(OXR_List)-1 do
     begin
      if aSL[i]=OXR_List[j].oi_name then
       begin
        SetLength(tmp_List, Length(tmp_List)+1);
        tmp_List[Length(tmp_List)-1]:= OXR_List[j];
       end;
     end;
   end;

  aSL.Free;

  {$IFNDEF TEST_ICONS}
  for i:=0 to Length(tmp_List)-1 do
   begin
         if (tmp_List[i].oi_json32.IsEmpty) and (not tmp_List[i].oi_json64.IsEmpty) then tmp_List[i].oi_name:= tmp_List[i].oi_name +     ' (x64)'
    else if (not tmp_List[i].oi_json32.IsEmpty) and (tmp_List[i].oi_json64.IsEmpty) then tmp_List[i].oi_name:= tmp_List[i].oi_name +     ' (x32)'
    else if (not tmp_List[i].oi_json32.IsEmpty) and (not tmp_List[i].oi_json64.IsEmpty) then tmp_List[i].oi_name:= tmp_List[i].oi_name + ' (x32/x64)';
   end;
  {$ENDIF}

  SetLength(OXR_List, 0);
  OXR_List:= Copy(tmp_List);
  SetLength(tmp_List, 0);
end;

procedure AddParseRuntimeFromJSON(const aRuntimeJsonFile, aActiveRuntime: String; a64bit: Boolean);
var
  aSL: TStringList;
  jsonData: TJSONData;
  aName,
  aDll: String;
  i: Integer;
  aFind: Boolean;
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
    aFind:= False;

    for i:=0 to Length(OXR_List)-1 do
     begin
      if OXR_List[i].oi_name=aName then
       begin
        aFind:= True;
        Break;
       end;
     end;

    if not aFind then
     begin
      SetLength(OXR_List, Length(OXR_List)+1);

      OXR_List[Length(OXR_List)-1].oi_name:=   aName;
      OXR_List[Length(OXR_List)-1].oi_active:= False;

      if a64bit then OXR_List[Length(OXR_List)-1].oi_json64:= aRuntimeJsonFile
                else OXR_List[Length(OXR_List)-1].oi_json32:= aRuntimeJsonFile;

      if aActiveRuntime= aRuntimeJsonFile then OXR_List[Length(OXR_List)-1].oi_active:= True;

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
      else if ContainsText( aName, 'pimax'  )  then OXR_List[Length(OXR_List)-1].oi_icon:= icon_pimax
      else if ContainsText( aName, 'monado' )  then OXR_List[Length(OXR_List)-1].oi_icon:= icon_monado
      else OXR_List[Length(OXR_List)-1].oi_icon:= icon_none;
     end
      else
       begin
        if a64bit then OXR_List[i].oi_json64:= aRuntimeJsonFile
                  else OXR_List[i].oi_json32:= aRuntimeJsonFile;
       end;
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
 {$IFDEF TEST_LIST}
 ini: TIniFile;
 {$ENDIF}
begin
 AppendMenu(GetSystemMenu(Handle, FALSE), MF_SEPARATOR, 0, '');
 AppendMenu(GetSystemMenu(Handle, FALSE), MF_STRING, SC_MyMenuItem, app_version);
 AppendMenu(GetSystemMenu(Handle, FALSE), MF_STRING, SC_MyMenuItem, app_copy);
 AppendMenu(GetSystemMenu(Handle, FALSE), MF_STRING, SC_MyMenuItem, app_url);

 ComboBoxEx1.ItemsEx.Clear;

 OXR_List:= nil;
 SetLength(OXR_List, 0);

 aActiveRuntime:= 'none';

 aSL:= TStringList.Create;
 aSL.Clear;

 // x64

 Registry:= TRegistry.Create;
  try
   Registry.RootKey:= HKEY_LOCAL_MACHINE;

   if Registry.OpenKeyReadOnly('Software\Khronos\OpenXR\1') then aActiveRuntime:= Registry.ReadString('ActiveRuntime').Trim;
   Registry.CloseKey;

   if Registry.OpenKeyReadOnly('Software\Khronos\OpenXR\1\AvailableRuntimes') then Registry.GetValueNames(aSL);
   Registry.CloseKey;

   for i:=0 to aSL.Count-1 do
    AddParseRuntimeFromJSON(aSL[i].Trim, aActiveRuntime, True);

   finally
    Registry.Free;
   end;

 aSL.Clear;

 // x32

 Registry:= TRegistry.Create;
  try
   Registry.RootKey:= HKEY_LOCAL_MACHINE;

   if Registry.OpenKeyReadOnly('Software\WOW6432Node\Khronos\OpenXR\1') then aActiveRuntime:= Registry.ReadString('ActiveRuntime').Trim;
   Registry.CloseKey;

   if Registry.OpenKeyReadOnly('Software\WOW6432Node\Khronos\OpenXR\1\AvailableRuntimes') then Registry.GetValueNames(aSL);
   Registry.CloseKey;

   for i:=0 to aSL.Count-1 do
    AddParseRuntimeFromJSON(aSL[i].Trim, aActiveRuntime, False);

   finally
    Registry.Free;
   end;

 aSL.Free;

 // test icons
 {$IFDEF TEST_ICONS}
 SetLength(OXR_List, 10);
 OXR_List[0].oi_name:='Oculus OpenXR (x32/x64)';
 OXR_List[0].oi_icon:= icon_oculus;
 OXR_List[1].oi_name:='SteamVR (x64)';
 OXR_List[1].oi_icon:= icon_steam;
 OXR_List[2].oi_name:='VirtualDesktopXR (Bundled) (x32/x64)';
 OXR_List[2].oi_icon:= icon_vd;
 OXR_List[3].oi_name:='Windows Mixed Reality (x32/x64)';
 OXR_List[3].oi_icon:= icon_wmr;
 OXR_List[4].oi_name:='Vive Runtime (x32/x64)';
 OXR_List[4].oi_icon:= icon_vive;
 OXR_List[5].oi_name:='Varjo OpenXR (x32/x64)';
 OXR_List[5].oi_icon:= icon_varjo;
 OXR_List[6].oi_name:='Meta OpenXR (x32/x64)';
 OXR_List[6].oi_icon:= icon_meta;
 OXR_List[7].oi_name:='Pimax XR (x64)';
 OXR_List[7].oi_icon:= icon_pimax;
 OXR_List[8].oi_name:='Monado XR Runtime (x64)';
 OXR_List[8].oi_icon:= icon_monado;
 OXR_List[9].oi_name:='Unknown OpenXR (x32)';
 OXR_List[9].oi_icon:= icon_none;
 {$ENDIF}

 SortArrayRuntimesAndSetBits;
 ComboBoxEx1.DropDownCount:= Length(OXR_List);

 {$IFDEF TEST_LIST}
 ini:= TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
 for i:=0 to Length(OXR_List)-1 do
  begin
   ini.WriteString( i.ToString, 'name',   OXR_List[i].oi_name);
   ini.WriteInteger(i.ToString, 'active', OXR_List[i].oi_active.ToInteger);
   ini.WriteInteger(i.ToString, 'icon',   OXR_List[i].oi_icon);
   ini.WriteString( i.ToString, 'json32', OXR_List[i].oi_json32);
   ini.WriteString( i.ToString, 'json64', OXR_List[i].oi_json64);
  end;
 ini.Free;
 {$ENDIF}

  for i:=0 to Length(OXR_List)-1 do
   begin
   ComboBoxEx1.ItemsEx.AddItem(OXR_List[i].oi_name, OXR_List[i].oi_icon);
   if OXR_List[i].oi_active then ComboBoxEx1.ItemIndex:=i;
   end;

  // command line params

  if ParamCount=0 then Exit;

  AllocConsole;
  IsConsole := True;
  SysInitStdIO;

  i:= StrToIntDef(ParamStr(1).Trim, -1);

  if (i>=0) and
     (i<=Length(OXR_List)-1) then
   begin
    SetActiveOpenXR(i);
   end
    else
     begin
      WriteLn(app_version);
      WriteLn(app_copy);
      WriteLn(app_url);
      WriteLn;
      WriteLn('Show available OpenXR list: use any one parameter except numbers!');
      WriteLn(ParamStr(0) + ' list');
      WriteLn(ParamStr(0) + ' help');
      WriteLn;

      if Length(OXR_List)<>0 then
       begin
        WriteLn('Available OpenXR indexes:');

        for i:=0 to Length(OXR_List)-1 do
         begin
          if OXR_List[i].oi_active then  WriteLn(i.ToString + ' : ' + OXR_List[i].oi_name + ' [ACTIVE]')
                                   else  WriteLn(i.ToString + ' : ' + OXR_List[i].oi_name);
         end;
       end
        else WriteLn('Available OpenXR not found!');

      WriteLn;
      WriteLn('Usage: To set active OpenXR use available index as param');
      WriteLn;
      WriteLn('Example:');
      WriteLn(ParamStr(0) + ' 1');
      WriteLn;
      WriteLn('Press Return to Exit');
      ReadLn;
     end;

  Halt(0);
end;

procedure TForm1.SetActiveOpenXR(const idXR: Integer);
{$IFNDEF TEST_ICONS}
var
 Registry: TRegistry;
begin

 // x64
 Registry:= TRegistry.Create;
  try
   Registry.RootKey:= HKEY_LOCAL_MACHINE;
   if Registry.OpenKey('Software\Khronos\OpenXR\1', False) then
    Registry.WriteExpandString('ActiveRuntime', OXR_List[idXR].oi_json64);
   Registry.CloseKey;
  finally
   Registry.Free;
  end;

 // x32
 Registry:= TRegistry.Create; //(KEY_WRITE or KEY_WOW64_32KEY);
  try
   Registry.RootKey:= HKEY_LOCAL_MACHINE;
   if Registry.OpenKey('Software\WOW6432Node\Khronos\OpenXR\1', False) then
    Registry.WriteExpandString('ActiveRuntime', OXR_List[idXR].oi_json32);
   Registry.CloseKey;
  finally
   Registry.Free;
  end;
{$ELSE}
begin
{$ENDIF}
end;

procedure TForm1.ComboBoxEx1Change(Sender: TObject);
begin
 SetActiveOpenXR(ComboBoxEx1.ItemIndex);
end;


{$IFDEF ALLOW_DARK}
initialization

SetDarkStyle;
{$ENDIF}

end.

