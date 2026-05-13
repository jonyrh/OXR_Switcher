program OXR_Switcher;

{$mode objfpc}{$H+}

uses
  Windows, SysUtils, Interfaces, Forms, Unit1, LazUtf8;

{$R *.res}

procedure HookParentConsole;
begin
 if AttachConsole(ATTACH_PARENT_PROCESS) then
  begin
   AssignFile(Output, 'CONOUT$');
   Rewrite(Output);

   AssignFile(ErrOutput, 'CONOUT$');
   Rewrite(ErrOutput);

   InOutRes := 0;
 end;
end;

var
  i, n: Integer;
  aDebug: Boolean;

begin
 RequireDerivedFormResource:=True;

 aDemo:=  False;
 aDebug:= False;
 n:= -1;

 if ParamCount>0 then
  begin
   for i:=1 to ParamCount do
    begin
     if UTF8LowerCase(ParamStr(i))='demo'  then aDemo:=  True;
     if UTF8LowerCase(ParamStr(i))='debug' then aDebug:= True;
     n:= StrToIntDef( ParamStr(i).Trim, -1);
    end;
  end;

 OXR_List:= nil;
 SetLength(OXR_List, 0);

 if not aDemo then GetRuntimes
              else GetDemoRuntimes;

 if aDebug then SaveDebugToIniFile;

 if (n<>-1) and
    (not aDemo) then
  begin
   HookParentConsole;

   if (n<>-1) and
      (n>=0) and
      (n<=Length(OXR_List)-1) then
    begin
     SetActiveOpenXR(n);
     Halt(0);
    end;

   Halt(0);
  end
   else
    begin
  Application.Scaled:=True;
     Application.Initialize;
     Application.CreateForm(TForm1, Form1);
     Application.Run;
    end;
end.

