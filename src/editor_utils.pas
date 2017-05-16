unit editor_utils;

{$mode objfpc}{$H+}

interface

uses
  controls,windows,messages,shellapi,forms,clipbrd,SysUtils,core_types,
     dialogs,ComCtrls,ExtCtrls,stdctrls,grids,Graphics,classes,inifiles;

  procedure loadControl(obj:tcontrol);
  procedure saveControl(obj:tcontrol);

implementation

const inifilename = 'editor.ini';

procedure loadControl(obj:tcontrol);
var
  ini:tinifile;
  edit:tedit;
  up:tupdown;
  s:string;
  i:integer;
  ch:tcheckbox;
begin
  ini:=tinifile.Create(appPath+inifilename);
  if obj is tedit then  begin
    edit:= obj as tedit;
    s:=ini.ReadString('options',edit.name,'');
    if s<>'' then edit.Text:=s;
  end;
  if obj is tupdown then  begin
    up:= obj as tupdown;
    i:=ini.ReadInteger('options',up.name,0);
    up.position:=i;
  end;
  if obj is TCheckBox then  begin
    ch:= obj as TCheckbox;
    ch.checked:=ini.Readbool('options',ch.name,false);
  end;
  if obj is TToggleBox then with (obj as ttogglebox) do begin
     checked:=ini.Readbool('options',name,false);
  end;
  ini.Free;
end;

procedure saveControl(obj:tcontrol);
var
  ini:tinifile;
  edit:tedit;
  up:tupdown;
  le:tlabelededit;
  ch:tcheckbox;
begin
  ini:=tinifile.Create(appPath+inifilename);
  if obj is tedit then  begin
    edit:= obj as tedit;
    ini.WriteString('options',edit.name,edit.text);
  end;
  if obj is tlabelededit then  begin
    le:= obj as tlabelededit;
    ini.WriteString('options',le.name,le.Text);
  end;
  if obj is tupdown then  begin
    up:= obj as tupdown;
    ini.WriteInteger('options',up.name,up.position);
  end;
  if obj is TCheckBox then  begin
    ch:= obj as TCheckbox;
    ini.writebool('options',ch.name,ch.checked);
  end;
  if obj is TToggleBox then with (obj as ttogglebox) do begin
     ini.writebool('options',name,checked);
  end;
  ini.Free;
end;

end.

