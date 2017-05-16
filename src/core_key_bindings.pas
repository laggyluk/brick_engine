unit core_key_bindings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fgl,TypInfo,core_types;
type
  //each action that can be executed by user pressing keys
  //on different systems key codes may be different so each action is paired with a number
  //(key code) that can be reasigned. also allows easy customization by user
  sActions = (
    //camera movement
    sA_cam_strafe_left,
    sA_cam_strafe_right,
    sA_cam_forwards,
    sA_cam_backwards,
    sA_cam_rotate_left,
    sA_cam_rotate_right, //orbit cam position around target
    sA_cam_move_up, //move camera position up
    sA_cam_move_down,  //move camera down
    sA_cam_rotate_up,
    sA_cam_rotate_down,
    sA_mouse_Btn_1,
    sA_mouse_Btn_2,
    sA_undefined//must be last
  );

  TKeyBindingsMap = specialize TFPGMap<integer,sActions>;

  { TKeyBindings }
  { TODO : ta mapa klawiszy chyba jest przerostem formy nad trescia. ale dziala wiec na razie jebac }
  TKeyBindings = class
    map:TKeyBindingsMap;
    const filename:string ='keys.ini';
    constructor create;
    destructor destroy;
    procedure resetToDefault;
    procedure loadFromFile(fiile:string);
    procedure saveToFile(fiile:string);
    procedure translateKey(key:integer;var action:sActions);
  end;

var
  KeyBindings:TKeyBindings;

implementation

{ TKeyBindings }

constructor TKeyBindings.create;
begin
  map:= TKeyBindingsMap.create;
  if FileExists(appPath+filename) then loadFromFile(appPath+filename)
  else resetToDefault;
end;

destructor TKeyBindings.destroy;
begin
  inherited;
  map.destroy;
end;

procedure TKeyBindings.resetToDefault;
var
  f:tstringlist;
  s: string;
  i:integer;
begin
  f:=tstringlist.create;
  f.add('sA_cam_strafe_left=65');
  f.add('sA_cam_strafe_right=68');
  f.add('sA_cam_forwards=87');
  f.add('sA_cam_backwards=83');
  f.add('sA_cam_rotate_left=81');
  f.add('sA_cam_rotate_right=69');
  f.add('sA_cam_move_up=86');
  f.add('sA_cam_move_down=67');
  f.add('sA_cam_rotate_up=82');
  f.add('sA_cam_rotate_down=70');
  f.add('sA_mouse_Btn_1=0');
  f.add('sA_mouse_Btn_2=1');
  f.savetofile(apppath+filename);
  f.free;
  //load back from file
  loadfromfile(apppath+filename)
end;

procedure TKeyBindings.loadFromFile(fiile: string);
var
    f:tstringlist;
    s: string;
    i,j,k:integer;
begin
  //load key binds from file
  f:=tstringlist.create;
  f.loadfromfile(filename);
  for i:=0 to integer(sA_undefined)-1 do begin
    s := GetEnumName(TypeInfo(sActions),integer(i));
    //search file
    for j:=0 to f.count-1 do if (pos(s,f[j])>0)then begin
      k:=pos('=',f[j]);
      s:=copy(f[j],k+1,length(f[j])-k);
      map[strtoint(s)]:=sActions(i);
      break;
    end;
  end;
  f.free;
end;

procedure TKeyBindings.saveToFile(fiile: string);
var
    f:tstringlist;
    s: string;
    i:integer;
begin
    f:=tstringlist.create;
    for i:=0 to integer(sA_undefined)-1 do begin
      s := GetEnumName(TypeInfo(sActions),integer(i));
      f.add(s+'='+inttostr(map.keys[map.IndexOfData(sActions(i))]));
    end;
    f.savetofile(fiile);
    f.free;
end;

procedure TKeyBindings.translateKey(key: integer; var action: sActions);
var
    i:integer;
begin
  i:=map.indexof(key);
  if i>-1 then action:=map[key] else
    action:=sActions.sA_undefined;
end;

end.

