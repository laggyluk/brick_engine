unit astar3d_world;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,astar3d_point3d;

type
  TArrayOfBoolean = array of Boolean;

  TWorld = class
  strict private
    sx: Integer;
    sy: Integer;
    sz: Integer;
    offsetIdx: Integer;
    worldBlocked: TArrayOfBoolean;
  public
    constructor Create(width: Integer; height: Integer); overload;
    constructor Create(width: Integer; height: Integer; depth: Integer); overload;
    function get_Left: Integer;
    function get_Right: Integer;
    function get_Bottom: Integer;
    function get_Top: Integer;
    function get_Front: Integer;
    function get_Back: Integer;
    property Left: Integer read get_Left;
    property Right: Integer read get_Right;
    property Bottom: Integer read get_Bottom;
    property Top: Integer read get_Top;
    property Front: Integer read get_Front;
    property Back: Integer read get_Back;
    procedure MarkPosition(position: TPoint3D; value: Boolean);
  strict private
    procedure MarkPositionEx(position: TPoint3D; value: Boolean);
  public
    function PositionIsFree(position: TPoint3D): Boolean;
  end;

implementation

{$AUTOBOX ON}
{$HINTS OFF}
{$WARNINGS OFF}

constructor TWorld.Create(width: Integer; height: Integer);
begin
  Create(width, height, 1);
end;

constructor TWorld.Create(width: Integer; height: Integer; depth: Integer);
var
  x: Integer;
  z: Integer;
  zz: Integer;
  y: Integer;
  yy: Integer;
  xx: Integer;
begin
  inherited Create;
  Self.sx := (width + 2);
  Self.sy := (height + 2);
  Self.sz := (depth + 2);
  Self.offsetIdx := (0 + 1) + ((0 + 1) + (0 + 1) * sy) * sx;
  setlength(worldBlocked,sx * sy * sz);    ;//new Boolean[sx * sy * sz];
  x := 0;
  while (x < Self.sx) do
  begin
    y := 0;
    while (y < Self.sy) do
    begin
      MarkPositionEx(TPoint3D.Create(x, y, 0), True);
      MarkPositionEx(TPoint3D.Create(x, y, (Self.sz - 1)), True);
      ///*PreInc*/;
      y+=1;
    end;
    //*PreInc*/;
    x+=1;
  end;
  y := 0;
  while (y < Self.sy) do
  begin
    z := 0;
    while (z < Self.sz) do
    begin
      MarkPositionEx(TPoint3D.Create(0, y, z), True);
      MarkPositionEx(TPoint3D.Create((Self.sx - 1), y, z), True);
      //*PreInc*/;
      z+=1;
    end;
    //*PreInc*/;
    y+=1;
  end;
  z := 0;
  while (z < Self.sz) do
  begin
    x := 0;
    while (x < Self.sx) do
    begin
      MarkPositionEx(TPoint3D.Create(x, 0, z), True);
      MarkPositionEx(TPoint3D.Create(x, (Self.sy - 1), z), True);
      //*PreInc*/;
      x+=1;
    end;
    //*PreInc*/;
    z+=1;
  end;
end;

function TWorld.get_Left: Integer;
begin
  Result := 0;
end;

function TWorld.get_Right: Integer;
begin
  Result := (Self.sx - 2);
end;

function TWorld.get_Bottom: Integer;
begin
  Result := 0;
end;

function TWorld.get_Top: Integer;
begin
  Result := (Self.sy - 2);
end;

function TWorld.get_Front: Integer;
begin
  Result := 0;
end;

function TWorld.get_Back: Integer;
begin
  Result := (Self.sz - 2);
end;

procedure TWorld.MarkPosition(position: TPoint3D; value: Boolean);
begin
  //Self.worldBlocked[((Self.offsetIdx + position.X) + )] := value;
  worldBlocked[offsetIdx + position.X + (position.Y + position.Z * sy) * sx] := value;
end;

procedure TWorld.MarkPositionEx(position: TPoint3D; value: Boolean);
begin
  //Self.worldBlocked[(position.X + )] := value;
  worldBlocked[position.X + (position.Y + position.Z * sy) * sx] := value;
end;

function TWorld.PositionIsFree(position: TPoint3D): Boolean;
begin
  result:= not worldBlocked[offsetIdx + position.X + (position.Y + position.Z * sy) * sx];
//  Result := (Self.worldBlocked[((Self.offsetIdx + position.X) + )] = False);
end;

end.


