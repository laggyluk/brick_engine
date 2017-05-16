unit core_octree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VectorGeometry;

type

  pOctNode = ^TOctNode;

  { TOctNode }

  TOctNode = class
    private
      kids:array [0..7] of TOctNode;
      size2:integer;
    public
      //center point
      pos:array[0..2] of Longint;
      //box size
      box:Cardinal;
      parent:tOctNode;
      procedure load(const filename:string);
      function getValue(x,y,z:Integer):byte;
      procedure setValue(x,y,z:Integer; value:byte);
      constructor create(size:cardinal);
    end;

   //look nice
   TOctTree = TOctNode;

implementation

{ TOctNode }

procedure TOctNode.load(const filename: string);
begin

end;

function TOctNode.getValue(x, y, z: Integer): byte;
begin

end;

procedure TOctNode.setValue(x, y, z: Integer; value: byte);
var
  kid:byte;
begin
  //which of the 8 children is target?
  //wtf
  if x>pos[0] then kid:=kid+1;
  if y>pos[1] then kid:=kid+4;
  if z>pos[2] then kid:=kid+2;
  if kids[kid]=nil then begin
    kids[kid]:=tOctNode.create(size2);
//    kids[kid].pos[0]:=;
  end;
//  kids[kid].setValue();
end;

constructor TOctNode.create(size: cardinal);
begin
  box:=size;
  size2:=size div 2;
  pos[0]:=size2;
  pos[1]:=size2;
  pos[2]:=size2;
end;

end.

