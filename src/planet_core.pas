unit planet_core;

{$mode objfpc}{$H+}

interface

uses
  Classes, forms,SysUtils,dglOpenGL,core_render,core_material_library,core_types,
  renderable_object_types;
const
  chunk_size = 16;  //
  active_chunks = 9;  //chunks visible on screen, one in center and rest surrounding

  //block defs
  btNone = 0;
  btStone = 1;
  btDirt = 2;

type

  rChunk = packed record
   data: array [0..chunk_size-1] of array [0..chunk_size-1]
         of array [0..chunk_size-1] of byte;
  end;
  pChunk = ^rChunk;
  { TPlanetCore }

  TPlanetCore = class
    private
      chunks: array [0..active_chunks-1] of rChunk; //visible chunks,  2d
      renderer:TRenderer;
    public
      constructor create;
      procedure render;
      procedure processKeyBoard( var Key: Word; Shift: TShiftState);
     // function getActiveChunk(id:integer):pChunk;
  end;

implementation

{ TPlanetCore }

constructor TPlanetCore.create;
var
  x,y,z,j:integer;
  teren:TTerrainchunk;
  teren2:TWithoutGeom;
begin
  apppath:=extractfilepath(application.ExeName);
  matLib:=TMaterialLib.create;
  renderer:=trenderer.create;
  teren:=TTerrainChunk.create;
  teren2:=TWithoutGeom.create;
  teren2.transpose(0.2,0.2,0.2);
  renderer.add(teren);
  renderer.add(teren2);

  for j:=0 to high(chunks) do
   for z:=0 to high(chunks[j].data) do
    for y:=0 to high(chunks[j].data) do
      for x:=0 to high(chunks[j].data) do
        if y=4 then chunks[j].data[x,y,z]:=btStone
        else chunks[j].data[x,y,z]:=btNone;


end;

procedure TPlanetCore.render;
begin
  renderer.render;
end;

procedure TPlanetCore.processKeyBoard(var Key: Word; Shift: TShiftState);
begin
  log(char(key));
  //if key = vk_left then camPosition[0]:=camPosition[0]-0.1;
  //if key = vk_right then camPosition[0]:=camPosition[0]+0.1;
  //if key = vk_up then camPosition[2]:=camPosition[2]-0.1;
  //if key = vk_down then camPosition[2]:=camPosition[2]+0.1;
end;

end.

