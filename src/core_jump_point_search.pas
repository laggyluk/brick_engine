unit core_jump_point_search;
{implementation based on Online Graph Pruning for Pathﬁnding on Grid Maps by
Daniel Harabor and Alban Grastien
NICTA and The Australian National University}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, graphics,VectorGeometry,core_types, fgl, core_listofrecords,
  core_chunk;

type
  //possible neighbours of a block (in 2d..)
  eNeighbours = (NN,NE,EE,SE,SS,SW,WW,WN);
  sNeigbours = set of eNeighbours;

  TJPSQueue = specialize TFPGList<coord_t>;
  function jumpPointSearch(A,B:TAffineVector;const filename:string):boolean;

implementation
var img:TBitmap;

function checkNeighbours(x,y:integer):sNeigbours;
begin

end;

function jumpPointSearch(A, B: TAffineVector; const filename: string): boolean;
var
  queue:TJPSQueue;
begin
  //use image as test map
  img:=tbitmap.Create;
  img.LoadFromFile(filename);

  img.free;
end;

end.

