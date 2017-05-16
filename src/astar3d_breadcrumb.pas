unit astar3d_breadCrumb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, astar3d_point3d, VectorTypes, VectorGeometry;

type

  { TSearchNode }

  TSearchNode = class
  public
    preserve:boolean;
    position: TPoint3D;
    cost: Integer;
    pathCost: Integer;
    fnext: TSearchNode;
    prev: TSearchNode;
    nextListElem: TSearchNode;
   // id:integer;
  public
    function hasNext:boolean;
    function getNext: TSearchNode;
    constructor Create(pposition: TPoint3D; pcost: Integer; ppathCost: Integer; nnext: TSearchNode);
    destructor destroy;override;
  end;

  //holds result of path search. better than stupid one above
  { TPath }

  TPath = class
   private
    data:array of TVector3i;
    Index:integer;
   public
    function getNext:TVector3f;
    function hasNext:boolean;
    function position:TVector3i;
    constructor create(fromNode:TSearchNode;size:integer);
    destructor destroy;override;
  end;

implementation

var counter:integer;

{ TPath }

function TPath.getNext: TVector3f;
begin
  inc(index);
end;

function TPath.hasNext: boolean;
begin
  if index<length(data) then result:=true else result:=false;
end;

function TPath.position: TVector3i;
begin
  result:=data[index];
end;

constructor TPath.create(fromNode: TSearchNode; size: integer);
var i:integer;
begin
  index:=0;
  setLength(data,size);
  i:=0;
  while fromNode.hasNext do begin
        data[i]:=fromNode.position;
        fromNode:=fromNode.getNext;
        inc(i);
  end;
  fromnode.free;
end;

destructor TPath.destroy;
begin

end;

{$HINTS OFF}
{$WARNINGS OFF}

destructor TSearchNode.destroy;
begin

  //self:=nil;
  //if prev<>nil then
  //    prev.destroy;
  dec(counter);
  inherited;
end;

function TSearchNode.hasNext: boolean;
begin
  if fnext<>nil then result:=true else result:=false;
end;

function TSearchNode.getNext: TSearchNode;
begin
  result:=fnext;
  freeandnil(self);
end;

constructor TSearchNode.Create(pposition: TPoint3D; pcost: Integer;
  ppathCost: Integer; nnext: TSearchNode);
begin
  inherited Create;
  Self.position := pposition;
  Self.cost := pcost;
  Self.pathCost := ppathCost;
  Self.fnext := nnext;
  if nnext<> nil then Self.fnext.prev:=self;
 // id:=counter;
  inc(counter);
end;

end.

