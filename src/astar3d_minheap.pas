unit astar3d_minheap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, astar3d_breadCrumb,fgl;

type


  { TMinHeap }

  TMinHeap = class
  private
    listHead: TSearchNode;
  public
    function HasNext: Boolean;
    procedure Add(item: TSearchNode);
    function ExtractFirst: TSearchNode;
    function clearAndFree(node: tsearchnode):integer;
    destructor destroy;override;
    constructor create;
  end;

implementation
type
    TAllNodes =  specialize TFpGList<TSearchNode>;
var
    allNodes : TAllNodes;
{$HINTS OFF}
{$WARNINGS OFF}

function TMinHeap.HasNext: Boolean;
begin
  Result := (Self.listHead <> nil);
end;

procedure TMinHeap.Add(item: TSearchNode);
var
  ptr: TSearchNode;
begin
  allNodes.add(item);
  if (Self.listHead = nil) then
    Self.listHead := item
  else
    if ((Self.listHead.hasnext = false) and (item.cost <= Self.listHead.cost)) then
    begin
      item.nextListElem := Self.listHead;
      Self.listHead := item;
    end
    else
    begin
      ptr := Self.listHead;
      while ((ptr.nextListElem <> nil) and (ptr.nextListElem.cost < item.cost)) do
      begin
        ptr := ptr.nextListElem;
      end;
      item.nextListElem := ptr.nextListElem;
      ptr.nextListElem := item;
    end;
end;

function TMinHeap.ExtractFirst: TSearchNode;
var
  _result: TSearchNode;
begin
  _result := Self.listHead;
  if listHead.nextListElem<>nil then Self.listHead := Self.listHead.nextListElem;
  Result := _result;
end;

function TMinHeap.clearAndFree(node: tsearchnode): integer;
var i:integer;
begin
  result:=0;
  node.preserve:=true;
  while node.fnext<>nil do begin
    node.fnext.preserve:=true;
    inc(result);
    node:=node.fnext;
  end;
  for i:=0 to allNodes.count-1 do if not allnodes[i].preserve then  allnodes[i].free;
  self.free;
end;

destructor TMinHeap.destroy;
begin
  allNodes.free;
  inherited;
end;

constructor TMinHeap.create;
begin
//  listHead:=nil;
  allnodes:=TAllNodes.create;
end;

end.

