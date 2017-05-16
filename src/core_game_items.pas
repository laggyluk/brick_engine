unit core_game_items;

{$mode objfpc}{$H+}

interface

uses
  core_types,
  VectorGeometry;

type
  //general class
  rGameObject = record
    //flags if array position can be reused
    allocated:boolean;
    id:integer;
    position:TAffineVector;
    //actor that is currently 'manipulating' the object
    ownerID:integer;
    //flags if update
    active:boolean;
    typ:eItemTypes;
    //item count for player inventory
    qty:integer;
  end;
  pGameObject = ^rGameObject;

  { TItemsManager }

  TItemsManager = class
   private
    fitems:array of rGameObject;
    lastUpdatedItem:integer;
    //thanks to 'class function' itemID's won't collide between different stashes
    class function getNewItemID:integer;
    function firstEmptyItem:integer;
   public
    constructor create;
    destructor destroy;override;
    //update all fitems. no need to call if it's a players' stash as those items
    //are not yet placed and active
    procedure update;
    //create item of given type
    function addItem(itemType: eItemTypes; itemQty: integer=1): pGameObject;
    function removeItem(itemID:integer):boolean;
    procedure copyItem(obj:pGameObject);
    //returns nearest item location of given type, be it by stash portal or on the ground
    //returns false if no such item in stash
    function getNearestItemID(itemType: eItemTypes; const origin: TAffineVector;
      var itemLocation: TAffineVector): integer;
    function getItem(itemID:integer):pGameObject;
    //count's item of given type
    function getItemCount(itemType:eItemTypes):integer;
    //returns number of fitems
    function count:integer;
  end;

var
  //manages items that are active in the world (or inactive, lying on the ground)
  itemsManager:TItemsManager;

implementation
var fLastItemID:integer;

{ TItemsManager }

class function TItemsManager.getNewItemID: integer;
begin
  result:=fLastItemID;
  inc(fLastItemID);
end;

function TItemsManager.firstEmptyItem: integer;
var i:integer;
begin
  for i:=0 to length(fitems)-1 do if fitems[i].allocated = false then begin
     result:=i;
     break;
  end;
end;

constructor TItemsManager.create;
var i:integer;
begin
  //fitems:=TGameObjectsList.create;
  setlength(fitems,256);
  for i:=0 to count-1 do fitems[i].allocated:=false;
end;

destructor TItemsManager.destroy;
var i:integer;
begin
  inherited destroy;
end;

procedure TItemsManager.update;
var i:integer;
begin
  //updates one item per frame
  for i:=lastUpdatedItem to count-1 do if (fitems[i].allocated) and (fitems[i].active) then with fitems[i] do begin
    lastUpdatedItem:=i;
    case typ of
     itWeedSeed:begin
       log('aaa');
     end;
    end;
    break;
  end;
  if lastUpdatedItem>count then lastUpdatedItem:=0;
end;

function TItemsManager.addItem(itemType: eItemTypes; itemQty: integer=1): pGameObject;
var s:string;
    i,j:integer;
begin
  result:=nil;
  for i:=0 to length(fitems)-1 do if fitems[i].allocated = false then with fitems[i] do begin
      allocated:=true;
      active:=false;
      id:=getNewItemID;
      typ:=itemType;
      ownerID:=-1;
      qty:=itemQty;
      result:=@fitems[i];
      exit;
  end;
  //if here then all then fitems neds to be expanded
  j:=count;
  setlength(fitems,count+256);
  //flag new items for reuse. hopefully this won't overwrite last item of old array..
  for i:=j to count-1 do fitems[i].allocated:=false;
  //run self again to create item
  addItem(itemType);
end;

function TItemsManager.removeItem(itemID: integer): boolean;
begin
  getItem(itemID)^.allocated:=false;
end;

procedure TItemsManager.copyItem(obj: pGameObject);
begin
  fitems[firstEmptyItem]:=obj^;
end;

function TItemsManager.getNearestItemID(itemType: eItemTypes;const origin:TAffineVector;
         var itemLocation: TAffineVector): integer;
var i:integer;
    f,minDistance:single;
begin
  minDistance:=MaxSingle;
  result:=-1;
  for i:=0 to count-1 do with fitems[i] do begin
    //item is interesting if of given search type or compared is a stash portal
    //stash portal's location is interesting as potential item's closest position
    if (allocated) and (typ=itemType) and (ownerID<>-1) then begin
        //item is owner free
         f:=VectorDistance(origin,position);
         if f<minDistance then begin
            minDistance:=f;
            itemLocation:=position;
            result:=i;
         end;
      end
  end;
end;

function TItemsManager.getItem(itemID: integer): pGameObject;
var i:integer;
begin
  //result:=@fitems[itemID];
  result:=nil;
  for i:=0 to length(fitems)-1 do if (fitems[i].allocated) and (fitems[i].id=itemID) then
  begin
     result:=@fitems[i];
     break;
  end;
end;

function TItemsManager.getItemCount(itemType: eItemTypes): integer;
var i:integer;
begin
  result:=0;
  for i:=0 to length(fitems)-1 do if (fitems[i].allocated) and (fitems[i].typ=itemType) then
      inc(result,fitems[i].qty);
end;

function TItemsManager.count: integer;
begin
  result:=length(fitems);
end;

initialization
fLastItemID:=0;//chtba nie trzeba bo to zmienna 'prawie globalna'

end.




