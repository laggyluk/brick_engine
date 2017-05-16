unit core_bullets;

{$mode objfpc}{$H+}

interface

uses
  core_types, VectorGeometry,
  core_orders_manager, core_utils, core_listofrecords, core_orders,
  core_terrain_generator
    {$IFNDEF SERVER} ,core_lights {$ENDIF}
  ;
type


  eBulletTypes = (buBlaster,buPlazma);

  { TBullets }

  TBullets = class
   type
    tData = array of rBulletData;
  private
    //inc/dec as bullets come and go
    bulletCount:integer;
    lastBullet:integer;
    function getCount: integer;
    function getItemByID(index: integer): pBulletData;
   public
    data:tData;
    //renderer doesn't use update, only physics
    procedure Update;
    //render the bullet shape. light is rendered in ds light pass, not here
    procedure render;
    function add(bulletID: integer; bullet: rBulletData): integer;
    property count:integer read getCount;
    property items[index:integer]:pBulletData read getItemByID;default;
    procedure remove(itemID:integer);
    function getNextBullet:pBulletData;
    constructor create;
    destructor destroy;override;
  end;


  function getNextBulletID: integer;

implementation
var
  lastBulletID:integer;

function getNextBulletID: integer;
begin
  result:=lastBulletID;
  inc(lastBulletID);
end;

function TBullets.getCount: integer;
begin
  result:=length(data);
end;

function TBullets.getItemByID(index: integer): pBulletData;
var i:integer;
begin
  for i:=0 to length(data)-1 do begin
    if data[i].ID=index then begin
       result:=@data[i];
       break;
    end;
  end;
end;

procedure TBullets.Update;
var oldVel,newPosition:TAffineVector;
    i:integer;
    limit:integer;
begin
  { TODO -copti : dodac jakis licznik aktywnych bulletow tak zeby caly array nie byl sprawdzany jezeli wszystkie aktywne zostaly zupdatowane }
  limit:=bulletCount;
  for i:=0 to length(data)-1 do if data[i].active then with data[i] do begin
   oldPosition:=Position;
   oldvel:=velocity;
   newPosition:=vectorScale(vectorAdd(OldVel, Velocity), 0.5 * deltat);
   Position := vectorAdd(Position , newPosition);
   //check if bullet went outside map
   if outOfMap(position) then begin
      oM.addOrder2i(orBlasterRemove,ID,integer(true));
      dec(bulletCount);
      dec(limit);
   end
   else
   //collision check
   if getBlockTypeAt(position)<>btNone then begin
      //remove bullet
      oM.addOrder2i(orBlasterRemove,ID,integer(true));
      //fry block
      { TODO -cficzer : wybuchy }
      setBlockAt(position,btNone);
      dec(bulletCount);
      dec(limit);
   end else
     oM.addOrder(orBlasterMove,ID,newposition,velocity);
  end;
end;

procedure TBullets.render;
begin

end;

function TBullets.add(bulletID:integer;bullet: rBulletData): integer;
var
    i:integer;
    flag:boolean;
begin
  flag:=false;
  //find empty space in array
  for i:=0 to length(data)-1 do if not data[i].active then begin
      flag:=true;
      break;
  end;
  //if no free space then resize array
  if not flag then begin
     i:=length(data);
     setlength(data,length(data)+32);
  end;
  bullet.ID:=bulletID;
  bullet.hSphereRadius:=0.1;
  data[i].active:=true;
  move(bullet,data[i],sizeof(rBulletData));
  result:=i;
  inc(bulletCount);
end;

procedure TBullets.remove(itemID: integer);
begin
  data[itemID].active:=false;
end;

function TBullets.getNextBullet: pBulletData;
var i:integer;
begin
  result:=nil;
  //gets newxt bullet till end of array/limit is reached then on next update resets
  //index
  for i:=lastBullet to bulletCount-1 do begin
    if (data[i].active=false) then continue;
    result:=@data[i];
    lastBullet:=i+1;
    exit;
  end;
  //cala lista przeiterowana, zwroc nulla
  if lastBullet>bulletCount then begin
     lastBullet:=0;
     result:=nil;
     exit;
  end;
  inc(lastBullet);
end;

constructor TBullets.create;
begin
  setlength(data,64);
//  data:=tdata.create
end;

destructor TBullets.destroy;
var i:integer;
begin
//  for i:=0 to data.Count-1 do bullets.data[i].Destroy;
//  data.free;
  inherited destroy;
end;


end.

