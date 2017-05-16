unit core_weapons;

{$mode objfpc}{$H+}

interface

uses
  VectorGeometry, core_orders_manager, core_types,
  core_bullets, core_orders, core_actor_definitions;

type

  eWeaponStates = (wsReloading,wsReady,wsEmpty,wsReloadingBullet);
  { TWeapon }

const instaHitWeapons : set of eWeaponTypes = [wtBlasterPistol];

type

  TWeapon = class
   private
    //started with reload
    fReloadTimer:single;
    //max magazine capacity
    fCapacity:integer;
    //current count of bullets in magazine
    fAmmo:integer;
    //current weapon state is in
    fstate:eWeaponStates;
    //when reload completes, magazine will be replenished with this count
    fRealoadBullets:integer;
    fWeaponType:eWeaponTypes;
   public
    //fov
    zoomFov:single;
    bulletType:eBulletTypes;
    //parent actors id
    ownerID:integer;
    color:TAffineVector;
    //time needed for reloading mag
    magReloadTime:single;
    //time needed for loading next shot
    bulletReloadTime:single;
    //how fast will bullet start to travel. what happens then depends on bullet update
    bulletLaunchForce:single;
    function shoot(actorID:integer;const origin:TAffineVector;dest:TAffineVector): boolean;
    //total player ammo should be passed as var,
    //reload will use as much as powinien and deplate players ammo by this ammmount
    procedure reload(var playersAmmo:integer);
    property capacity:integer read fCapacity write fCapacity;
    property state:eWeaponStates read fstate write fstate;
    property weaponType:eWeaponTypes read fWeaponType;
    procedure update;
    constructor create(typ:eWeaponTypes;actorID:integer);
  end;

implementation

{ TWeapon }

function TWeapon.shoot(actorID:integer;const origin:TAffineVector;dest:TAffineVector): boolean;
var i:integer;
    o:rorder;
begin
  result:=false;
  //if weapon is loaded and ready then shoot
  if (fstate=wsReady) and (fAmmo>0) then begin
     dec(fAmmo);
     result:=true;
     fstate:=wsReloadingBullet;
     fReloadTimer:=0;
     i:=oM.addOrder;
     o:=om.queue[i];
     //calculate velocity for bullet
     SubtractVector(dest,origin);
     NormalizeVector(dest);
     //insta hit or projectile?
     if fWeaponType in instaHitWeapons then begin
        //issue a scan hit
        with o do begin
          order:=orInstaHit;
          int1:=actorID;
          int2:=getNextBulletID;
          int3:=integer(bulletType);
          v1:=origin;
          v2:=dest;
          //v3:=vector3fmake((random(7)+3)/10,(random(7)+3)/10,(random(7)+3)/10);
          //v3:=vectorScale(VectorNormalize(origin),3);
        end;
     end else begin
       ScaleVector(dest,bulletLaunchForce);
       with o do begin
         order:=orBlasterShot;
         int1:=actorID;;
         int2:=getNextBulletID;
         int3:=integer(bulletType);
         v1:=origin;
         v2:=dest;
         //v3 is color
         //v3:=vector3fmake((random(7)+3)/10,(random(7)+3)/10,(random(7)+3)/10);
         v3:=vectorScale(VectorNormalize(origin),3);
       end;
     end;
     om.queue[i]:=o;
  end else
  if (fstate=wsReady) and (fAmmo=0) then begin
     //make click sound and switch to empty
     fstate:=wsEmpty;
  end else
  if (fstate=wsEmpty) then begin
     //oM.wantReload
     om.addOrder2i(orWantReload,ownerId,0);
  end;
end;

procedure TWeapon.reload(var playersAmmo: integer);
begin
  if fstate=wsReloading then exit;
  if playersAmmo>0 then fstate:=wsReloading;
  fReloadTimer:=0;
  if playersAmmo-fCapacity>=0 then begin
     playersAmmo-=fCapacity;
     fRealoadBullets:=fCapacity;
  end else begin
     fRealoadBullets:=playersAmmo;
     playersAmmo:=0;
  end;
end;

procedure TWeapon.update;
begin
  case fstate of
    wsReloading:begin
      fReloadTimer+=deltaT;
      if fReloadTimer>magReloadTime then begin
         fstate:=wsReady;
         fammo+=fRealoadBullets;
      end;
    end;
    wsReloadingBullet:begin
      fReloadTimer+=deltaT;
      if fReloadTimer>bulletReloadTime then begin
         fstate:=wsReady;
      end;
    end;
  end;
end;

constructor TWeapon.create(typ: eWeaponTypes; actorID: integer);
begin
  case typ of
    wtBlasterPistol:color:=Vector3fmake(0,1,0);
    wtEnemyBlasterPistol:color:=Vector3fmake(0,0,1);
  end;
  ownerID:=actorID;
  bulletReloadTime:=0.2;
  magReloadTime:=1.5;
  fCapacity:=10;
  fAmmo:=fCapacity;
  fweaponType:=typ;
  bulletLaunchForce:=64;
  bulletType:=buBlaster;
  state:=wsReady;
  zoomFov:=30;
end;

end.

