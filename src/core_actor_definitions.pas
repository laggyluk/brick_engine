unit core_actor_definitions;

{$mode objfpc}{$H+}

//some stuff is common for physics actor and render actor. here they are
interface

uses
  VectorGeometry;

type

  eWeaponTypes = (wtNonde,wtBlasterPistol,wtEnemyBlasterPistol,wtPlasmaRifle);
  eWeaponMod = (wmNone,wmMelee,wmBarell);
  eArmorMods = (amNone,amShield,amRegen);

  eActorTypes = (eNone, eStrach, eKosmo, eBlueMarine);

  rActorDefinition = record
    width,height,depth,width2,height2,depth2:single;
    barrelPosition:TAffineVector;
    assetName:string[32];
    typ:eActorTypes;
    //built from width, height itp
    size:TAffineVector;
  end;

  function getActorCommonProperties(actorType:eActorTypes):rActorDefinition;
  procedure initializeActorCommonProperties;

var
  rStrachDefinition,rBlueMarine:rActorDefinition;

implementation

function getActorCommonProperties(actorType: eActorTypes): rActorDefinition;
begin
  case actorType of
     eStrach:result:=rStrachDefinition;
     eBlueMarine:result:=rBlueMarine;
  end;
end;

procedure initializeActorCommonProperties;
begin
  //teraz orCreateActor bedzie musial miec w parametrze typ aktora
  with rStrachDefinition do begin
    depth:=1.3;
    width:=1.6;
    height:=4;
    height2:=height / 2;
    depth2:=depth / 2;
    width2:=width / 2;
    setVector(size,width2,height2,depth2);
    typ:=eActorTypes.eStrach;
    assetName:='strach';
  end;

  with rBlueMarine do begin
    depth:=1.3;
    width:=1.8;
    height:=4;
    height2:=height / 2;
    depth2:=depth / 2;
    width2:=width / 2;
    setVector(size,width2,height2,depth2);
    typ:=eActorTypes.eBlueMarine;
    assetName:='blueMarine';
  end;
end;

initialization
//could be somwhere else
initializeActorCommonProperties;
end.

