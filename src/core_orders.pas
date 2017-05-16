unit core_orders;

{$mode delphi}{$H+}

interface

uses
  VectorGeometry;

type

  {$PACKENUM 1}
  //each possible action is represented by a command. each of modules can process
  //some set of commands that interests it
  orders = (
    //creates actor. actor id, actor type, int3>-1 = faction defaults in actor constructor,
    //v1=position, v2=size
     orCreateActor
    ,orDestroyActor
    //int1 actorId, int2= bullet,sword,punch?. int3=attackerID, v1:=intersection point
    ,orActorHurt
    //toggles actor's AI. int1=actorID, int2=1 on 0 off
    ,orToggleBrain
    ,orScaleActorXYZ //scales actor .-||-.not implemented?
    //p. int1 actorID, v1 new position , v2 destination
    ,orMoveActor //moveActor towards vector. actorID, vector3f
    //r. rotate in xyz usign f1,f2,f3
    ,orRotateActor
    //rotate in Y using f1
    ,orRotateActorY
    //int1 actorID,int2=1 followpath 0 not follow,v1 destination
    ,orWantMove //actor wants to move.
    //orders from input device
    //int1 actorID, int2= buttons 0-3, int3 buttons 4-7, int4 buttons 8-11, int5 pow?
    //v1 axes xyz, v2 axes ruv,
    ,orControllerState
    //world snapshot sent to clients
    ,orSnapShot
    //v1=min, v2=max,int1=jobType,int2=jobID
    ,orCreateJobArea
    //jobID,int2=0 move start; int2=1 move end; int2=2 move both;
    ,orModifyJobArea
    //tells system to remove selection/zone. int0=areaID
    ,orRemoveJobArea
    //sent by actor to inform system to start anim, sounds or whatever.actorID, eJobType
    ,orWorkStarted
    //set by actor when work finished. actorID, jobID
    ,orWorkComplete
    //something happened and job is not longer available for completion
    //int1 actorID, int2 jobID
    ,orBreakJob
    //actor needs a path computed. actorID,int2 >0 flying allowed, int3>0 isjobSearch ,v1 start, v2 end,
    ,orNeedPath
    //pathfinder found requested path and sends it's id on the path list. actorID, pathID
    ,orPathFound
    //physics finished moving actor along a path. int1 actorID
    ,orPathFinished
    //aktor szuka celu, logika sprawdza cz go widzi?
    //i co znaczy ze szuka celu. czy tylko kierunku strzalu czy id przeciwnika?
    //,orFindShotVector
    //create a blaster bullet. int1 actorID, int2 bulletid, int3 bulletType, v1 position, v2 vel, v3 color
    ,orBlasterShot
    //params same as blaster, except that it doens't fly and v2=ray direction
    ,orInstaHit
    //actor/player wants to shoot. but if he's weapon ready?
    ,orWantShoot
    //player is pressing reload btn
    ,orWantReload
    //blaster bullet moving. int1=bulletID
    ,orBlasterMove
    //remove bullet. int1 bulletID, int2 boolean(siletn)
    ,orBlasterRemove
    //when player joins. int1 =
    //cos tu trzeba pomyslec
    //,orCreateFaction
    //could carry which player requested. affects logic and physics
    ,orGamePause
    ,orGameUnPause
    //signal subsystems that they should clear prepare for actors load
    ,orMapLoad
    //signal actors loaded. relevant when loading actors from file so no need in multi?
    ,orMapLoaded
    //first thing sent to host when connecting, carry login in payload
    ,orLogin
    //response for orLogin, returns actor id in int1 and 'port' number in int2 to use
    ,orLoggedIn
    //inform client that his login attempt was refused
    ,orConnectionRefused
    //sent from client when closing game
    ,orDisconnect
    //client is ready for game start or some shit
    ,orClientReady
    //actor says he built something. check resources and build it
    //int1 actorID, int2 block type, v1 position
    ,orModifyBlock
    //actor had dug a block. add it to resource
    ,orDigBlock
    //create item creation order. int1 job id, int2 item typ, v1 target position, v2 rot
    ,orCreateItemGhost
    //remove item from stash to 'real world', probably also stick crate texture to some actor for hauling
    //int1 item type,int2  itemID,int3  jobID, int4 actorID, v1 position,
    ,orFromStashToWorld
    //itemID, actorID, ,jobID,v1 position
    ,orItemInstall
    //carry and place item. int1 job id, int2 item typ, v1 target position, v2 rot
    ,orHaulItem
    //set camera mode
    //some modes want to follow given actor. int1=actorID, int2=eCameraModes
    ,orCameraMode
    //sets camera zoom to value indicated by weapon zoom. int1=actorID, zoomIn int2=1, zoomOut int2=0. cameraID?
    ,orCameraWeaponZoom
    //craete auomata(water/oxygen) emmiter. should be processed in TLogic probably but not implemented yet
    ,orCreateAutomata
    ,orMax
    );

  { rOrder }

  rOrder = record
    //flags for deletion. maybe there should be a set of flags, one for each subsystem
    //but not every subsystem cares about every order..
    eatenFlags:bitpacked record
      eatenLogic,
      eatenSound,
      eatenRender,
      eatenPhysics,
      eatenNetwork,
      Bit5,
      Bit6,
      Bit7: Boolean;
    end;
    //message counter
    id:cardinal;
    order:orders;
    //parameters
    int1,int2,int3,int4:integer;
    f1,f2,f3,f4:single;
    v1,v2,v3:TAffineVector;
    procedure init;
    class operator Equal(rOrd1, rOrd2: rOrder)B: Boolean;
  end;
  pOrder = ^rOrder;

implementation

{ rOrder }

procedure rOrder.init;
begin
  eatenFlags.eatenLogic:=false;
  eatenFlags.eatenSound:=false;
  eatenFlags.eatenRender:=false;
  eatenFlags.eatenPhysics:=false;
  //message counter
  id:=-1;
  order:=orMax;
  //parameters
  int1:=0;int2:=0;int3:=0;int4:=0;
  f1:=0;f2:=0;f3:=0;f4:=0;
  setVector(v1,0,0,0);
    setVector(v2,0,0,0);
      setVector(v3,0,0,0);
end;

class operator rOrder.Equal(rOrd1, rOrd2: rOrder)B: Boolean;
begin
  result:=rOrd1.id=rord2.id;
end;

end.

