unit core_physics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, core_types, VectorGeometry, fgl, core_orders_manager,
  core_chunk, core_utils, VectorTypes, math, astar3d_breadCrumb, core_pathfinding,
  core_bullets, core_player,core_listofrecords,core_orders,core_animations,
  core_camera,GeometryBB, core_actor_definitions
  {$IFNDEF SERVER} ,core_render, renderable_object_types{$ENDIF}
  ;
const
      //pressing action btn longer means player wants to climb/run
      //lower means evasive roll/use world
      actionBtnHoldValue = 0.5;
      //forward roll is dir+space, if space was released longer than this then no roll
      actionReleaseTreshold = 0.1;
      rollLength = 3;
      gravity = 0.1;
type

  { TFlatPhysicsUnit }

  TFlatPhysicsUnit = class
   private
    fBarrelPos: TAffineVector;
    oldDestination:TAffineVector;
    //last valid position rounded to block coords
    oldBlokPos,oldOldBlokPos:TAffineVector;
    fallSpeed:single;
    function getBarrelPos:TAffineVector;
   public
    common:rActorDefinition;
    //actor can climb that high with action button
    manualClimbHeight:single;
    //actor won't stop on obstacle that gigh, just go ontop
    autoClimbHeight:single;
    mass:single;
    jumpVelocity:single;
    //how silny będzie skok
    jumpStrength:single;
    actorID:integer;
    //actually shouldnt be needed here
    assetID:integer;
    velocity:TAffineVector;
    position,oldPosition:TAffineVector;
    rotation,oldRotation:TAffineVector;
    scale,oldScale:TAffineVector;
    anglesAxes:TAffineVector;
    destination,destinationNext:TAffineVector;
    staraCwiara:integer;
    //old distance to targt, check if not overshot
    oldDistance:single;
    path:TPath;
    //flags that unit is following path and shouldnt lerp to destination till last navpoint
    followingPath:boolean;
    maxRotSpeed,accel,maxAccel:TAffineVector;
    sleeping:boolean;
    //ignores gravity
    canFly:boolean;
    maxAcceleration:single;
    maxRunningSpeed,maxWalkingSpeed:single;
    acceleration:single;
    ///I guess this defy the concept of having this shit in array
    animations:TActorAnimations;
    //'action' button (cover/jump/roll) was pressed for that long
    //pressing btn start the timer, releasing stops it. not releasing is also ok for continuus action
    //but timer needs to be reseted after initiated action finishes
    actionBtnPressedTime:single;
    //time that passed since action button was released
    actionBtnReleasedTime:single;
    //is action btn down?
    actionBtnPressed:boolean;
    //flags that player is not pressing any of movement btns
    noDirection:boolean;
    hitBox:TAABB;
    hitSphere:TBSphere;
    //flags if hit box needs updating
    lastHitBoxUpdate:cardinal;
    procedure stop;
    procedure update;
    procedure climbAnimationFinish;
    //calculate hitbox when raycast with actor is performed but only once per frame
    procedure updateHitBox;
    property BarrelPos: TAffineVector read getBarrelPos write fBarrelPos;
    constructor create;
    destructor destroy;override;
  end;
  pFlatPhysicsUnit = ^TFlatPhysicsUnit;
  rFlatPhysicsUnitList = specialize TFPGList <TFlatPhysicsUnit>;

  { TPhysicsEngine }

  TPhysicsEngine = class
   private
    skipUpdate:boolean;
    bullets:TBullets;
    paused:boolean;
    //limits array search
    activeActors:integer;
    //buffer for worldsnapshot and instaHit test
    tempSnapShotBuf:tbytes;
    function createActor(const identity: eActorTypes): TFlatPhysicsUnit;
    //from this point bullets fly
//    function getBarrelPos[actorIndex:integer]: TAffineVector;
   public
    actors: rFlatPhysicsUnitList;
    function rayHitActor(rayStart, rayDir: TAffinevector;
      var hitPoint: TAffinevector): boolean;
    procedure update;
    function indexOf(id:integer):integer;
    //performs a ray cast vs actors check, can build and use current world state
    //or supplied history snapshot
    procedure instaHitSnapshot(rayStart, rayDir: pAffineVector; shooterID: integer;
      buildOnly: boolean; data: tBytes; addr: cardinal);
    constructor create;
    destructor destroy;
  end;

var
  PhysicsEngine: TPhysicsEngine;
  //how close to destination is enough to stop

implementation

{ TPhysicsEngine }
var tempFlag:boolean;
procedure TPhysicsEngine.update;
var
  i,j,index:integer;
  u:TFlatPhysicsUnit;
  p1,p2,crossHairs:taffinevector;
  b:rBulletData;
  pb:pBulletData;
  cmd:rorder;
  flag:boolean;
  a:rAnim1f;
  hs:TBSphere;
  m:tmatrix;
  f:single;
begin
  //get orders
  for i:=0 to oM.queue.Count-1 do begin
    cmd:=oM.queue[i];
    if cmd.eatenFlags.eatenPhysics then continue;
    with cmd do  begin
       eatenFlags.eatenPhysics:=true;
//       if skipUpdate then contin3ue;
       case order of
        orCreateActor: begin
         if options.debug.orders then log('physics unit created id: '+inttostr(int1));
            u:=CreateActor(eactortypes(int2));
            u.actorID:=int1;
            //typ:= int2;
            u.position:=v1;
            u.destination:=v1;
            u.oldPosition:=v1;
         end;
        orMapLoad:begin
          for index:=0 to actors.Count-1 do actors[index].Free;
          actors.Clear;
          for index:=0 to bullets.Count-1 do bullets.remove(index);
          skipUpdate:=true;
          if gameMode then skipUpdate:=false;
        end;
        orMapLoaded:begin
          //if gameMode then skipUpdate:=false;
        end;
        //only issued in physic. other modules update position based on that
        {orMoveActor: begin
         //if options.debug.orders then log('physics actor move: '+inttostr(int2));
          index:=indexof(int1);
          if index>-1 then begin
            u:=actors[index];
            //u.oldPosition:=u.position;
            u.position:=v1;
            u.destination:=v2;
          end;
        end;}

        orControllerState:if gameMode then begin
          //if camera.mode<>cmFree then
          index:=indexof(int1);
          if index>-1 then begin
             u:=actors[index];
             //if (camera.mode<>cmFree) or (u.actorID<>player1.actorID) then
             with u do begin
             //some actions ignore user imput for time being
             if (eHumanActorAnimations(animations.anim.anims[0].typ) in animAbortables) then begin
              //ignore noise
              flag:=false;
              if abs(v1[0])<analogNoiseLevel then v1[0]:=0 else flag:=true;
              if abs(v1[1])<analogNoiseLevel then v1[1]:=0 else flag:=true;
              if flag then begin
                 //we need to rotate input by same angle to go in direction of look
                 v1:=VectorRotateAroundZ(v1,camera.inputRot);
                 //if camera looking angle is beyong treshold then rotate actor target to
                 //new direction to face it
                 //if camera.mode=cm3rdPerson then begin
                 //   f:=abs(camera.inputRot-u.rotation[1]);
                 //   if f>0.5 then begin
                 //     f:=camera.inputRot-u.rotation[1];
                 //     v1:=VectorRotateAroundZ(v1,f);
                 //   end;
                 //end;
//                 position:=VectorRotateAroundY(position,camera.inputRot);
                 oM.addOrder(orWantMove,actorID,0,
                 vector3fmake(position[0]+v1[0],position[1],position[2]+v1[1]));
              end;
              noDirection:=not flag;
              destinationNext:=v2;
              //check button press state
              //kolacze mi sie ten stan
              //if bitget(cmd.int2,2) then begin
              //  //zoom
              //  //animated zoom?
              //  //camera.CreatePerspectiveM(20,camera.aspect, camera.zNear, camera.zFar);
              //  if camera.zoomed=false then oM.addOrder2i(orCameraWeaponZoom,player1.actorID,1);
              //  //camera.zNear:=0.5;
              //end else if camera.zoomed then oM.addOrder2i(orCameraWeaponZoom,player1.actorID,0);
              if bitget(cmd.int2,3) then begin
                //won't work with pad?!
               // crossHairs:=camera.crossHairs;
                crossHairs:=camera.crossHairs;
                oM.addOrder(orWantShoot,actorID,getBarrelPos,crossHairs);
              end;
              if bitget(cmd.int2,4) then begin
                oM.addOrder2i(orWantReload,actorID,0);
              end;
              //action button. when pressed, value increases
              actionBtnPressed:=bitget(cmd.int2,1);
              if actionBtnPressed then begin
                 actionBtnPressedTime+=deltaT;
                 actionBtnReleasedTime:=0;
              end
              else actionBtnReleasedTime+=deltaT;
             end;
            end;
          end;
        end;
        orWantMove: begin
          index:=indexof(int1);
          if index>-1 then with actors[index] do begin
          //u:=unitsList[cmd.int1];
           destination:=v1;
           sleeping:=false;
           followingPath:=boolean(cmd.int2);
           if followingPath then begin
             j:=pathManager.paths.IndexOf(cmd.int1);
             if j>-1 then begin
                path:=pathManager.paths[cmd.int1];
                destination:=position;
                //SetVector(accel,0,0,0);
             end else log('achtung! physics.update: orWantMove - path doesn''t exist');
          end;
         end;
        end;
        orBlasterShot: begin
          //b:=TBlasterShot.create;
          b.create(int3);
          b.ID:=int2;
          b.ownerID:=int1;
          //b.assetId:=int1;
          b.position:=v1;
          b.velocity:=v2;
          b.orient;
          //b.velocity:=vectorNormalize(VectorSubtract(v2,v1));
          bullets.Add(int2,b);
        end;
        orInstaHit:begin
          instaHitSnapshot(@v1,@v2,int1,false,nil,0);
        end;
        orBlasterRemove:with bullets[int1]^ do begin
          //b.light.Destroy;
          active:=false;
          //bullets.remove(ID);
          //b.Destroy;
        end;
        orGamePause:paused:=true;
        orGameUnPause:paused:=false;
        orDestroyActor:begin
          index:=indexof(int1);
          if index>-1 then begin
            actors[index].Free;
            actors.delete(index);
          end;
        end;

       end;
    end;
    oM.queue[i]:=cmd;
 end;

  //update units
  if (not paused) and (not skipUpdate) then for i:=0 to actors.count-1 do actors[i].update;

  //update bullets
  if not paused then{ TODO : change bullets back to normal list }
     bullets.Update;

  repeat
    pb:=bullets.getNextBullet;
    if pb=nil then break;
    hs.center:=pb^.position;
    hs.Radius:=pb^.hSphereRadius;
    for i:=0 to actors.Count-1 do begin
      if pb^.ownerID=actors[i].actorID then continue;
      //god mode
      if actors[i].actorID=player1.actorID then continue;
      if BSphereIntersectsBSphere(actors[i].hitSphere,hs) then with actors[i] do begin
        //test precise hit with bbox
        { TODO -copti : actor box will be calculated for every bullet entering his bounding sphere. could be done once }
        updateHitBox;
        //log('actor sphere-bullet hit!');
        //dir not normalized?
        //tu jest jaki uj pogrzebany
        if RayCastBoxIntersect(pb^.oldPosition,VectorSubtract(pb^.position,pb^.oldPosition),
                  hitBox.min,hitbox.max,@p1) then
                  //if PointSegmentDistance(p1,pb^.oldPosition,pb^.position)<0.01 then
                  begin
        //if PointInAABB(pb^.position,hab) then begin
           log('actor hitbox hit!');
           //destroy bullet and damage actr?
           om.addOrder(orActorHurt,actorID,0,p1);
           //om.addOrder(orDestroyActor,actors[i].actorID,0);
           om.addOrder(orBlasterRemove,pb^.ID,0);
           { TODO : don't destroy piercing bullets }
           bullets.remove(pb^.ID);
        end;
      end;
    end;
  until false;
  //insta hit weapons test
  //should happen only when someone squeezes a trigger.
  //so maybe we can construct a snapshot at that time and perform check using it
  //request from past could be made also, using different dataset
  //czyli funkcja bez parametru powinna konstruowac snapshota a z parametrem uzyc go

end;

procedure TPhysicsEngine.instaHitSnapshot(rayStart, rayDir: pAffineVector;
  shooterID: integer; buildOnly: boolean; data: tBytes;addr:cardinal);
var i,count:integer;
    u:TFlatPhysicsUnit;
    offset:cardinal;
    hitbox:TAABB;
    point:TAffineVector;
    sm:smallint;
    w,id:word;
begin
  offset:=addr;
  { TODO -copti : check if timestamp is same, if so then don't build }
  //if no snapshot data is passed or we want to then build snapshot using current state
  if (data=nil) or (buildOnly) then begin
    //we can either fill supplied buffer or use temp
    if data=nil then data:=@tempSnapShotBuf[0];
    //write snapshot info
    move(netFrame,data[offset],4); //frameID
    w:=actors.count;
    move(w,data[offset+4],2); //actors count
    offset+=6;
    //copy actor's meaningful data
    for i:=0 to actors.count-1 do begin
       u:=actors[i];
       w:=u.actorID;
       move(w,data[offset],2);
       move(u.position,data[offset+2],12);
       move(u.velocity,data[offset+14],12);
       //type
       data[offset+26]:=byte(u.common.typ);
       //rotY
       sm:=round(radtodeg(u.rotation[1]));
       move(sm,data[offset+27],2);
       //hitbox
       move(u.hitBox,data[offset+29],sizeof(TAABB));
       offset+=snapEntitySize;
    end;
    offset:=addr;
  end;
  if buildOnly then exit;
  //now perform a ray cast on that data

  //entities count
  move(data[offset+4],w,2);
  offset+=6;
  count:=w;
  for i:=0 to count-1 do begin
      //+6 is for snapshot header: netFrame and entities count
      move(data[offset+(snapEntitySize*i)],id,2);
      if id=shooterID then continue;
      move(data[offset+29+(snapEntitySize*i)],hitbox,sizeof(hitbox));
      if RayCastBoxIntersect(
                         rayStart^,
                         rayDir^,
                         hitBox.min,hitbox.max,
                         @point)
      then begin
           log('insta Hit!');
           om.addOrder(orActorHurt,id,0,shooterID,point);
      end;
  end;
end;

constructor TPhysicsEngine.create;
begin
//  unitsList:=TFlatPhysicsUnitList.Create;
  actors:=rFlatPhysicsUnitList.create;
  bullets:=Tbullets.create;
  setlength(tempSnapShotBuf,snapPageSize);
end;

destructor TPhysicsEngine.destroy;
var
  i:integer;
begin
//  for i:=0 to unitsList.Count-1 do unitsList.data[i].Destroy;
//  unitsList.Destroy;
//  for i:=0 to bullets.Count-1 do bullets.data[i].Destroy;
  actors.free;
  bullets.free;;
end;

function posBlocked(pos:TVector3f):boolean;
var x,y,z,cx,cz:cardinal;
    tmp:TVector3i;
begin
  result:=false;
  tmp[0]:=round(pos[0])+world_width2;
  tmp[2]:=round(pos[2])+world_depth2;
  tmp[1]:=round(pos[1]);
  if (tmp[0]>world_width-1) or (tmp[1]>chunk_height-4) or (tmp[2]>world_depth-1)
      or (tmp[0]<0) or (tmp[1]<1) or (tmp[2]<0) then
          result:=false else
  begin
    cx:=tmp[0] div chunk_width;
    cz:=tmp[2] div chunk_width;
    x:= tmp[0] mod chunk_width;
    y:= tmp[1];
    z:= tmp[2] mod chunk_width;
    if
        (eblockTypes(chunksPointer^[cx,cz].blocks[x,y+3,z]) in mineralsNonBlocking) and //head
        (eblockTypes(chunksPointer^[cx,cz].blocks[x,y+2,z]) in mineralsNonBlocking)
       // and //torso
       // (chunksPointer^[cx,cz].blocks[x,y+1,z]=byte(btNone)) // pelvis
       //and (chunksPointer^[cx,cz].blocks[x,y,z]=byte(btNone)) //legs
       //and (eblocktypes(chunksPointer^[cx,cz].blocks[x,y-1,z])in mineralsWalkable) //ground
       then result:=true;
  end;
end;

{ TFlatPhysicsUnit }

procedure TFlatPhysicsUnit.stop;
begin
  destination:=position;
  velocity[0]:=0;
  velocity[2]:=0;
end;

procedure TFlatPhysicsUnit.update;
const stopSoftness = 0.005;
var
  dirtyPos,dir,pos,oldVel,v,newPosition,newRotation,newDest:TAffineVector;
  ang,ang2,distanceXZ,distanceY,h:single;
  noRot:boolean;
  cwiara:integer;
  i,j:integer;
  f:single;
  maxSpeed:single;
  o:rorder;
  validPosition:boolean;
begin
  //update animations, they modify things so remember important things first?
  animations.update;
  noRot:=false;
  //remember stuff
  dirtyPos:=position;
  newPosition:=position;
  oldScale:=scale;
  newRotation:=rotation;
  newDest:=destination;
  validPosition:=true;

  if (animations.fCurrentState in animGravitables) then begin
    //for rolling animation use old direction to avoid mid jump rotation weirdness
    if (animations.fCurrentState=asRollingForward) then newDest:=oldDestination;
    dir:=VectorSubtract(newDest,position);
    distanceXZ:=VectorLength(vector3fMake(dir[0],0,dir[2]));
    distanceY:=dir[1];
    NormalizeVector(dir);
    oldVel:=velocity;

    //if action btn is held down then we wanna run
    maxSpeed:=maxWalkingSpeed;
    //don't try to change state for animations that can't be canceled by user
    if (animations.fCurrentState in animAbortables) then begin
      // jak stoimy to nie wlaczaj odrazu biegu
      if (actionBtnPressedTime>=actionBtnHoldValue) and (actionBtnPressed)
          and (animations.fCurrentState<>asStanding) then begin
         maxSpeed:=maxRunningSpeed;
         animations.setState(asRunning);
      end
      else //roll, but in which direction?
      if (actionBtnPressedTime>0) and (actionBtnPressedTime<actionBtnHoldValue)
          and (actionBtnPressed=false) and (fallSpeed=0) and (distanceXZ>0)
          and (actionBtnReleasedTime<actionReleaseTreshold) then begin
         actionBtnPressedTime:=0;
         //set same newDest so actor doesn't magically move back
         setVector(newDest,
                   position[0]+dir[0]*rollLength*2.1,
                   position[1],
                   position[2]+dir[2]*rollLength*2.2);
         //move x
         animations.anims[asRollingForward].anims[0].start:=position[0];
         animations.anims[asRollingForward].anims[0].finish:=newDest[0];
         //move z
         animations.anims[asRollingForward].anims[1].start:=position[2];
         animations.anims[asRollingForward].anims[1].finish:=newDest[2];
         //move y up
         animations.anims[asRollingForward].anims[3].start:=position[1];
         animations.anims[asRollingForward].anims[3].finish:=position[1]+common.height *0.7;
         animations.setState(asRollingForward);
      end
      else
       //jak nic innego to set state walking?
      if (animations.fCurrentState<>asWalking) and (distanceXZ>0) then begin
          animations.setState(asWalking);
          actionBtnPressedTime:=0;;
      end
      //jak ani idzie ani biegnie i dystans do celu to ustaw asStanding
      else if ((animations.fCurrentState=asWalking)
          or (animations.fCurrentState=asRunning)) and (distanceXZ=0)  or (noDirection)
          then begin
               animations.setState(asStanding);
               newDest:=newPosition;
          end;
    end;

    if abs(accel[1])>10 then begin
       log('physics actor.update: warp wtf?!');
       accel[1]:=1;;
    end;
    accel:=vectorlerp(accel,maxAccel,deltaT);

    //vel:=vectorAdd(vel,vectorScale(dir,acceleration));
    velocity:=vectorScale(dir,accel);

    //if VectorLength(velocity)>0.1 then
    velocity:=vectorScale(velocity,maxSpeed*deltaT);

    velocity:=vectorScale(vectorAdd(velocity,oldVel),0.5);
    //fallSpeed:=(gravity*deltaT);
    velocity[1]-=fallSpeed;
    //velocity[1]+=jumpVelocity*3;

    addVector(newPosition,velocity);

    //y axis. kolizje z ziemia
    setvector(v,newPosition[0],newPosition[1]-0.2,newPosition[2]);
    if ((getBlockTypeAt(v) in mineralsNonBlocking)) then
       fallSpeed+=gravity*deltaT
    else begin
        fallSpeed:=0;
        newPosition[1]:=getBlockCollisionUp(newPosition)-0.4;
        //round(newPosition[1]);
    end;
    if fallSpeed>1 then fallSpeed:=1;

   if (path=nil) and (distanceXZ<0.2) then begin
      //target reached
       newDest:=newPosition;
       noRot:=true;
       //setVector(accel,0,accel[1],0);
  //     sleeping:=true;
     end else if (path<>nil) and (distanceXZ<0.2) then newDest:=destinationNext;

    //no walking outside the map
    wrapPosition(newPosition);

       //newPosition[1]:=round(newPosition[1])+0.55;

    //x,z wall collision check.
    //check next position on course rather than present
    v:=VectorScale(dir,0.5);
    AddVector(v,newPosition);
    wrapPosition(v);
    //first check if there is enough space to fit the actor
    j:=willActorFit(v,common.height);
    //not all blocks are empty but first one is within autoclimb                    www
    if (j>0) then begin
       //check if there's enough space to fit in at new height
       if (j<=autoClimbHeight) then
       else if j<=manualClimbHeight then begin
          //if action button pressed and animation not w toku then..
          //skok jest widoczny przez to ze snapuje do srodka kwadrata
          if (actionBtnPressedTime>actionBtnHoldValue)
              and (eHumanActorAnimations(animations.anim.anims[0].typ)<>asClimbing) then
          begin
             //set same newDest so actor doesn't magically move back
             setVector(newDest,
                       position[0]+dir[0],
                       position[1]+j,
                       position[2]+dir[2]);
             //move up
             animations.anims[asClimbing].anims[0].start:=position[1];
             animations.anims[asClimbing].anims[0].finish:=newDest[1];
             //move x
             animations.anims[asClimbing].anims[1].start:=position[0];
             animations.anims[asClimbing].anims[1].finish:=newDest[0];
             //move z
             animations.anims[asClimbing].anims[2].start:=position[2];
             animations.anims[asClimbing].anims[2].finish:=newDest[2];
             //activate animation
             animations.setState(asClimbing);
          end else begin
            setVector(newPosition,oldposition[0],newPosition[1],oldposition[2]);
            accel[0]:=0;accel[2]:=0;
            noRot:=true;
          end;
          //if not pressed then stay
       end else begin
          setVector(newPosition,oldposition[0],newPosition[1],oldposition[2]);
          accel[0]:=0;accel[2]:=0;
          noRot:=true;
          end;
       //check if we are not jumping into the wall
       if (j>autoClimbHeight) and (animations.fCurrentState=asRollingForward) then
       begin
        //if so then stop position animation but not rotation
        //animations.anim.anims[0].stop;
        animations.anim.anims[0].start:=newposition[0];
        animations.anim.anims[0].duration-=animations.anim.anims[0].time;
        animations.anim.anims[0].time:=0;
        animations.anim.anims[0].finish:=oldOldBlokPos[0];

        animations.anim.anims[1].start:=newposition[2];
        animations.anim.anims[1].finish:=oldOldBlokPos[2];
        animations.anim.anims[1].duration-=animations.anim.anims[1].time;
        animations.anim.anims[1].time:=0;

        animations.anim.anims[3].start:=newposition[1];
        animations.anim.anims[3].finish:=oldOldBlokPos[1];
        animations.anim.anims[3].duration-=animations.anim.anims[3].time;
        animations.anim.anims[3].time:=0;
        validPosition:=false;
       // log('aaa wtf');
        //newPosition[0]:=oldOldBlokPos[0];
        //newPosition[1]:=animations.anim.anims[3].start;
        //newPosition[2]:=oldOldBlokPos[2];

        //animations.setState(asstanding);
        //a moze by tak przelaczyc tu stan na cos innego co jest nie gravitable az sie
        //skonczy. albo co
       end;
    end;
    //now check again for low obstacles at current pos
    j:=willActorFit(position,common.height);
    if (j>0) and (j<=autoClimbHeight) and (willActorFit(vector3fmake(newPosition[0],newPosition[1]+j,newPosition[2]),common.height)=0)
        then newPosition[1]+=j;
  end;
  //update renderable
  if not VectorEquals(newPosition,oldPosition) then begin
     o.order:=orMoveActor;
     o.int1:=actorID;
     o.v1:=VectorLerp(newPosition,position,deltaT);
     o.v2:=newDest;
     o.eatenFlags.eatenPhysics:=true;
     oM.addOrder(o);
     //update bounding boxs
     hitSphere.Center:=position;
     hitSphere.Center[1]+=common.height2;
     updateHitBox;
     //hitBox.min:=VectorSubtract(position,vector3fmake(width2,0,depth2);
     //hitBox.max:=VectorAdd(position,vector3fmake(width2,height,depth2);
     end
     else begin
        //stop
       noRot:=true;
       accel[0]:=0;accel[2]:=0;
     end;
  //face dir of movement
  { TODO : przy zatrzymaniu vektor kierunku albo sie ustawia na zero albo coś bo kierunek patrzenia zmienia się nagle o 180 a widać to jako taki zgrzyt mały.  }
  if noRot=false then begin
     ang:= vectorGeometry.arctan2(newDest[2] - newPosition[2], newDest[0] - newPosition[0]);
     ang:=pi_half - ang;
     if abs(ang-oldRotation[1])>pi then begin
        ang:=ang-pi_2;
     end;
     if distanceXZ>0.1 then begin

       if abs(ang-oldRotation[1])<0.001 then ang:=oldRotation[1]
       else ang:=lerp(oldRotation[1],ang,deltaT*maxRotSpeed[1]);
       {if ang<>oldRotation[1] then begin
          //log(floattostr(ang)+' rot '+floattostr(anglesAxes[1]-ang));
          oM.addOrderi1f1(orRotateActorY,actorID,ang);
       end;}
       newRotation[1]:=ang;
    end;
  end;

  if not vectorequals(oldScale,scale) then
     oM.addOrder(orScaleActorXYZ,actorID,scale);

  rotation:=newRotation;
  if not vectorequals(oldRotation,newRotation) then begin
       o.init;
       o.order:=orRotateActor;
       o.int1:=actorID;
       o.v1:=newRotation;
       o.f1:=common.height / 2;
       om.addorder(o);
       //updateHitBox;
    end;
  position:=newPosition;

  destination:=newDest;
  oldPosition:=newPosition;
  oldRotation:=rotation;
  oldDestination:=destination;

  v:=vectorRound(position);
  if (validPosition) and not (VectorEquals(oldblokPos,v)) then begin
     oldOldBlokPos:=oldBlokPos;
     oldBlokPos:=v;
  end;
  //if (VectorEquals(position,dirtyPos)) and (ang=0) then
  //   sleeping:=true;
end;

procedure TFlatPhysicsUnit.climbAnimationFinish;
begin
  stop;
  actionBtnPressedTime:=0;
end;

procedure TFlatPhysicsUnit.updateHitBox;
var m:tmatrix;
begin
  if lastHitBoxUpdate=globalFrame then exit;
  //hitBox.min:=VectorSubtract(position,vector3fmake(width2,0,depth2));
  //hitBox.max:=VectorAdd(position,vector3fmake(width2,height,depth2));
  hitBox.min:=vector3fmake(-common.width2,0,-common.depth2);
  hitBox.max:=vector3fmake(common.width2,common.height,common.depth2);
  m:=CreateRotationMatrixY(rotation[1]);
  //m:=IdentityHmgMatrix;
  TranslateMatrix(m,position);
  AABBTransform(hitBox,m);
  lastHitBoxUpdate:=globalFrame;
end;

constructor TFlatPhysicsUnit.create;
begin
  noDirection:=true;
end;

destructor TFlatPhysicsUnit.destroy;
begin
  animations.free;
  inherited destroy;
end;

function TFlatPhysicsUnit.getBarrelPos: TAffineVector;
begin
  result:=position;
  //units height or sth?
  result:=VectorAdd(position,fBarrelPos);
end;

function TPhysicsEngine.rayHitActor(rayStart, rayDir: TAffinevector;var hitPoint:TAffinevector):boolean;
var
  i:integer;
  distance,f:single;
  wlot,wylot,ray,dir:TVector;
  wynik:taffinevector;
  hit:boolean;
begin
  result:=false;
  distance:=MaxSingle;
  ray:=vectormake(rayStart,1);
  for i:=0 to actors.Count-1 do begin
    if actors[i].actorID=player1.actorID then continue;
    //lepiej najpierw zbadać odleglosc czy odrazu hitbox?
    f:=VectorDistance(rayStart,actors[i].position);
    if f<distance then begin
      Dir:=vectormake(rayDir,1);
      hit:=0<RayCastSphereIntersect(ray,Dir,vectormake(actors[i].hitSphere.Center,1),actors[i].hitSphere.Radius,wlot,wylot);
      if hit then begin
        result:=true;
        distance:=f;
        hitPoint:=vector3fmake(wlot);
        //log('mouse ray hit!');
      end;
    end;
  end;
end;

function TPhysicsEngine.createActor(const identity: eActorTypes): TFlatPhysicsUnit;
var i:integer;
begin
  //find space in array or resize it
  i:=actors.Add(TFlatPhysicsUnit.create);
  result:=actors[i];
  with actors[i] do begin
  mass:=1;
  common:=getActorCommonProperties(identity);
  maxAcceleration:=0.2;
  setVector(maxAccel,0.2,gravity,0.2);
  setVector(maxRotSpeed,1,5,1);
  canFly:=false;
  jumpStrength:=1;

  hitSphere.radius:=common.height2;

  hitBox.min:=VectorSubtract(position,vector3fmake(common.width2,0,common.depth2));
  hitBox.max:=VectorAdd(position,vector3fmake(common.width2,common.height,common.depth2));

  manualClimbHeight:=2;
  autoClimbHeight:=1;
  setVector(fBarrelPos,0,common.height*0.55,0);
  setVector(scale,1,1,1);
  //standing animation
  //w zasadzie to nie znamy jeszcze skali bo sie sama ustawia po wczytaniu
  //modelu. wiec taka animacja nie powinna sie odpalac do czasu skonczenia
  //wczytywania levela
  //don't animate value, just wait
  animations:=TActorAnimations.create;
  animations.anims[asStanding]:=TAnimationSet.create(asStanding,1);
  with animations.anims[asStanding].anims[0] do begin
    duration:=1;
    dither:=atWait;
    loop:=ltPingPong;
    repeats:=0;
    typ:=integer(eHumanActorAnimations.asStanding);
  end;

  //walking animation
  animations.anims[asWalking]:=TAnimationSet.create(asWalking,1);
  //x axis roll
  with animations.anims[asWalking].anims[0] do begin
    start:=degtorad(-5); finish:=degtorad(2);
    duration:=0.2; repeats:=0; dither:=atLinear; value:=@rotation[2];
    loop:=ltPingPong;
  end;
  //running animation. same as walking but faster
  animations.anims[asRunning]:=TAnimationSet.create(asRunning,1);
  //x axis roll
  with animations.anims[asRunning].anims[0] do begin
    start:=degtorad(-5); finish:=degtorad(2);
    duration:=0.1; repeats:=0; dither:=atLinear; value:=@rotation[2];
    loop:=ltPingPong;
  end;

  //climbing obstacle that is within manualClimb.}
  //need to animate y to go up, then x and z to move to new block
  animations.anims[asClimbing]:=TAnimationSet.create(asClimbing,3);
  //after moving over new block, switch to standing anim
  animations.anims[asClimbing].next:=asStanding;
  animations.anims[asClimbing].onEnd:=@result.climbAnimationFinish;
  with animations.anims[asClimbing].anims[0] do begin
    duration:=1; repeats:=1; dither:=atLinear; value:=@position[1];
    typ:=integer(eHumanActorAnimations.asClimbing);
  end;
  with animations.anims[asClimbing].anims[1] do begin
    duration:=0.5; repeats:=1; dither:=atLinear; value:=@position[0];
    delay:=animations.anims[asClimbing].anims[0].duration;
    typ:=integer(eHumanActorAnimations.asClimbing);
  end;
  with animations.anims[asClimbing].anims[2] do begin
    //start and finish must be initialized before each play
    duration:=0.5; repeats:=1; dither:=atlinear; value:=@position[2];
    delay:=animations.anims[asClimbing].anims[0].duration;
    typ:=integer(eHumanActorAnimations.asClimbing);
  end;

  //roll animation
  animations.anims[asRollingForward]:=TAnimationSet.create(asRollingForward,4);
  animations.anims[asRollingForward].next:=asStanding;
  animations.anims[asRollingForward].onEnd:=@result.stop;
  //x movement
  with animations.anims[asRollingForward].anims[0] do begin
    duration:=1; repeats:=1; dither:=atLinear; value:=@position[0];
  end;
  //z movement
  with animations.anims[asRollingForward].anims[1] do begin
   repeats:=1; dither:=atLinear; value:=@position[2];
   duration:=animations.anims[asRollingForward].anims[0].duration;
  end;
  //x rotation
  with animations.anims[asRollingForward].anims[2] do begin
    start:=0;finish:=degtorad(360);
    repeats:=1; dither:=atLinear; value:=@rotation[0];
    duration:=animations.anims[asRollingForward].anims[0].duration;
  end;
  //y movement
  with animations.anims[asRollingForward].anims[3] do begin
    repeats:=2; dither:=atLinear; value:=@position[1]; loop:=ltPingPong;
    duration:=animations.anims[asRollingForward].anims[0].duration / 2;
    //delta:=true; start:=0; finish:=height;
  end;
//  on level load finish get actor scale and start animation ?
//  or move matrices from renderable
 //but then I don't really need 'standing' animation on game start
  animations.setState(asStanding);

  maxRunningSpeed:=60;
  maxWalkingSpeed:=35;

  if identity=eKosmo then begin
      maxAcceleration:=0.2;
      canfly:=false;
  end else if identity=eStrach then begin
      maxAcceleration:=0.2;
  end else if identity=eBlueMarine then begin
      maxAcceleration:=0.2;
    end;
  end;

end;

function TPhysicsEngine.indexOf(id: integer): integer;
var i:integer;
    limit:integer;
begin
  result:=-1;
  for i:=0 to actors.count-1 do begin
   if actors[i].actorid=id then begin
      result:=i;
      break;
   end;
  end;
end;


end.

