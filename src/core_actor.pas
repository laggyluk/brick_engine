unit core_actor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, core_types, VectorGeometry, core_orders_manager,
  core_behaviour_tree, core_job, core_utils, math, typinfo, core_actor_definitions,
  astar3d_breadCrumb, core_weapons, core_orders, core_beh_tree_types;

const maxFood = 100;


type

  //will serve as asset id
  unitTypes = (
   utPod,
   utKosmonauta
   );

  ePathStates = (psNotAssigned,psNotFound,psFound,psSearching);

  { TActor }

  TActor = class
  private
    fCanFly: boolean;
    fpathState: ePathStates;
    lastUpdateT:extended;
    inaccessibleJobs:TListOfIntegers;
    inaccessibleJobsTimer:single;
    deltaTime:single;//deltaT is not reliable as not every actor is updated every frame
    fBarrelPos: TAffineVector;
    function getBarrelPos: TAffineVector;
    function getCanFly: boolean;
    procedure setPathState(AValue: ePathStates);
  protected
    factorID: integer;
    //sort of state for use inside actor functions. can be iterated to reflect
    //current stage in function progress. zeroed when called function name differs
    //from one called before
    FuncStep:integer;
    procedure setActorID(AValue: integer);
  public
    health:word;
    healthMax:integer;
    //some stats
    data: record
      str:byte;//strength
      int:byte;//inteligence
      dex:byte;//dexterity
      sta:byte;//stamina
      wis:byte;//wisdom
      con:byte;//constitution
      level:byte;//character level
      exp:integer;//experience
      //actor is capable of performing some tasks. those sets should be updated
      //on load and on level up
      jobsAllowed:jobSet;
      jobsPreffered:jobSet;
      job:tjob;
      //fuel/food
      fuelLeft:integer;
      fuelMax:integer;

      //some vars for internal use
      v1,v2,v3:TAffineVector;
      targetActorId:integer;
      idlePoint:TAffineVector;
    end;
    timers: record
      t1,t2,t3,t4,t5:single;
      t1Max,t2Max,t3Max,t4Max,t5Max:single;
    end;
    //this is used when sending sound orders. kosmonauta/krowa/alien
    sound:record
      noteMin:integer;
      noteMax:integer;
    end;
    isPlayer:boolean;
    //let's say that actor can belong to player/ evil force/ other multi player
    faction:byte;
    followingPath,pathFinished:boolean;
    //max sensor range (for detecting enemies)
    eyeRange:integer;
    //wzrost. could be obtained from atlas. na sztywno narazie
    height:integer;
    path:TPath;
    lastFunc:string;
    //params passed to func from beh node
    params:array[0..9] of string;
    //index of assigned atlas subtexture
    assetID:integer;
    position,
    destination,
    //next nav point after current
    destinationNext:TAffineVector;
    birthPosition:TAffineVector;
    //asset name for internal use
    Name:string;
    //root of actor's behaviour tree
    brain:TBehTreeNode;
    weapon:TWeapon;
    //total ammo player is carrying
    ammo:integer;
    common:rActorDefinition;
    //randomize some point near stored position. param0 says if pos is in v1 or v2
    //and param1 is max distance from point to use for randomization
    function idleAroundPoint: nodeStates;
    //when actor learns new skill or sth, node for that skill should be added to the tree
    procedure AddJobSkill(jobType:eJobTypes);
    //action do nothing in random interval from min to max
    function wait: nodeStates;
    //action find block in current job. type of block defined by job
    function findBlockInArea: nodeStates;
    //travel to point. param0 says which point to use (V1,V2..)
    function gotoPointV1: nodeStates;
    function doesTreeFitJob:nodeStates;
    //dig the block. param0 stores if v1/v2 to use
    function dig:nodeStates;
    //work on building a block
    function build: nodeStates;
    //work on moving an item from one place to another. probably from stash portal
    function pickUpItem: nodeStates;
    function placeItem: nodeStates;
    //wait for fob from job manager
    function searchForJob:nodeStates;
    //follow path in data.path
    function followPath:nodestates;
    //find path to nearest player/enemy. enemy id is in data.targetActorId
    function findPathToEnemy:nodestates;
    function shootTarget:nodeStates;
    function melee:nodestates;
    //check if targetActorId>-1
    function gotTarget:nodeStates;
    //basing on the parm string it saves v vector to v1/v2..
    procedure saveDataInVector(const parm: string; v: TAffineVector);
    procedure update;
    //sets funcStep to 0
    procedure resetFuncStep;
    //increases func step
    procedure nextFuncStep;
    //when actor needs a path to some place obtained, use this to flag the path search state
    property pathState:ePathStates read fpathState write setPathState;
    function jobIsInnacessible(jobID:integer):boolean;
    property canFly:boolean read getCanFly write fCanFly;
    property actorID:integer read factorID write setActorID;
    //save to txt for a savegame
    procedure saveToLines(str:TStringList);
    //reset brain and state
    procedure reset;
    //from this point bullets fly
    property barrelPos:TAffineVector read getBarrelPos write fBarrelPos;
    constructor create(const identity: eActorTypes; ID: integer;
      _actionCallback: TBehFuncType);
    destructor destroy;override;
  end;
  pActor = ^TActor;

implementation
//uses core_job_manager;

{ TActor }

procedure TActor.setPathState(AValue: ePathStates);
begin
  if fpathState=AValue then Exit;
  fpathState:=AValue;
end;

function TActor.getBarrelPos: TAffineVector;
begin
  result:=position;
  //units height or sth?
  result:=VectorAdd(position,fBarrelPos);
end;


function TActor.getCanFly: boolean;
begin
  //maybe can't fly if out of fuel?
  result:=fCanFly;
end;

procedure TActor.setActorID(AValue: integer);
begin
  //if factorID=AValue then Exit;
  factorID:=AValue;
  brain.actorID:=factorid;
end;

function TActor.idleAroundPoint: nodeStates;
var x,y,z:integer;
   //max distance to randomize in
   maxDist,loop:integer;
begin
  //'anchor' point should be already in v1/v2 before calling this func
  result:=nsRunning;
  maxDist:=strtoint(params[0]);
  loop:=0;
  case FuncStep of
  0:begin
      { TODO -cpotential bug : this is crap. could fall from ledge or sth. needs pathfinding  }
    while loop<15 do begin
      z:=round(data.idlePoint[2])-maxDist+random(maxDist*2);
      x:=round(data.idlePoint[0])-maxDist+random(maxDist*2);
      clamp(x,-world_width2,world_width2);
      clamp(z,-world_depth2,world_depth2);
      for y:=round(data.idlePoint[1]-maxDist) to round(data.idlePoint[1]+maxDist) do
       begin
         if getBlockTypeAt(vector3fmake(x,y,z))=btNone then
         begin
           data.v2:=vector3fmake(x,position[1],z);
          //.data.v1:=vector3fmake(0,y-0.5,0);
           oM.addOrder(orWantMove,actorID,data.v2);
           nextFuncStep;
           timers.t1:=0;
           exit;
         end;
       end;
      inc(loop);
    end;
   end;
  //and what if he can't get there for some reason?
   1:begin
      if VectorDistance(position,data.v2)<destProximity then result:=nsSuccess
      else begin
       //if position didnt change for few frames then we are blocked and need new target
        if  timers.t1>5 then result:=nsSuccess;
        if VectorEquals(position,data.v1) then timers.t1+=1
        else begin
          data.v1:=position;
          timers.t1:=0;
        end;
      end;
   end;
  end;

end;

function TActor.wait: nodeStates;
begin
  //first hit. setup
  result:=nsRunning;
  case FuncStep of
   0:begin
     timers.t2Max:=randomrange(strtoint(params[0]),strtoint(params[0]));
     timers.t2:=0;
     nextFuncStep;
     exit;
     end;
   1:begin
     //time flies
     timers.t2+=deltaTime;
     if timers.t2<timers.t2Max then exit;
     //time passed
     timers.t1:=0;
     result:=nsSuccess;
   end;
  end
end;

function firstBlockInBox(selMin,selMax:TAffineVector;ofSet: sMinerals;var resultPos:TAffineVector):boolean;
var
  v:TAffineVector;
  h,x,y,z,mmin,mmax,xx,zz:integer;
  blok:eBlockTypes;
begin
  result:=false;
  mmax:=round(max(selMin[1],selMax[1]));
  mmin:=round(min(selMin[1],selMax[1]));

  for y:=mmin to mmax do
   for x:=round(selMin[0]) to round(selMax[0]) do
    for z:=round(selMin[2]) to round(selMax[2]) do
        begin
          blok:=getBlockTypeAt(vector3fmake(x,y,z));
          if blok=btNone then continue;
          if blok in ofSet then begin
             //set the block position in v1
             result:=true;
             resultPos:=vector3fmake(x,y,z);
             //nextFuncStep;
             break;
          end;
        end;
end;

//doesn't work, dont use
function TActor.findBlockInArea: nodeStates;
var
  v:TAffineVector;
  h,x,y,z:integer;
begin
  result:=nsFailed;
  case eJobTypes(data.job.typ) of
    //jtDig uses an area and actor searches for a diggable block within
    jtDig:begin
      //any block <> btNone, btWater, lava or sth?
      if firstBlockInBox(data.job.v1,data.job.v2,mineralsDiggable,data.v1) then
         result:=nsSuccess  else
         begin
           //no more blocks in this job area
           data.job.complete; //invalid?
           data.job:=nil;
           result:=nsFailed;
         end;
      //oM.addOrder(orWantMove,actorID,data.v1);
    end;
    //jtBuild uses single block jobs
    jtBuild:begin
      //data.v1:=data.job.v1;
      //result:=nsSuccess;
    end;
  end;

end;

function TActor.gotoPointV1: nodeStates;
begin
  result:=nsRunning;
  case FuncStep of
  0:begin
       data.v1[1]+=0.5;
       oM.addOrder(orWantMove,actorID,integer(false),data.v1);
       nextFuncStep;
    end;
  1:if  VectorDistance(position,data.v1)<destProximity then  begin
        result:=nsSuccess;
    end;
  //and what if he can't get there for some reason?
  end;
end;

function TActor.doesTreeFitJob: nodeStates;
begin
  if data.job=nil then result:=nsFailed else
  if eJobTypes(GetEnumValue(TypeInfo(eJobTypes),params[0]))= data.job.typ then
     result:=nsSuccess else
        result:=nsFailed;
end;

function TActor.build: nodeStates;
var
  //blok:eBlockTypes;
  v:TAffineVector;
begin
  result:=nsRunning;
  case FuncStep of
  0:begin
       //data.v1[1]+=0.5;
       timers.t1Max:=getRequiredJobTime(jtBuild);
       timers.t1:=0;
       oM.addOrder2i(orWorkStarted,actorID,integer(jtBuild));
       nextFuncStep;
    end;
  1:begin
      timers.t1+=deltaTime;
      if timers.t1>timers.t1Max then begin
          data.job.complete;
//         a huja tam complete. jedna cegielka mniej dopier
         //moze send order jakis ze cegla znikla czy cos bo ta funkcja jest zjebana
         //i jeszcze culling musi byc
          //blok:=getBlockTypeAt(data.job.v1);
          v:=position;
          oM.addOrder2i(orWorkComplete,actorId,data.job.id);
          oM.addOrder(orModifyBlock,actorID,integer(data.job.i1),data.job.v1);
          //terrain.setBlockAndCull(data.job.v1,eBlockTypes(data.job.i1));//target block type
          data.job:=nil;
          //for idling around here rather than last idle point
          data.idlePoint:=position;
          result:=nsSuccess;
         end;
      end;
    end;
end;

function TActor.pickUpItem: nodeStates;
var
  blok:eBlockTypes;
  stashPosition:TAffineVector;
  o:rorder;
  i:integer;
begin
  result:=nsRunning;
  case FuncStep of
  0:begin
       //wait a bit before picking up the item
       timers.t1Max:=getRequiredJobTime(jtPickUpItem);
       timers.t1:=0;
       //oM.addOrder2i(orWorkStarted,actorID,integer(jtDig));
       nextFuncStep;
    end;
  1:begin
      timers.t1+=deltaTime;
      //pickup the item and continue
      if timers.t1>timers.t1Max then begin
         //find nearest stash portal position
         stashposition:=data.job.v2;
         //send request for new path to target
         pathState:=psSearching;
         oM.addOrder(orNeedPath,actorID,integer(canFly),1,position,stashPosition);
         nextFuncStep;
      end;
    end;
  2:begin
      if pathState=psNotFound then begin
         result:=nsFailed;
         //free job ownership since path to it doesn't exist
         inaccessibleJobs.add(data.job.ID);
         data.job.ownerID:=-1;
         //drop item or return to stash
      end;
      //path was found and is assigned to this actor's 'path'
      if pathState=psFound then begin
         result:=nsSuccess;
         //remove item from stash and add to itemsManager as inactive?
         //item type, itemID, jobID, ACTOR ID, position
        // niech w int4 bedzie actor id dzieki czemu renderer bedzie wiedzial ko niesie cegle
         i:=oM.addOrder;
         o:=oM.queue[i];
         o.order:=orFromStashToWorld;
         o.int1:=data.job.i1;
         o.int2:=data.job.i2;
         o.int3:=data.job.id;
         o.int4:=actorID;
         o.v1:=position;
         om.queue[i]:=o;
      end;
    end;
  end;
end;

function TActor.placeItem: nodeStates;
begin
  //go to stash, grab item, go to mount point
  result:=nsRunning;
  case FuncStep of
  0:begin
       //there's no separate job for 'installing' item so lets just extend current job time
       timers.t1Max:=getRequiredJobTime(jtPickUpItem)*2;
       timers.t1:=0;
       //oM.addOrder2i(orWorkStarted,actorID,integer(jtDig));
       nextFuncStep;
    end;
  1:begin
      timers.t1+=deltaTime;
      //pickup the item and continue
      if timers.t1>timers.t1Max then begin
        //activate item
        oM.addOrder(orItemInstall,data.job.i2,actorID,data.job.ID,data.job.v2);
        data.job.complete;
        data.job:=nil;
        result:=nsSuccess;
      end;
    end;
  end;
end;

function TActor.dig: nodeStates;
var
  blok:eBlockTypes;
  v:TAffineVector;
begin
  result:=nsRunning;
  case FuncStep of
  0:begin
       //data.v1[1]+=0.5;
       timers.t1Max:=getRequiredJobTime(jtDig);
       timers.t1:=0;
       oM.addOrder2i(orWorkStarted,actorID,integer(jtDig));
       nextFuncStep;
    end;
  1:begin
      timers.t1+=deltaTime;
      if timers.t1>timers.t1Max then begin
        data.job.complete;
//         a huja tam complete. jedna cegielka mniej dopier
        //moze send order jakis ze cegla znikla czy cos bo ta funkcja jest zjebana
        //i jeszcze culling musi byc
         blok:=btNone;
         v:=position;
         oM.addOrder2i(orWorkComplete,actorId,data.job.id);
         oM.addOrder(orModifyBlock,actorID,integer(data.job.i1),data.job.v1);
         //terrain.setBlockAndCull(data.job.v1,eBlockTypes(data.job.i1));//target block type
         data.job:=nil;
         //for idling around here rather than last idle point
         data.idlePoint:=position;
         result:=nsSuccess;
         end;
      end;
    end;
end;

function TActor.searchForJob: nodeStates;
begin
  result:=nsRunning;
  //order with request was sent to job manager, now wait for results
  case FuncStep of
  0:begin
      //jobManager.findJob(self);
      //no suitable job found, abort
      if data.job=nil then result:=nsFailed;
      //job found, wait for path calculation
      nextFuncStep;
    end;
  1:begin
      //path to job can't be obtained
      //add job to skip list and search for another? should list be private?
      if pathState=psNotFound then begin
         result:=nsFailed;
         //free job ownership since path to it doesn't exist
         inaccessibleJobs.add(data.job.ID);
         data.job.ownerID:=-1;
      end;
      //path was found and is assigned to this actor's 'path'
      if pathState=psFound then result:=nsSuccess;
    end;
  end;
end;

function TActor.followPath: nodestates;
var v:TAffineVector;
    distanceXZ:single;
    i:integer;
begin
  //moze niech kazdy navpoint zwieksza func step?
  result:=nsRunning;
  //jezeli nie ma patha to fail. musi sie jeszcze nilowac gdzies
  if path=nil then begin
     result:=nsFailed;
     exit;
  end;
  if followingPath =false then begin
     //aktorze, zostaw path following fizyce
     //move to the first nav point
     v:=toUnitPos(path.position);
     destination:=v;
     //data.v1:=v;
     oM.addOrder(orWantMove,actorID,integer(true),position);
     //nextFuncStep;
     followingPath:=true;
     pathFinished:=false;
  end;
  if (followingPath) and not (pathFinished) then begin
     //continue following the path
     v:=VectorSubtract(destination,position);
     v[1]:=0;
     distanceXZ:=VectorLength(v);
     if distanceXZ<0.5 then begin
       if path.hasNext then begin
          path.getnext;
          destination:=toUnitPos(path.position);
          //log('next nav point');
          if path.hasNext then begin
             path.getnext;
             destinationNext:=toUnitPos(path.position);
          end else destinationNext:=destination;

          v:=VectorSubtract(destination,position);
          v[1]:=0;
       end else begin
         //path:=nil;//?
         followingPath:=false;
         log('path end');
         { TODO -cfix : what about path that is not gonna be ended cause of interrupt by enemy? }
         //this causes a sudden stop
         destination:=position;
         oM.addOrder2i(orPathFinished,actorID,0);
       end;
     end;
     //swap axes
     v[1]:=v[2];
     //run or walk?
     i:=0;
     //i:=bitset(i,1);
     oM.addOrder(orControllerState,actorID,i,0,v,destinationNext)
  end;
  if pathFinished then begin
     result:=nsSuccess;
     followingPath:=false;
     pathFinished:=false;
  end;
end;

function TActor.findPathToEnemy: nodestates;
begin
  result:=nsRunning;
  case fpathState of
    psNotAssigned: begin
      om.addOrder(orNeedPath,actorID,0,0,position,data.v2);
      pathState:=psSearching;
     end;
    psNotFound:result:=nsFailed;
    psFound:result:=nsSuccess;

  end;
  {
  //if pathState=psSearching;
  //order with request was sent to job manager, now wait for results
  case FuncStep of
  0:begin
      //jobManager.findJob(self);
      //no suitable job found, abort
      if data.job=nil then result:=nsFailed;
      //job found, wait for path calculation
      nextFuncStep;
    end;
  1:begin
      //path to job can't be obtained
      //add job to skip list and search for another? should list be private?
      if pathState=psNotFound then begin
         result:=nsFailed;
         //free job ownership since path to it doesn't exist
         inaccessibleJobs.add(data.job.ID);
         data.job.ownerID:=-1;
      end;
      //path was found and is assigned to this actor's 'path'
      if pathState=psFound then result:=nsSuccess;
    end;
  end;        }
end;

function TActor.shootTarget: nodeStates;
var
  //v:TAffinevector;
    ang:single;
begin
  //target id should be in data.target
  //calculate vector
  weapon.shoot(actorID,barrelPos,data.v2);
{  oM.addOrder(orBlasterShot,getNextBulletID,actorID,0,barrelPos,//barrel height from ground
                vectorScale(data.v1,44),//velocity
                weapon.color);//color}
  //face the direction of shootin. from position and target vectors
  //v:=vectorSubtract(position-data.v2);
  //patzr sie na cel
  ang:=arctan2(data.v2[2] - position[2], data.v2[0] - position[0]);
  ang:=pi_half - ang;
  //set rotation or rotate?
  om.addOrder(orRotateActorY,actorID,0,ang,0,0);
  result:=nsSuccess;
  if weapon.state=wsEmpty then weapon.reload(ammo);
end;

function TActor.melee: nodestates;
begin
  //not implemented yet
  result:=nsFailed;
end;

function TActor.gotTarget: nodeStates;
begin
  if data.targetActorId>-1 then result:=nsSuccess else result:=nsFailed;
end;

procedure TActor.saveDataInVector(const parm: string;v:TAffineVector);
begin
  if parm='v1' then data.v1:=v else
     if parm='v2' then data.v2:=v;
end;

procedure TActor.update;
begin
  //if wants to move than should send a 'wish' command not direct move command
  //oM.addOrder(orMoveActor,actorID,vectoradd(position,vector3fmake(0.01,0,0.01)));
  deltaTime:=totalTime-lastUpdateT;
  //if controlledActorID<>actorID then
  //moved to logic loop as one brain is updated per frame
  //brain.update;
  if weapon<>nil then weapon.update;
  lastUpdateT:=totalTime;
  //clear innacessible jobs list, maybe some can be reached now
  if inaccessibleJobsTimer>60 then begin
    //maybe should be also resseted when new innacessible job is added?
    inaccessibleJobsTimer:=0;
    inaccessibleJobs.clear;
  end else inaccessibleJobsTimer+=deltaTime;
end;

procedure TActor.resetFuncStep;
begin
  FuncStep:=0;
end;

procedure TActor.nextFuncStep;
begin
  inc(FuncStep);
end;

function TActor.jobIsInnacessible(jobID: integer): boolean;
var i:integer;
begin
  result:=false;
  for i:=0 to inaccessibleJobs.count-1 do if jobID=inaccessibleJobs[i] then begin
    result:=true;
    break;
  end;
end;

procedure TActor.saveToLines(str: TStringList);
begin
  str.add('[actor]');
  //asset name must be first as tactor.create(assetName)
  str.Add('assetName='+common.assetName);
  str.Add('actorID='+inttostr(actorID));
  str.Add('name='+name);
  //do i need that?
  //str.add('str='+inttostr(data.str));
  //str.add('int='+inttostr(data.int));
  str.add('posX='+floattostr(position[0]));
  str.add('posY='+floattostr(position[1]));
  str.add('posZ='+floattostr(position[2]));
  str.add('faction='+inttostr(faction));
  str.add('[/actor]');
  str.add('');
end;

procedure TActor.reset;
begin
  brain.reset;
  data.job:=nil;
end;

procedure TActor.AddJobSkill(jobType: eJobTypes);
var
   filename,skillName:string;
begin
  skillName:=EnumToStrIng(typeinfo(eJobTypes),integer(jobType));
{  filename:=appPath+'aiTrees'+DirectorySeparator+skillName;
  if not fileexists(filename) then begin
     log('TActor::AddJobSkill() script not found: '+filename);
     exit;
  end;
  //first try adding the ai. script to the tree and then add job to alloweg list
  if brain.AddTreeNode('jobTree',filename,noLast) then begin}
       include(data.jobsAllowed,jobType);
       log('ActorID:'+inttostr(actorID)+' '+name+' learned '+skillName);
  //end;
end;

constructor TActor.create(const identity: eActorTypes; ID: integer;_actionCallback:TBehFuncType);
var
  desire,node:TBehTreeNode;
begin
  //physical attributes like maxSpeed are in TFlatPhysicsUnit in physics!
  brain:=TBehTreeNode.create(ntPriority,_actionCallback);
  inaccessibleJobs:=TListOfIntegers.create;
  data.targetActorId:=-1;
  common:=getActorCommonProperties(identity);
  //case
  //brain.load(appPath+'aiTrees\default.ai');//right..
  data.str:=10;
  data.int:=10;
  data.con:=10;
  data.dex:=10;
  data.sta:=10;
  data.exp:=0;
  data.wis:=10;
  eyeRange:=30;
  height:=4;
  health:=5;
  sound.noteMin:=80;
  sound.noteMax:=95;
  //pozycja dziury wylotowej dla kul
  setVector(fBarrelPos,0,height*0.55,0);
  canFly:=false;
  ammo:=99999;
  case identity of
    eKosmo:begin
      faction:=0;
      eyeRange:=30;
      //AddJobSkill(jtDig);
      //AddJobSkill(jtBuild);
      //AddJobSkill(jtPickUpItem);
      weapon:=tweapon.create(wtBlasterPistol,actorid);
      ammo:=128;
    end;
    eStrach:begin
      faction:=0;
      eyeRange:=30;
      weapon:=tweapon.create(wtBlasterPistol,actorid);
      ammo:=128;
    end;
    eBlueMarine:begin
      faction:=1;
      eyeRange:=40;
      weapon:=tweapon.create(wtEnemyBlasterPistol,actorID);
      brain.load(appPath+'aiTrees'+directoryseparator+'vader.ai');
    end;
  end;
  actorid:=id;
end;

destructor TActor.destroy;
begin
  if weapon<>nil then weapon.free;
//  if data.job<>nil then data.job.Free;
  brain.free;
  inaccessibleJobs.free;
  inherited;
end;

end.

