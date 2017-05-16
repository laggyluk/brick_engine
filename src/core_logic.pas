unit core_logic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, core_types, core_actor, core_classes, VectorGeometry, fgl,
  core_orders_manager, core_behaviour_tree,  core_job, core_chunk,
  astar3d_breadCrumb, VectorTypes, core_player,
  core_game_items, core_orders, core_listofrecords,core_beh_tree_types,
  core_actor_definitions, core_camera,core_terrain_generator, core_pathfinding
  {$IFNDEF SERVER} ,core_render{$ENDIF}
  ;

type

  TActorList = specialize TFPGMap<integer,TActor>;

  { TGameLogic }

  TGameLogic = class(TUpdateable)
   private
    flastActorID:integer;
   public
    skipUpdate:boolean;
    actors:TActorList;
    //ehn adding new actors we need to track their id to not add with same
    //inc by delta every frame
    timeFood:single;
    //inc by 1 every frame
    timeFrame:integer;
    paused:boolean;
    procedure update;override;
    procedure addActor(un:TActor);
    procedure updateTasks;
    //save target actorID in  actr.data.targetActorId
    //and normalized vector towards target in actr.data.v1. target pos in v2
    function findEnemyInSight(actr: TActor): nodeStates;
    function getNextFreeId:integer;
   // function callActorFunc(actorID:integer;const func:string;const params:pFuncParams):nodeStates;
    function callActorFunc(Method_Name: string; const params:pFuncParams;actorID:integer;behNode:pointer): nodeStates;
    procedure saveActorsToFile(filename:string);
    procedure resetBrains;
    //breaks on first matching name and returns id
    function findActorByName(const actorName:string):integer;
    constructor create;
    destructor destroy;override;
  end;

var
  logic:TGameLogic;

implementation

{ TGameLogic }

procedure TGameLogic.update;
var
  i,index:integer;
  cmd:rorder;
  u:TActor;
  searchPath:TPath;
  j:tjob;
  blok:byte;
  obj:pGameObject;
  a:rAnim1f;
begin
  //process orders
  for i:=0 to oM.queue.Count-1 do begin
    cmd:=oM.queue[i];
    if not cmd.eatenFlags.eatenLogic then with cmd do  begin
       eatenFlags.eatenLogic:=true;
       case order of
         orCreateActor: begin
          if options.debug.orders then log('logic unit created id: '+inttostr(int1));
          u:=TActor.create(eActorTypes(int2),int1,@callActorFunc);
          //u.Name:=actor;
          //u.assetID:=int2;
          if int3>-1 then u.faction:=int3;
          setVector(u.birthPosition,v1);
          setVector(u.position,v1);
          u.data.idlePoint:=u.position;
          //u.position[0]:=f1;u.position[1]:=f2;u.position[2]:=f3;
          actors.Add(int1,u);
         end;
         orMoveActor: begin
           index:= actors.IndexOf(int1);
           if index>-1 then begin
              u:=actors[int1];
              setVector(u.position,v1);
              setVector(u.destination,v2);
           end;
         end;
         //orCreateJobArea:;
         orModifyJobArea:begin
            //jobManager.modifyArea(cmd.int1,cmd.v1,cmd.v2);
            //if cmd.int2=0 then jobManager.modifyAreaMin(cmd.int1,cmd.v1);
            //if cmd.int2=1 then jobManager.modifyAreaMax(cmd.int1,cmd.v1);
            //if cmd.int2=2 then jobManager.modifyArea(cmd.int1,cmd.v1,cmd.v2);
          end;
         //job going offline for some reason
         orBreakJob:begin
           if actors.IndexOf(cmd.int1)>-1 then
               actors[int1].reset;
         end;
         //mr ludzik says he started to work. sanity check?
         //orWorkStarted:
         //signalize pathManager that actor needs a path calculated
         orNeedPath:begin
           index:= actors.IndexOf(int1);
           if index>-1 then pathManager.findPath(int1,cmd.v1,cmd.v2,cmd.int2>0,cmd.int3>0);
         end;
         //order from pathManager signals that path was found(or not)
         orPathFound:begin
            searchPath:=pathManager.paths[cmd.int1];
            index:= actors.IndexOf(int1);
            if index>-1 then begin
              u:=actors[int1];
              u.path:=searchPath;
              if searchPath=nil then u.pathState:=psNotFound else
                 u.pathState:=psFound;
              end
            else begin
              searchPath.Free;
              pathManager.paths.remove(int1);
            end;
         end;
         orPathFinished:begin
           u:=actors[cmd.int1];
           u.followingPath:=false;
           u.pathFinished:=true;
           freeandnil(u.path);
           pathManager.paths.Remove(cmd.int1);//?
         end;
         orGamePause:paused:=true;

         orGameUnPause:paused:=false;

         orModifyBlock:begin
           //check if it's legal? actor id is in int1
           if actors.IndexOf(int1)>-1 then begin;
           //one resource more/less less
           if cmd.int2=0 then begin
              blok:=terrain.getBlockInWorld(round(cmd.v1[0]),round(cmd.v1[1]),round(cmd.v1[2]),true)^;
              inc(players[actors[cmd.int1].faction].resources[blok])
           end else
              dec(players[0].resources[cmd.int2]);
           terrain.setBlockAndCull(cmd.v1,eBlockTypes(cmd.int2));//target block type
           end;
         end;
         orFromStashToWorld:begin
           //what about which player item belongs to? probably each order should have this stamped
           //create new item in the world
           //itemsManager.addItem(eItemTypes(cmd.int1));
           //itemsManager.copyItem(player1.stash.getItem(cmd.int2));
           //remove item from players stash
           //player1.stash.removeItem(cmd.int2);
         end;
         orItemInstall:begin
           obj:=itemsManager.getItem(cmd.int1);
           obj^.active:=true;
           if obj^.typ=itStashPortal then player1.stashPortals.Add(cmd.int1,cmd.v1);
         end;
         orDestroyActor:begin
           //make assigned job ownerless.// moved to actor.free
           //j:=jobManager.getJobByID(int1);
           //if j<>nil then j.ownerID:=-1;
           index:=actors.IndexOf(int1);
           if index>-1 then begin
             actors.data[index].Free;
             actors.delete(index);
           end;
         end;
         orToggleBrain:begin
           index:= actors.IndexOf(cmd.int1);
           if index>-1 then actors.data[index].brain.skip:=not boolean(cmd.int2);
         end;
         //actor or player wants to shoot. check weapon and animation state
         orWantShoot:begin
           index:= actors.IndexOf(cmd.int1);
           if index>-1 then begin
              u:=actors.data[index];
              u.weapon.shoot(u.actorID,v1,v2);
              //u.shoot;
           end;
         end;
         orWantReload:begin
           index:= actors.IndexOf(cmd.int1);
           if index>-1 then begin
              //primary, secondary weapon ammo?
              actors.data[index].weapon.reload(actors.data[index].ammo);
           end;
         end;
         orMapLoad:begin
           for index:=0 to actors.Count-1 do actors.data[index].Free;
           actors.Clear;
         end;
         //when game is loaded we need to set lastActorID to biggest existing id +1
         orMapLoaded:begin
           flastActorID:=-1;
           for index:=0 to actors.Count-1 do begin
             if actors.data[index].actorID>flastActorID then flastActorID:=actors.data[index].actorID;
             if (gameMode=false ) and (actors.data[index].actorID <> player1.actorID) then setRemoteBrain(flastActorID);
           end;
           if flastActorID>-1 then inc(flastActorID);
         end;
         orCameraWeaponZoom:begin
           index:= actors.IndexOf(cmd.int1);
           if index>-1 then begin
              if int2=0 then camera.zoom(camera.defaultFov,false)
              else camera.zoom(actors.data[index].weapon.zoomFov,true);
              //log('aaa');
           end;
         end;
         orActorHurt:begin
           index:= actors.IndexOf(cmd.int1);
           if index>-1 then begin
             u:=actors.data[index];
             u.health-=1;
             if u.health=0 then om.addOrder(orDestroyActor,u.actorID,0);
           end;
         end;
         orCreateAutomata:begin
           log('TLogic.Update:: orCreateAutomata not implemented');
         end;
       end;
     end;
    oM.queue[i]:=cmd;
  end;
  timeFood:=timeFood+deltaT;
  //update tasks list
  if not paused then updateTasks;
end;

procedure TGameLogic.addActor(un: TActor);
begin

end;

procedure TGameLogic.updateTasks;
var
  i:integer;
begin
  //powiedzmy ze polecenia ktore gracz wydaje sa traktowane jako zadania
  //zadanie szuka sobie wykonawcy
  inc(timeFrame);
  //one actor logic updated per frame might be a good idea. or not.
  if (not skipUpdate) and (actors.Count>0) then begin
      for i:=0 to actors.count-1 do actors.data[i].update;
      //update one brain per frame
      actors.data[timeFrame mod actors.Count].brain.update;
  end;
//  if timeFrame mod 30 = 0 then jobManager.update;
  pathManager.update;
  itemsManager.update;
end;

function TGameLogic.findEnemyInSight(actr:TActor): nodeStates;
var
  enemy,temp:tactor;
  ch:tVector2i;
  i:integer;
  rayStart,rayVector,offset,blok:TAffineVector;
  distance,closest:single;
begin
  result:=nsFailed;
  enemy:=nil;
  closest:=MaxSingle;
  //should be casted from head position to top and bottom of potential target?
  for i:=0 to actors.count-1 do begin
    temp:=actors.Data[i];
    if temp=actr then continue;
    //check only if in maximum firing/eye range?
    if not (temp.faction in getEnemies(actr.faction)) then continue;
    distance:=VectorDistance(actr.position,temp.position);
    //save position of nearest enemy anyways. in v2
    if distance<closest then
        actr.data.v2:=temp.position;
    if distance> actr.eyeRange then continue;
    //if terrain collision on the line then continue
    setvector(rayStart,actr.barrelPos);
    //wezmy pod uwage ze chunki sa numerowane od 0,0 a tu sreodek jest w 0,0
    offset:=vector3fMake(world_width2,0,world_width2);
    addvector(rayStart,offset);
    setVector(rayVector,temp.barrelPos);
    addvector(rayVector,offset);

    //v2[1]+=chunk_height;
    if rayBlocksIntersect(round(rayStart[0]),round(rayStart[1]) ,round(rayStart[2]),
                       round(rayVector[0]),round(rayVector[1]),round(rayVector[2]),
                       ch,blok,round(distance),false)
      then begin
        continue;
      end;
    enemy:=temp;
    //if here then it's enemy in sight and in range.
    //could also iterate all to get closest one
  end;
  if enemy<>nil then begin
    actr.data.targetActorId:=enemy.actorID;
    actr.data.v1:=VectorNormalize(VectorSubtract(enemy.position,actr.position));
    actr.data.v2:=enemy.barrelPos;//head?
    //log('enemy id: '+inttostr( actr.data.targetActorId));
    result:=nsSuccess;
  end else result:=nsFailed;
end;

function TGameLogic.getNextFreeId: integer;
begin
  inc(flastActorID);
  result:=flastActorID;
end;

function TGameLogic.callActorFunc(Method_Name: string;
  const params: pFuncParams; actorID: integer; behNode: pointer): nodeStates;
var actr:tactor;
begin
  result:=nsSuccess;
  actr:=actors[actorID];
  if actr.lastFunc<>Method_Name then begin
     actr.resetFuncStep;
     actr.lastFunc:=Method_Name;
  end;
  case Method_Name of
    //szuka sobie zajecia sposrod dostepnych
    'findJob': begin
                //sets actor's job to nil if not found
               result:=actr.searchForJob;
               end;
    //szwenda sie kolo punktu w v1
    //param 0 - max distance
    'idleAroundPoint': begin
            actr.params[0]:=(params^[0]);
            result:=actr.idleAroundPoint;
           end;
    'saveIdlePoint':begin
            actr.data.idlePoint:= actr.position;
            end;
    //param 0 min time
    //param 1 max time. result is randomized in that range
    'wait':begin
            actr.params[0]:=(params^[0]);
            actr.params[1]:=(params^[1]);
            result:=actr.wait;
           end;
    'findBlockInArea':result:=actr.findBlockInArea;
    'gotoPointV1':begin
            result:=actr.gotoPointV1;
           end;
    'dig':begin
            result:=actr.dig;
          end;
    'build':result:=actr.build;
    'fail':begin
        result:=nsFailed;
        end;
    'followPath':result:=actr.followPath;
    'shoot':result:=actr.shootTarget;
    //find a target within actors sight and return id
    'findEnemyInSight':result:=findEnemyInSight(actr);
    //actor can do different jobs, check if particular tree branch fits the job
    'doesTreeSolveJob':begin
          actr.params[0]:=(params^[0]);
          result:=actr.doesTreeFitJob;
        end;
    'pickUpItem':result:=actr.pickUpItem;
    'placeItem':result:=actr.placeItem;
    'findPathToEnemy':result:=actr.findPathToEnemy;
    'melee':result:=actr.melee;
    'gotTarget':result:=actr.gotTarget;
    {teraz ten maly skurwysynek musi obsluzyc noszenie gowien
    wziasc pozycje itemu z stashu playera, pozycje najblizszego stash portalu
    i wybrac blizsze.
    przy ukonczeniu 'budowy' stash portalu niechze go dodaje do isty portali playera}
    else log('achtung! gameLogic.callActorFunc:: missing function: '+Method_Name);
    end;
end;

procedure TGameLogic.saveActorsToFile(filename: string);
var
  i:integer;
  str:tstringlist;
begin
  str:=TStringList.create;
  filename:=ChangeFileExt(filename,'.actors');
  for i:=0 to actors.count-1 do actors.data[i].saveToLines(str);
  if str.count>0 then str.Delete(str.count-1);
  str.SaveToFile(filename);
  str.free;
end;

procedure TGameLogic.resetBrains;
var i:integer;
begin
  for i:=0 to actors.Count-1 do actors.data[i].reset;
end;

function TGameLogic.findActorByName(const actorName: string): integer;
var i:integer;
begin
  result:=-1;
  for i:=0 to actors.count-1 do if actors[i].name=actorName then begin
    result:=i;
    break;
  end;
end;

constructor TGameLogic.create;
begin
  flastActorID:=-1;
  actors:=TActorList.Create;
//  jobManager:=TJobManager.create;
  skipUpdate:=true;
end;

destructor TGameLogic.destroy;
var
  i:integer;
begin
//  jobManager.Destroy;
  for i:=0 to actors.count-1 do actors.data[i].Destroy;
  actors.Destroy;
//  for i:=0 to tasks.count-1 do tasks.Destroy;
//  tasks.Destroy;
end;

end.

