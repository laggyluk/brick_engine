unit core_pathfinding;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, VectorGeometry,core_types, core_chunk,
  graphics, VectorTypes,astar3d_pathFinder,
  astar3d_point3d, astar3d_breadCrumb,core_listofrecords,
  core_orders_manager,core_orders;

  //right
{concept: unit requires a path to target. path manager calculates route and
  stores result for next call in a map. }

type



  //maps actor indexes to path search result
  TPathMap = specialize TFPGMap<integer,TPath>;
  TNeededPaths = specialize TFPGMap<integer,TStartFinish>;

  { TPathManager }

  TPathManager = class
   private
    { TSearchThread }
    type
    TSearchThread = class (TThread)
        paused:boolean;
        procedure Execute; override;
        function findPathStar3D(ab: TStartFinish): TPath;
        Constructor Create(CreateSuspended : boolean);
    end;
   protected
    thread:TSearchThread;
    neededPaths:TNeededPaths;

    function findPathJPS(A,B:TAffineVector;const filename:string):boolean;
   public
    paths:TPathMap;
    function getNextPoint(actorID:integer):TAffineVector;
    function findPath(actorID: integer; start, finish: TAffineVector;
      flyngAlilowed, iisJobSearch: boolean): TAffineVector;
    //i guess when actor is destroyed his path may be freed
    procedure freePath(actorID:integer);
    //save map slice to a bitmap
    procedure saveSlice(yLevel:integer);
    procedure update;
    constructor create;
    destructor destroy;
  end;
var
  pathManager:TPathManager;

implementation
var
  currentSearch:record
   ab:TStartFinish;
   actorID:integer;
   finished:boolean;
  end;
  pathFinderStar:TPathFinder;
  foundPaths:TPathMap;

{ TPathManager.TSearchThread }

procedure TPathManager.TSearchThread.Execute;
begin
  while not Terminated do begin
    if paused then begin
              sleep(100);
              continue;
    end;
    //first shoult try and find straight line?
    foundPaths.Add(currentSearch.actorID,findPathStar3D(currentSearch.ab));
    currentSearch.finished:=true;
    paused:=true;
  end;
end;

function TPathManager.TSearchThread.findPathStar3D(ab: TStartFinish):TPath;
var
    pos:TVector3i;
    path:TSearchNode;
    a,b:tpoint3d;
    listLength:integer;
begin

    a:=vector3imake(
                          ab.start[0]+world_width2,
                          ab.start[1],
                          ab.start[2]+world_depth2);
    b:=vector3imake(
                       ab.finish[0]+world_width2,
                       ab.finish[1],
                       ab.finish[2]+world_depth2);
    path:=pathFinderStar.FindPathList(a,b,ab.flyngAllowed,ab.isJobSearch,listLength);
    result:=nil;
    if path<>nil then begin
       result:=TPath.create(path,listLength);
       //result.free;
    end;
   //debug draw
 {
    if path<>nil then
    while (path.fnext <> nil) do
    begin
      //oM.addOrder(orCreateJobArea,jtDig,random(999999),);
      //log(('Route: ' + path.next.position.ToString));
      pos:=(path.fnext.position);
      //pos[1]:=chunk_height-pos[1];
      setBlockAt(pos,btOxy9,true);
      path := path.fnext;
    end;
  }

end;

constructor TPathManager.TSearchThread.Create(CreateSuspended: boolean);
begin
  inherited create(CreateSuspended);
  FreeOnTerminate:=false;
  paused:=true;
end;


{ TPathManager }

function TPathManager.getNextPoint(actorID: integer): TAffineVector;
begin
  //if by accident path doesn't exist then add
end;

function TPathManager.findPath(actorID: integer; start, finish: TAffineVector;
  flyngAlilowed, iisJobSearch: boolean): TAffineVector;
var p,b:TStartFinish;
begin
  //if path request already exists then swap
  if neededPaths.IndexOf(actorID)>-1 then begin
     p.fromVectors(start,finish);
     neededPaths[actorID]:=p;
  end else begin
     b:=p.fromVectors(start,finish);
     b.flyngAllowed:=flyngAlilowed;
     b.isJobSearch:=iisJobSearch;
     neededPaths.Add(actorID,b);
  end;
  //thread.paused:=false;
end;

function TPathManager.findPathJPS(A, B: TAffineVector; const filename: string
  ): boolean;
begin

  result:=true;
end;

procedure TPathManager.freePath(actorID: integer);
begin
  { TODO -cwazne : niechaj path sie usuwa z pathmanagera kiedy aktor ginie }
  paths[actorID].free;
  paths.Remove(actorID);
end;

procedure TPathManager.saveSlice(yLevel: integer);
var
  i,j,x,y,z,w,h:integer;
  groundBlok,midBlok,topBlok:eblockTypes;
  bmp:tbitmap;
  col:tcolor;
begin
  bmp:=tbitmap.create;
  bmp.SetSize(world_width,world_depth);
  for x:=0 to world_width-1 do
    for z:=0 to world_depth-1 do begin
      col:=clGreen;
      groundBlok:=eblockTypes(chunksPointer^[x div chunk_width,z div chunk_width].blocks[
        x mod chunk_width,ylevel,z mod chunk_width]);
      midBlok:=eblockTypes(chunksPointer^[x div chunk_width,z div chunk_width].blocks[
        x mod chunk_width,ylevel+2,z mod chunk_width]);
      topBlok:=eblockTypes(chunksPointer^[x div chunk_width,z div chunk_width].blocks[
        x mod chunk_width,ylevel+3,z mod chunk_width]);
      if (groundBlok<>btNone) and (midBlok<>btNone) then col:=clRed;
      bmp.canvas.Pixels[x,z]:=col;
    end;
  bmp.SaveToFile(appPath+'slice.bmp');
  bmp.free;
end;

procedure TPathManager.update;
var ind:integer;
begin
  //path search thread finished processing
  if currentSearch.finished then begin
     //move path from found list to public list
     //ind:=-1;
     if paths.IndexOf(currentSearch.actorID)>-1 then begin
        paths[currentSearch.actorID].free;
        paths.remove(currentSearch.actorID);
        //paths[currentSearch.actorID].free;
        //paths[currentSearch.actorID]:=foundPaths[currentSearch.actorID];
        paths.Add(currentSearch.actorID,foundPaths[currentSearch.actorID]);

     end else
         ind:=paths.Add(currentSearch.actorID,foundPaths[currentSearch.actorID]);
     foundPaths.Remove(currentSearch.actorID);
     currentSearch.finished:= false;
     oM.addOrder2i(orPathFound,currentSearch.actorID,ind);
     thread.paused:=true;
  end else
  if (thread.paused) and (neededPaths.count>0) and (not currentSearch.finished) then begin
     currentSearch.ab:=neededPaths.Data[0];
     currentSearch.actorID:=neededPaths.Keys[0];
     neededPaths.Delete(0);
     thread.paused:=false;
  end;
end;

constructor TPathManager.create;
begin
  paths:=TPathMap.Create;
  paths.Duplicates:=TDuplicates.dupError;
  neededPaths:=TNeededPaths.create;
  foundPaths:=TPathMap.Create;
  pathFinderStar:=TPathFinder.create;
  thread:=tsearchthread.Create(false);
end;

destructor TPathManager.destroy;
var
  i:integer;
  p,n:tsearchnode;
begin
  //will self terminate after search is done so wait for it
  thread.paused:=true;
  thread.Terminate;
  thread.WaitFor;
  thread.Free;
 // thread.WaitFor;
  for i:=0 to paths.Count-1 do paths.Data[i].free;
  paths.Destroy;
  neededPaths.Destroy;
  for i:=0 to foundPaths.Count-1 do foundPaths.Data[i].destroy;
  foundPaths.Destroy;
  pathFinderStar.Destroy;
end;

end.

