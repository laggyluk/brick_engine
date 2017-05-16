unit astar3d_test;
 //http://roy-t.nl/index.php/2011/09/24/another-faster-version-of-a-2d3d-in-c/
{$mode objfpc}{$H+}

interface

uses
  windows,Classes, SysUtils,astar3d_breadCrumb,astar3d_pathFinder,astar3d_world,astar3d_point3d;

type
  TArrayOfString = array of string;

  { leProgram }

  leProgram = class
    procedure execute;
  strict private
    procedure Main(args: TArrayOfString);static;
  end;

var
  writeline : procedure(const s:string);

implementation

{$HINTS OFF}
{$WARNINGS OFF}

procedure leProgram.execute;
begin
  main(nil);
end;

procedure leProgram.Main(args: TArrayOfString);
type
  TArrayOfArrayOfSystem_Double = array of array of System.Double;
  TArrayOfSystem_Double = array of System.Double;
var
  crumb2: TSearchNode;
  ts: TTime;
  start: TDateTime;
  i: Integer;
  z: Integer;
  y: Integer;
  x: Integer;
  random: integer;
  world: TWorld;
  bench: TArrayOfSystem_Double;
  startTime,endTime:cardinal;
  pathFinder:TPathFinder;
begin
  pathFinder:=TPathFinder.create;
  endTime:=0;
  setlength(bench,1000);
  world := TWorld.Create(10, 10, 10);
  randomize;// := Random.Create;
  x := 0;
  while (x < world.Right) do
  begin
    y := 0;
    while (y < world.Top) do
    begin
      z := 0;
      while (z < world.Back) do
      begin
        if (((((x + y) + z) mod 3) = 0) and (((x + y) + z) <> 0)) then
          world.MarkPosition(TPoint3D.Create(x, y, z), True);
        //*PostInc*/;
        z+=1;
      end;
      //*PostInc*/;
      y+=1;
    end;
    //*PostInc*/;
    x+=1;
  end;
  i := 0;
    startTime:=GetTickCount;
  while (i < length(bench)) do
  begin
    start := Now;

    PathFinder.FindPathList(world, TPoint3D.Zero, TPoint3D.Create(5, 8, 9));
    if endTime=0 then endTime:=GetTickCount;
    ts := Now - start;
    bench[i] := startTime-GetTickCount;

    ///*PostInc*/;
    i+=1;
  end;
  writeLine(inttostr(endTime-startTime));
  WriteLine((('Total time: ' + inttostr(startTime-GetTickCount)) + 'ms'));
  WriteLine('Average time: ' + floattostr((startTime-GetTickCount) / Length(bench)) + 'm' +
    's');
//  WriteLine((('Max: ' + bench.Max) + 'ms'));
//  WriteLine((('Min: ' + bench.Min) + 'ms'));
  WriteLine('Output: ');
  crumb2 := PathFinder.FindPathList(world, TPoint3D.Zero, TPoint3D.Create(5, 8, 9));
  WriteLine(('Start: ' + crumb2.position.ToString));
  while (crumb2.next <> nil) do
  begin
    WriteLine(('Route: ' + crumb2.next.position.ToString));
    crumb2 := crumb2.next;
  end;
  WriteLine(('Finished at: ' + crumb2.position.ToString));
  pathfinder.Free;
end;

end.

