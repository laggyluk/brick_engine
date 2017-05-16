unit core_job;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VectorGeometry, core_types, core_utils,typinfo,VectorTypes;

type

  {Job types}
  eJobTypes = (jtDig, jtBuild, jtPickUpItem, jtMax);

  jobSet = set of eJobTypes;

  { TJob }

  TJob = class
   public
    id:integer;
    typ:eJobTypes;
    m:tmatrix;
    active:boolean;
    completed:boolean;
    ownerID:integer;
    //block position
    position:TAffineVector;
    //some params
    v1,v2,v3:TAffineVector;
    i1,i2,i3:integer;
    f1,f2,f3:single;
    //this could be overloaded for different job types and serve as end action
    procedure complete;
    constructor create;
  end;
  //returns time needed to be spent on job to complete it. Skill level should be
  //used with that number to make work shorter
  function getRequiredJobTime(jobType:eJobTypes):single;

var
  JobColors:array [jtDig..jtMax] of TVector4f;

implementation

//actually coulde be class method
function getRequiredJobTime(jobType:eJobTypes): single;
begin
  result:=0;
  case jobType of
    jtDig:result:=3;
    jtBuild:result:=2;
    jtPickUpItem:result:=3;
    else result:=3;
  end;
  if result=0 then log('core_job::getRequiredJobTime no definition for '+EnumToStrIng(typeinfo(eJobTypes),integer(jobType)));
end;

{ TJob }

procedure TJob.complete;
begin
  log('TJob:: job '+ inttostr(id)+' completed!');
  completed:=true;
end;

constructor TJob.create;
begin
  id:=-1;
  active:=false;
  completed:=false;
end;

initialization
  setVector(jobColors[jtDig],0.5,0.1,0.1,1);
  setVector(jobColors[jtBuild],0.4,0.4,0.1,1);
end.

