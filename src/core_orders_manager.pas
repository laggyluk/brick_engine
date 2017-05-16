unit core_orders_manager;

{$mode objfpc}{$H+}

interface

uses
  core_classes, VectorGeometry, fgl, core_types, core_job,
  core_orders;

type

  //message
  TOrderList = specialize TFPGList<rOrder>;

  { TOrdersManager }
  //class that separates different 'modules'
  TOrdersManager = class (TUpdateable)
    flastID:cardinal;
    queue:TOrderList;
    procedure update;override;
    procedure addOrder(msg:orders;i1,i2:integer;fl1,fl2,fl3:single);
    procedure addOrder(msg: orders; i1: integer; fl1, fl2, fl3: single);
    procedure addOrder(msg: orders; i1: integer; vec3:TAffineVector);
    procedure addOrder(msg: orders; i1,i2: integer; vec3:TAffineVector);
    procedure addOrder(msg: orders; i1,i2: integer; v1,v2:TAffineVector);
    procedure addOrder(msg: orders; i1,i2: integer);
    procedure addOrder(msg: orders; i1,i2,i3: integer; v1,v2,v3:TAffineVector);
    procedure addOrder(msg: orders; i1: integer; v1,v2:TAffineVector);
    procedure addOrder(msg: orders; i1, i2, i3: integer; vec3: TAffineVector);
    procedure addOrder(msg: orders; i1, i2, i3: integer; v1,v2: TAffineVector);
    procedure addJobArea(min, max: TAffineVector; jobType: eJobTypes; jobId: integer);
    procedure addOrder2i(msg: orders; i1: integer; i2: integer);
    procedure addOrderI1f1(msg: orders; i1: integer; f1: single);
    procedure addOrder(msg:orders);
    function addShoot(actorId: integer; origin, dest: TAffineVector; shotType: orders
      ): integer;
    //make camera follow given actor
    function addCameraMode(actorId: integer;cameraMode:eCameraModes): integer;
    function addOrder: integer;
    function addOrder(o:rorder):integer;
    constructor create;
    destructor destroy;

  end;

var
  oM:TOrdersManager;

implementation

{ TOrdersManager }

procedure TOrdersManager.update;
var
  i:integer;
  cmd:rOrder;
begin
  for i:=queue.count-1 downto 0 do begin
   cmd:=queue[i];
   with cmd do begin
    //mark for deletion instead?
      if (eatenFlags.eatenSound) and (eatenFlags.eatenLogic)
      {$IFNDEF SERVER}and (eatenFlags.eatenRender){$ENDIF}
      and (eatenFlags.eatenPhysics) and (eatenFlags.eatenNetwork) then begin
//          queue[i].Destroy;
          queue.Delete(i);
          //continue;
      end;
   end;
   //queue[i]:=cmd;
  end;
  i:=queue.count;
end;


function TOrdersManager.addOrder:integer;
var o:rorder;
begin
  fillchar(o,sizeof(o),false);
  with o do begin
   id:=oM.flastID;
   inc(oM.flastID);
   eatenFlags.eatenLogic:=false;
   eatenFlags.eatenRender:=false;
   eatenFlags.eatenSound:=false;
   eatenFlags.eatenPhysics:=false;
   eatenFlags.eatenNetwork:=false;
  end;
  result:=queue.Add(o);
end;

function TOrdersManager.addOrder(o: rorder): integer;
begin
  o.id:=oM.flastID;
  inc(oM.flastID);
  result:=queue.Add(o);
end;

procedure TOrdersManager.addOrder(msg: orders; i1, i2: integer; fl1, fl2,
  fl3: single);
var
  o:rOrder;
  i:integer;
begin
  fillchar(o,sizeof(o),false);
  i:=queue.Add(o);
  with o do begin
   id:=oM.flastID;
   inc(oM.flastID);
   eatenFlags.eatenLogic:=false;
   eatenFlags.eatenRender:=false;
   eatenFlags.eatenSound:=false;
   eatenFlags.eatenPhysics:=false;
   eatenFlags.eatenNetwork:=false;
   order:=msg;
   int1:=i1;int2:=i2;
   //new position
   f1:=fl1;f2:=fl2;f3:=fl3;
  end;
  queue[i]:=o;
  //oM.AddOrder(o);
end;

procedure TOrdersManager.addOrder(msg: orders; i1: integer; fl1, fl2, fl3: single);
var
  o:rOrder;
  i:integer;
begin
  fillchar(o,sizeof(o),false);
  i:=queue.Add(o);
  with o do begin
   id:=oM.flastID;
   inc(oM.flastID);
   eatenFlags.eatenLogic:=false;
   eatenFlags.eatenRender:=false;
   eatenFlags.eatenSound:=false;
   eatenFlags.eatenPhysics:=false;
   eatenFlags.eatenNetwork:=false;
   order:=msg;
   int1:=i1;
   //new position
   f1:=fl1;f2:=fl2;f3:=fl3;
  end;
  queue[i]:=o;
  //oM.AddOrder(o);
end;

procedure TOrdersManager.addOrder(msg: orders; i1: integer; vec3: TAffineVector
  );
var o:rorder;
    i:integer;
begin
  i:=addorder;
  o:=queue[i];
  o.order:= msg;
  o.int1:=i1;
  o.int2:=0;
  o.v1:=vec3;
  queue[i]:=o;
end;

procedure TOrdersManager.addOrder(msg: orders; i1, i2: integer;
  vec3: TAffineVector);
var o:rorder;
    i:integer;
begin
  i:=addorder;
  o:=queue[i];
  o.order:= msg;
  o.int1:=i1;
  o.int2:=i2;
  o.v1:=vec3;
  queue[i]:=o;
end;

procedure TOrdersManager.addOrder(msg: orders; i1, i2, i3: integer;  vec3: TAffineVector);
var o:rorder; i:integer;
begin
  i:=addorder;
  o:=queue[i];
  o.order:= msg;
  o.int1:=i1;
  o.int2:=i2;
  o.int3:=i3;
  o.v1:=vec3;
  queue[i]:=o;
end;

procedure TOrdersManager.addOrder(msg: orders; i1, i2, i3: integer; v1,
  v2: TAffineVector);
var o:rorder;    i:integer;
begin
  i:=addorder;
  o:=queue[i];
  o.order:= msg;
  o.int1:=i1;
  o.int2:=i2;
  o.int3:=i3;
  o.v1:=v1;
  o.v2:=v2;
  queue[i]:=o;
end;

procedure TOrdersManager.addOrder(msg: orders; i1, i2: integer; v1,
  v2: TAffineVector);
var o:rorder;    i:integer;
begin
  i:=addorder;
  o:=queue[i];
  o.order:= msg;
  o.int1:=i1;
  o.int2:=i2;
  o.v1:=v1;
  o.v2:=v2;
  queue[i]:=o;
end;

procedure TOrdersManager.addOrder(msg: orders; i1, i2: integer);
var o:rorder;   i:integer;
begin
  i:=addorder;
  o:=queue[i];
  o.order:= msg;
  o.int1:=i1;
  o.int2:=i2;
//  o.int3:=i3;
queue[i]:=o;
end;

procedure TOrdersManager.addOrder(msg: orders; i1, i2, i3: integer; v1, v2,
  v3: TAffineVector);
var o:rorder;     i:integer;
begin
  i:=addorder;
  o:=queue[i];
  o.order:= msg;
  o.int1:=i1;
  o.int2:=i2;
  o.int3:=i3;
  o.v1:=v1;
  o.v2:=v2;
  o.v3:=v3;
  queue[i]:=o;
end;

procedure TOrdersManager.addOrder(msg: orders; i1: integer; v1,
  v2: TAffineVector);
var o:rorder;   i:integer;
begin
  i:=addorder;
  o:=queue[i];
  o.order:= msg;
  o.int1:=i1;
  o.int2:=0;
  o.v1:=v1;
  o.v2:=v2;
  queue[i]:=o;
end;

procedure TOrdersManager.addOrderI1f1(msg: orders; i1: integer; f1: single);
begin
  addOrder(msg,i1,f1,0,0);
end;

procedure TOrdersManager.addOrder(msg: orders);
var o:rorder;   i:integer;
begin
  i:=addorder;
  o:=queue[i];
  o.order:=msg;
  queue[i]:=o;
end;

function TOrdersManager.addShoot(actorId: integer; origin, dest: TAffineVector;
         shotType:orders): integer;
var o:rorder;
begin
  result:=addorder;
  o:=queue[result];
  with o do begin
   order:=shotType;
   int1:=actorId;
   v1:=origin;
   v2:=dest;
  end;
  queue[result]:=o;
end;

function TOrdersManager.addCameraMode(actorId: integer; cameraMode: eCameraModes
  ): integer;
var o:rorder;
begin
  result:=addorder;
  o:=queue[result];
  with o do begin
       order:=orCameraMode;
       int1:=actorId;
       int2:=integer(cameraMode);
  end;
  queue[result]:=o;
end;

procedure TOrdersManager.addOrder2i(msg: orders; i1: integer; i2: integer);
begin
  addOrder(msg,i1,i2,0,0,0);
end;

procedure TOrdersManager.addJobArea(min, max: TAffineVector;
  jobType: eJobTypes; jobId: integer);
var o:rorder;
    i:integer;
begin
  i:=addorder;
  o:=queue[i];
  with o do begin
    order:=orCreateJobArea;
    v1:=min;
    v2:=max;
    int1:=integer(jobType);
    int2:=jobId;
  end;
  queue[i]:=o;
end;

constructor TOrdersManager.create;
begin
  queue:=TOrderList.Create;
end;

destructor TOrdersManager.destroy;
var i:integer;
begin
//  for i:=0 to queue.Count-1 do queue[i].Destroy;
  queue.free;
end;


end.

