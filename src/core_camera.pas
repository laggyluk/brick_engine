unit core_camera;

{$mode objfpc}{$H+}

interface

uses
  Classes,core_types,vectortypes,  VectorGeometry;

type

  { TCamera }

  TCamera = class
   private
    fposition : TAffineVector;
    ftarget : TAffineVector;

    left90Down,right90Down:boolean;
    rot90FromAngle:single;
    rot90Time:single;
    rot90angle:single;
    xAngle:single;
    procedure setCamMode(AValue: eCameraModes);
    procedure setCrosshairPos(AValue: TAffineVector);
    procedure setPosition(AValue: TAffineVector);
    procedure setTarget(AValue: TAffineVector);
   public
    //flags if camera is in zoomed state
    Zoomed: boolean;
    //when calling zoom it will go to this value. is not function parameter because then
    //it's not every intereset subsystem knows how much to zoom
    zoomFov:single;
    //when zooming out, it goues till this value. initialized at create
    defaultFov:single;
    //in rot 90 mode this holds current angle diff from original
    inputRot:single;
    //indicates that camera position changed and stuff like culling needs update
    //is not reset by camera
    didMove:boolean; //if something changed then flag recalculate
    worldToCameraMatrix:TMatrix;     //looks like it's a 'world' matrix
    fmode:eCameraModes;
    movementSpeed,maxMovementSpeed,
    movementSpeedY,
    strafeMovementSpeed,rotateYspeed,rotateSpeedYMax,
    rotateSpeedZMax:single; //forward/back/strafe movement speed
    rotateZSpeed:single;
    projectionMatrix:TMAtrix;
    fov,aspect,zNear,zFar:single ;
    frustum:TFrust;//for culling stuff
    //holds analog values set by the input controller, figure out where to move using this
    //trigger buttons axis is leftStick[2]
    leftStick,rightStick:TAffineVector;
    //in follow mode it needs someone to follow, position is filled be renderer
    ftargetActorID:integer;
    ftargetActorPos:TAffineVector;
    //sets up desired distance from target in follow mode
    followDistance:TAffineVector;
    //constricts the zoom value
    maxZoomFollow,minZoomFollow:single;
    fCrossHairs:TAffineVector;
    procedure update;
    //free movement along map edge, fallout shelter style
    procedure updateEdgeMode;
    //free flight mode
    procedure updateFreeMode;
    //follow actor, no rotation
    procedure updateFollowMode;
    //isometric view
    procedure update90Mode;
    //view from camera
    procedure updateMouseLook;
    procedure rotate(keyDown:boolean;direction:boolean);
    procedure setOrigin(x,y,z:single);
    procedure setTarget(x,y,z:single);
    procedure processKeyboard(key: integer; keyDown: boolean; shift: TshiftState);
    procedure processMouse(Down: boolean; shift: TshiftState; x, y: integer);
    procedure processMouseMove(shift: TshiftState;x,y:integer);
    procedure CreatePerspectiveM(ifov,iaspect,izNear,izFar:single);
//  procedure setAboveGround(distance:single);
    procedure clickLeft90(down: boolean);
    procedure clickRight90(down: boolean);
    //set actor to follow, id is only to adjust position when cam gets assigned to another actor
    procedure setTargetActor(targetPos: TAffineVector;targetID:integer);
    property mode:eCameraModes read fmode write setCamMode;
    property position:TAffineVector read fposition write setPosition;
    property target:TAffineVector read ftarget write setTarget;
    property targetActorID:integer read ftargetActorID;
    property crossHairs:TAffineVector read fCrossHairs write setCrosshairPos;
    procedure zoom(newFov: single; isZoomed: boolean);
    constructor create;
   private
    mousePos,oldMousePos,lastMouseClick:tpointf;
    mouseBtn1,mouseBtn2:boolean;
    accel:single;//przyspieszenie
    rotateVel:TVector3f;//current rotation speed;
    rotateVelMax:TVector3f;//rotation speed limit
    destAngle:TVector3f;//to which angle we are trying to turn to
    rotate_left:boolean;
    rotate_right:boolean;
    strafeRight,strafeLeft:boolean;
    move_forward,move_backward:boolean;
    move_cam_up,move_cam_down:boolean;
    rotate_cam_up,rotate_cam_down:boolean;
  end;
  //common function for checking hitbox vs mouse
  function mouseHitboxIntersect(x, y: integer; boxCenter,boxSize: taffineVector): boolean;
  function ScreenToWorld(const aPoint: TAffineVector):TAffineVector;
  function ScreenToVector(const aPoint: TAffineVector):TAffineVector;

var camera:TCamera;

implementation

const xaxis:tvector = (1,0,0,1);
      yaxis:tvector = (0,1,0,1);

{ TCamera }

procedure TCamera.setPosition(AValue: TAffineVector);
begin
  fposition:=AValue;
  didmove:=true;
end;

procedure TCamera.setTarget(AValue: TAffineVector);
begin
  ftarget:=AValue;
  didmove:=true;
end;

procedure TCamera.setTargetActor(targetPos: TAffineVector; targetID: integer);
begin
  //if ftargetActorPos=AValue then Exit;
  fTargetActorPos:=targetPos;
  if targetID<>ftargetActorID then begin
    fposition:=VectorAdd(ftarget,followDistance);
    ftargetActorID:=targetID;
    ftarget:=targetPos;
  end;
end;

procedure TCamera.zoom(newFov:single; isZoomed: boolean);
begin
  zoomed:=isZoomed;
  didMove:=true;
  fov:=newFov;
  { TODO -cfix : cos jest zjebane z zoomowaniem fovem bo sie rozjezdza widok i np pocisk wyglada inaczej }
  camera.CreatePerspectiveM(fov,camera.aspect, camera.zNear, camera.zFar);
end;

procedure TCamera.update;
var
   temp,oldP,oldT:TVector3f;
   angle,oldStrafe:single;
   q:tquaternion;
   r:tmatrix;
   f:single;
begin
  case mode of
    cmFree:updateFreeMode;
    cm90:update90Mode;
    cmFollow:updateFollowMode;
    cm3rdPerson:updateMouseLook;
    cmEdge:updateEdgeMode;
  end;
  { TODO : flag if there is actually enything to process or just skip all}
  { TODO -csmoothness : i think deltaT is what's causing jerky camera movement }
end;

procedure TCamera.updateEdgeMode;
var
  v:TVector3f;
  newPos:TVector3f;
  f,oldHeight,zoomSpeed:single;
begin
  { TODO : flag if there is actually enything to process or just skip all}
  { TODO -csmoothness : i think deltaT is what's causing jerky camera movement }
  if leftStick[0]<>0 then begin
    f:=maxMovementSpeed*leftStick[0]*deltaT;
    fposition[0] += f;
    ftarget[0] += f;
  end;
  if leftStick[1]<>0 then begin
    f:=-maxMovementSpeed*leftStick[1]*deltaT;
    fposition[1] += f;
    ftarget[1] += f;
  end;
{
  //smooth new height with previous
  f:=(oldHeight-ftarget[1]);
  f:=lerp(oldHeight,ftarget[1],deltaT);
  setVector(fposition,newPos[0],f+followDistance[1],newPos[2]);
  ftarget[1]:=f;
}
  didMove:=true;
end;

procedure TCamera.updateFreeMode;
var
   v:TVector3f;
   v1:tvector;
   temp,oldP,oldT:TVector3f;
   angle,oldStrafe:single;
   q:tquaternion;
   r:tmatrix;
   f:single;
   flag:boolean;
begin
  //where to go? check the destination
  //this is some fucked adaptation of old camera movement system
  //rotate jest w czym? destination starczy tylko na 6 kierunkow
  //wiec cos trzeba odpuscic na padzie gdzie jest tylko jeden stick dla kamery
  //i 2 wajchy analogowe, ew przelaczenie na mouse look i oba sticki steruja kamera
  rotate(rightStick[1]<>0,rightStick[1]>0);
  strafeLeft:=leftStick[0]<0;
  strafeRight:=leftStick[0]>0;
  move_forward:=leftStick[1]<0;
  move_backward:=leftStick[1]>0;
  move_cam_up:=leftStick[2]<0;
  move_cam_down:=leftStick[2]>0;

  rotate_cam_down:=rightStick[0]>0;
  rotate_cam_up:=rightStick[0]<0;
  {
  sA_cam_strafe_left: strafeLeft:=keyDown;
  sA_cam_strafe_right: strafeRight:=keyDown;
  sA_cam_forwards:move_forward:=keyDown;
  sA_cam_backwards:move_backward:=keyDown;
  sA_cam_move_up: move_cam_up:=keyDown;
  sA_cam_move_down: move_cam_down:=keyDown;
  sA_cam_rotate_down: rotate_cam_down:=keyDown;
  sA_cam_rotate_up: rotate_cam_up:=keyDown;
  }
  { TODO : flag if there is actually enything to process or just skip all}
  { TODO -csmoothness : i think deltaT is what's causing jerky camera movement }
  //adjust camera speed based on distance from the ground? not really cause maps
  //can have different height and ground is not flat
  //keyb rotate
  if rotate_left then begin
    if rotateYSpeed<rotateSpeedYMax then //accelerate
       rotateYSpeed:=rotateYSpeed+accel*rotateSpeedYMax*deltaT;
  end;
  if rotate_right then begin
   if rotateYSpeed>-rotateSpeedYMax then
      rotateYSpeed:=rotateYSpeed-accel*rotateSpeedYMax*deltaT;
  end;
  if (rotate_right=false) and (false=rotate_left) then rotateYSpeed:=0;
  //mouse look x axis rotate
  { TODO : speed varies with mouse click distance from screen center }
 {
  if (mouseBtn1) and (lastMouseClick.x<0) then begin
    if rotateYSpeed<rotateSpeedMax then //accelerate
       rotateYSpeed:=rotateYSpeed+accel*rotateSpeedMax*deltaT;
  end;
  if (mouseBtn1) and (lastMouseClick.x>0) then begin
    if rotateYSpeed>-rotateSpeedMax then //accelerate
       rotateYSpeed:=rotateYSpeed-accel*rotateSpeedMax*deltaT;
  end; }
  if (abs(rotateYSpeed)>0) then begin //x axis
    VectorSubtract(ftarget,position,ftarget);
    setVector(v,target);
    RotateVectorAroundY(v,rotateYSpeed*deltaT);
    setVector(ftarget,v);
    addvector(ftarget,position);
    //if something changed then flag recalculate
    didMove:=true;
  end;
  if not (rotate_right or rotate_left or mouseBtn1) and (abs(rotateYSpeed)>0) then begin
       rotateYSpeed:=rotateYSpeed*0.5;
    if abs(rotateYSpeed)<0.0001 then rotateYSpeed:=0;
  end;

   //mouse look y axis rotate
  //if (mouseBtn1) and (lastMouseClick.y<screenResolution.y div 2) then begin
  if (rotate_cam_up) then begin
    if rotateZSpeed<rotateSpeedZMax then //accelerate
       rotateZSpeed:=rotateZSpeed+accel*rotateSpeedZMax*deltaT;
    lastMouseClick.y:=lastMouseClick.y+1;
  end;
  //if (mouseBtn1) and (lastMouseClick.y>screenResolution.y div 2) then begin
  if (rotate_cam_down) then begin
    if rotateZSpeed>-rotateSpeedZMax then //accelerate
       rotateZSpeed:=rotateZSpeed-accel*rotateSpeedZMax*deltaT;
     lastMouseClick.y:=lastMouseClick.y-1;
  end;
  if (abs(rotateZSpeed)>0) then begin //x axis
    oldT:=ftarget;
    oldP:=fposition;
    VectorSubtract(ftarget,fposition,ftarget);

    v1:=VectorCrossProduct(yaxis,vectormake(target,1));
    setvector(v,v1);
    q:=QuaternionFromAngleAxis(30*-rotateZSpeed*deltaT,v);
 //   NormalizeQuaternion(q);
    r:=QuaternionToMatrix(q);
    ftarget:=VectorTransform(ftarget,r);

    addvector(ftarget,fposition);
    //gimb lock fix
    if (abs(ftarget[0]-fposition[0])<1) and (abs(ftarget[2]-fposition[2])<1) then begin
      ftarget:=oldt;
      fposition:=oldp;
    end;
    didMove:=true;
  end;
  if not (rotate_cam_down or rotate_cam_up) and (abs(rotateZSpeed)>0) then begin
       rotateZSpeed:=rotateZSpeed *0.5;
    if abs(rotateZSpeed)<0.0001 then rotateZSpeed:=0;
  end;

  oldStrafe:=strafeMovementSpeed;
  //strafe
  if strafeLeft then begin
    if strafeMovementSpeed<maxMovementSpeed then //accelerate
       strafeMovementSpeed:=(oldStrafe+strafeMovementSpeed+accel*maxMovementSpeed*deltaT)*0.5;
  end;
  if straferight then begin
   if strafeMovementSpeed>-maxMovementSpeed then
      strafeMovementSpeed:=(oldStrafe+strafeMovementSpeed-accel*maxMovementSpeed*deltaT)*0.5;
  end;
  if strafeMovementSpeed<>0 then begin
     VectorSubtract(ftarget,fposition,temp);
     NormalizeVector(temp);
     temp[1]:=0;//preserve elevation
     RotateVectorAroundY(temp,1.57079633);
     ScaleVector(temp,strafeMovementSpeed*deltaT);
     Addvector(fposition,temp);
     Addvector(ftarget,temp);
     didMove:=true;
  end;
  if not (strafeLeft or straferight) and (abs(strafeMovementSpeed)>0) then begin
       strafeMovementSpeed:=strafeMovementSpeed*0.5;
    if abs(strafeMovementSpeed)<0.0001 then strafeMovementSpeed:=0;
  end;
  //forward/back
  if move_forward then begin
    if MovementSpeed<maxMovementSpeed then //accelerate
       movementSpeed:=movementSpeed+accel*maxMovementSpeed*deltaT;
  end;
  if move_backward then begin
   if MovementSpeed>-maxMovementSpeed then
      movementSpeed:=movementSpeed-accel*maxMovementSpeed*deltaT;
  end;
  if MovementSpeed<>0 then begin
      VectorSubtract(ftarget,fposition,temp);
      NormalizeVector(temp);
      temp[1]:=0;//preserve elevation
      Vectorscale(temp,movementSpeed*deltaT,temp);
      setVector(v1,temp);
      Addvector(fposition,v1);
      Addvector(ftarget,v1);
      didMove:=true;
   end;
  if not (move_backward or move_forward) and (abs(MovementSpeed)>0) then begin
     MovementSpeed:=MovementSpeed*0.5;
     if abs(MovementSpeed)<0.0001 then MovementSpeed:=0;
  end;
  //cam position up/down
  if move_cam_up then begin
    if MovementSpeedY<maxMovementSpeed then //accelerate
       MovementSpeedY:=MovementSpeedY+accel*maxMovementSpeed*deltaT;
  end;
  if move_cam_down then begin
   if MovementSpeedY>-maxMovementSpeed then
      MovementSpeedY:=MovementSpeedY-accel*maxMovementSpeed*deltaT;
  end;
  if MovementSpeedY<>0 then begin
      setVector(temp,0,1,0);
      Vectorscale(temp,MovementSpeedY*deltaT,temp);
      setVector(v1,temp);
      Addvector(fposition,v1);
      Addvector(ftarget,v1);
      didMove:=true;
   end;
  if not (move_cam_up or move_cam_down) and (abs(MovementSpeedY)>0) then begin
       MovementSpeedY:=MovementSpeedY*0.5;
    if abs(MovementSpeedY)<0.0001 then MovementSpeedY:=0;
  end;
end;

procedure TCamera.updateFollowMode;
var
   v:TVector3f;
   v1:tvector;
   newPos,temp,oldP,oldT:TVector3f;
   angle,oldStrafe:single;
   q:tquaternion;
   r:tmatrix;
   f,oldHeight,zoomSpeed:single;

begin
  { TODO : flag if there is actually enything to process or just skip all}
  { TODO -csmoothness : i think deltaT is what's causing jerky camera movement }

//  rotate(rightStick[1]<>0,rightStick[1]<0);
//  strafeLeft:=leftStick[0]<0;
//  strafeRight:=leftStick[0]>0;
//  move_forward:=leftStick[1]<0;
//  move_backward:=leftStick[1]>0;
//  move_cam_up:=leftStick[2]<0;
//  move_cam_down:=leftStick[2]>0;

//  rotate_cam_down:=rightStick[0]>0;
//  rotate_cam_up:=rightStick[0]<0;
   // VectorLerp(ftarget,ftargetActorPos,deltaT,ftarget);
    oldHeight:=ftarget[1];
    ftarget:= fTargetActorPos;
    //ftarget[1]:=oldHeight;
    //zoom(dolly :p) in?
//    position:=VectorAdd(ftarget,followDistance);

    if rightStick[0]<>0 then begin
      { TODO -cimprovement : convert zoom/dolly speed to a property }
      zoomSpeed:=20;
      //constrict the zoom in/out distance
      f:=VectorLength(followDistance);
      if ((rightStick[0]>0) and (f<maxZoomFollow)) or ((rightStick[0]<0) and (f>minZoomFollow)) then begin
         v:=VectorNormalize(followDistance);
         AddVector(followDistance, VectorScale(v,zoomSpeed*rightStick[0]*deltaT));
      end;
    end;
    //rotation around target

    //camera needs to stay in given distance from target
    newPos:=VectorAdd(ftarget,followDistance);
    //move our operations center
    SubtractVector(newPos,ftarget);
    //rotate and shit
    f:=rightStick[1]*deltaT;
    RotateVectorAroundY(newPos,f);
    if abs(inputRot)+f>pi then inputRot:=0;
    inputRot+=f;
    //move back
    AddVector(newPos,ftarget);

    //if camera is rotated then followDistance vector needs to
    //change to reflect new pos
    followDistance:=VectorSubtract(newPos,ftarget);
    //smooth new height with previous
    f:=(oldHeight-ftarget[1]);
    f:=lerp(oldHeight,ftarget[1],deltaT);
    //if f>0 then f:=-0.01 else if f<0 then f:=0.01 else f:=0;
    //f:=oldHeight+f;
    setVector(fposition,newPos[0],f+followDistance[1],newPos[2]);
    ftarget[1]:=f;

    didMove:=true;
end;

procedure TCamera.update90Mode;
var
   v:TVector3f;
   v1:tvector;
   newPos,temp,oldP,oldT:TVector3f;
   angle,oldStrafe:single;
   q:tquaternion;
   r:tmatrix;
   f,oldHeight,zoomSpeed:single;

begin
    oldHeight:=ftarget[1];
    ftarget:= fTargetActorPos;

    if rightStick[0]<>0 then begin
      { TODO -cimprovement : convert zoom/dolly speed to a property }
      zoomSpeed:=20;
      //constrict the zoom in/out distance
      f:=VectorLength(followDistance);
      if ((rightStick[0]>0) and (f<maxZoomFollow)) or ((rightStick[0]<0) and (f>minZoomFollow)) then begin
         v:=VectorNormalize(followDistance);
         AddVector(followDistance, VectorScale(v,zoomSpeed*rightStick[0]*deltaT));
      end;
    end;
    //rotation around target

    //camera needs to stay in given distance from target
    newPos:=VectorAdd(ftarget,followDistance);
    //move our operations center
    SubtractVector(newPos,ftarget);
    //rotate and shit
    f:=lerp(rot90FromAngle,rot90angle, rot90Time / 2);
    RotateVectorAroundY(newPos,f);
    rot90angle-=f;
    rot90Time+=deltaT;
    if rot90Time>2 then rot90Time:=2;
    //move back
    AddVector(newPos,ftarget);

    //if camera is rotated then followDistance vector needs to
    //change to reflect new pos
    followDistance:=VectorSubtract(newPos,ftarget);
    //smooth new height with previous
    f:=(oldHeight-ftarget[1]);
    f:=lerp(oldHeight,ftarget[1],deltaT);
    //if f>0 then f:=-0.01 else if f<0 then f:=0.01 else f:=0;
    //f:=oldHeight+f;
    setVector(fposition,newPos[0],f+followDistance[1],newPos[2]);
    ftarget[1]:=f;

    didMove:=true;
end;

procedure TCamera.updateMouseLook;
var
  v,v2:TVector3f;
  v1:tvector;
  newPos,newTarget,origin,oldP,oldT:TVector3f;
  q:tquaternion;
  r:tmatrix;
  f,f1:single;
  flag:boolean;
begin
 // ftarget:= fTargetActorPos;
  //nasz srodek przeksztalcen jest krok w prawo od pozycji aktora i na wysokosci kamery
  //ale ignorujemy odleglosc w osi z
  //pozycje aktora i tak trzeba by odjac wiec odrazu ja olejmy
  origin:=vector3fmake(0.0,6,0);
  newPos:=followDistance;//vector3fmake(0,0,followDistance[2]);
  //calculate rot angle from input
  f:=mouseSensitivityX*-rightStick[1]*deltaT;
  //rotate camera position in Y axis by angle
  RotateVectorAroundY(newPos,f);

  v1:=VectorCrossProduct(yaxis,vectormake(newPos,1));
  setvector(v,v1);
  f1:=-mouseSensitivityY*rightStick[2]*deltaT;
  if mouseInvertY then f1:=-f1;
  q:=QuaternionFromAngleAxis(f1,v);
  r:=QuaternionToMatrix(q);
  newPos:=VectorTransform(newPos,r);
  //addvector(newTarget,ftargetActorPos);


  //add actor position
  addVector(newPos,ftargetActorPos);
  //rotate follow distance to new position
  RotateVectorAroundY(followDistance,f);
  followDistance:=VectorTransform(followDistance,r);
  //rotate forward movement direction
  if abs(inputRot)+f>pi then inputRot:=0;
  inputRot+=f;
  //set target to place where origin/barrel position should be
  newtarget:=vectorAdd(ftargetActorPos,origin);
  {
  newPos:=vectorAdd(fTargetActorPos,followDistance);
  newTarget:=ftarget;
  v:=vectorSubtract(fposition,newPos);
  //move target same amount as position to follow player
  addVector(newTarget,v);

  //mouse look y axis rotate
  if (rightStick[1]<>0) then begin //x axis
    VectorSubtract(newTarget,newPos,newTarget);
    setVector(v,newtarget);
    f:=mouseSensitivityX*-rightStick[1]*deltaT;
    RotateVectorAroundY(v,f);
    setVector(newTarget,v);
    addvector(newTarget,newPos);

    //RotateVectorAroundY(followDistance,f);
    inputRot+=f;
    //followDistance:=VectorSubtract(newPos,newtarget);
    didMove:=true;
  end;
  if (rightStick[2]<>0) then begin //x axis
    oldT:=newTarget;
    oldP:=newPos;
    VectorSubtract(newTarget,newPos,newTarget);

    v1:=VectorCrossProduct(yaxis,vectormake(newTarget,1));
    setvector(v,v1);
    f:=mouseSensitivityY*rightStick[2]*deltaT;
    if mouseInvertY then f:=-f;
    q:=QuaternionFromAngleAxis(f,v);
 //   NormalizeQuaternion(q);
    r:=QuaternionToMatrix(q);
    newTarget:=VectorTransform(newTarget,r);
    addvector(newTarget,newPos);
    //gimb lock fix
    if (abs(newTarget[0]-newPos[0])<1) and (abs(newTarget[2]-newPos[2])<1) then begin
      newTarget:=oldt;
      newPos:=oldp;
    end;
    didMove:=true;
  end;
  //if position didn't change then slide target
  if not didmove then begin
    subtractVector(newTarget,newPos);
    normalizeVector(newTarget);
    scaleVector(newTarget,2000);
    addVector(newTarget,newPos);
  end;              }
  didMove:=true;
  target:=newTarget;
  position:=newPos;

  //crosshairs:=ftarget;
  //crosshairs[1]+=fposition[1];
end;

procedure TCamera.rotate(keyDown: boolean; direction: boolean);
begin
  //key is down so accelerate to max rotation speed
  rotate_left:=direction and keyDown;
  rotate_right:=not direction and keyDown;
 // if rotate_left=rotate_right then rotate_left:=false;
end;

procedure TCamera.setOrigin(x, y, z: single);
begin
  setvector(fposition,x,y,z);
  didMove:=true;
end;

procedure TCamera.setTarget(x, y, z: single);
begin
  didMove:=true;
  setVector(ftarget,x,y,z);
end;

procedure TCamera.processKeyboard(key: integer; keyDown: boolean;
  shift: TshiftState);
begin
{  case Action of
   sA_cam_rotate_left: rotate(keyDown,true);
   sA_cam_rotate_right: rotate(keydown,false);
   sA_cam_strafe_left: strafeLeft:=keyDown;
   sA_cam_strafe_right: strafeRight:=keyDown;
   sA_cam_forwards:move_forward:=keyDown;
   sA_cam_backwards:move_backward:=keyDown;
   sA_cam_move_up: move_cam_up:=keyDown;
   sA_cam_move_down: move_cam_down:=keyDown;
   sA_cam_rotate_down: rotate_cam_down:=keyDown;
   sA_cam_rotate_up: rotate_cam_up:=keyDown;
  end;}
end;

procedure TCamera.processMouse(Down: boolean; shift: TshiftState;x,y:integer);
begin
  //set target point for cam movement
  lastMouseClick.x:=x;
  lastMouseClick.y:=y;
end;

procedure TCamera.processMouseMove(shift: TshiftState;x, y: integer);
var
   v:TVector3f;
   v1:tvector;
   temp:TVector3f;
   speed:single;
   angle:single;
begin
  oldMousePos.x:=x;
  oldMousePos.y:=y;
end;

procedure TCamera.CreatePerspectiveM(ifov, iaspect, izNear, izFar: single);
begin
  fov:=ifov;aspect:=iaspect;zNear:=izNear;zFar:=izFar;
  projectionMatrix:=CreatePerspectiveMatrix(fov, aspect, zNear, zFar);
end;

{procedure TCamera.setAboveGround(distance: single);
begin
  position[1]:=getBlockCollisionUp(camera.position)+distance;
  target:=position;
  target[1]-=distance;
end;
 }
procedure TCamera.clickLeft90(down:boolean);
var f:single;
begin
  if (down) and (left90Down=false) then begin
     rot90angle+=degtorad(90);
     //rot90FromAngle:=rot90angle;
     rot90Time:=0;
     ///log('r click');
     left90Down:=true;
     f:=degtorad(90);
     if abs(inputRot)+f>pi then inputRot:=0;
     inputRot+=f;
  end;// else left90Down:=true;
  if (left90Down) and (not down) then left90Down:=false;
end;

procedure TCamera.clickRight90(down:boolean);
var f:single;
begin
  if (down) and (right90Down=false) then begin
     rot90angle-=degtorad(90);
     //rot90FromAngle:=rot90angle;
     rot90Time:=0;
     //log('r click');
     right90Down:=true;
     f:=degtorad(90);
     if abs(inputRot)-f>pi then inputRot:=0;
     inputRot-=degtorad(90);
  end;// else right90Down:=true;
  if (right90Down) and (not down) then right90Down:=false;
end;

function mouseHitboxIntersect(x, y: integer; boxCenter,boxSize: taffineVector): boolean;
var
    iPnt,rayVector,rayStart:TAffineVector;
begin
  SetVector(rayStart, camera.position);
  SetVector(rayVector, ScreenToVector(AffineVectorMake(x+viewport[0], viewport[3]-y+viewport[1], 0)));
  NormalizeVector(rayVector);
  result:= RayCastBoxIntersect(rayStart,rayVector,
           VectorSubtract(boxCenter, boxSize),
           VectorAdd(boxCenter, boxSize),@iPnt);
end;

function ScreenToWorld(const aPoint: TAffineVector):TAffineVector;
var
  rslt: TVector;
begin
  if UnProject(
    VectorMake(aPoint),
    MatrixMultiply(camera.worldToCameraMatrix,camera.projectionMatrix),
     viewport,
    rslt) then
    Result := Vector3fMake(rslt)
  else
    Result := aPoint;
end;

function ScreenToVector(const aPoint: TAffineVector):
  TAffineVector;
begin
  Result := VectorSubtract(ScreenToWorld(aPoint),camera.position);
end;

procedure TCamera.setCamMode(AValue: eCameraModes);
begin
  if fmode=AValue then Exit;
  case avalue of
    cm90: setVector(followDistance,25,30,35);
    cmFollow: setVector(followDistance,0,20,35);
    cm3rdPerson: begin
      setVector(followDistance,0,0,16);
      position:=vectorAdd(ftargetActorPos,followDistance);
      target:=ftargetActorPos;
    end;
    cmEdge:begin
                                                                                //distance from map
      SetVector(fposition,0,chunk_height2*block_size+10,world_depth2*block_size+50);
      SetVector(ftarget,0,fposition[1],0);
    end;
  end;
  fmode:=AValue;
end;

procedure TCamera.setCrosshairPos(AValue: TAffineVector);
begin
  //if fCrossHairs=AValue then Exit;
  //fCrossHairs:=vectorAdd(AValue,followDistance);
  fCrossHairs:=avalue;
end;


constructor TCamera.create;
begin
  setVector(rotateVelMax,10,5,20);
  maxMovementSpeed:=40;
  rotateSpeedYMax := 1;
  rotateSpeedZMax:= 2;
  accel:=0.9*maxMovementSpeed;
  CreatePerspectiveM(45.0, 4.0 / 3.0, 0.1, 350);
  didmove:=true;
  mode:=cmFree;
  //setOrigin(0,-220,-40);
  //setTarget(30,-240,3.6);
  setVector(followDistance,25,30,35);
  setOrigin(0,110,50);
  setTarget(0,70,00);
  //how much we can zoom in/out in follow mode
  maxZoomFollow:=50;
  minZoomFollow:=10;
  ftargetActorID:=-1;
  defaultFov:=45;
  zoomFov:=45;
end;

end.

