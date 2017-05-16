unit core_input;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF windows}
    windows,
  {$ENDIF}
  Classes, SysUtils, zgl_joystick, core_orders, VectorGeometry, core_Utils,
  inifiles, core_types, core_orders_manager, core_player,
  core_network;

type

  rKeys = record
   //movement keys?
   dForward,dBackward,dLeft,dRight:integer;
   //camera
   cForward,cBackward,cLeft,cRight,cUp,cDown,
   cRotLeft,cRotRight,cRotUp,cRotDown:integer;
   //action buttons
   b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11:integer;
  end;

  { TInputManager }

  TInputManager = class
   private
    keyStates:array [0..255] of boolean;
    keyMap,padMap:rKeys;
    //flags key was pressed
    keyChanged:boolean;
    //turn off for pad/joy
    mouseLook:boolean;
    fMouseLocked:boolean;
    savedCursor:system.thandle;
    oldMousePos:tpoint;
    procedure lockMouseCursor(lock:boolean);
   public
    joyCount:integer;
    winCenterX,winCenterY:integer;
    procedure init;
    procedure keyEvent(key: byte;down:boolean);
    procedure update;
    //trap mouse cursor in window center
    property mouseLocked:boolean read fMouseLocked write lockMouseCursor;
  end;

var
  inputManager : TInputManager;

implementation

{ TInputManager }

procedure TInputManager.init;
var
  ini:tinifile;
begin
  joyCount := joy_Init();
  mouseLook:=true;
  ini:=tinifile.create(appPath+'keys.ini');
  with keyMap do begin
    //movement
    dForward:=ini.ReadInteger('keyboard','dForward',87);
    dBackward:=ini.ReadInteger('keyboard','dBackward',83);
    dLeft:=ini.ReadInteger('keyboard','dLeft',65);
    dRight:=ini.ReadInteger('keyboard','dRight',68);
    //camera
    cForward:=ini.ReadInteger('keyboard','cForward',69);
    cBackward:=ini.ReadInteger('keyboard','cBackward',81);
    cLeft:=ini.ReadInteger('keyboard','cLeft',82);
    cRight:=ini.ReadInteger('keyboard','cRight',70);
    cRotLeft:=ini.ReadInteger('keyboard','cRotLeft',3);
    cRotRight:=ini.ReadInteger('keyboard','cRotRight',2);
    cUp:=ini.ReadInteger('keyboard','cUp',67);
    cDown:=ini.ReadInteger('keyboard','cDown',86);

    //buttons
    b0:=ini.ReadInteger('keyboard','b0',32); //action btn
    b1:=ini.ReadInteger('keyboard','b1',1); //zoom
    b2:=ini.ReadInteger('keyboard','b2',0); //shoot? mouse left btn
    b3:=ini.ReadInteger('keyboard','b3',82); //reload
    b4:=ini.ReadInteger('keyboard','b4',69); //
    b5:=ini.ReadInteger('keyboard','b5',81); //
    b6:=ini.ReadInteger('keyboard','b6',52); //
    b7:=ini.ReadInteger('keyboard','b7',53); //
    b8:=ini.ReadInteger('keyboard','b8',54); //
    b9:=ini.ReadInteger('keyboard','b9',55); //
    b10:=ini.ReadInteger('keyboard','b10',56); //
    b11:=ini.ReadInteger('keyboard','b11',57); //
  end;
  with padMap do begin
    {
    //movement
    dForward:=ini.ReadInteger('keyboard','dForward',87);
    dBackward:=ini.ReadInteger('keyboard','dBackward',83);
    dLeft:=ini.ReadInteger('keyboard','dLeft',65);
    dRight:=ini.ReadInteger('keyboard','dRight',68);
    //camera
    cForward:=ini.ReadInteger('keyboard','cForward',81);
    cBackward:=ini.ReadInteger('keyboard','cBackward',69);
    cLeft:=ini.ReadInteger('keyboard','cLeft',82);
    cRight:=ini.ReadInteger('keyboard','cRight',70);
    cRotLeft:=ini.ReadInteger('keyboard','cRotLeft',3);
    cRotRight:=ini.ReadInteger('keyboard','cRotRight',2);
    cUp:=ini.ReadInteger('keyboard','cUp',67);
    cDown:=ini.ReadInteger('keyboard','cDown',86);
          }
    //buttons
    b0:=ini.ReadInteger('pad','b0',0); //ctrl
    b1:=ini.ReadInteger('pad','b1',1); //alt
    b2:=ini.ReadInteger('pad','b2',2); //space
    b3:=ini.ReadInteger('pad','b3',3); //y - reload
    b4:=ini.ReadInteger('pad','b4',4); //space
    b5:=ini.ReadInteger('pad','b5',5); //alt
    b6:=ini.ReadInteger('pad','b6',6); //space
    b7:=ini.ReadInteger('pad','b7',7); //alt
    b8:=ini.ReadInteger('pad','b8',8); //space
    b9:=ini.ReadInteger('pad','b9',9); //space
    b10:=ini.ReadInteger('pad','b10',10); //space
    b11:=ini.ReadInteger('pad','b11',11); //space
  end;
  ini.free;
end;

procedure TInputManager.keyEvent(key: byte; down: boolean);
var o:rOrder;
    int1,int2,int3,int4:integer;
    v1,v2:TAffineVector;
begin
  //when key is pressed on input device, flag in array is lit if unpressed
  //then it's cleared
  keyStates[key]:=down;
  keyChanged:=true;
  {int1:=0;int2:=0;int3:=0;int4:=0;
  setVector(v1,0,0,0);
  setVector(v2,0,0,0);
  with keyMap do begin
   //movement
   if key=dLeft then v1[0]:=-1
   else if key=dRight then v1[0]:=1
   else if key=dForward then v1[1]:=-1
   else if key=dBackward then v1[1]:=1
   //camera
   //guziki z wychylem. lewy to -1 prawy to 1
   else if key=cUp then v1[2]:=1
   else if key=cDown then v1[2]:=-1

   else if key=cLeft then v2[0]:=-1
   else if key=cRight then v2[0]:=1
   else if key=cForward then v2[1]:=-1
   else if key=cBackward then v2[1]:=1
   //jakas 3 os 'V' ktorej nie mam w padzie
   //else if key=cWtf then v2[2]:=-1
   //else if key=cWtf2 then v2[2]:=1

   //buttons
   else if key=b0 then int1:=bitSet(int1,1)
   else if key=b1 then int1:=bitSet(int1,2)
   else if key=b2 then int1:=bitSet(int1,3)
   else if key=b3 then int1:=bitSet(int1,4)

   else if key=b4 then int2:=bitSet(int2,1)
   else if key=b5 then int2:=bitSet(int2,2)
   else if key=b6 then int2:=bitSet(int2,3)
   else if key=b7 then int2:=bitSet(int2,4)

   else if key=b8 then int3:=bitSet(int3,1)
   else if key=b9 then int3:=bitSet(int3,2)
   else if key=b10 then int3:=bitSet(int3,3)
   else if key=b11 then int3:=bitSet(int3,4)
  end;
  //no spam check?
  if (v1[0]<>0) or (v1[1]<>0) or (v1[2]<>0) or
     (v2[0]<>0) or (v2[1]<>0) or (v2[2]<>0) or
     (int1<>0) or (int2<>0) or (int3<>0) then begin
       o:=ordersManager.addOrder;
       o.order:=orControllerState;
       o.int1:=int1;
       o.int2:=int2;
       o.int3:=int3;
       o.v1:=v1;
       o.v2:=v2;
  end;
          }
end;

procedure TInputManager.update;
var i:integer;
    o:rOrder;
    mousePos:tpoint;
const remoteMovement = false;
begin
  //zbieramy input z klawiatur i padow i traktujemy jako jedno
  if remoteMovement=false then begin
    i:=oM.addOrder;
    o:=om.queue[i];
  end;
  with o do begin
    order:=orControllerState;
      joy_Proc;
      //u_FloatToStr( joy_AxisPos( 0, JOY_AXIS_X ) )
      int1:=player1.actorID;
      //using 3 spare ints we can encode 12 buttons
      if (joy_Down(0,padMap.b0)) or (keyStates[keyMap.b0]) then int2:=bitSet(int2,1);
      if (joy_Down(0,padMap.b1)) or (keyStates[keyMap.b1]) then int2:=bitSet(int2,2);
      if (joy_Down(0,padMap.b2)) or (keyStates[keyMap.b2]) then int2:=bitSet(int2,3);
      if (joy_Down(0,padMap.b3)) or (keyStates[keyMap.b3]) then int2:=bitSet(int2,4);
      if (joy_Down(0,padMap.b4)) or (keyStates[keyMap.b4]) then int3:=bitSet(int3,1);
      if (joy_Down(0,padMap.b5)) or (keyStates[keyMap.b5]) then int3:=bitSet(int3,2);
      if (joy_Down(0,padMap.b6)) or (keyStates[keyMap.b6]) then int3:=bitSet(int3,3);
      if (joy_Down(0,padMap.b7)) or (keyStates[keyMap.b7]) then int3:=bitSet(int3,4);

      if (joy_Down(0,padMap.b8)) or (keyStates[keyMap.b8]) then int4:=bitSet(int4,1);
      if (joy_Down(0,padMap.b9)) or (keyStates[keyMap.b9]) then int4:=bitSet(int4,2);
      if (joy_Down(0,padMap.b10)) or (keyStates[keyMap.b10]) then int4:=bitSet(int4,3);
      if (joy_Down(0,padMap.b11)) or (keyStates[keyMap.b11]) then int4:=bitSet(int4,4);

      if 0<=joyCount then v1[0]:=joy_AxisPos( 0, JOY_AXIS_X );
      if keyStates[keyMap.dLeft] then v1[0]+= -1;
      if keyStates[keyMap.dRight] then v1[0]+= 1;
      v1[1]:=joy_AxisPos( 0, JOY_AXIS_Y );
      if keyStates[keyMap.dForward] then v1[1]+= -1;
      if keyStates[keyMap.dBackward] then v1[1]+= 1;
      v1[2]:=joy_AxisPos( 0, JOY_AXIS_Z );
      if keyStates[keyMap.cDown] then v1[2]+= -1;
      if keyStates[keyMap.cUp] then v1[2]+= 1;
      v2[0]:=joy_AxisPos( 0, JOY_AXIS_R );
      if keyStates[keyMap.cLeft] then v2[0]+= -1;
      if keyStates[keyMap.cRight] then v2[0]+= 1;
      v2[1]:=joy_AxisPos( 0, JOY_AXIS_U );
      if keyStates[keyMap.cForward] then v2[1]+= -1;
      if keyStates[keyMap.cBackward] then v2[1]+= 1;
      v2[2]:=joy_AxisPos( 0, JOY_AXIS_V );
      if keyStates[keyMap.cRotLeft] then v2[2]+= -1;
      if keyStates[keyMap.cRotRight] then v2[2]+= 1;
      ScaleVector(v1,stickSensitivityL);
      ScaleVector(v2,stickSensitivityR);
      //mouse look?
      if mouseLocked then begin
         MousePos:=getMousePosition;
         {$IFDEF windows}
         SetCursorPos(winCenterX,winCenterY);
         {$ENDIF}
         v2[1]:=mousePos.x-oldMousePos.x;
         v2[2]:=mousePos.y-oldMousePos.y;
         //if v1[1]>20 then v1[1]:=20 else if v2[1]<-20 then v1[1]:=-20;
         //if v2[1]>20 then v2[1]:=20 else if v2[1]<-20 then v2[1]:=-20;
        // NormalizeVector(v1);
        // ScaleVector(v2,deltaT);
         oldMousePos:=getMousePosition;
        end;
      end;

    joy_ClearState();
    //copy player's input state to network module so it can send it when appropriate
    move(o,network.playerInputState,sizeof(o));
    //network.playerInputState;
   if remoteMovement=false then om.queue[i]:=o;
end;

procedure TInputManager.lockMouseCursor(lock: boolean);
begin
  if (lock) and (lock<>fMouseLocked) then begin
      ShowCursor(false);
  end else if lock=false then ShowCursor(true);
  fMouseLocked:=lock;
end;

end.

