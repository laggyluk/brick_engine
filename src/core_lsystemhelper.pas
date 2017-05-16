unit core_LSystemHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,core_types,  core_listofrecords, fgl, VectorGeometry,
  core_utils,VectorTypes,core_block_brush, core_chunk;

type

  TLSystemRules = specialize TFPGList<TLSystemRule>;
  TLSystemRendererStack = specialize TFPGList<TLSystemRendererState>;
  { TLSystemRulesHelper }
  {
    F	forward step
    f	forward step without drawing
    +	turn left by angle
    -	turn right by angle
    ^	pitch up by angle
    &	pitch down by angle
    /	roll right by angle
    \	roll left by angle
    |	turn around
    [	push current state of turtle onto a stack
    ]	pop state from a stack and set as current tutrle state
  }
   TLSystemRulesHelper = class
   private
     fStack:TLSystemRendererStack;
     dimMin,dimMax:tvector3f;
     fcolor:eBlockTypes;
     rot:TQuaternion;
     procedure line3DtoBrush(x0, y0, z0, x1, y1, z1: integer; blok: eBlockTypes);
     procedure lineToF(x, y, z: single; asBrush: boolean);
     procedure moveToF(x, y, z: single);
   public
     brush:TBlockBrush;
     rules:TLSystemRules;
     pos,offset:TAffineVector;
     fState : TLSystemRendererState;
//     zrobic stringliste zamiast setu zeby bloki mogly sie powtarzac
     colors:tstringlist;
     procedure Render(const axiom: String; angle: single; var size: TVector3f;leColors:tstringlist;
       noDraw: boolean; toBrush: boolean);
     procedure moveForward(size:taffinevector);
     procedure AddRule(const predecessor, successor : String);
     function Apply(const axiom : String) : String; overload;
     function Apply(const axiom : String; iterations : Integer) : String; overload;
     //use noDraw to get back tree dimensions in the'size' var
     constructor create;
     destructor destroy;
  end;

implementation


procedure TLSystemRulesHelper.Render(const axiom: String; angle: single;
  var size: TVector3f; leColors: tstringlist; noDraw: boolean; toBrush: boolean
  );
var
   i : Integer;
   max:single;
begin
   //BeginPath;
   fstack.Clear;
   fstate.x:=0;
   fstate.y:=0;
   fstate.z:=0;
   fstate.AX:=90;
   MoveToF(FState.X, FState.Y ,FState.z);
   for i := 1 to length(axiom) do begin
      {
        F	forward step
        f	forward step without drawing
        +	turn left by angle
        -	turn right by angle
        ^	pitch up by angle
        &	pitch down by angle
        /	roll right by angle
        \	roll left by angle
        |	turn around
        [	push current state of turtle onto aX stack
        ]	pop state from aX stack and set as current tutrle state
      }
      //dodac 3d i kolorki

      case axiom[i] of
         'F' : begin
            moveForward(size);
            if noDraw then MoveToF(FState.X, FState.Y, FState.z)
            else  LineToF(FState.X, FState.Y, FState.z,toBrush);
         end;
         'f' : begin //?
            moveForward(size);
            MoveToF(FState.X, FState.Y, FState.z)
         end;
         '+' : FState.AX := FState.AX+Angle;
         '-' : FState.AX := FState.AX-Angle;
         '^' : FState.AZ := FState.AZ+Angle;//?
         '&' : FState.AZ := FState.AZ-Angle;//?
         '/' : FState.AY := FState.AY+Angle;//?
         '\' : FState.AY := FState.AY-Angle;//?
         '|' : begin
                 FState.AX := FState.AX+180;//?
                 FState.AY := FState.AY+180;//?
                 FState.AZ := FState.AZ+180;//?
               end;
         '[' : begin //push
                FStack.Insert(0,FState);
               end;
         ']' : begin
            //fstate:=FStack.Pop;
              fstate:=fstack[0];
              fstack.Delete(0);
              MoveToF(FState.X, FState.Y, FState.z);
         end;
         '0'..'9':begin
            fcolor:=strToMineral(lecolors[strtoint(axiom[i])]);
            //mineralFromSet(colors,strtoint(axiom[i]));
         end;
      end;
   end;
   //calculate dimensions
   dimMin:=VectorNegate(dimMin);
   if noDraw then begin
      size:=VectorAdd(dimMin,dimMax);
      max:=size[0];
      if (size[1]>max) then max:=size[1];
      if size[2]>max then max:=size[2];
      setVector(size,max,max,max);
   end;
end;

procedure TLSystemRulesHelper.lineToF(x, y, z: single;asBrush:boolean);
var v:TAffineVector;
begin
 // y:=-y;
 // log('lineToF: '+floattostr(x)+','+floattostr(y));
  //plot line from pos to x,y
  setvector(v,pos[0],pos[1],pos[2]);
  vectoradd(v,offset,v);
  if asBrush then line3DtoBrush(round(v[0]),round(v[1]),round(v[2]),round(offset[0]+x),round(offset[1]+y),round(offset[2]+z),fcolor)
  else
     line3d(round(v[0]),round(v[1]),round(v[2]),round(offset[0]+x),round(offset[1]+y),round(offset[2]+z),fcolor);
  setvector(pos,x,y,z);
end;

procedure TLSystemRulesHelper.line3DtoBrush(x0,y0,z0,x1,y1,z1:integer;blok:eBlockTypes);
var
    x, delta_x, step_x  :integer;
    y, delta_y, step_y  :integer;
    z, delta_z, step_z  :integer;
    swap_xy, swap_xz            :boolean;
    drift_xy, drift_xz          :integer;
    cx, cy, cz,stepCount        :integer;
    len:single;
begin
    stepCount:=0;
    len:=VectorDistance(vector3fmake(x0,y0,z0),vector3fmake(x1,y1,z1));
    swap_xy := Abs(y1 - y0) > Abs(x1 - x0);
    if swap_xy then begin
        SwapAB(x0, y0);
        SwapAB(x1, y1);
    end;
    swap_xz := Abs(z1 - z0) > Abs(x1 - x0);
    if swap_xz then begin
        SwapAB(x0, z0);
        SwapAB(x1, z1);
    end;
    delta_x := Abs(x1 - x0);
    delta_y := Abs(y1 - y0);
    delta_z := Abs(z1 - z0);
    drift_xy  := delta_x div 2;
    drift_xz  := (delta_x div 2);
    step_x := 1;  if (x0 > x1) then  step_x := -1;
    step_y := 1;  if (y0 > y1) then  step_y := -1;
    step_z := 1;  if (z0 > z1) then  step_z := -1;
    y := y0;
    z := z0;
    x:= x0;
    while (stepCount<=len) do begin
        cx := x;    cy := y;    cz := z;
        if swap_xz then SwapAB(cx, cz);
        if swap_xy then SwapAB(cx, cy);
        inc(stepCount);
        //check if we are not outside of chunks interns
        { TODO -copti : that should never happen in calling routine anyways so this check shouldn't be here }
        if not ((cx>=world_width) or (cx<0) or
           (cy>=chunk_height) or (cy<0) or
           (cz>=world_depth) or (cz<0)) then
           begin
             if (cx=x1) and (cy=y1) and (cz=z1) then break;
             brush.addBlok(vector3fmake(cx,cy,cz),blok);
          end;
        //log('test:' + inttostr(a) + ', ' + inttostr(b) + ', ' + inttostr(c));
        //log(': ' + inttostr(cx) + ', ' + inttostr(cy) + ', ' + inttostr(cz));
        drift_xy := drift_xy - delta_y;
        drift_xz := drift_xz - delta_z;
        if drift_xy < 0 then begin
            y := y + step_y;
            drift_xy := drift_xy + delta_x;
        end;
        if drift_xz < 0 then begin
            z := z + step_z;
            drift_xz := drift_xz + delta_x;
        end;
        x:=x+step_x;
    end;
    //log('chunk: ' + inttostr(chx) + ', ' + inttostr(chy));
end;

procedure TLSystemRulesHelper.moveForward(size: taffinevector);
var
    moveVector:TAffineVector;
    mat:tmatrix;
begin
  rot:=QuaternionFromRollPitchYaw(fstate.Ax,fstate.Ay,fstate.Az);
  mat:=QuaternionToMatrix(rot);
  moveVector:=VectorTransform(fwdVector,mat);
  moveVector:=vectorScale(moveVector,size);
  fstate.x+=moveVector[0];
  fstate.y+=moveVector[1];
  fstate.z+=moveVector[2];
end;

procedure TLSystemRulesHelper.moveToF(x, y, z: single);
begin
 // y:=-y;
 // log('moveToF: '+floattostr(x)+','+floattostr(y));
  setvector(pos,x,y,z);
  //measure dimensions
  if (x<0) and (dimMin[0]<0.0001) then if x<dimMin[0] then dimMin[0]:=x;
  if (x>0) and (dimMax[0]>-0.0001) then if x>dimMax[0] then dimMax[0]:=x;

  if (y<0) and (dimMin[1]<0.0001) then if y<dimMin[1] then dimMin[1]:=y;
  if (y>0) and (dimMax[1]>-0.0001) then if y>dimMax[1] then dimMax[1]:=y;

  if (z<0) and (dimMin[2]<0.0001) then if z<dimMin[2] then dimMin[2]:=z;
  if (z>0) and (dimMax[2]>-0.0001) then if z>dimMax[2] then dimMax[2]:=z;
end;

{ TLSystemRulesHelper }

procedure TLSystemRulesHelper.AddRule(const predecessor, successor : String);
var r:tLSystemRule;
begin
//   new(r);
   r.Predecessor:=predecessor;
   r.Successor:=successor;
   rules.Add(r);
end;

function TLSystemRulesHelper.Apply(const axiom : String) : String;
var
   i : Integer;
   s : String;
   rule : TLSystemRule;
begin
   for i := 1 to length(axiom) do begin
      s := axiom[i];
      for rule in rules do begin
         if s = rule.Predecessor then begin
            s := rule.Successor;
            break;
         end;
      end;
      Result := Result+s;
   end;
end;

function TLSystemRulesHelper.Apply(const axiom : String; iterations : Integer) : String;
begin
   Result := axiom;
   while iterations > 0 do begin
      Result := Apply(Result);
      Dec(iterations);
   end;
end;

constructor TLSystemRulesHelper.create;
begin
  rules:=TLSystemRules.create;
  fstack:=TLSystemRendererStack.create;
  brush:=TBlockBrush.create;
end;

destructor TLSystemRulesHelper.destroy;
begin
  brush.Destroy;
  fstack.Destroy;
  rules.destroy;
end;

end.

