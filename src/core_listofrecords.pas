unit core_listofrecords;

{$mode delphi}{$H+}

interface

uses sysutils, VectorGeometry, VectorTypes, core_types
  //{$IFNDEF SERVER}
  ,core_lights
  //{$ENDIF}
  ;

type

   { TBrushData }

   TBrushData = record
     position:TAffineVector;
     typ:eblocktypes;
     class operator Equal(rOrd1, rOrd2: TBrushData)B: Boolean;
   end;

   { TLSystemRule }

   TLSystemRule = record
      Predecessor : String;
      Successor : String;
      class operator Equal(rOrd1, rOrd2: TLSystemRule)B: Boolean;
   end;
   pLSystemRule = ^TLSystemRule;

   { TLSystemRendererState }

   TLSystemRendererState = record
      X, Y ,Z: single;
      AX, AY, AZ : single;
      class operator Equal(rOrd1, rOrd2: TLSystemRendererState)B: Boolean;
   end;

   { coord_t }

   coord_t = record
   	 x,y:integer;
     class operator Equal(rOrd1, rOrd2: coord_t)B: Boolean;
   end;

   { TStartFinish }

   TStartFinish = record
     start,finish:TVector3i;
     flyngAllowed,isJobSearch:boolean;
     function fromVectors(v1,v2:TAffineVector):TStartFinish;
     function fromVectors(i1,i2:TVector3i):TStartFinish;overload;
     class operator Equal(rOrd1, rOrd2: TStartFinish)B: Boolean;
   end;

   { rBulletData }

   rBulletData = record
    ID:integer;
    //instant hit test
    instantHit:boolean;
    //hitbox sphere radius. not used with instant hit weapons
    hSphereRadius:single;
    ownerID:integer;
    scaleM,translateM,transformM,rotationM:TMatrix;
    active:boolean;
    //typ pocisku
    typ:byte;
    position,velocity,oldPosition, direction:TAffineVector;
    color:TVector4f;
    //flaged for removal
   // {$IFNDEF SERVER}
    light:TPointLight;
    function createLight(col: taffineVector): TPointLight;
    //{$ENDIF}
    //face the direction of movement. should be needed only after creating
    procedure orient;

    function create(bulletType: byte): rBulletData;
    class operator Equal(rOrd1, rOrd2: rBulletData)B: Boolean;
   end;
   pBulletData = ^rBulletData;



   { rAnim1f }
   //single float animation
   eAnimationTypes = (atLinear,atPower,atQuad,atSin,atWait);
   eLoopTypes = (ltOneShot,ltPingPong);

   pAnim1f = ^rAnim1f;

   rAnim1f = record
     istart, istop,idelay,iduration:single;
     //if not nil then anim will be initialized from pointers on restart
     pStart,pStop:pfloat;
     start,duration, delay:single;
     finish:single;
     //points to value being subject of animation
     value:psingle;
     //how many times was played already
     fCount:integer;
     time:single;
     //how many times was supposed to be played
     repeats:integer;
     dither:eAnimationTypes;
     typ:integer;
     loop:eLoopTypes;
     id:integer;
     playing:boolean;
     //add up to current value
     delta:boolean;
     //start from current value
     startFromCurrent:boolean;
     //on first initializes some iValues
     initialized:boolean;
     debugMsg:string[16];
     procedure play;
     procedure stop;
     procedure reset;
     class operator Equal(rOrd1, rOrd2: rAnim1f)B: Boolean;
   end;

   { rColour }

   rColour = record
    r,g,b,a:single;
    class operator Equal(rOrd1, rOrd2: rColour)B: Boolean;
   end;

implementation

{ rColour }

class operator rColour.Equal(rOrd1, rOrd2: rColour)B: Boolean;
begin
  result:=(rord1.a=rord2.a) and (rord1.b=rord2.b) and (rord1.g=rord2.g) and (rord1.r=rord2.r);
end;

{ rAnim1f }

procedure rAnim1f.play;
begin
  playing:=true;
  fCount:=0;
  time:=0;
  if not initialized then begin
    idelay:=delay;
    iduration:=duration;
    initialized:=true;
  end else begin
    delay:=idelay;
    duration:=iduration;
  end;
  if dither=atWait then exit;
  if (pStart<>nil) then start:=pStart^;
  if (pStop<>nil) then finish:=pStop^;
  if delta then begin
    start:=value^+start;
    finish:=value^+finish;
  end;
  if startFromCurrent then start:=value^;
  istart:=start;
  istop:=finish;
{ TODO -cfix : w przypadku delaya moglo by nie inicjowac tu wartosci tylko przy pierwszej
odegranej klatce }
end;

procedure rAnim1f.stop;
begin
  playing:=false;
end;

procedure rAnim1f.reset;
begin
  stop;
  initialized:=false;
end;

class operator rAnim1f.Equal(rOrd1, rOrd2: rAnim1f)B: Boolean;
begin
   result:=rord1.ID=rord2.id;
end;

{ rBulletData }

procedure rBulletData.orient;
var
  pos,tar,up:TVector;
  m:tmatrix;
begin
  setVector(pos,0,0,0,1);
  setVector(tar,Velocity,1);
  setVector(up,upVector,0);
  m:=CreateLookAtMatrix(pos,tar,up);
  invertmatrix(m);
  rotationM:=m;
  TranslateMatrix(translateM,position);
  MatrixMultiply(scaleM,rotationM,transformM);
  MatrixMultiply(transformM,translateM,transformM);
end;

//{$IFNDEF SERVER}
function rBulletData.createLight(col: taffineVector): TPointLight;
begin
  setVector(color,col,1);
  light:=tpointlight.create(col,position,5.5,0.0,0.0,1.0);
  result:=light;
end;
//{$ENDIF}


function rBulletData.create(bulletType: byte): rBulletData;
begin
  typ:=bulletType;
  active:=true;
  setVector(color,0,1,0,1);
  scaleM:=IdentityHmgMatrix;
  ScaleMatrix(scaleM,0.2);
  scaleM[3,3]:=1;
  scaleM[2,2]:=1;
  transformM:=IdentityHmgMatrix;
  translateM:=IdentityHmgMatrix;
  rotationM:=IdentityHmgMatrix;
  result:=self;
end;

class operator rBulletData.Equal(rOrd1, rOrd2: rBulletData)B: Boolean;
begin
  result:=rord1.ID=rord2.id;
end;

function TStartFinish.fromVectors(v1, v2: TAffineVector): TStartFinish;
begin
  result.start:=vector3imake(round(v1[0]),round(v1[1]),round(v1[2]));
  result.finish:=vector3imake(round(v2[0]),round(v2[1]),round(v2[2]));
end;

function TStartFinish.fromVectors(i1, i2: TVector3i): TStartFinish;overload;
begin
  result.start:=i1;
  result.finish:=i2;
end;

class operator TStartFinish.Equal(rOrd1, rOrd2: TStartFinish)B: Boolean;
begin

end;

{ coord_t }

class operator coord_t.Equal(rOrd1, rOrd2: coord_t)B: Boolean;
begin

end;

{ TBrushData }

class operator TBrushData.Equal(rOrd1, rOrd2: TBrushData)B: Boolean;
begin

end;

{ TLSystemRendererState }

class operator TLSystemRendererState.Equal(rOrd1, rOrd2: TLSystemRendererState)
  B: Boolean;
begin

end;

{ TLSystemRule }

class operator TLSystemRule.Equal(rOrd1, rOrd2: TLSystemRule)B: Boolean;
begin

end;


{ TLSystemRulesHelper }

initialization

end.
