unit core_utils;

{$mode objfpc}{$H+}
interface

uses
  {$ifdef windows}
    windows,
  {$endif}
  Classes, typinfo, graphics,SysUtils,core_types,VectorGeometry,vectorTypes,
  math;

type
  TFixedPoint = Smallint;
  pFixedPoint = ^TFixedPoint;

const
  FIX_ONE = 1 shl 12;  // 1 in Q3.12 signed fixed point numbers is 2^12=4096
  FIX_MAX: TFixedPoint = $7FFF; // biggest number  $7FFF =  7.9999
  FIX_MIN: TFixedPoint = MINSHORT; // smallest number $8000 = -8.0000
  FP_MAX: single = 7.9999;
  FP_MIN: single = -8.0000;

  //fixed point math. store float in two bytes. range -8..8
  function FloatToFix(const a: double): TFixedPoint;
  function FixToFloat(const a: TFixedPoint): single;

  function FirstDelimiter(const Delimiters, S: string): Integer;
  //remove spaces & tabs from string
  function removeBlanks(const s:string):string;
  function getMousePosition:tpoint;
  //line is a ray from mouse rzutowane on the chunks grid
  function eatString(var s: string;separator:char = ';'):string;
  // translate position with coordinates at world center to one suitable for direct chunks data access
  //wtf warning
  function posToChunkPos(v:TAffineVector):TVector3i;
  //wtf warning
  function posToChunkPos(v:TVector3i):TAffineVector;
  //translate position from array to 0,0 world center
  //wtf warning
  function posChunkToPos(v:TVector3i):TAffineVector;
  //wtf warning
  function posChunkToPos(v: TVector3f): TAffineVector;
  function toUnitPos(v:tvector3i): TAffineVector;
  //translates world coordinates to block position in chunk
  //origin is in corner not at 0,0

  //wraps vector to world bounduaries
  procedure wrapPosition(var position:TAffineVector);
  //check if position is out of map
  function outOfMap(pos:TAffineVector;centered:boolean=true):boolean;
  //get block type at absolute position
  function getBlockTypeAt(pos:TAffineVector):eblocktypes;inline;

  //convert 3i to 3f
  function vectorConvert(vi: TVector3i):TAffineVector;
  function vectorConvert(vi:TAffineVector ):TVector3i;
  //vector of floats to tcolor
  function vecToColor(col:tvector4f):tcolor;
  function vecToColor(col:tvector3f):tcolor;overload;
  //converts tcolor to vector 3f
  function colorToVec3(col:tcolor):tvector3f;
  function colorToVec4(col:tcolor):tvector4f;
  function pointInTriangle(p1, p2, p3,point: TVector2f):boolean;
  function pickRandomMineral(leSet:sMinerals):eBlockTypes;
  function intToMineral(n: integer): sMinerals;
  function mineralFromSet(leSet:sMinerals;index:integer):eBlockTypes;
  function getSetSize(leSet:sMinerals):integer;
  function strToMineral(const s:string):eBlockTypes;
  //returns mineral name withouth the bt prefix
  function mineralToStr(m:eBlockTypes):string;
  function EnumToStrIng(const TypeInfo: pTypeInfo; Ix: Integer): string;
  function minusWorld(v:TaffineVector):TaffineVector;
  function mouseToArray(v:TaffineVector):TaffineVector;
  function minusWorld(v:TVector3i):TVector3i;
  procedure clamp(var val:integer;minimum,maximum:integer);
  procedure clamp(var val:single;minimum,maximum:single);
  //fast simple random number generator
  function XorShift(range:uint32):uint32;
  // Return minimum distance between line segment vw and point p
  function minimum_distance(a, b, p:TVector3f):single;
  function wordWrap(var InpStr : String; WrapWidth : Integer): String;
  //IWantSpaces >> I Want Spaces
  function splitCapitalsWithSpaces(const line:string):string;
  //bit operations
  function BitGet(const aValue: Cardinal; const Bit: Byte): Boolean;
  function BitSet(const aValue: Cardinal; const Bit: Byte): Cardinal;
  function BitClear(const aValue: Cardinal; const Bit: Byte): Cardinal;
  function BitEnable(const aValue: Cardinal; const Bit: Byte; const Flag: Boolean): Cardinal;
  //kierunek na ukladzie w spolzednych liczac clockiwse od NE=0
  function kierunek(px,pY,centerX,centerY: single): integer;
  function VectorRound(v:taffinevector):taffinevector;
  procedure roundVector(var v: taffinevector);


implementation

var
  //xorshift 'statics'
  xrx :uint32;
  xry :uint32;
  xrz :uint32;
  xrw :uint32;

function FloatToFix(const a: double): TFixedPoint;
begin
  Result := Trunc(a * FIX_ONE);
end;

function FixToFloat(const a: TFixedPoint): single;
begin
  Result := a / FIX_ONE;
end;

//get if a particular bit is 1
function BitGet(const aValue: Cardinal; const Bit: Byte): Boolean;
begin
  Result := (aValue and (1 shl Bit)) <> 0;
end;

//set a particular bit as 1
function BitSet(const aValue: Cardinal; const Bit: Byte): Cardinal;
begin
  Result := aValue or (1 shl Bit);
end;

//set a particular bit as 0
function BitClear(const aValue: Cardinal; const Bit: Byte): Cardinal;
begin
  Result := aValue and not (1 shl Bit);
end;

//Enable o disable a bit
function BitEnable(const aValue: Cardinal; const Bit: Byte; const Flag: Boolean): Cardinal;
begin
  Result := (aValue or (1 shl Bit)) xor (Integer(not Flag) shl Bit);
end;

function kierunek(px,pY,centerX,centerY: single): integer;
begin
  if (px>centerX) and (py>centerY) then result:=0 else
  if (px>centerX) and (py=centerY) then result:=1 else
  if (px>centerX) and (py<centerY) then result:=2 else
  if (px=centerX) and (py<centerY) then result:=3 else
  if (px<centerX) and (py<centerY) then result:=4 else
  if (px<centerX) and (py=centerY) then result:=5 else
  if (px<centerX) and (py>centerY) then result:=6 else
  if (px=centerX) and (py>centerY) then result:=7 else
  result:=-1;
end;

function VectorRound(v: taffinevector): taffinevector;
begin
  result[0]:=round(v[0]);
  result[1]:=round(v[1]);
  result[2]:=round(v[2]);
end;

procedure roundVector(var v: taffinevector);
begin
  v[0]:=round(v[0]);
  v[1]:=round(v[1]);
  v[2]:=round(v[2]);
end;

function mineralToStr(m: eBlockTypes): string;
begin
  result:=GetEnumName(typeinfo(eBlockTypes),integer(m));
  result:=copy(result,3,length(result)-2);
end;

function EnumToStrIng(const TypeInfo: pTypeInfo; Ix: Integer): string;
 begin
   Result := GetEnumName(TypeInfo, ix) ;
 end;

function intToMineral(n: integer): sMinerals;
var
  Op: eBlockTypes;
begin
  Result:= [];
  for Op:= Low(eBlockTypes) to High(eBlockTypes) do
    if n and (1 shl ord(Op)) > 0 then Include(Result, Op);
end;

function mineralFromSet(leSet:sMinerals;index:integer):eBlockTypes;
var
  i:integer;
  item:eBlockTypes;
begin
  i:=0;
  result:=btUndefined;
  for item in leSet do begin
     if i=index then begin
        result:=item;
        break;
     end;
     inc(i);
  end;
end;

function getSetSize(leSet: sMinerals): integer;
var e:eBlockTypes;
begin
  result:=0;
  for e:=low(eBlockTypes) to high(eBlockTypes) do if e in (leSet) then
      inc(result);
end;

function strToMineral(const s: string): eBlockTypes;
begin
 // result:=GetEnumValue(TypeInfo(TAbc), s);
  result:=eblocktypes(GetEnumValue(typeinfo(eBlockTypes),s));
end;

function pickRandomMineral(leSet:sMinerals):eBlockTypes;
var
  i,j:integer;
  item:eBlockTypes;
begin
  i:=0;
  result:=btUndefined;
  for item in leSet do begin
     inc(i);
  end;
  j:=random(i);
  i:=0;
  for item in leSet do begin
     if i=j then begin
       result:=item;
       break;
     end;
     inc(i);
  end;
end;



function removeBlanks(const s: string): string;
var
  i, j: Integer;
begin
  SetLength(Result, Length(s));
  j := 0;
  for i := 1 to Length(s) do begin
    if not (s[i] in [#9,#32]) then begin
      inc(j);
      Result[j] := s[i];
    end;
  end;
  SetLength(Result, j);
end;

function getMousePosition: tpoint;
begin
   {$ifdef windows}
     getcursorpos(result);
   {$endif}
end;

function eatString(var s: string;separator:char = ';'):string;
var i:integer;
begin
  result:='';
  if s='' then exit;
  for I := 1 to length(s) do if s[i]=separator then break;
  if s[i]<>separator then begin
    result:=copy(s,1,i);
    s:='';
  end
  else begin
      result:=copy(s,1,i-1);
      s:=copy(s,i+1,length(s)-i);
  end;
end;

function minusWorld(v:TaffineVector):TaffineVector;
begin
  result[0]:=v[0]-world_width2;
  result[1]:=v[1];
  result[2]:=v[2]-world_depth2;
end;

function mouseToArray(v: TaffineVector): TaffineVector;
begin
  result[0]:=v[0]+world_width2;
  result[1]:=v[1];
  result[2]:=v[2]+world_depth2;
end;

function minusWorld(v: TVector3i): TVector3i;
begin
  result[0]:=v[0]-world_width2;
  result[1]:=v[1];
  result[2]:=v[2]-world_depth2;
end;

function posToChunkPos(v: TAffineVector): TVector3i;
begin
  result:=Vector3imake(round(v[0]-world_width2),-round(v[1]),round(v[2]-world_depth2));
end;

function posToChunkPos(v: TAffineVector): TAffineVector;
begin
  result:=Vector3fmake(v[0]-world_width2,-v[1],v[2]-world_depth2);
end;

function posChunkToPos(v: TVector3f): TAffineVector;
begin
  result:=Vector3fMake(v[0]+world_width2,chunk_height-v[1],v[2]+world_depth2);
end;

function posToChunkPos(v: TVector3i): TAffineVector;
begin
  result:=Vector3fMake(v[0]-world_width2,v[1],v[2]-world_depth2);
end;

function posChunkToPos(v: TVector3i): TAffineVector;
begin
  result:=Vector3fMake(v[0]+world_width2,chunk_height-v[1],v[2]+world_depth2);
end;

function toUnitPos(v: tvector3i): TAffineVector;
begin
  //result:=minusWorld(vector3fmake(v[0],v[1],v[2]));
  result:=vector3fmake(v[0]-world_width2,v[1]-0.5,v[2]-world_depth2);
end;



procedure wrapPosition(var position: TAffineVector);
begin
  //no walking outside the map
  if position[0]<=-world_width2 then position[0]:=-world_width2+1;
  if position[0]>=world_width2-1 then position[0]:=world_width2-1;
  if position[1]>=chunk_height-1 then position[1]:=chunk_height;
  if position[1]<=0 then position[1]:=0;
  if position[2]<=-world_depth2-1 then position[2]:=-world_depth2+1;
  if position[2]>=world_depth2-1 then position[2]:=world_depth2-1;
end;

function outOfMap(pos: TAffineVector;centered:boolean=true): boolean;
begin
  result:=false;
  //test@@@
 // setvector(pos,127.924194,65.5296478,-68.9813004);
  if centered then begin
    if pos[0]<=-world_width2+1 then result:=true else
    if pos[0]>=world_width2-1 then result:=true else
    if pos[1]<=0 then result:=true else
    if pos[1]>=chunk_height-1 then result:=true else
    if pos[2]<=-world_depth2+1 then result:=true else
    if pos[2]>=world_depth2-1 then result:=true;
  end else begin
      //this actually returns true for blocks being one block from edge
      if pos[0]<=0 then result:=true else
      if pos[0]>=world_width-1 then result:=true else
      if pos[1]>=chunk_height-1 then result:=true else
      if pos[1]<=0 then result:=true else
      if pos[2]<=0 then result:=true else
      if pos[2]>=world_depth-1 then result:=true;
  end;
end;

function getBlockTypeAt(pos: TAffineVector): eblocktypes;
begin
  //check if inside world dimensions?
//  if (pos[1]>0) or (pos[1]
  try
    //blokpos:=absolutePositionToChunkBlok(ch,pos);
    //result:=eblocktypes(chunksPointer^[ch[0],ch[1]].blocks[blokPos[0],blokPos[1],blokPos[2]]);
    //p[0]:=round(pos[0]);p[1]:=round(pos[1]);p[2]:=round(pos[2]);
    result:=eblocktypes(getWorldCenteredF(pos[0],pos[1],pos[2]));
  except
    log('getBlockTypeAt:: error, position out of bounds!');
  end;
end;



function vectorConvert(vi: TVector3i):TAffineVector;
begin
  result[0]:=vi[0];
  result[1]:=vi[1];
  result[2]:=vi[2];
end;

function vectorConvert(vi: TAffineVector): TVector3i;
begin
  result[0]:=round(vi[0]);
  result[1]:=round(vi[1]);
  result[2]:=round(vi[2]);
end;

function vecToColor(col: tvector4f): tcolor;
begin
  result:=rgbtocolor(round(col[0] * 255),round(col[1] * 255),round(col[2] * 255));
end;

function vecToColor(col: tvector3f): tcolor;
begin
  result:=rgbtocolor(round(col[0] * 255),round(col[1] * 255),round(col[2] * 255));
end;


function colorToVec3(col: tcolor): tvector3f;
begin
  result[0]:=red(col) / 255;
  result[1]:=green(col) / 255;
  result[2]:=blue(col) / 255;
end;

function colorToVec4(col: tcolor): tvector4f;
begin
  result[0]:=red(col) / 255;
  result[1]:=green(col) / 255;
  result[2]:=blue(col) / 255;
  result[3]:=1;
end;


function pointInTriangle(p1, p2, p3,point: TVector2f):boolean;
var
  alpha,beta,gamma:single;
begin
  result:=false;
  alpha := ((p2[1] - p3[1])*(point[0] - p3[0]) + (p3[0] - p2[0])*(point[1] - p3[1])) /
          ((p2[1] - p3[1])*(p1[0] - p3[0]) + (p3[0] - p2[0])*(p1[1] - p3[1]));
  beta := ((p3[1] - p1[1])*(point[0] - p3[0]) + (p1[0] - p3[0])*(point[1] - p3[1])) /
         ((p2[1] - p3[1])*(p1[0] - p3[0]) + (p3[0] - p2[0])*(p1[1] - p3[1]));
  gamma := 1.0 - alpha - beta;
  if (gamma>0)  and (beta>0) and (alpha>0) then result:=true;
end;

procedure clamp(var val: integer; minimum, maximum: integer);
begin
  val:=max(minimum, min(val, maximum));
end;

procedure clamp(var val: single; minimum, maximum: single);
begin
  val:=max(minimum, min(val, maximum));
end;

procedure XorShiftRandomize;
begin
  xry:=GetTickCount;
end;

function XorShift(range: uint32): uint32;
var
    t:uint32;
begin
    t := xrx xor (xrx shl 11);
    xrx := xry;
    xry := xrz;
    xrz := xrw;
    xrw :=(xrw xor (xrw shr 19) xor (t xor (t shr 8))) mod range;
    result:= xrw;
end;

function FirstDelimiter(const Delimiters, S: string): Integer;
var
  P, Q: PChar;
  Len : Integer;
begin
  Result := 0;
  P := Pointer(Delimiters) ;
  Q := Pointer(s) ;
  Len := StrLen(Q) ;
  while Result < Len do
    if (Q[Result] <> #0) and (StrScan(P, Q[Result]) <> nil) then
      Exit
    else
      Inc(Result) ;
end;

function minimum_distance(a, b, p:TVector3f):single;
var
  l2,t:single;
  projection:TVector3f;
begin
  l2 := VectorLength(vectorSubtract(a, b));  // i.e. |b-a|^2 -  avoid a sqrt
  if (l2 = 0.0) then result:=vectorLength(vectorSubtract(p, a)) else
  begin   // a == b case
    // Consider the line extending the segment, parameterized as a + t (b - a).
    // We find projection of point p onto the line.
    // It falls where t = [(p-a) . (b-a)] / |b-a|^2
    t := VectorDotProduct(vectorSubtract(p , a),vectorSubtract( b , a)) / l2;
    if (t < 0.0) then result:=vectorLength(vectorSubtract(p, a))       // Beyond the 'a' end of the segment
    else
      if (t > 1.0) then result:= vectorLength(vectorSubtract(p, b)) else
      begin// Beyond the 'b' end of the segment
        projection := vectorAdd(a , VectorScale (vectorSubtract(b, a),t));  // Projection falls on the segment
        result:= vectorLength(vectorSubtract(p, projection));
      end;
  end;
end;

function wordWrap(var InpStr: String; WrapWidth: Integer): String;
var
  L,w:string;
begin
  w:=eatString(InpStr,' ');
  while (length(L+w)+1< WrapWidth) and (w<>'') do begin
    L:=L+' '+w;
    w:=eatString(InpStr,' ')
  end;
  if w<>'' then begin
    if L[1]=' ' then l:=copy(l,2,length(l)+1);
    if L[length(l)]=' ' then l:=copy(l,1,length(l));
    InpStr:=w+' '+InpStr;
    result:=L;
  end else begin
    if L[1]=' ' then l:=copy(l,2,length(l)+1);
    InpStr:=L;
    result:='';
  end;
end;

function splitCapitalsWithSpaces(const line: string):string;
var i,j:integer;
  flag:boolean;
begin
  result:=line;
  repeat
    j:=0;
    for i:=2 to length(line)-1 do if (line[i] in ['a'..'z']) and (line[i+1] in ['A'..'Z']) then begin
        j+=1;
        result:=copy(line,1,i)+' '+copy(line,i+1,length(line));
        break;
    end;
    flag:=false;
    for i:=1 to length(line)-1 do
      if (result[i] in ['a'..'z']) and (result[i+1] in ['A'..'Z'])then flag:=true;
  until (flag=false) or (j=0);;
end;


initialization
  //xorshift statics initialization
  xrx := 123456789;
  xry := 362436069;
  xrz := 521288629;
  xrw := 88675123;
  XorShiftRandomize;
  XorShift(8);

end.

