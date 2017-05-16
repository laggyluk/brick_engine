unit core_block_brush;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,VectorGeometry,core_types,VectorTypes,fgl,core_listofrecords,
  core_utils;

type
  //brush can be build in viewport from blocks

  eBrushPaintModes = (bpmPaint,bpmSample,bpmErase);
  { TBlockBrush }

  TBrushDataList = specialize TFPGList<TBrushData>;

  TBlockBrush = class
    private
      stream:tfilestream;
    public
     data:TBrushDataList;
     //defines where it should be renderer during creation. guess that first point which cursor clicked
     anchorPoint:Tvector3f;
     rotaxis:integer;
     //establishes hierarchy relation between limbs
     ID,parentID:integer;
     pivot:TVector3f;// marks the block that serves as pivot for limb rotations
     procedure addBlok(pos:TAffineVector;blok:eBlockTypes);
     procedure removeBlok(pos:TAffineVector);
     procedure load(filename:string);
     procedure saveAs(filename:string);
     procedure clear;
     procedure rotateY(dir:boolean);
     procedure rotateX(dir:boolean);
     procedure rotatez(dir:boolean);
     //rotates in axis marked by rotaxis
     procedure rotate(dir:boolean);
     //creates box shaped brush
     procedure Box(x,y,z:integer;blokType:eBlockTypes);
     procedure setPivot(pos: Tvector3f);
     //fills all non empty blocks with new block type
     procedure fill(newMaterial:eBlockTypes);
     constructor create;
     destructor destroy;override;
  end;
  pBlockBrush = ^TBlockBrush;
  TBrushList = specialize TFPGList<TBlockBrush>;

//var


implementation

{ TBlockBrush }

procedure TBlockBrush.addBlok(pos:TAffineVector;blok:eBlockTypes);
var
  i:integer;
  p:TAffineVector;
  d:TBrushData;
begin
  for i:=0 to data.count-1 do begin
    d:=data[i];
    //if block with those coordinates already exists in brush then change only its type
    if VectorEquals(d.position,pos) then begin
       D.typ:=blok;
       exit;
    end;
  end;
  //try to find empty space before expanding array
  for  i:=0 to data.count-1 do begin
    d:=data[i];
    if d.typ=btNone then begin
       d.typ:=blok;
       d.position:=pos;
       exit;
    end;
  end;
  //if here then blok doesn't exist yet
  d.position:=pos;
  d.typ:=blok;
  data.Add(d);
  //first clickedd block determines default anchor point
  if data.Count=1 then anchorPoint:=pos;
end;

procedure TBlockBrush.removeBlok(pos: TAffineVector);
var
  i:integer;
  d:TBrushData;
begin
  for i:=0 to data.count-1 do begin
    d:=data[i];
    //if block with those coordinates already exists in brush then change only its type
    if VectorEquals(d.position,pos) then begin
       d.typ:=btNone;
       exit;
    end;
  end;
end;

procedure TBlockBrush.load(filename: string);
var
  c,i:integer;
  d:TBrushData;
begin
try
  data.Clear;
  Stream:=tfilestream.Create(filename,fmOpenRead);
  c:=0;
  //read size of data
  stream.read(c,sizeof(c));
  //resize array
//  setlength(data,c);
  //read positions and values
  for i:=0 to c-1 do begin
     stream.read(d.position,sizeof(d.position));
     stream.read(d.typ,sizeof(d.typ));
     data.Add(d);
  end;
finally
  stream.Free;
end;
end;

procedure TBlockBrush.saveAs(filename: string);
var
  c,i:integer;
begin
try
  Stream:=tfilestream.Create(filename,fmCreate or fmOpenWrite);
  c:=0;
  //ski empty blocks
  for i:=0 to data.count-1 do if data[i].typ<>btNone then inc(c);
  //data length
  stream.Write(c,sizeof(c));
  //write positions and values
  for i:=0 to data.count-1 do if data[i].typ<>btNone then begin
     stream.Write(data[i].position,sizeof(data[i].position));
     stream.Write(data[i].typ,sizeof(data[i].typ));
  end;
finally
  stream.Free;
end;
end;

procedure TBlockBrush.clear;
var d:TBrushData;
begin
  for d in data do data.Remove(d);
  data.clear;
end;

procedure TBlockBrush.rotateY(dir: boolean);
var
  i:integer;
  v:single;
  d,d1:TBrushData;
begin
  if dir then begin //rotate left or sth
    for i:=0 to data.count-1 do begin
        d:=data[i];
        v:=-d.position[0];
        d.position[0]:=d.position[2];
        d.position[2]:=v;
        data[i]:=d;
    end;
  end else
  for i:=0 to data.count-1 do begin
      d:=data[i];
      v:=d.position[0];
      d.position[0]:=-d.position[2];
      d.position[2]:=v;
      data[i]:=d;
  end;
end;

procedure TBlockBrush.rotateX(dir: boolean);
var
  i:integer;
  v:single;
  d:TBrushData;
begin
  if dir then begin //rotate left or sth
    for i:=0 to data.count-1 do begin
        d:=data[i];
        v:=-d.position[1];
        d.position[1]:=d.position[2];
        d.position[2]:=v;
        data[i]:=d;
    end;
  end else
  for i:=0 to data.count-1 do begin
      d:=data[i];
      v:=d.position[1];
      d.position[1]:=-d.position[2];
      d.position[2]:=v;
      data[i]:=d;
  end;
end;

procedure TBlockBrush.rotatez(dir: boolean);
var
  i:integer;
  v:single;
  d:TBrushData;
begin
  if dir then begin //rotate left or sth
    for i:=0 to data.count-1 do begin
        d:=data[i];
        v:=-d.position[1];
        d.position[1]:=d.position[0];
        d.position[0]:=v;
        data[i]:=d;
    end;
  end else
  for i:=0 to data.count-1 do begin
      d:=data[i];
      v:=d.position[1];
      d.position[1]:=-d.position[0];
      d.position[0]:=v;
      data[i]:=d;
  end;
end;

procedure TBlockBrush.rotate(dir: boolean);
begin
  if rotaxis=0 then rotatex(dir);
  if rotaxis=1 then rotatey(dir);
  if rotaxis=2 then rotatez(dir);
end;

procedure TBlockBrush.Box(x, y, z: integer;blokType:eBlockTypes);
var
  xx,yy,zz,i:integer;
  d:TBrushData;
begin
  clear;
//  setlength(data,x*y*z);
  i:=0;
  for yy:=0 to y-1 do
    for zz:=0 to z-1 do
      for xx:=0 to x-1 do begin
        d.typ:=blokType;
        d.position:=vector3fmake(xx,yy,zz );
        data.Add(d);
       // log(format('i: %d x: %d y: %d z:%d',[i,xx,yy,zz]));
      end;
end;

constructor TBlockBrush.create;
begin
  rotaxis:=1;
  data:=TBrushDataList.create;
end;

destructor TBlockBrush.destroy;
begin
  data.free;
end;

procedure TBlockBrush.setPivot(pos: Tvector3f);
begin
  pivot:=pos;
end;

procedure TBlockBrush.fill(newMaterial: eBlockTypes);
var i:integer;
    b:TBrushData;
begin
  for i:=0 to data.Count-1 do if data[i].typ<>btNone then  begin
     b:=data[i];
     b.typ:=newMaterial;
     data[i]:=b;
  end;
end;

end.

