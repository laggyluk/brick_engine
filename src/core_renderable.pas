unit core_renderable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,VectorGeometry,dglOpenGL, fgl, core_types;
type
  //base class for renderable objects that have their own render
  { TRenderable }
  TRenderable = class
  protected
    fvisible:boolean;
    procedure setVisible(AValue: boolean);
  public
    //when flagged then calc transform matrix
    dirty:boolean;
    transformM:TMatrix;
    translateM:TMatrix;
    scaleM:TMatrix;
    vbo:GLuint;
    vao:GLuint;
    ibo:GLuint;
    verticesCount:integer;
    verticesStart:integer;
    shaderDesc:string; //sort of id for debugging purposes
    function getPosition: TAffineVector;
    procedure setPosition(AValue: TAffineVector);overload;
    procedure setPosition(x,y,z:single);overload;
    procedure setScale(AValue: single);
    function getScale:single;
    procedure render;virtual;
    procedure translate(x,y,z:single);
    procedure translate(v:TAffineVector);overload;
    procedure zeroPosition;
    procedure scaleY(y:single);
    procedure scaleX(x:single);
    //actually sets/loads given material
    property transform:TMatrix read translateM;
    property position:TAffineVector read getPosition write setPosition;
    property scale:single read getScale write setScale ;
    procedure calcTransform;
    property  isVisible:boolean read fvisible write setVisible; //if false than won't be drawn. used by culling and stuff
    constructor create;
    destructor destroy;override;
  end;
  PRenderable = ^TRenderable;
  TRenderablesList = specialize TFPGList<TRenderable>;

implementation

{ TRenderable }

procedure TRenderable.translate(x, y, z: single);
begin
  //translateM[3][0]:=translateM[3][0]+x;
  //translateM[3][1]:=translateM[3][1]+y;
  //translateM[3][2]:=translateM[3][2]+z;
  TranslateMatrix(translateM,vectormake(x,y,z));
  dirty:=true;
end;

procedure TRenderable.translate(v: TAffineVector);
begin
  TranslateMatrix(translateM,v);
  dirty:=true;
end;

procedure TRenderable.zeroPosition;
begin
  //position[0]:=0;
  //position[1]:=0;
  //position[2]:=0;
  setPosition(0,0,0);
  dirty:=true;
end;

procedure TRenderable.scaleY(y: single);
begin
  scaleM[1][1]:=y;
  dirty:=true;
end;

procedure TRenderable.scaleX(x: single);
begin
  scaleM[0][0]:=x;
  dirty:=true;
end;

procedure TRenderable.calcTransform;
begin
  MatrixMultiply(scaleM,translateM,transformM);
  dirty:=false;
end;

procedure TRenderable.setVisible(AValue: boolean);
begin
  fvisible:=AValue;
  if fvisible=false then Exit;
  dirty:=true;
end;

function TRenderable.getPosition: TAffineVector;
begin
  makeVector(result,translateM[3][0],translateM[3][1],translateM[3][2]);
end;

procedure TRenderable.setPosition(AValue: TAffineVector);
begin
  translateM[3][0]:=AValue[0];
  translateM[3][1]:=AValue[1];
  translateM[3][2]:=AValue[2];
  dirty:=true;
end;

procedure TRenderable.setPosition(x, y, z: single);
begin
  translateM[3][0]:=x;
  translateM[3][1]:=y;
  translateM[3][2]:=z;
  dirty:=true;
end;

procedure TRenderable.setScale(AValue: single);
begin
  scaleM:=IdentityHmgMatrix;
  ScaleMatrix(scaleM,avalue);
  scaleM[3,3]:=1;
  dirty:=true;
end;

function TRenderable.getScale: single;
begin
  { TODO -cfix : should take average of each axis i guess }
  result:=scaleM[0,0];
end;

constructor TRenderable.create;
begin
  scaleM:=IdentityHmgMatrix;
  transformM:=IdentityHmgMatrix;
  translateM:=IdentityHmgMatrix;
  dirty:=true;
  isvisible:=true;
//  kids:=TRenderableList.Create;
end;

destructor TRenderable.destroy;
var
  i:integer;
begin
  inherited;
//  for i:=0 to kids.Count-1 do kids[i].destroy;
//  kids.destroy;

end;

procedure TRenderable.render;
begin
  if dirty then calcTransform;
end;

end.

