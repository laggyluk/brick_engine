unit renderable_object_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,dglOpenGL,VectorGeometry,core_types,core_camera,
  core_material_library,core_matrixStack,core_renderable,core_texture, fgl,
  core_actor_definitions;

type

  { TModel }
  //do trzymania voxelowych modeli
  //to gowno jest jak miniaturowy terrain chunk +rotation Matrix
  TModel = class (TRenderable)
    mvp,rotationM:tmatrix;
    name:string;
    itemType:eItemTypes;
    pivot:TAffineVector;
    constructor create(buffer:PDataBuffer;count:integer);
    destructor destroy;
    procedure render(leMvp, leTransform: pMatrix);
  end;

  { TTerrainChunk }
  TTerrainChunk = class(TRenderable)
    mvp:tmatrix;
    constructor create(buffer:PDataBuffer;byteCount:integer);
    destructor destroy;
    procedure render;override;
    procedure updateVBO(buffer:PDataBuffer;byteCount:integer);
  end;

  { TSelectionCube }

  TSelectionCube = class (TRenderable)
   private
    fcolor:tvector4f;
    procedure setColor(AValue: TVector4f);
   public
    property color:TVector4f read fcolor write setColor;
    procedure render;override;
    constructor create;
    destructor destroy;override;
  end;

  { TPointLightCube }
  //cube light gizmo for deferred renderrer
  TPointLightCube = class (TRenderable)
    procedure render(nullPass:boolean);
    constructor create;
    destructor destroy;override;
  end;

  { TQuad }
  TQuad = class (TRenderable)
    procedure render;override;
    constructor create;
    destructor destroy;override;
  end;

  { TTexturedQuad }
  // quad is not scaled to texture texture is
  //for flat ppl maybe
  TTexturedQuad = class (TQuad)
   protected
    //remember rotation from 'set' commands to avoid costly calculations when getting. and im stupid
    angles:TAffinevector;
   public
    mvp,rotationM:tmatrix;
    //texutre indexes in units atlas
    texFront,texSide,texBack:integer;
    mvpLocation,worldMatricLocation:glint;
    procedure render;override;
    //rotates around y axis
    procedure setRotationY(const angle:single);
    procedure setRotationX(const angle:single);
    procedure setRotationZ(const angle:single);
    procedure setRotationXYZ(const ax,ay,az:single;heightOffset:single = 0);
    function getRotation:TAffineVector;
    function getYAngle:single;
    constructor create(textureInAtlasIndex:integer);
  end;

  //texture quad that is not square like parent class but conforms to sprite size
  { TUIQuad }
  TUIQuad = class(TTexturedQuad)
    procedure render;override;
    constructor create(atlas:TTextureAtlas;textureInAtlasIndex:integer);
  end;

  //since modules are separated by messaging sysyem then each interested modelue needs
  //coresponding class to represent values needed by it

  { TFlatUnitRenderable }
  { TODO -copti : te sprity uzywaja uniformow zamiast uv w vbo, powinny byc jak uiquad }
  TFlatUnitRenderable = class(TTexturedQuad)
    actorID:integer;
    //flags if crate should be drawn over the actor. and servs as it's id
    crateActorID:integer;
    assetID:integer;
    common:rActorDefinition;
    procedure update(angleY: single; sideView: boolean);
    constructor create;
  end;
  //maps actorID to unit
  TFlatUnitRenderableMap = specialize TFPgMap<integer,TFlatUnitRenderable>;

implementation

const

    cubeVerts :array [0..63] of GLfloat =
    (                   //front face
      -0.51, 0.51, 0.51,1,  //top left
      //colors
        1,0,0,0,
       0.51, -0.51, 0.51,1, //bottom right
        1,0,0,0,
      -0.51, -0.51, 0.51,1, //left bottom
      1,0,0,0,
       0.51, 0.51, 0.51,1,  //right top
       1,0,0,0,
                         //back face
       -0.51, 0.51, -0.51,1,  //top left
       0,0,1,0,
        0.51, -0.51, -0.51,1, //bottom right
      0,0,1,0,
       -0.51, -0.51, -0.51,1, //left bottom
       0,0,1,0,
        0.51, 0.51, -0.51,1,  //right top
       0,0,1,0
    );

    cubeIndices: array [0..35] of GLuByte =
    (
      0,2,1, //front
      0,1,3,
      3,1,7, //r side
      1,5,7,
      4,5,6, //back
      4,7,5,
      0,6,2, //l side
      6,0,4,
      0,3,4, //top
      4,3,7,
      2,5,1, //bottom
      2,6,5
    );
    cubeNoColor :array [0..31] of GLfloat =
    (                   //front face
      -0.51, 0.51, 0.51,1,  //top left
      //colors
       0.51, -0.51, 0.51,1, //bottom right
      -0.51, -0.51, 0.51,1, //left bottom
       0.51, 0.51, 0.51,1,  //right top
                         //back face
       -0.51, 0.51, -0.51,1,  //top left
        0.51, -0.51, -0.51,1, //bottom right
       -0.51, -0.51, -0.51,1, //left bottom
        0.51, 0.51, -0.51,1  //right top
    );
    quadVerts: array [0..17] of GLfloat =
    (
      -1,1,0,
      -1,-1,0,
      1,-1,0,
      -1,1,0,
      1,-1,0,
      1,1,0
    );

    texQuadVerts: array [0..17] of GLfloat =
    (
      -0.5,1,0,
      -0.5,0,0,
      0.5,0,0,
      -0.5,1,0,
      0.5,0,0,
      0.5,1,0
    );

{ TUIQuad }

procedure TUIQuad.render;
begin
  //no need to set those on each quad render?
  glUniformMatrix4fv(mvpLocation, 1, bytebool(GL_FALSE), @mvp);
  glUniformMatrix4fv(worldMatricLocation, 1, bytebool(GL_FALSE), @transformM);

  glBindVertexArray(vao);
  glDrawArrays(GL_TRIANGLEs, 0,6);
  glBindVertexArray(0);
end;

constructor TUIQuad.create(atlas: TTextureAtlas; textureInAtlasIndex: integer);
var
   buf:array [0..29] of GLfloat;
begin
  shaderDesc:='ui quad';
  //create quad from atlas params
  with atlas.data[textureInAtlasIndex] do begin
    //xyz
    buf[0]:=0;
    buf[1]:=h;
    buf[2]:=0;
    buf[3]:=(x) / atlas.width;
    buf[4]:=(y)/ atlas.height;

    buf[5]:=0;
    buf[6]:=0;
    buf[7]:=0;
    buf[8]:=x / atlas.width;
    buf[9]:=(y+h)/ atlas.height;

    buf[10]:=w;
    buf[11]:=h;
    buf[12]:=0;
    buf[13]:=(x+w) / atlas.width;
    buf[14]:=(y)/ atlas.height;

    buf[15]:=0;
    buf[16]:=0;
    buf[17]:=0;
    buf[18]:=x / atlas.width;
    buf[19]:=(y+h)/ atlas.height;

    buf[20]:=w;
    buf[21]:=0;
    buf[22]:=0;
    buf[23]:=(x+w) / atlas.width;
    buf[24]:=(y+h)/ atlas.height;

    buf[25]:=w;
    buf[26]:=h;
    buf[27]:=0;
    buf[28]:=(x+w) / atlas.width;
    buf[29]:=(y)/ atlas.height;

  end;
  glGenBuffers(1,@vbo);
  glGenVertexArrays(1, @vao);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glBufferData(GL_ARRAY_BUFFER,sizeof(buf),@buf[0],GL_DYNAMIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER,0);
  //setup vao
  glBindVertexArray(vao);
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glVertexAttribPointer(0, 3, GL_FLOAT, bytebool(GL_FALSE),5*sizeof(single), nil);
  //tutaj stride byc moglby
  glVertexAttribPointer(1, 2, GL_FLOAT, bytebool(GL_FALSE), 5*sizeof(single), pointer(3*sizeof(single)));
  glBindVertexArray(0);
  glDisableVertexAttribArray(0);
  glDisableVertexAttribArray(1);

  glBindBuffer(GL_ARRAY_BUFFER,0);

  translateM:=IdentityHmgMatrix;
  scaleM:=IdentityHmgMatrix;
  rotationM:=IdentityHmgMatrix;
  setVector(angles,0,0,0);
  texFront:=textureInAtlasIndex;
end;


{ TFlatUnitRenderable }

procedure TFlatUnitRenderable.update(angleY: single;sideView:boolean);
begin
//  if sideView then begin
     //setRotationY(angles[1]+degtorad(45));
    //rotationM:=CreateRotationMatrixY(angleY);
    //if sideView then scaleM[0,0]:=common.depth else scaleM[0,0]:=common.width;
//    dirty:=true;
//  end;
  if dirty then begin
     MatrixMultiply(scaleM,rotationM,transformM);
     MatrixMultiply(transformM,translateM,transformM);
     dirty:=false;
     //chuj
  end;
//  end;
  MatrixMultiply(transformM,worldCameraMatrix,mvp);
end;

constructor TFlatUnitRenderable.create;
begin
  translateM:=IdentityHmgMatrix;
  scaleM:=IdentityHmgMatrix;
  rotationM:=IdentityHmgMatrix;
  texFront:=0;
  crateActorID:=-1;
end;

{ TTexturedQuad }
procedure TTexturedQuad.render;
begin
  //no need to set those on each quad render?
  glUniformMatrix4fv(mvpLocation, 1, bytebool(GL_FALSE), @mvp);
  glUniformMatrix4fv(worldMatricLocation, 1, bytebool(GL_FALSE), @transformM);

  glEnableVertexAttribArray(0);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glVertexAttribPointer(0, 3, GL_FLOAT, bytebool(GL_FALSE), 0, nil);
  glDrawArrays(GL_TRIANGLES, 0,6);
  glDisableVertexAttribArray(0);
end;

procedure TTexturedQuad.setRotationY(const angle: single);
begin
  angles[1]:=angle;
  rotationM:=CreateRotationMatrixY(angle);
  dirty:=true;
end;

procedure TTexturedQuad.setRotationX(const angle: single);
begin
  angles[0]:=angle;
  rotationM:=CreateRotationMatrixX(angle);
  dirty:=true;
end;

procedure TTexturedQuad.setRotationZ(const angle: single);
begin
  angles[2]:=angle;
  rotationM:=CreateRotationMatrixZ(angle);
  dirty:=true;
end;

procedure TTexturedQuad.setRotationXYZ(const ax, ay, az: single;
  heightOffset: single);
begin
  //ef wtf
  angles[0]:=ax;
  angles[1]:=ay;
  angles[2]:=az;
  rotationM:=IdentityHmgMatrix;
  //rotationM:=translateM;
  TranslateMatrix(rotationM,vector3fmake(0,-heightOffset,0));
  //QuaternionFromRollPitchYaw();
  rotationM:=turn(rotationM,-ay);
  rotationM:=pitch(rotationM,-ax);
  rotationM:=roll(rotationM,-az);
  TranslateMatrix(rotationM,vector3fmake(0,+heightOffset,0));
  dirty:=true;
end;

function TTexturedQuad.getRotation: TAffineVector;
begin
  result:=angles;
end;

function TTexturedQuad.getYAngle: single;
begin
  result:=angles[1];
end;

constructor TTexturedQuad.create(textureInAtlasIndex:integer);
begin
  shaderDesc:='flat quad';

  glGenBuffers(1,@vbo);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glBufferData(GL_ARRAY_BUFFER,sizeof(texquadVerts),@texquadVerts[0],GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER,0);

  translateM:=IdentityHmgMatrix;
  scaleM:=IdentityHmgMatrix;
  rotationM:=IdentityHmgMatrix;
  setVector(angles,0,0,0);
  texFront:=textureInAtlasIndex;
end;

{ TPointLightCube }

procedure TPointLightCube.render(nullPass:boolean);
var
   t:tmatrix;
   g:glint;
begin
 // inherited;
 // matrixStack.push(@transformM);
  //translate or transform?

//    t:=cameraToClipMatrix * worldToCameraMatrix * modelToWorldMatrix
  //MatrixMultiply(transformM,camera.worldToCameraMatrix,t);
  //MatrixMultiply(t,camera.projectionMatrix,t);
  //t:=IdentityHmgMatrix;
  //MatrixMultiply(translateM,scaleM,transformM);
  if nullPass then  glUniformMatrix4fv(matlib.shaderNullDS.gWVP, 1, bytebool(GL_FALSE), @transformM)
   else glUniformMatrix4fv(matlib.shaderCubeLightDS.unifMvp, 1, bytebool(GL_FALSE), @transformM);

  glBindVertexArray(vao);
  glDrawElements(GL_TRIANGLES, sizeof(cubeIndices), GL_UNSIGNED_BYTE, nil);
  glBindVertexArray(0);
 // matrixStack.pop;
end;


constructor TPointLightCube.create;
var
   s:string;
begin
  inherited;
  shaderDesc:='cubic light';
  glGenVertexArrays(1, @vao);
  glGenBuffers(1,@vbo);
  glGenBuffers(1, @ibo);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glBufferData(GL_ARRAY_BUFFER,sizeof(cubeNoColor),@cubeNoColor[0],GL_STATIC_DRAW);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(cubeIndices), @cubeIndices[0], GL_STATIC_DRAW);

  glBindBuffer(GL_ARRAY_BUFFER,0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

  //setup cube vao
 	glBindVertexArray(vao);
  glEnableVertexAttribArray(0);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  //stride to rozmiar calej paczki czyli vert+color dlatego 8
  glVertexAttribPointer(0, 4, GL_FLOAT, bytebool(GL_FALSE), 0, nil);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo);
  glBindVertexArray(0);

  glBindBuffer(GL_ARRAY_BUFFER,0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  glDisableVertexAttribArray(0);
  //getMaterial('simple');
  //s:=gluErrorString(glGetError());
end;

destructor TPointLightCube.destroy;
begin
  inherited destroy;
end;

{ TQuad }

procedure TQuad.render;
var
   s:string;
   t:tmatrix;
begin
  //t:=IdentityHmgMatrix;
  //matrixStack.push(@t); //move camera instead of world -style
  glEnableVertexAttribArray(0);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glVertexAttribPointer(0, 3, GL_FLOAT, bytebool(GL_FALSE), 0, nil);
  glDrawArrays(GL_TRIANGLES, 0,6);
  glDisableVertexAttribArray(0);
//  glBindBuffer(GL_ARRAY_BUFFER,0);
end;

constructor TQuad.create;
var
   s:string;
begin
  inherited;
  shaderDesc:='flat quad';
  translateM:=IdentityHmgMatrix;
  glGenBuffers(1,@vbo);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glBufferData(GL_ARRAY_BUFFER,sizeof(quadVerts),@quadVerts[0],GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER,0);
  //s:=gluErrorString(glGetError());
end;

destructor TQuad.destroy;
begin
  inherited destroy;
end;

{ TTerrainChunk }

procedure TTerrainChunk.render;
var
   kid:TRenderable;
   s:string;
begin
 // inherited;
  //for some reason matrix transformation order needs to be reveresed interrain chunk..
  //byc moze dlatego musialem odwracac wszystko w pickingu breshanma
  if dirty then begin
    //there been a transformation or camera moved so recalculate stuff
    MatrixMultiply(translateM,scaleM,transformM);
//    mvp:=cameraToClipMatrix * worldToCameraMatrix * modelToWorldMatrix
    MatrixMultiply(transformM,worldCameraMatrix,mvp);
    dirty:=false;
  end;
  //matrixStack.push(@transformM);
  glUniformMatrix4fv(matlib.shaderChunkDS.unifMVP, 1, bytebool(GL_FALSE), @mvp);
  glUniformMatrix4fv(matlib.shaderChunkDS.m_WorldMatrixLocation, 1, bytebool(GL_FALSE), @translateM);
  glBindVertexArray(vao);
  //if wtf=2 then
  glDrawArrays(GL_TRIANGLEs, 0,verticesCount);
  glBindVertexArray(0);
 // matrixStack.pop;
end;

//count to ilosc single *(vertex + kolor) ale jeszcze trzeba pomozyc przez rozmiar single
constructor TTerrainChunk.create(buffer:PDataBuffer;byteCount:integer);
var
   s:string;
   u:glint;
begin
  inherited Create();
  mvp:=IdentityHmgMatrix;
  shaderDesc:='geom terrain chunk';
  translateM:=IdentityHmgMatrix;
  verticesCount:=(byteCount div 2) div 3; //div 2 because color and position, div3 cause triangle is made of 3
  glGenVertexArrays(1, @vao);
  glGenBuffers(1,@vbo);
 // glGenBuffers(1, @ibo);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glBufferData(GL_ARRAY_BUFFER,byteCount,@buffer^[0],GL_DYNAMIC_DRAW );
  glBindBuffer(GL_ARRAY_BUFFER,0);
  //setup vao
  glBindVertexArray(vao);
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glVertexAttribPointer(0, 4, GL_FLOAT, bytebool(GL_FALSE),8*sizeof(single), nil);
  //tutaj stride byc moglby
  glVertexAttribPointer(1, 4, GL_FLOAT, bytebool(GL_FALSE), 8*sizeof(single), pointer(4*sizeof(single)));
  glBindVertexArray(0);
  glDisableVertexAttribArray(0);
  glDisableVertexAttribArray(1);
  glBindBuffer(GL_ARRAY_BUFFER,0);
  //getMaterial('geom');
  //s:=gluErrorString(glGetError());
end;

procedure TTerrainChunk.updateVBO(buffer:PDataBuffer;byteCount:integer);
begin
  //glDeleteBuffers(1, @vbo);
  //glGenBuffers(1,@vbo);
  verticesCount:=(byteCount div sizeOf(rBufferDataBit));
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glBufferData(GL_ARRAY_BUFFER,byteCount,@buffer^[0],GL_DYNAMIC_DRAW );
  glBindBuffer(GL_ARRAY_BUFFER,0);
end;

destructor TTerrainChunk.destroy;
begin
  inherited;
  glDeleteBuffers(1, @vbo);
  glDeleteVertexArrays (1, @vao);
end;


procedure TSelectionCube.setColor(AValue: TVector4f);
begin
  //if fcolor=AValue then Exit;
  fcolor:=AValue;
  glUniform4f(matlib.shaderSelectionCube.unifColor,AValue[0],AValue[1],AValue[2],AValue[3]);
end;

procedure TSelectionCube.render;
var
   t:tmatrix;
begin
  inherited;
  //matrixStack.push(@transformM);
  //glUniformMatrix4fv(uniformMvp, 1, bytebool(GL_FALSE), @mvpMatrix);
  glUniformMatrix4fv(matlib.shaderSelectionCube.unifModelToWorldMatrix, 1, bytebool(GL_FALSE), @transformM);
  glBindVertexArray(vao);
  glDrawElements(GL_TRIANGLES, sizeof(cubeIndices), GL_UNSIGNED_BYTE, nil);
  glBindVertexArray(0);
//  for kid in kids do kid.render;
 // matrixStack.pop;
end;

constructor TSelectionCube.create;
var
   s:string;
begin
  inherited;
  shaderDesc:='colored cube';
  fcolor:=vector4fmake(0.1,0.1,0.3,0.5);
  translateM:=IdentityHmgMatrix;
 	glGenVertexArrays(1, @vao);
  glGenBuffers(1,@vbo);
  glGenBuffers(1, @ibo);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glBufferData(GL_ARRAY_BUFFER,sizeof(cubeNoColor),@cubeNoColor[0],GL_STATIC_DRAW);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(cubeIndices), @cubeIndices[0], GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER,0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

  //setup cube vao
  glBindVertexArray(vao);
  glEnableVertexAttribArray(0);
  //glEnableVertexAttribArray(1);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  //stride to rozmiar calej paczki czyli vert+color dlatego 8
  glVertexAttribPointer(0, 4, GL_FLOAT, bytebool(GL_FALSE),0, nil);
  //verts interleaved with colour
  //glVertexAttribPointer(1, 4, GL_FLOAT, bytebool(GL_FALSE), 8*sizeof(single),  pointer(4*sizeof(single)));
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo);
  glBindVertexArray(0);

  glBindBuffer(GL_ARRAY_BUFFER,0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  glDisableVertexAttribArray(0);
  //glDisableVertexAttribArray(1);
  //getMaterial('simple');
  //s:=gluErrorString(glGetError());
end;

destructor TSelectionCube.destroy;
begin
  inherited destroy;
  glDeleteBuffers(1, @vbo);
  glDeleteVertexArrays (1, @vao);
  glDeleteBuffers(1, @ibo);
end;

{ TModel }

constructor TModel.create(buffer: PDataBuffer; count: integer);
var
   s:string;
   u:glint;
   i:integer;
   r:rBufferDataBit;
begin
  inherited Create();
  mvp:=IdentityHmgMatrix;
  rotationM:=IdentityHmgMatrix;
  scaleM:=IdentityHmgMatrix;
  shaderDesc:='geom terrain chunk';
  translateM:=IdentityHmgMatrix;
  //find the pivot point
  for i:=0 to count-1 do
  verticesCount:=(Count div sizeOf(rBufferDataBit));
  glGenVertexArrays(1, @vao);
  glGenBuffers(1,@vbo);
 // glGenBuffers(1, @ibo);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glBufferData(GL_ARRAY_BUFFER,count,@buffer^[0],GL_DYNAMIC_DRAW );
  glBindBuffer(GL_ARRAY_BUFFER,0);
  //setup vao
  glBindVertexArray(vao);
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glVertexAttribPointer(0, 4, GL_FLOAT, bytebool(GL_FALSE),8*sizeof(single), nil);
  //tutaj stride byc moglby
  glVertexAttribPointer(1, 4, GL_FLOAT, bytebool(GL_FALSE), 8*sizeof(single), pointer(4*sizeof(single)));
  glBindVertexArray(0);
  glDisableVertexAttribArray(0);
  glDisableVertexAttribArray(1);
  glBindBuffer(GL_ARRAY_BUFFER,0);
  //getMaterial('geom');
  //s:=gluErrorString(glGetError());
end;

destructor TModel.destroy;
begin
  inherited;
  glDeleteBuffers(1, @vbo);
  glDeleteVertexArrays (1, @vao);
end;

procedure TModel.render(leMvp, leTransform: pMatrix);
var
   kid:TRenderable;
   s:string;
begin
  //glUniform1i(matlib.shaderChunkDS.ghostUnif,integer(ghost));
  glUniformMatrix4fv(matlib.shaderChunkDS.unifMVP, 1, bytebool(GL_FALSE), leMvp[0,0]);
  glUniformMatrix4fv(matlib.shaderChunkDS.m_WorldMatrixLocation, 1, bytebool(GL_FALSE), leTransform[0,0]);
  glBindVertexArray(vao);
  glDrawArrays(GL_TRIANGLEs, 0,verticesCount);
  glBindVertexArray(0);
end;

end.

