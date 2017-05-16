unit core_dynamic_object;

{$mode objfpc}{$H+}

interface

uses
  Classes, VectorGeometry, core_renderable,
  core_block_brush, dglOpenGL, core_types, core_material_library,
  core_matrixStack,core_chunk;

type

  //only because needs custom render proc..

  { TLimb }

  TLimb = class (TRenderable)
    mvp:tmatrix;
    kids:TList;
    ID,parentID:integer;
    //searches for a limb with given id
    function indexOf(index:integer):tlimb;
    procedure render;override;
    constructor create;
    destructor destroy;override;
  end;

  //those are more like singletons. jednostki beda trzymaly indeks do tablicy 'oryginalow'
  //needs a vbo, and some other shit

  { TDynamicObject }

  //inherits from renderable to have transformation stuff available
  TDynamicObject = class(TRenderable)
    //root 'limb'
    root:TLimb;
    //creates mesh from brush/chunk data
    procedure fromBrush(brushes:TBrushList);
    procedure addMeshVert(x,y,z,w,r,g,b,a:single);
    procedure init(buffer:PDataBuffer;count:integer);
    procedure updateVbo;
    procedure render;override;
    constructor create;
    destructor destroy;
  end;

implementation

var bufPosition:integer;

{ TLimb }

function TLimb.indexOf(index: integer): tlimb;
var
   i:integer;
begin
//  result:=nil;
  if id=index then result:=self else
    for i:=0 to kids.Count-1 do result:=tlimb(kids[i]).indexOf(index);
end;

procedure TLimb.render;
var
   i:integer;
begin
  //calc transfoorm?
  if dirty then begin
    //there been a transformation or camera moved so recalculate stuff
    MatrixMultiply(translateM,scaleM,transformM);
//    mvp:=cameraToClipMatrix * worldToCameraMatrix * modelToWorldMatrix
    MatrixMultiply(transformM,worldCameraMatrix,mvp);
    dirty:=false;
  end;
  matrixStack.push(@transformM);
  glUniformMatrix4fv(matlib.shaderChunkDS.unifMVP, 1, bytebool(GL_FALSE), @mvp);
  glUniformMatrix4fv(matlib.shaderChunkDS.m_WorldMatrixLocation, 1, bytebool(GL_FALSE), @translateM);
  glDrawArrays(GL_TRIANGLEs, verticesStart,verticesCount);
  for i:=0 to kids.Count-1 do tlimb(kids[i]).render;
  matrixStack.pop;
end;

constructor TLimb.create;
begin
  inherited;
  mvp:=IdentityHmgMatrix;
  kids:=tList.Create;
end;

destructor TLimb.destroy;
var
   i:integer;
begin
  for i:=0 to kids.count-1 do tlimb(kids[i]).destroy;
  kids.free;
  inherited;
end;

{ TDynamicObject }

procedure TDynamicObject.fromBrush(brushes: TBrushList);
var
  j,i:integer;
  x1,x2,y,z,r,g,b:single;
  limb:TLimb;
begin
  {
  bufPosition:=0;
  for i:=0 to brushes.count-1 do begin
    //we want to keep the hierarchy among limbs to reflect that from brush so..
    if brushes[i].parentID=-1 then begin
      limb:=root;
      limb:=TLimb.create;
      limb.ID:=0;
      limb.parentID:=-1;
      limb.verticesStart:=bufPosition;
    end
    else begin  //search all limbs for parent
      limb:=TLimb.create;
      limb.ID:=brushes[i].id;
      limb.parentID:=brushes[i].parentId;
      limb.verticesStart:=bufPosition;
      //find parent and add this limb as it's child
      root.indexOf(brushes[i].parentID).kids.Add(TLimb(limb));
    end;
    //brushes[i].
    for j:=0 to length(brushes[i].data)-1 do with brushes[i].data[j] do begin
      x1:=position[0];
      x2:=x1;
      y:=position[1];
      z:=position[2];
      r:=blockColors[typ][0];
      g:=blockColors[typ][1];
      b:=blockColors[typ][2];
      addMeshVert(x1-0.5,y+0.5,z-0.5,1,r,g,b,5);
      addMeshVert(x2+0.5,y+0.5,z+0.5,1,r,g,b,5);
      addMeshVert(x2+0.5,y+0.5,z-0.5,1,r,g,b,5);
      addMeshVert(x1-0.5,y+0.5,z-0.5,1,r,g,b,5);
      addMeshVert(x1-0.5,y+0.5,z+0.5,1,r,g,b,5);
      addMeshVert(x2+0.5,y+0.5,z+0.5,1,r,g,b,5);
      inc(bufPosition);
    end;
    limb.verticesCount:=bufPosition-limb.verticesStart;
  end;
  updateVbo;  }
end;

procedure TDynamicObject.addMeshVert(x,y,z,w,r,g,b,a:single);
begin
  chunkVertexBuffer[bufPosition].a:=a;
  chunkVertexBuffer[bufPosition].x:=x;
  chunkVertexBuffer[bufPosition].y:=y;
  chunkVertexBuffer[bufPosition].z:=z;
  //chunkVertexBuffer[bufPosition].w:=1;
  //colour from blocks definition get's a bit tinted by altitude:
  chunkVertexBuffer[bufPosition].r:=r;//+(y/50);
  chunkVertexBuffer[bufPosition].g:=g;//+(y/50);
  chunkVertexBuffer[bufPosition].b:=b;//(random(2)+90)/100;//blockColors[block][2]+(y/50);
  inc(bufPosition);
end;

procedure TDynamicObject.init(buffer:PDataBuffer;count:integer);
var
   s:string;
   u:glint;
begin
  //inherited Create();
{  mvp:=IdentityHmgMatrix;
  shaderDesc:='dynamic obj';
  translateM:=IdentityHmgMatrix;
  verticesCount:=count div 8; //x,y and coulour?
 	glGenVertexArrays(1, @vao);
  glGenBuffers(1,@vbo);
 // glGenBuffers(1, @ibo);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glBufferData(GL_ARRAY_BUFFER,count*sizeof(single),@buffer^[0],GL_STATIC_DRAW);
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
  glBindBuffer(GL_ARRAY_BUFFER,0);
  glDisableVertexAttribArray(0);
  glDisableVertexAttribArray(1);
  //getMaterial('geom');
  s:=gluErrorString(glGetError());
  }
end;

procedure TDynamicObject.updateVbo;
begin
  //let's keep all limbs verts in single vbo
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glBufferData(GL_ARRAY_BUFFER,bufPosition*8*sizeof(single),@chunkVertexBuffer[0],GL_STATIC_DRAW);
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
  glBindBuffer(GL_ARRAY_BUFFER,0);
  glDisableVertexAttribArray(0);
  glDisableVertexAttribArray(1);
end;

procedure TDynamicObject.render;
var
   i:integer;
begin
  if dirty then begin
    //there been a transformation or camera moved so recalculate stuff
    MatrixMultiply(translateM,scaleM,transformM);
//    mvp:=cameraToClipMatrix * worldToCameraMatrix * modelToWorldMatrix
    //MatrixMultiply(transformM,worldCameraMatrix,mvp);
    dirty:=false;
  end;
  //matrixStack.push(@transformM);
  //render each limb
  glBindVertexArray(vao);
  root.render;
end;

constructor TDynamicObject.create;
begin
  inherited;
 	glGenVertexArrays(1,@vao);
  glGenBuffers(1,@vbo);
  root:=tlimb.create;
end;

destructor TDynamicObject.destroy;
begin
  inherited;
  glDeleteBuffers(1, @vbo);
  glDeleteBuffers(1, @vao);
  root.destroy;
end;


end.

