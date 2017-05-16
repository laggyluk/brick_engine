unit core_mesh;

{$mode objfpc}{$H+}

interface

uses
  fileutil,Classes, SysUtils, dglOpenGL, VectorGeometry, fgl, VectorTypes,
  core_types,core_chunk, renderable_object_types, core_utils;

type

  { TModelInstance }
  //holds transform that is used for model rendering but doesn't use own render and vbo's
  TModelInstance = class(TTexturedQuad)
    ghost:boolean;
    name:string;
    constructor create(modelIndex:integer);
  end;
  //list of objects on screen
  TModelInstanceList = specialize TFPGMap <integer,TModelInstance>;

  { TMeshManager }

  TMeshManager = class
   private
    initialized:boolean;
   public
    ghostInstances,instances:TModelInstanceList;
    //atlas of models. to render instance use this index
    models:array of TModel;
    //for gizmo rendering
    ghost:boolean;
    //loads all supported files from target folder and sends them to vbo
    procedure loadModelsFromFolder(const path:string);
    //renders elements
    procedure render;
    function getModelByName(const name:string):TModel;
    function addModelInstance(itemID: integer; itemType:eItemTypes; beGhost:boolean;position,
      rotation: TVector3f): TModelInstance;
    procedure addModelInstance(modelInstance: TModelInstance; itemID: integer;
      beghost: boolean);
    //use -1 as id to free selectedItem gizmo
    procedure freeModelInstance(itemID:integer;isGhost:boolean);
    procedure removeModelInstance(itemID:integer;isGhost:boolean);
    function getModelInstance(itemID: integer; beghost: boolean): TModelInstance;
    destructor destroy;override;
    constructor create;
  end;

var
  meshManager:TMeshManager;

implementation

{ TModelInstance }

constructor TModelInstance.create(modelIndex: integer);
begin
  texFront:=modelIndex;
  mvp:=IdentityHmgMatrix;
  rotationM:=IdentityHmgMatrix;
  scaleM:=IdentityHmgMatrix;
  translateM:=IdentityHmgMatrix;
  dirty:=true;
end;

{ TMeshManager }

procedure TMeshManager.loadModelsFromFolder(const path: string);
var
   str:tstringlist;
   i:integer;
   c,j:cardinal;
   b:rBufferDataBit;
   v,f:cardinal;
   s:string;
   st:TFilestream;
begin
  if initialized then begin
     for i:=0 to length(models)-1 do models[i].Free;
  end;
  str:=findallfiles(path,'*.vbo',true);
  setlength(models,str.count);
  for i:=0 to str.count-1 do begin
      //load data to buffer and run model through it
      st:=tfilestream.create(str[i],fmOpenRead);
      takeCareOfDataBufferSize(st.Size+256);
      c:=0;j:=0;
      repeat
        st.ReadBuffer(b,sizeof(b));
        c+=sizeof(b);
        chunkVertexBuffer[j]:=b;
        inc(j);
      until c>=st.Size;
      models[i]:=TModel.create(@chunkVertexBuffer,c);
      s:=extractfilename(str[i]);
      models[i].name:=copy(s,1,length(s)-4);
      st.free;
  end;
  str.free;
  initialized:=true;
end;

procedure TMeshManager.render;
var
   i:integer;
begin
  if ghostInstances.count>0 then begin
     glPolygonMode(GL_FRONT, GL_LINE);
     for i:=0 to ghostInstances.count-1 do with ghostInstances.data[i] do begin
         if dirty then begin
            MatrixMultiply(scaleM,rotationM,transformM);
            MatrixMultiply(transformM,translateM,transformM);
            dirty:=false;
         end;
         //if worldMoved then
         MatrixMultiply(transformM,worldCameraMatrix,mvp);
         models[texFront].render(@mvp,@transformM);
     end;
     glPolygonMode(GL_FRONT, GL_FILL);
  end;
  for i:=0 to instances.count-1 do with instances.data[i] do begin
      if dirty then begin
         MatrixMultiply(scaleM,rotationM,transformM);
         MatrixMultiply(transformM,translateM,transformM);
         dirty:=false;
      end;
      //if worldMoved then
      MatrixMultiply(transformM,worldCameraMatrix,mvp);
      models[texFront].render(@mvp,@transformM);
  end;
end;

function TMeshManager.getModelByName(const name: string): TModel;
var i:integer;
begin
  result:=nil;
  for i:=0 to length(models)-1 do if models[i].name=name then begin
      result:=models[i];
      break;
  end;
end;

function TMeshManager.addModelInstance(itemID: integer; itemType: eItemTypes;
         beGhost:boolean;position, rotation: TVector3f): TModelInstance;
var i,j:cardinal;
   mt:TModelInstance;
begin
  j:=length(models)-1;
  for i:=0 to j do if models[i].itemType=itemType then begin
     mt:=TModelInstance.create(i);
     mt.setRotationXYZ(rotation[0],rotation[1],rotation[2]);
     mt.translate(position);
     mt.scale:=models[i].scale;
     //worldMoved:=true;//wtf hack
    // instances[j].setRotationXYZ(rotation[0],rotation[1],rotation[2]);
     result:=mt;
     if not beghost then instances.add(itemID,mt)
      else ghostInstances.add(itemID,mt);
     exit;
  end;
  log('achting! TMeshManager.addModelInstance:: model not found: '+EnumToStrIng(typeinfo(eItemTypes),integer(itemType)));
end;

procedure TMeshManager.addModelInstance(modelInstance: TModelInstance;itemID:integer;
  beghost: boolean);
begin
  if beghost then ghostInstances.Add(itemID,modelInstance) else
    instances.Add(itemID,modelInstance);
end;

procedure TMeshManager.freeModelInstance(itemID: integer; isGhost: boolean);
begin
  if isGhost then begin
     ghostInstances[itemID].free;
     ghostInstances.Remove(itemID);
  end else begin
      instances[itemID].free;
      instances.remove(itemID);
  end;
end;

procedure TMeshManager.removeModelInstance(itemID: integer; isGhost: boolean);
begin
  if isGhost then begin
     ghostInstances.Remove(itemID);
  end else begin
      instances.remove(itemID);
  end;
end;

function TMeshManager.getModelInstance(itemID: integer; beghost: boolean
  ): TModelInstance;
begin
  if beghost then result:=ghostInstances[itemID] else
     result:=instances[itemID];
end;

destructor TMeshManager.destroy;
var i:integer;
begin
  for i:=0 to length(models)-1 do models[i].free;
  for i:=0 to instances.count-1 do instances.data[i].free;
  instances.free;
  for i:=0 to ghostInstances.count-1 do ghostInstances.data[i].free;
  ghostInstances.free;
end;

constructor TMeshManager.create;
begin
  instances:=TModelInstanceList.create;
  ghostInstances:=TModelInstanceList.create;
end;


end.

