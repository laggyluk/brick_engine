unit core_render;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fgl,dglOpenGL,vectorGeometry,core_material_library,core_types,
  core_camera,core_utils,geometryBB, core_matrixStack,renderable_object_types,
  core_gBuffer,core_lights,VectorTypes,core_block_brush, core_dynamic_object,core_texture,
  core_orders_manager, core_mesh, core_listofrecords, core_job, core_ui, core_bullets,
  core_orders, core_player, core_actor_definitions,core_chunk;

const
  //how many texture pixels are a width of block. for setting quad size
  pixelsPerBlock = 16;

type

  //indexes of chunks visible after culling are stored together with distance to cam. for hittest
  rDistanceToCam = record
    chunkPointer:PChunk;
    distanceToCam:single;
  end;

  //list of objects marked for rendering
  //TVisibleObjsList = specialize TFPGList<TVisualObject>;
  TTerrainChunkList = specialize TFPGList<TTerrainChunk>;
  TJobAreasList = specialize TFPGMap<integer,TJob>;
  { TRenderer }

  { TODO : nie rysowac obiektow za plecami kamery }
  TRenderer = class
   private
    //visualize blocks belonging to currently edited brush/limb
    procedure renderBrush(brush:pBlockBrush);
    procedure setJobSelection(job:tjob;v1,v2:TAffineVector);
   public
    frameCounter:integer;
    //renderables are sorted by their material type and added to list utilising this material type
    renderListChunks: TTerrainChunkList;
    //for rendering bilboard sprites
    //jobs selection areas
    jobAreas:TJobAreasList;
    flatPplQuad:TTexturedQuad;
    unitsAtlas:TTextureAtlas;
    actorsMap:TFlatUnitRenderableMap;
    bullets:TBullets;
    //selection cube
    selectionCube:TSelectionCube;
    //selected 'dynamicobj' brush
    dynamicObjBrush:TDynamicObject;
    //if not nil then points to currently edited character limb. for outline
    memberLimb:TBlockBrush;
    //active blocks brush gizmo for rendering. created&destroyed by toolbar!
    quadFullScreen:TQuad;//dir light's full screen quad
    pointLightCube:TPointLightCube;//
    //forward lights
    spotLightsF:TSpotLights; //0..max_spot_ligths
    pointLightsF:TPointLights; //0..max_pointLights
    //deferred lights lists
    pointLightsDS:TListOfPointLights;
    dirLight:TDirectionalLight;//?o rly?
    blockBrush: TBlockBrush;
    gizmos:record
      lights:boolean;
      brush:boolean;
      memberLimbBrush:boolean;
      selectionCube:boolean;
    end;
    //when true, skip level rendering and show some image, console msgs or sth
    showLoadingScreen:boolean;
    procedure setBrush(brush:pBlockBrush);
    procedure calcMatrices;
    procedure render;
    procedure forwardRender;
    //point lights pass for deferred mode
    procedure renderPointLights;
    procedure DeferredRender;
    procedure DSGeometryPass;
    procedure DsSsaoPass;
    procedure DSUnitsPass;
    procedure DSStencilPass(PointLightIndex:integer);
    procedure DSPointLightPass(PointLightIndex:integer);
    procedure DSDirectionalLightPass;
    procedure reshape(w,h:integer);
    procedure calcCulling;
    //returns count of lights ignoring ones created by bullets and shit
    function getLevelLightsCount:integer;
    procedure add(obj:TTerrainChunk);//ads object to render list fitting it's material
    constructor create;
    procedure init(w,h:integer;deferred:boolean);
    destructor destroy;override;
    //interface procedures?
    function setDirectionalLight(col,dir:TAffineVector;ambientIntens,diffuseIntens:single):boolean;
    function addPointLight:TPointLight;
    procedure addPointLight(light:tpointlight);
    procedure removePointLight(light:TPointLight);
    procedure removeLights;
    //don't move around with cursor
    procedure renderBrushInPlace(brush:pBlockBrush);
    procedure update;//process the orders
    //creates actor instance for rendering
    function createActorInstance(actorID,typ:integer;position:TVector3f;size:TAffineVector):TFlatUnitRenderable;
    procedure saveLights(filename:string);
    procedure loadLights(filename:string);
  end;


var
    renderer:TRenderer;
    flagLoaded:boolean;
implementation

procedure TRenderer.calcMatrices;
var
   up: TVector = (0,1,0,0);//to check w=0/1
begin
    camera.worldToCameraMatrix:=CreateLookAtMatrix(vectorMake(camera.Position,1),vectormake(camera.Target,1),up);
    matrixMultiply(camera.worldToCameraMatrix,camera.projectionMatrix,worldCameraMatrix);
    //modelMatrix:=IdentityHmgMatrix;
    //MatrixMultiply(cameraMatrix,modelMatrix,mvpMatrix); //bug? kolejnosc mnozenia
    //mvpMatrix:= MatrixMultiply(mvpMatrix,projectionMatrix);
    //setcursorpos(left+panel1.Height div 2,top+panel1.width div 2);
end;


procedure TRenderer.renderPointLights;
var
 i,j:integer;
 s:string;
 l:tpointlight;
 mvp:tmatrix;
begin
  matlib.shaderCubeLightDS.use;
  //transpose the light cube through iterating lights
  //set eye pos
  //  z jakiegos zjebanego powodu ten uniform nie jest zainicjowany
  glUniform3f(matlib.shaderCubeLightDS.m_eyeWorldPosLocation, camera.position[0],
                                                            camera.position[1],
                                                            camera.position[2]);
  for i:=0 to pointLightsDS.Count-1 do begin
    l:= pointLightsDS[i];
    //light position is cached to save time on calculating matrix transform each time
    //cube is translated
    //setup light
    matlib.shaderCubeLightDS.setlight(@l);
    if (not l.dirty) and (not worldMoved) then begin
      pointLightCube.transformM:=l.transform; //use saved transform
      pointLightCube.render(false);
    end
    else begin //let the cube calculate new transform
      l.dirty:=false;
      pointLightCube.scale:=pointLightsDS[i].size;
      pointLightCube.position:=pointLightsDS[i].position;
      //mvp:=cameraToClipMatrix * worldToCameraMatrix * modelToWorldMatrix
      MatrixMultiply(pointLightCube.scaleM,pointLightCube.translateM,pointLightCube.transformM);
      MatrixMultiply(pointLightCube.transformM,worldCameraMatrix,l.transform);
      pointLightCube.transformM:=l.transform;
      pointLightCube.render(false);
     //l.transform:=matrixMultiply(pointLightCube.transformM; //save transform in light
    end;
  end;
end;

procedure TRenderer.reshape(w, h: integer);
var
 i:integer;
begin
  //w:=w -250;
  //h:=h-100;
  camera.CreatePerspectiveM(camera.fov, (w/ single(h)), 0.1, camera.zFar);
  glViewport(0, 0, w, h);
  viewport[2]:=w;
  viewport[3]:=h;
  viewport[0]:=w div 2;
  viewport[1]:=h div 2;
  gbuffer.init(w,h);

  matlib.resize;
  glUseProgram(0);
  camera.didMove:=true;
  calcCulling;
  glScissor(0,0,w,h);
end;

procedure TRenderer.calcCulling;
var
 x,y,i,index:integer;
 v,min,max,c,dir:TAffineVector;
 ch:tchunk;
 tempR:rDistanceToCam;
 m,matMVP: TMatrix;
 frustum : TFrustum;
 ft:tfrust;
 box:TAABB;
 dist:single;
 p1,p2,p3,point:tvector2f;
begin
  calcMatrices;
  worldMoved:=true;

 // if options.renderer.ssao then begin //hack. ssao i frustum się jebią w paszcze
 {
    for y:=0 to active_chunks_h-1 do
      for x:=0 to active_chunks_w-1 do
        chunksPointer^[x,y].renderable.isVisible:=true;
  exit;}
 // end;
  {
  //this method checks if chunk center lies within triangle created from camera
  //position to some distane towards horizon/(target?)
  dist:=camera.zFar-camera.zNear;
  VectorCrossProduct(camera.position,camera.target,v);
  NormalizeVector(v);
  ScaleVector(v,150); //szerokosc 'widzialnego horyzontu'
  VectorSubtract(camera.target,camera.position,dir);
  NormalizeVector(dir);
  VectorScale(dir,150,dir);//odleglosc plaszczyzny odciecia od kamery
  Vectoradd(dir,v,min);
  VectorSubtract(dir,v,max);
  VectorScale(dir,0.2,dir);
  VectorSubtract(camera.position,dir,dir);
  p1:=vector2fmake(dir[0],dir[2]);
  p2:=vector2fmake(min[0],min[2]);
  p3:=vector2fmake(max[0],max[2]);
  for y:=0 to active_chunks_h-1 do
    for x:=0 to active_chunks_w-1 do begin
      ch:=chunksPointer^[x,y];
      //p1[0]:=camera.position[0];p1[1]:=camera.position[2];
      //2nd and 3rd point should lay on line prostopadla do camera-dir
      //VectorSubtract(camera.target,camera.position,v);
      //NormalizeVector(v);
      //ScaleVector(v,camera.zFar-camera.zNear);
      if pointInTriangle(p1,p2,p3,
            vector2fmake(ch.center[0],ch.center[2])) then ch.renderable.isVisible:=true
      else ch.renderable.isVisible:=false;
    end;

  exit;
  }
  //this method calculates if chunk hitbox lies within frustrum 'mesh'
  //hitboxes/centres may be fucked up. also might be slower than above method?
//  frustum:=ExtractFrustumFromModelViewProjection(worldCameraMatrix);
  //iterate through terrain chanks and check if in frustrum

  frustum:=ExtractFrustumFromModelViewProjection(worldCameraMatrix);
  for y:=0 to active_chunks_h-1 do
    for x:=0 to active_chunks_w-1 do begin
      ch:=chunksPointer^[x,y];
      ch.renderable.isVisible:=false;
      //distance check should be 'embedded' in frustum already
     // if VectorDistance(camera.position,ch.renderable.position)>camera.zFar then continue;
      m:=ch.renderable.translateM;
      //m:=ch.renderable.transformM;
      MatrixMultiply(m,camera.worldToCameraMatrix,matMVP);
      MatrixMultiply(matMVP, camera.projectionMatrix,matMVP);
      box.min:=VectorSubtract(ch.renderable.position, ch.hitBoxSize);
      box.max:=VectorAdd(ch.renderable.position, ch.hitBoxSize);
      box.max[1]:=camera.position[1];
      if FrustumContainsAABB(frustum,box)=scNoOverlap then
         ch.renderable.isVisible:=false
      else
         ch.renderable.isVisible:=true;
    end;
end;

function TRenderer.getLevelLightsCount: integer;
var i:integer;
begin
  result:=0;
  for i:=0 to pointLightsDS.count-1 do
      if pointLightsDS[i].staticLight then inc(result)
end;

procedure TRenderer.add(obj: TTerrainChunk);
begin
  if renderListChunks=nil then renderListChunks:=TTerrainChunkList.Create;
  renderListChunks.Add(obj);
end;


procedure TRenderer.init(w,h:integer;deferred:boolean);
var
  l:tpointLight;
begin
  //setup gl state
  glClearColor (0.0,0.0,0.0,0) ; // background clear color
  glCullFace(GL_BACK);
  glFrontFace(GL_CCW);
 // glPointSize(20);
  glEnable( GL_CULL_FACE ) ;            // back face culling enable

  //glShadeModel(GL_SMOOTH)
  glEnable(GL_DEPTH_TEST);
  glDepthMask(bytebool(GL_TRUE));
  glClearDepth(1.0);
  glDepthRange(0.0, 1.0);
  glEnable(GL_SCISSOR_TEST);
  glStencilOpSeparate(GL_BACK, GL_KEEP, GL_INCR, GL_KEEP);
  glStencilOpSeparate(GL_FRONT, GL_KEEP, GL_DECR, GL_KEEP);

  selectionCube:=TSelectionCube.create;

  unitsAtlas:=TTextureAtlas.create(GL_TEXTURE_2D);
  unitsAtlas.load(apppath+'textures\units.atlas');
  //w zasadzie to one quad to rule them all wiec powinna istniec jakas insza struktura
  //do reprezentacji poszczegonych jednostek. no ale to pozniej
  matlib.shaderFlatPpl.use;
  flatPplQuad:=TTexturedQuad.create(unitsAtlas.getIndexOf('pod'));
  //flatPplQuad.translate(0,-220,0);
  //set tex size. should be done for each different one at render time..
  glUniform2f(matlib.shaderFlatPpl.texSize,round(unitsAtlas.texture.img.png.Width),round(unitsAtlas.texture.img.png.height));

  //i guess that it should load render options from ini here and then

  gbuffer:=Tgbuffer.Create;
  gbuffer.init(w,h);
  quadFullScreen:=TQuad.create;
  pointLightCube:=TPointLightCube.create;
  matlib.resize;
  //add default directional light
  dirLight:=TDirectionalLight.create(
                  vector3fmake(1,1,1),
                  vectornormalize(vector3fmake(0.3,-1.0,2.0)),
                  0.4,
                  0.2) ;
  gizmos.selectionCube:=true;
end;


constructor TRenderer.create;
begin
  //camera:=tcamera.create;
  jobareas:=TJobAreasList.create;
  matrixStack:=TMatrixStack.create;
  pointLightsDS:=TListOfPointLights.create;
//  dirLight:=TListOfDirLights.Create;
  worldCameraMatrix:=IdentityHmgMatrix;
  actorsMap:=TFlatUnitRenderableMap.Create;
  //blockBrush:=TBlockBrush.create;
  bullets:=TBullets.create;
end;

//not used actually
destructor TRenderer.destroy;
var i,j:integer;
begin
  //for i:=0 to bullets.Count-1 do bullets.data[i].Destroy;
  bullets.free;
  //leaks in editor mode, doesn't in game mode
  //if editor<>nil then if blockBrush<>nil then blockBrush.free;
  if selectionCube<>nil then selectionCube.destroy;
  if pointLightCube<>nil then pointLightCube.destroy;
  if quadFullScreen<>nil then quadFullScreen.destroy;
  matrixStack.destroy;
  //camera.destroy;

  for i:=0 to pointLightsDS.Count-1 do pointLightsDS[i].Destroy;
  pointLightsDS.Destroy;
  dirLight.Destroy;
  for i:=0 to length(pointLightsF)-1 do if pointLightsF[i]<>nil then pointLightsF[i].Destroy;
  for i:=0 to length(spotLightsF)-1 do if spotLightsF[i]<>nil then spotLightsF[i].Destroy;
  if gbuffer<>nil then gbuffer.destroy;
  if flatPplQuad<>nil then flatPplQuad.destroy;
  if unitsAtlas<>nil then unitsAtlas.destroy;
  for i:=0 to actorsMap.Count-1 do actorsMap.Data[i].Destroy;
  actorsMap.Destroy;
  for i:=0 to jobAreas.Count-1 do jobAreas.Data[i].destroy;
  jobareas.destroy;
  renderListChunks.Free;
  //for i:=0 to length(renderLists)-1 do begin
  //  renderlists[i].list.destroy;
  //end;
  inherited;
end;

function TRenderer.setDirectionalLight(col, dir: TAffineVector; ambientIntens,
  diffuseIntens: single): boolean;
begin
  dirLight.setUp(col,dir,ambientIntens,diffuseIntens);
end;

function TRenderer.addPointLight:TPointLight;
begin
  result:=pointLightsDS[pointLightsDS.Add(tpointlight.Create(vector3fmake(1,1,1),
                        vector3fmake(0,0,0),
                        2.5,0.0,0.0,1.0))];
  if result=nil then log('Achtung! TRenderer.addPointLight failed!');
end;

procedure TRenderer.addPointLight(light: tpointlight);
begin
  pointlightsDS.Add(light);
end;

procedure TRenderer.removePointLight(light:TPointLight);
begin
  if light=nil then begin
    log('achtung! Renderer.removePointLight:: null light');
    exit;
  end;
  pointLightsDS.Remove(light);
  freeandnil(light);
end;

procedure TRenderer.removeLights;
var p:TPointLight;
begin
  for p in pointLightsDS do p.destroy;
  pointLightsDS.Clear;
end;

procedure TRenderer.DSGeometryPass;
var
  wtf:TTerrainChunk;
  i:integer;
  m:tmatrix;
Begin
    matlib.shaderChunkDS.use;
    gbuffer.BindForGeomPass;
    // Only the geometry pass updates the depth buffer
    glDepthMask(bytebool(GL_TRUE));
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glEnable(GL_DEPTH_TEST);
    //glUniform1i(matlib.shaderChunkDS.ghostUnif,integer(false));
    for i:=0 to renderListChunks.Count-1 do
       if renderListChunks[i].isVisible then
              renderListChunks[i].render;
    //and now dynamic objects
    //note. that sort of wirefame rendering is obsolete..

    meshManager.render;

    if dynamicObjBrush<>nil then dynamicObjBrush.render;

                    {
    m:=selectionCube.transformM;
    //selectionCube.transformM:=TBlasterShot.transform;
      for i:=0 to bullets.Count-1 do begin
        selectionCube.transformM:=bullets[i].transform;
        if not VectorEquals(selectionCube.color,bullets[i].color) then
               selectionCube.color:=bullets[i].color;
        selectionCube.render;
      end;
    selectionCube.transformM:=m;  }
    // When we get here the depth buffer is already populated and the stencil pass
    // depends on it, but it does not write to it.
   // glDepthMask(bytebool(GL_FALSE));
end;

procedure TRenderer.DsSsaoPass;
begin
  matlib.shaderSSAO.use;
  gbuffer.BindForSsaoPass;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  //should switch write target for later blending?
  quadFullScreen.render;
end;

procedure TRenderer.DSUnitsPass;
var
  i,ind:integer;
  ang,ang2,f:single;
  pos,pos2:TAffineVector;
  actr:TFlatUnitRenderable;
begin
   // glDepthMask(bytebool(GL_TRUE));
    //draw same tex on both sides
    glDisable( GL_CULL_FACE ) ;
    matlib.shaderFlatPpl.use;
    unitsAtlas.Bind(GL_TEXTURE0);
    //setup texture size for different atlases
    glUniform2f(matlib.shaderFlatPpl.texSize,round(unitsAtlas.texture.img.png.Width),round(unitsAtlas.texture.img.png.height));

    //set uniforms.
    flatPplQuad.mvpLocation:=matlib.shaderFlatPpl.MVP;
    flatPplQuad.worldMatricLocation:=matlib.shaderFlatPpl.m_WorldMatrixLocation;
    ang:=arctan2(camera.target[2] - camera.position[2], camera.target[0] - camera.position[0]);
    ang:=pi_half - ang;
    //ang:=arctan2(camera.position[0]*camera.target[2] - camera.position[2]*camera.target[0], camera.position[0]-camera.target[0] + camera.position[2]*camera.target[2]);
    pos:=VectorSubtract(camera.position,camera.target);
    for i:=0 to actorsMap.Count-1 do begin
      actr:=actorsMap.data[i];
      //if actorsMap.data[i].actorID<>player1.actorID then continue;
      //mozna by miec w atlasie teksture dla boku i tylu tez
      //sprawdzajac roznice kata miedzy soba a graczem  i wybrac ktora tex wyswietlic
      //pos2:=VectorSubtract(actorsMap.data[i].position,camera.target);
      //a.x*b.y - a.y*b.x, a.x*b.x + a.y*b.y
      //ang:=arctan2(pos[0]*pos2[2] - pos[2]*pos2[0], pos[0]-pos2[0] + pos[2]*pos2[2]);
      //ang:=arctan2(pos2[2] - pos[2], pos2[0] - pos[0]);
      //ang2:=0;
      ang2:=actorsMap.data[i].getYAngle;
      //odjac od obydwu target i wtedy sprawdzic kat
      f:=DistanceBetweenAngles(ang2,ang);
      //log(format('%f %f %f',[ang,ang2,radtodeg(f)]));
      //if globalFrame mod 30 = 0 then log(floattostr(ang));
      ind:=actr.texFront ;
      if f < pi_half  then ind:=actr.texBack;
      {if (f > 1) and (f<2) then begin
         ind:= actr.texSide;
         ang2:=ang2+degtorad(90);
      end;}
      //ind:=actorsMap.data[i].texFront;
      glUniform4f(matlib.shaderFlatPpl.texRect
                                   ,unitsAtlas.data[ind].x
                                   ,unitsAtlas.data[ind].y
                                   ,unitsAtlas.data[ind].w
                                   ,unitsAtlas.data[ind].h);
      actorsMap.data[i].update(ang2,ind=actr.texSide);
      flatPplQuad.mvp:=actorsMap.data[i].mvp;
      flatPplQuad.transformM:=actorsMap.data[i].transformM;
      //jeszcze niech sie resaizuje quad do dozmairu textury?
      flatPplQuad.render;
    end;
    glEnable(GL_CULL_FACE);

 glDepthMask(bytebool(GL_FALSE));
end;

procedure TRenderer.DSStencilPass(PointLightIndex:integer);
var
  l:TPointLight;
  i:integer;
begin
    matlib.shaderNullDS.use;
    // Disable color/depth write and enable stencil
    gbuffer.BindForStencilPass;
    glEnable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);
    glClear(GL_STENCIL_BUFFER_BIT);
    // We need the stencil test to be enabled but we want it
    // to succeed always. Only the depth test matters.
    glStencilFunc(GL_ALWAYS, 0, 0);

    //light properties doesn't need to be setup for null pass so just set box position and scale
    l:= pointLightsDS[PointLightIndex];
    //light position is cached to save time on calculating matrix transform each time
    if (not l.dirty) and (not worldMoved) then begin
      pointLightCube.transformM:=l.transform; //use saved transform
      pointLightCube.render(true);
    end
    else begin //let the cube calculate new transform
      l.dirty:=false;
      pointLightCube.scale:=l.size;
      pointLightCube.position:=l.position;
      //mvp:=cameraToClipMatrix * worldToCameraMatrix * modelToWorldMatrix
      MatrixMultiply(pointLightCube.scaleM,pointLightCube.translateM,pointLightCube.transformM);
      MatrixMultiply(pointLightCube.transformM,worldCameraMatrix,l.transform);
      pointLightCube.transformM:=l.transform;
      pointLightCube.render(true);
     //l.transform:=matrixMultiply(pointLightCube.transformM; //save transform in light
    end;
end;

procedure TRenderer.DSPointLightPass(PointLightIndex:integer);
var
  l:TPointLight;
begin
    gbuffer.BindForLightPass;
    matlib.shaderCubeLightDS.use;
    glUniform3f(matlib.shaderCubeLightDS.m_eyeWorldPosLocation, camera.position[0],
                                                              camera.position[1],
                                                              camera.position[2]);
    glStencilFunc(GL_NOTEQUAL, 0, $00FF);

    glDisable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glBlendEquation(GL_FUNC_ADD);
    glBlendFunc(GL_ONE, GL_ONE);

    glEnable(GL_CULL_FACE);
    glCullFace(GL_FRONT);
    //setup light
    l:=pointLightsDS[PointLightIndex];
    matlib.shaderCubeLightDS.setlight(@l);
    pointLightCube.transformM:=l.transform;
    pointLightCube.render(false);
    glCullFace(GL_BACK);
    glDisable(GL_BLEND);
end;

procedure DSFinalPass;
begin
    gbuffer.BindForFinalPass;
    glBlitFramebuffer(0, 0, viewport[2], viewport[3],
                      0, 0, viewport[2], viewport[3], GL_COLOR_BUFFER_BIT, GL_LINEAR);
end;

procedure TRenderer.DSDirectionalLightPass;
var
  i:integer;
begin
  gbuffer.BindForLightPass;
  matlib.shaderQuadDS.use;
  glUniform3f(matlib.shaderQuadDS.m_eyeWorldPosLocation, camera.position[0],
                                                          camera.position[1],
                                                          camera.position[2]);
  matlib.shaderQuadDS.setLight(@dirLight);
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBlendEquation(GL_FUNC_ADD);
  glBlendFunc(GL_ONE, GL_ONE);

  quadFullScreen.render;//err?

  glDisable(GL_BLEND);
end;

procedure TRenderer.renderBrush(brush:pBlockBrush);
var
  i:integer;
  pos,offset:TAffineVector;
  d:tbrushData;
begin
   if brush^.data.count=0 then exit;
   pos:=selectionCube.position;
   offset:=VectorSubtract(pos,brush^.data[0].position);
   offset[0]:=-offset[0]; offset[1]:=-offset[1]; offset[2]:=-offset[2];
   for d in brush^.data do if d.typ<>btNone then
   begin
       selectionCube.setPosition(VectorSubtract(d.position,offset));
       selectionCube.render;
   end;
   //selectionCube.position:=pos;
end;

procedure TRenderer.setJobSelection(job: tjob; v1, v2: TAffineVector);
var size:TAffineVector;
begin
  // log(format('size x: %f y: %f z: %f',[size[0],size[1],size[2]]));
    job.v1:=v1;
    job.v2:=v2;
    job.m:=IdentityhmgMatrix;
    vectorAdd(v2,vector3fmake(1,0,1),v2);
    v1[1]-=1;
    size:=VectorSubtract(v2,v1);
    //size:=VectorAbs(size);
    TranslateMatrix(job.m,vectoradd(v1,vectorAdd(vectorScale(size,0.5),vector3fMake(-0.5,0.5,-0.5))));
    job.m[0,0]:=v2[0]-v1[0];
    job.m[1,1]:=v2[1]-v1[1];
    job.m[2,2]:=v2[2]-v1[2];
end;

procedure TRenderer.setBrush(brush: pBlockBrush);
begin
  blockBrush:=brush^;
  gizmos.brush:=true;
//  gizmos
end;

procedure TRenderer.renderBrushInPlace(brush:pBlockBrush);
var
  i:integer;
  pos,sel,offset:TAffineVector;
begin
   if (brush=nil) or (brush^.data.count=0) then exit;
   pos:=brush^.anchorPoint;
   sel:=selectionCube.position;
   offset:=VectorSubtract(pos,brush^.data[0].position);
   offset[0]:=-offset[0]; offset[1]:=-offset[1]; offset[2]:=-offset[2];
   for i:=0 to brush^.data.count-1 do if brush^.data[i].typ<>btNone then
   begin
       selectionCube.setPosition(VectorSubtract(brush^.data[i].position,offset));
       selectionCube.render;
   end;
   selectionCube.position:=sel;
end;

procedure TRenderer.DeferredRender;
var
  i:integer;
  dir:TDirectionalLight;
  transformM,scaleM,rotationM,translateM:tmatrix;
  pos,offset:TAffineVector;
  job:tjob;
  jt:eJobTypes;
begin
  gbuffer.StartFrame;
  DSGeometryPass;
  //render flat people
  DSUnitsPass;
  if options.renderer.ssao then DsSsaoPass;
  // We need stencil to be enabled in the stencil pass to get the stencil buffer
  // updated and we also need it in the light pass because we render the light
  // only if the stencil passes.
  glEnable(GL_STENCIL_TEST);
  for i:=0 to pointLightsDS.count-1 do begin
      DSStencilPass(i);
      DSPointLightPass(i);
  end;
  // The directional light does not need a stencil test because its volume
  // is unlimited and the final pass simply copies the texture.
  glDisable(GL_STENCIL_TEST);
  DSDirectionalLightPass;
  matlib.shaderSelectionCube.use;
  selectionCube.color:=colorSelection;
  if gizmos.selectionCube then selectionCube.render;
  transformM:=selectionCube.transformM;
  scaleM:=selectionCube.scaleM;
  translateM:=selectionCube.translateM;
  selectionCube.dirty:=false;
 // rotationM:=selectionCube.rotationM;
  pos:=selectionCube.position;
  //enable reading from depthbuffer
  //glReadBuffer(GL_COLOR_ATTACHMENT3);
  glEnable(GL_DEPTH_TEST);
  glDepthMask(bytebool(GL_true));
  //blaster bullets //with blur?

  for i:=0 to bullets.Count-1 do if bullets.data[i].active then begin
    selectionCube.transformM:=bullets.data[i].transformM;
    //selectionCube.position:=bullets.data[i].position;
    //if not VectorEquals(selectionCube.color,bullets[i].color) then
    selectionCube.color:=bullets.data[i].color;
    selectionCube.dirty:=false;
    selectionCube.render;
  end;
  //selectionCube.transformM:=transformM;
  glDisable(GL_DEPTH_TEST);
  DSFinalPass;
  selectionCube.color:=colorGray;
  //everything past this point can't be depth tested
  //render lights gizmos?                                 h
  if gizmos.lights then begin
       //selectionCube.transformM:=transformM;
       selectionCube.scaleM:=scaleM;
       //selectionCube.translateM:=translateM;
     for i:=0 to pointLightsDS.Count-1 do begin
         selectionCube.setPosition(pointLightsDS[i].position);
         selectionCube.render;
     end;
  end;
  //render active brush 'outline'?
  glEnable(GL_BLEND);
  if (gizmos.brush) or (gizmos.memberLimbBrush) then begin
     if (gizmos.brush) and (blockBrush<>nil) then renderBrush(@blockBrush);
     if (gizmos.memberLimbBrush) then renderBrushInPlace(@memberLimb);
  end;
  //render job selections

  if jobAreas.Count>0 then begin
    //selectionCube.color:=colorJobArea;
     jt:=jtMax;
    for i:=0 to jobAreas.count-1 do  begin
      if jobAreas.data[i].active then begin
        if jt<>jobAreas.data[i].typ then begin
           selectionCube.color:=jobColors[jobAreas.data[i].typ];
           jt:=jobAreas.data[i].typ;
        end;
        selectionCube.transformM:=jobAreas.data[i].m;
        //selectionCube.dirty:=true;
        selectionCube.render;
      end;
    end;
  end;

  selectionCube.color:=vector4fmake(0.2,0.5,0.3,1);
  selectionCube.setPosition(0,-50,0);
  selectionCube.transformM:=transformM;
  selectionCube.scaleM:=scaleM;
  selectionCube.translateM:=translateM;


  //render ui
  //glBlendFunc(GL_ONE,GL_SRC_ALPHA);//DUNNO, could experiment to find proper blending for translucent ui
  glDisable(GL_BLEND);
  uiManager.render;

  //glDisable(GL_BLEND);
end;

procedure TRenderer.render;
var
   i,j:integer;
   s:string;
   dir:TDirectionalLight;
begin
  //deferred mode?
  //if Options.renderer.deferred then
  frameCounter:=frameCounter+1;
  if showLoadingScreen then begin
    //show some image
  end else
      DeferredRender;
  //else forwardRender;
  if worldMoved then worldMoved:=false;
  glUseProgram(0);
end;

procedure TRenderer.forwardRender;
var
   i:TTerrainChunk;
begin

  matlib.shaderChunkF.use;
  matlib.shaderChunkF.setDirectionalLight(@dirLight);
  matlib.shaderChunkF.SetPointLights(2,@pointLightsF);
  matlib.shaderChunkF.SetSpotLights(2,@spotLightsF);
  matlib.shaderChunkF.SetEyeWorldPos(camera.position);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
//glClear(GL_COLOR_BUFFER_BIT);
  for i in renderListChunks do
     if i.isVisible then
             i.render();

  matlib.shaderSelectionCube.use;
  selectionCube.render;
end;

procedure TRenderer.update;
var
  i,index:integer;
  u:TFlatUnitRenderable;
  cmd:rorder;
  job:tjob;
  b:rBulletData;
  pb:pBulletData;
  f:single;
  l:TPointLight;
  wt:tvector3f;
  crateID:integer;
begin
 for i:=0 to oM.queue.Count-1 do begin
  cmd:=oM.queue[i];
  if cmd.eatenFlags.eatenRender then continue;
  with cmd do  begin
  cmd.eatenFlags.eatenRender:=true;
  //don't process if actor doesn't exist and it's not his creation order
  { TODO -copti : this shouldn't be a general check but in each command as they are made in such way that map search will be done twice }
 // if (actorsMap.IndexOf(cmd.int1)=-1) and (cmd.order<>orCreateActor) then exit;
  case order of
    //add an actor with given asset id
    orCreateActor: begin
      if options.debug.orders then log('render unit created id: '+inttostr(int1));
      createActorInstance(int1,int2,v1,v2);
    end;
    orMoveActor:begin
     //it should be actorID,unit map not list
      u:=actorsMap[int1];
      u.setPosition(v1);
      if u.crateActorID<>-1 then
        //offset a bit to not overlap and draw a little higher than actors feet
        actorsMap[u.crateActorID].setPosition(v1[0],v1[1]+1,v1[2]-0.01);
    end;
    orRotateActor: begin
      u:=actorsMap[int1];
      u.setRotationXYZ(v1[0],v1[1],v1[2],f1);
    end;
    orRotateActorY: begin
      u:=actorsMap[int1];
      u.setRotationY(f1);
      if u.crateActorID<>-1 then actorsMap[u.crateActorID].setRotationY(f1);
    end;
    orScaleActorXYZ:begin
      u:=actorsMap[int1];
      u.scaleY(v1[1]);
      u.scaleX(v1[0]);
    end;
    //create scaled cube that marks selection area
    orCreateJobArea: begin
      job:=tjob.create;
      job.id:=cmd.int2;
      job.active:=true;
      job.typ:=ejobtypes(cmd.int1);
      setJobSelection(job,v1,v2);
      jobAreas.Add(job.id,job);
    end;
    orModifyJobArea:begin
      job:=jobAreas[cmd.int1];
      if cmd.int2=0 then setJobSelection(job,cmd.v1,job.v2);
      if cmd.int2=1 then setJobSelection(job,job.v1,cmd.v1);
      if cmd.int2=2 then setJobSelection(job,cmd.v1,cmd.v2);
    end;
    orWorkStarted:begin
      //start animation of some sort
    end;
    orWorkComplete:begin
      //remove job visuals and job from list
      job:=jobAreas[int2];
      jobAreas.Remove(int2);
      job.Destroy;
    end;
    orRemoveJobArea:begin
      //remove job visuals and job from list
      job:=jobAreas[int1];
      jobAreas.Remove(int1);
      job.Destroy;
    end;
    orBlasterShot:begin
//      b:=TBlasterShot.create;
      b.create(int3);
      b.ID:=int2;
      b.ownerId:=int1;
      b.position:=v1;
      b.velocity:=v2;
      b.orient;
      //create dynamic light
      l:=b.createLight(v3);
      l.staticLight:=false;
      addPointLight(l);
      bullets.Add(int2,b);
    end;
    orBlasterMove:begin
      //delta is added to position, this may cause visual wtf with lag/packet drop
      with bullets[int1]^ do begin
        position:=v1;
        TranslateMatrix(transformM, position);
        light.position:=vectorAdd(light.position,v1);
      end;
    end;
    orBlasterRemove:begin
      pb:=bullets[int1];
      removePointLight(pb^.light);
      //mark for deletion
      pb^.active:=false;
      //bullets.remove(pb^.ID);
      //b.Destroy;
    end;
    orCreateItemGhost:begin
      meshManager.addModelInstance(cmd.int1,eItemTypes(cmd.int2),boolean(cmd.int3),cmd.v1,cmd.v2);
    end;
    //item is being moved from stash portal to destination. show a crate icon over actor
    orFromStashToWorld:begin
      //create 'crate' instance
      //w v2 musi byc size. przynajmniej dopoki nie bedzie wspolnej bazy z ktorej powstaja aktorzy
      u:=createActorInstance(int1,unitsAtlas.getIndexOf('crate'),v1,v2);
      u.setRotationY(actorsMap[cmd.int4].getRotation[1]);
      crateID:=u.actorID;
      //attach crate to actor
      actorsMap[cmd.int4].crateActorID:=crateID;
    end;
    //item is being installed. remove the crate image from hauling actor and move
    //item from ghost render to normal render
    orItemInstall:begin
      meshManager.addModelInstance(meshManager.getModelInstance(cmd.int3,true),cmd.int3,false);
      meshmanager.removeModelInstance(cmd.int3,true);
      u:=actorsMap[cmd.int2];
      actorsMap[u.crateActorID].Free;
      actorsMap.Remove(u.crateActorID);
      u.crateActorID:=-1;
    end;
    orDestroyActor:begin
      actorsMap[int1].Free;
      actorsMap.Remove(int1);
    end;
    //camera movement
    orControllerState:begin
      index:=actorsMap.indexof(int1);
      if (camera.mode=cmFree) or (camera.mode=cmEdge) or ((index>-1) and (actorsMap.data[index].actorID=player1.actorID)) then begin
      //   u:=actorsMap.data[index];
         //ignore noise
         if abs(v2[0])<analogNoiseLevel then v2[0]:=0; //x axis
         if abs(v2[1])<analogNoiseLevel then v2[1]:=0; //z
         if abs(v2[2])<analogNoiseLevel then v2[2]:=0; //y
         if abs(v1[0])<analogNoiseLevel then v1[0]:=0; //x axis
         if abs(v1[1])<analogNoiseLevel then v1[1]:=0; //z
         if abs(v1[2])<analogNoiseLevel then v1[2]:=0; //y
         //only render is interested in camera move so no need for new order
         //oM.addOrder(orWantMove,index,0,vector3fmake(u.position[0]+v2[0],u.position[1],u.position[2]+v2[1]));
         camera.leftStick:=v1;
         camera.rightStick:=v2;
         //90 degree rotation
         if camera.mode=cm90 then begin
            camera.clickLeft90(bitget(cmd.int2,2));
            camera.clickRight90(bitget(cmd.int2,1));
         end;
         //check button press state
         //if bitget(cmd.int1,1) then u.jump;
      end;
    end;
    orCameraMode:begin
       camera.mode:=eCameraModes(int2);
       if int1>-1 then begin
         index:=actorsMap.IndexOf(int1);
         if index>-1 then begin
            u:=actorsMap.data[index];
            camera.setTargetActor(u.position,u.actorID);

         end else log('achtung! renderer::orCameraFollow actorID not found')
       end;
    end;
    orMapLoad:begin
      for index:=0 to actorsMap.Count-1 do actorsMap.data[index].Free;
      actorsMap.Clear;
      for index:=0 to bullets.Count-1 do bullets.remove(index);
    end;
    orCameraWeaponZoom:matlib.resize;
  end;
 end;
  oM.queue[i]:=cmd;
 end;
 //update camera's target actor
 index:=-1;
 //if camera is in follow mode then give it actor coords
 if (camera.mode<>cmFree) then begin
    index:=actorsMap.IndexOf(camera.targetActorID);
    if index>-1 then begin
        u:=actorsMap.Data[index];
        camera.setTargetActor(u.position,u.actorID);
    end;
 end;
 //for i:=0 to bullets.Count-1 do bullets.data[i].update;
 //update units transforms
 //for i:=0 to actorsMap.Count-1 do actorsMap.data[i].update;
end;

function TRenderer.createActorInstance(actorID, typ: integer;
  position: TVector3f; size: TAffineVector): TFlatUnitRenderable;
var s:string[32];
begin
 result:=TFlatUnitRenderable.create;
 result.actorID:=actorID;
 with result do begin
   //get that actor data
   common:=getActorCommonProperties(eactortypes(typ));
   texFront:=unitsAtlas.getIndexOf(common.assetName);
   s:=common.assetName;
   texBack:=unitsAtlas.getIndexOf(s+'Back');
   texSide:=unitsAtlas.getIndexOf(s+'Side');
   translate(position);
   //make quad fit the proportions. let's say that 16 pixels is one block width
   scaleX(unitsAtlas.data[assetID].w/pixelsPerBlock);
   scaleY(unitsAtlas.data[assetID].h/pixelsPerBlock);
 end;
 actorsMap.Add(actorID,result);
end;

procedure TRenderer.saveLights(filename: string);
var
  i:integer;
  fileStream:TFilestream;
begin
   filename:=ChangeFileExt(filename,'.lights');
   fileStream:=tfilestream.Create(filename,fmOpenWrite or fmCreate);
   //directional
   fileStream.Write(dirLight.color,sizeof(dirLight.color));
   fileStream.Write(dirLight.direction,sizeof(dirLight.direction));
   fileStream.Write(dirLight.ambientIntensity,sizeof(dirLight.ambientIntensity));
   fileStream.Write(dirLight.diffuseIntensity ,sizeof(dirLight.diffuseIntensity));
   //point lights
   fileStream.Write(getLevelLightsCount,sizeof(integer));
   for i:=0 to pointLightsDS.Count-1 do if pointLightsDS[i].staticLight then
   with pointLightsDS[i] do begin
     fileStream.Write(color,sizeof(color));
     fileStream.Write(position,sizeof(position));
     fileStream.Write(ambientIntensity,sizeof(ambientIntensity));
     fileStream.Write(diffuseIntensity,sizeof(diffuseIntensity));
     fileStream.Write(attenuation,sizeof(attenuation));
   end;
   filestream.Free;
end;

procedure TRenderer.loadLights(filename: string);
var
  x,y,z,j,w,h:integer;
  c:longint;
  fileStream:TFilestream;
  position,color:TAffineVector;
  ambientIntensity,diffuseIntensity:single;
  attenuation:rAttenuation;
  l:tpointlight;
begin
 removeLights;
 filename:=ChangeFileExt(filename,'.lights');
 fileStream:=tfilestream.Create(filename,fmOpenReadWrite);

 //load dir light
 fileStream.Read(dirLight.color,sizeof(dirLight.color));
 fileStream.Read(dirLight.direction,sizeof(dirLight.direction));
 fileStream.read(dirLight.ambientIntensity,sizeof(dirLight.ambientIntensity));
 fileStream.read(dirLight.diffuseIntensity ,sizeof(dirLight.diffuseIntensity));
 //read number of pointlights
 fileStream.read(y,sizeof(y));
 //load point lights
 for x:=0 to y-1 do begin
   fileStream.Read(color,sizeof(color));
   fileStream.Read(position,sizeof(position));
   fileStream.Read(ambientIntensity,sizeof(ambientIntensity));
   fileStream.Read(diffuseIntensity,sizeof(diffuseIntensity));
   fileStream.Read(attenuation,sizeof(attenuation));
   l:=tpointlight.create(color,position,diffuseIntensity,attenuation.constant,
       attenuation.linear,attenuation.exp);
   addPointLight(l);
 end;
 filestream.Free;
end;

end.

