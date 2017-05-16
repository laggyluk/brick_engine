unit core_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, forms,SysUtils,core_types,
  core_camera, math,core_terrain_generator,
  core_classes,core_chunk,VectorGeometry,VectorTypes,
  inifiles, core_logic, core_orders_manager, core_sound, core_physics, core_pathfinding,
  core_game, core_atmosphere, LCLType, core_input,
  core_game_items, core_player, core_orders, core_network//,core_editor,
  {$IFNDEF SERVER}
  ,core_ui,core_render,core_material_library,core_mesh
  ,core_block_brush
  {$ENDIF};

type

  //there are few ways to handle mouse input, depending on what is going on
  eInputStates = (
    iItemHorizontalZone, //place item on the ground. need to check if ground & position is valid
               //and enable shift-drag for multiple items placement

    iFreeZone, //select zone without restrictions i.e. for digging.
               //still should it check for i.e water?
    iSelect,   //select units etc.
    iBuild,    //create jobs from blueprint click
    iPlaceItem, //place selected item
    iShoot     //shooting stuff with mouse
    );

  { TPlanetCore }

  TPlanetCore = class
      chunks: TChunksArray; //visible chunks,  2d
    private
      fInputState: eInputStates;
      updateables:TUpdateablesList;
      cam_positionX,cam_positionY:integer;//current camera position in world /chunks
      chunksHitBox:TAffineVector;
      maxMouseSelectDistance:single;//max distance to track block under mouse
      blockUnderMouse:TVector3b;//coordinates of selection block
      chunkUnderMouse:TVector2i;//selection block is in this chunk
      oldMousePos:tVector2i;//old mouse position
      //flags for detecting two buttons pressed simultaneously
      leftBtnDown,rightBtnDown:boolean;
      dblMouseMove:integer;
      editingSelectionArea:integer;
      //blok under mouse at old position
      oldMouseBlok:TVector3b;
      procedure setInputState(AValue: eInputStates);
    public
      editorMode:boolean;
      destructor destroy;override;
      constructor create;
      procedure render;
      procedure update(deltaTime:single);
      procedure resizeViewport(w,h:integer);
      procedure OnKeyBoard( var Key: Word; down: boolean; Shift: TShiftState);
      procedure OnMouseClick(Button: integer;down:boolean;Shift: TShiftState; X, Y: Integer);
      procedure OnMouseMove(Shift: TShiftState; X, Y: Integer);
      procedure OnMouseWheel(Shift: TShiftState;WheelDelta: Integer; MousePos: TPoint);
      //moves selection cube snapping to xz plane
      procedure mouseAxisSnapXZMove(Shift: TShiftState; X, Y: Integer);
      //moves selection cube snapping to existing blocks
      procedure mouseWorldIntersect(x,y:integer);
      procedure initChunks;
      procedure init(w, h: integer);
      procedure saveMap(filename:string);
      procedure loadMap(filename:string);
      procedure addUpdateable(u:TUpdateable);
      procedure setMapSize(chunkW,chunkH,mapW,mapH:integer);
      //sometimes there's need to be a brush gizmo drawn under mouse, or selection or sth
      property inputState:eInputStates read fInputState write setInputState;
    end;

var
  core:TPlanetCore;

implementation

{ TPlanetCore }

procedure TPlanetCore.setMapSize(chunkW, chunkH, mapW, mapH: integer);
var i,j:integer;
    doinit:boolean;
begin
  if atmosphere<>nil then   atmosphere.pause;
  active_chunks_w:=mapW;
  active_chunks_h:=mapH;//map size
  chunk_width:= chunkW;  //
  chunk_width2:= chunk_width div 2;
  chunk_widthf:= chunk_width * block_size;
  chunk_width2f:= chunk_width * block_size div 2;
  chunk_xy_size:=chunk_width*chunk_width;
  chunk_height:=chunkH;//255?

  chunk_height2:= chunk_height div 2;
  chunk_heightf:= chunk_height *block_size;
  chunk_height2f:= (chunk_height *block_size) div 2;
  chunk_size:= chunk_width*chunk_width*chunk_height;
  world_width:= active_chunks_w*chunk_width;
  world_width2:= world_width div 2;
  world_depth:= active_chunks_h*chunk_width; //atually world depth?
  world_depth2:= world_depth div 2;
  world_xy_size:=world_width*world_depth;
 // setlength(TBlocksBuffer,chunkW,chunkH,chunkW);
  //uwolnic istniejace chunki i create again?
  doinit:=false;
  if length(chunks)>0 then
    for i:=0 to length(chunks)-1 do
    for j:=0 to length(chunks[0])-1 do
      if chunks[i,j]<>nil then begin
         {$IFNDEF SERVER}
         renderer.renderListChunks.Remove(chunks[i,j].renderable);
         {$ENDIF}
         chunks[i,j].destroy;
      end;

  setlength(chunks,active_chunks_w,active_chunks_h);
  chunksPointer:=@chunks;
  setVector(chunksHitBox,world_width2*block_size,
                         (chunk_height*block_size) /2,
                         world_depth2*block_size);
  //set world data size
  setlength(world,world_width*world_depth*chunk_height);
  initChunks;

end;

procedure TPlanetCore.setInputState(AValue: eInputStates);
begin
  {$IFNDEF SERVER}
  if fInputState=AValue then Exit;
  //first hide/remove gizmos from other input modes
  case fInputState of
    iPlaceItem:begin
      meshManager.freeModelInstance(-1,true);
      //alembik.selectedItem:=nil;
    end;
    iBuild:begin
      //renderer.blockBrush:=nil;
      renderer.gizmos.brush:=false;
      renderer.gizmos.selectionCube:=false;
    end;
  end;
  case avalue of
    iBuild:begin
      if renderer.blockBrush<>nil then renderer.gizmos.brush:=true;
      renderer.gizmos.selectionCube:=false;
    end;
    iPlaceItem:renderer.gizmos.selectionCube:=false;
  end;
  //hide selection cube in shooting mode
  if gameMode then begin
    if avalue=iShoot then renderer.gizmos.selectionCube:=false
    else  renderer.gizmos.selectionCube:=true;
  end;
  fInputState:=AValue;
  {$ENDIF}
end;

constructor TPlanetCore.create;
begin
  modelsFolder:=apppath+'objects'+DirectorySeparator;

end;

procedure TPlanetCore.render;
begin
  {$IFNDEF SERVER}
  renderer.render;
  inc(globalFrame);
  {$ENDIF}
end;

procedure TPlanetCore.update(deltaTime:single);
var
  i:integer;
begin
  totalTime+=deltaTime;
  deltaT:=deltaTime;
  if deltaT>100 then begin
    log('delta spike');
    deltaT:=1;
  end;
  if showLoadingScreen=false then begin
    for i:=0 to updateables.count-1 do
      updateables[i].update;
    {$IFNDEF SERVER}
    camera.update;
    renderer.update;
    {$ENDIF}

    soundManager.update;
    physicsEngine.update;
    logic.update;
    oM.update;
    uiManager.Update;
    //process chunks/cache if camera changed position
    {$IFNDEF SERVER}
    if camera.didMove then begin
      renderer.calcCulling;
      camera.didMove:=false;
    end;
    {$ENDIF}
  end;
  inputManager.update;
  network.update;
end;

procedure TPlanetCore.resizeViewport(w, h: integer);
begin
  {$IFNDEF SERVER}
  renderer.reshape(w,h);
  log('display resolution: '+inttostr(w)+' x '+inttostr(h));
  {$ENDIF}
end;

procedure TPlanetCore.OnKeyBoard(var Key: Word; down: boolean; Shift: TShiftState);
var
   i:integer;
begin
  {
  keyBindings.translateKey(integer(key),action);
  // log(inttostr(integer(action)));
  //log(inttostr(key));

  case action of //camera related actions
   sA_cam_strafe_left,sA_cam_strafe_right,sA_cam_forwards,sA_cam_backwards,
   sA_cam_rotate_left,sA_cam_rotate_right,sA_cam_move_up,sA_cam_move_down,
   sA_cam_rotate_down,sA_cam_rotate_up:
     camera.processKeyboard(action,down,shift);//move the world not the camera
 end;
 }
  inputManager.keyEvent(key,down);

  if (key=VK_RETURN) and (down) then begin
    i:=integer(camera.mode);
    inc(i);
    if i=integer(cmMax) then i:=integer(cmFree);
    om.addOrder2i(orCameraMode,player1.actorID,i);
  end;
  if key=27 then inputManager.mouseLocked:=false;
end;

procedure TPlanetCore.OnMouseClick(Button: integer; down:boolean;Shift: TShiftState; X,
  Y: Integer);
var
  ch:TVector2i;
  pos,groundPos,stashPos:TAffineVector;
  blk:tvector3b;
  flag:boolean;
  groundID,stashID,itemID:integer;
  rayVector,rayStart:TAffineVector;
begin
  {$IFNDEF SERVER}

  //set mouse keys down flags
  if button=0 then leftBtnDown:=down;
  if button=1 then rightBtnDown:=down;

//  log(inttostr(Button));
//  keyBindings.translateKey(integer(Button),action); //eh wtf

  case inputState of
    {
    iFreeZone:begin
       if (down) and (button=0) then
          editingSelectionArea:=jobManager.startDrawingArea(renderer.selectionCube.position,renderer.selectionCube.position,jtDig);
       if (not down) and (button=0) then begin
          jobManager.finishDrawingArea(editingSelectionArea);
          editingSelectionArea:=-1;
          //oM.addJobArea(selectionStart,renderer.selectionCube.position,jtDig);
       end;
    end;
    iBuild:begin
       if (not down) and (button=0) then
         jobManager.fromBrush(renderer.blockBrush,renderer.selectionCube.position,jtBuild);
       if (not down) and (button=1) then
         jobManager.removeJobsByBrush(renderer.blockBrush,renderer.selectionCube.position);
    end;
    iPlaceItem:begin
      if (not down) and (button=0) then begin
        //najpierw wypadalo by sprawdzic czy sa surowce na produkcje? chyba ze item juz wyprodukowany
        //bedzie trzeba znalezc droge ze stasha do docelowej lokacji

        //create item in factory and add to stash
        itemID:=player1.stash.addItem(eItemTypes(alembik.selectedItem.itemType))^.id;
        //anyways first look for the item on the ground
        groundId:=itemsManager.getNearestItemID(eItemTypes(alembik.selectedItem.itemType),
                                                alembik.selectedItem.model.position,
                                                groundPos);
        //then in stash
        stashID:=player1.getNearestStashPortal(alembik.selectedItem.model.position,stashPos);
        pos:=stashPos;
        //check which is shorter if there's item also on the ground
        if (groundID>-1) then
          if VectorDistance(groundPos,alembik.selectedItem.model.position)<VectorDistance(stashPos,alembik.selectedItem.model.position) then begin
            pos:=groundPos;
            itemID:=groundID;
          end;
        //in case that we are building a stash portal itself then it will pop up out of nowhere, built by worker
        if eItemTypes(alembik.selectedItem.itemType)=itStashPortal then pos:=alembik.selectedItem.model.position;
        //create a job, v2 is destination for the item, v1 is item pickup location
        jobManager.addCreateItemJob(eItemTypes(alembik.selectedItem.itemType),
                                    itemID,
                                    alembik.selectedItem.model.position,
                                    pos,
                                    alembik.selectedItem.model.getRotation);

        {item powinien byc stwozony i dodany do jakiejs listy zeby bylo wiadomo skad dokad
        go niesc. stwozony laduje w stashu odrazu?
        wtedy ludzik szuka drogi do najblizszego stash portalu, idzie tam wyjmuje item
        odejmuje ze stashlisty , idzie do celu i go tam kladzie - dodaje do listy
        dzialajacych w swiecie itemow}
      end;
    end;
}
    //this shouldn't be here unless it's minecraft..1
    iItemHorizontalZone:begin
      //renderer.blockBrush.paint(renderer.selectionCube.position,bpmPaint);

    end;
    iShoot:begin
      //for tpp view need to cast some rays and set where camera is pointing to
      //for shooting stuff
      if (camera.mode=cm3rdPerson) and (down) then begin
         //ray start is where camera is
         SetVector(rayStart, camera.position);
         //ray direction is from click coorinates
         SetVector(rayVector, ScreenToVector(AffineVectorMake(x+viewport[0], viewport[3]-y+viewport[1], 0)));
         NormalizeVector(rayVector);
         //first need to check ray collisions with units
         if not PhysicsEngine.rayHitActor(rayStart,rayVector,pos) then begin
           //if no unit hit then find what is block position is under the mouse cursor
           mouseWorldIntersect(x,y);
           pos:=renderer.selectionCube.position;
         end;
         //pos:=renderer.selectionCube.position;
         //then with terrain
         camera.crossHairs:=pos;

      end;
      if Button=1 then begin
         oM.addOrder2i(orCameraWeaponZoom,player1.actorID,integer(down))
      end;
      //log(floattostr(physicsEngine.actors[player1.actorID].rotation[1]));
      inputManager.keyEvent(Button,down);
    end;
  end;
  {$ENDIF}
end;

procedure TPlanetCore.OnMouseMove(Shift: TShiftState; X, Y: Integer);
var scale:integer;
    o:rorder;
    i:integer;
    button:integer;
begin
  {$IFNDEF SERVER}
  //cam look i guess
  camera.processMouseMove(Shift,x,y);
  if gameMode then begin
    if editingSelectionArea>-1 then begin
       i:=oM.addOrder;
       o:=om.queue[i];
       o.order:=orModifyJobArea;
       o.int1:=editingSelectionArea;
       o.int2:=1;
       o.v1:=renderer.selectionCube.position;
       om.queue[i]:=o;
       if ssShift in shift then begin
          //shouldnt be linear movement but sclale with odleglosc
          scale:=round((1/VectorLength(VectorSubtract(renderer.selectionCube.position,camera.position)))*150);
          if scale=0 then scale:=1;
          if y>oldMousePos[1] then dblMouseMove+=1;
          if y<oldMousePos[1] then dblMouseMove-=1;
          if dblMouseMove>=scale then begin
             dblMouseMove:=0;
             renderer.selectionCube.translate(0,-1,0);
          end else
          if dblMouseMove<=-scale then begin
             dblMouseMove:=0;
             renderer.selectionCube.translate(0,1,0);
          end;
          blockUnderMouse[1]:=round(renderer.selectionCube.position[1]+chunk_height-1);
       end else
           mouseAxisSnapXZMove(Shift, X, Y)
    end else
    //when shooting don't move block under mouse cause it shouldn't even be visible
    if inputState<>iShoot then
    begin
      if ssShift in shift then mouseAxisSnapXZMove(Shift, X, Y)
      else begin
         //brush draw
         mouseWorldIntersect(x,y);
         //if not VectorEquals(oldMouseBlok,blockUnderMouse) then
         //  if (inputState=iBuild) and (leftBtnDown) then
         //     jobManager.fromBrush(renderer.blockBrush,renderer.selectionCube.position,jtBuild);
         oldMouseBlok:=blockUnderMouse;
      end
    end
  end else begin
//    if editor<>nil then editor.OnMouseMove(shift,x,y);
    //move the selection rect
    if ssShift in shift then mouseAxisSnapXZMove(Shift, X, Y)
      else mouseWorldIntersect(x,y);
         //brush draw
    button:=-1;
    if leftBtnDown then button:=0;
    if rightBtnDown then button:=1;
    //middle btn?
    if not VectorEquals(oldMouseBlok,blockUnderMouse) and (button>-1)  then begin
//        editor.OnMouseClick(Button,false,shift,x,y);
    end;
    oldMouseBlok:=blockUnderMouse;
  end;
  //if inputState=iPlaceItem then
     //alembik.selectedItem.model.position:=VectorAdd(renderer.selectionCube.position,vector3fMake(0,-0.5,0));
  oldMousePos[0]:=x;
  oldMousePos[1]:=y;
  {$ENDIF}
end;

procedure TPlanetCore.OnMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint);
begin
  {$IFNDEF SERVER}
  //if ssShift in shift then begin
  if inputState=iPlaceItem then begin
     //rotate selectet object by 45*
     {if WheelDelta>0 then alembik.selectedItem.model.setRotationY(
        alembik.selectedItem.model.getRotation[1]+ degtorad(45))
     else alembik.selectedItem.model.setRotationY(
          alembik.selectedItem.model.getRotation[1]-degtorad(45))}
  end else
  if inputState=iBuild then begin
     renderer.blockBrush.rotate(wheelDelta>0);
  end else
  begin
     if WheelDelta>0 then renderer.selectionCube.translate(0,1,0)
     else renderer.selectionCube.translate(0,-1,0);
     blockUnderMouse[1]:=round(renderer.selectionCube.position[1]-1);
     //brush draw if mouse buttons down
     if (leftBtnDown) or (rightBtnDown) then OnMouseMove(shift,oldMousePos[0],oldMousePos[1]);
  end;
 // end;
 {$ENDIF}
end;

procedure TPlanetCore.mouseAxisSnapXZMove(Shift: TShiftState; X, Y: Integer);
var
  rayStart,rayVector,plane,norm,iPnt:TVector;
  h,oldh:single;
begin


  SetVector(rayStart, camera.position);

  SetVector(rayVector, ScreenToVector(AffineVectorMake(x+viewport[0], viewport[3]-y+viewport[1], 0)));
  NormalizeVector(rayVector);
  h:=blockUnderMouse[1]+1;
  oldh:=blockUnderMouse[1];
  setVector(plane,0,h,0,1);
  setVector(norm,0,-1,0,0);
  if RayCastPlaneIntersect(rayStart,rayVector,plane,norm,@iPnt) then begin
//     log(Format('hit: %f,%f,%f ', [iPnt[0],h+1,iPnt[2]]));
     {$IFNDEF SERVER}
     renderer.selectionCube.position:=vector3fmake(round(ipnt[0]),ipnt[1],round(ipnt[2]));

     //uwaga bo cos sie moglo zjebac, weczesniej to blokUndemouse nie byl tu zmieniany
     blockUnderMouse :=absolutePositionToChunkBlok(chunkUnderMouse,renderer.selectionCube.position);
     blockUnderMouse[1]:=round(oldh);
     {$ENDIF}
  end;

end;

procedure TPlanetCore.mouseWorldIntersect(x, y: integer);
var
  BoxPos, afScale,iPnt,rayVector,rayStart,offset,blok ,v:TAffineVector;
  f:single;
begin
  //camera inside chunks hitbox?
    setVector(boxPos,0,chunk_height2,0);
    setVector(afScale,chunksHitBox);
    SetVector(rayStart, camera.position);

    SetVector(rayVector, ScreenToVector(AffineVectorMake(x+viewport[0], viewport[3]-y+viewport[1], 0)));
    NormalizeVector(rayVector);

    //if not
    RayCastBoxIntersect(rayStart,rayVector,
            VectorSubtract(BoxPos, afScale),
            VectorAdd(BoxPos, afScale),@iPnt);
    //then //camera is outside world hitbox
      //log('cam outside chunks area');

    //inside or not, iPnt now holds ray start
    setvector(rayStart,iPnt);
    //wezmy pod uwage ze chunki sa numerowane od 0,0 a tu sreodek jest w 0,0
    offset:=vector3fMake(world_width2,0,world_width2);
    addvector(rayStart,offset);
    scaleVector(rayVector,maxMouseSelectDistance);
    addvector(rayVector,rayStart);
    //gwozdz programu
    if rayBlocksIntersect(round(rayStart[0]),round(rayStart[1]),round(rayStart[2]),
                       round(rayVector[0]),round(rayVector[1]),round(rayVector[2]),
                       chunkUnderMouse,
                       blok)
    then begin
      blockUnderMouse:=vector3bMake(round(blok[0]),round(blok[1]),round(blok[2]));
      blok[0]:=blok[0]+(chunkUnderMouse[0]*chunk_width);
      //this 'extrudes' block position over the flat surface but in y, not towards cam
      blok[1]+=1;
      blok[2]:=blok[2]+(chunkUnderMouse[1]*chunk_width);
      {$IFNDEF SERVER}
      renderer.selectionCube.position:=VectorSubtract(blok,offset);
      {$ENDIF}

      //ustawiamy celownik na blok pod mysza. przez to strzlal z reki
      //nie bedzie precyzyjny przy okazji ;p przynajmniej jesli chodzi o strzal we wroga
      //camera.crossHairs:=renderer.selectionCube.position;
      //camera.crossHairs[1]-=1;
    end;
//    f:=VectorDistance(renderer.selectionCube.position,camera.crossHairs);
//    camera.crossHairs:=vectorScale(camera.crossHairs,f/maxMouseSelectDistance);

end;

procedure TPlanetCore.initChunks;
var
  x,z:integer;
  offsetx,offsety,offsetz:single;
  wtf:tchunk;
begin
  //generate heightmap
//  terrain.perlin(1024,1024);
 // terrain.loadFromImage('test.bmp');
  { TODO : rozmiar buffora jest dla vec4 ale mozna by pewnie uzywac vec3 dla kolorow i vert
    ale tez mozliwe ze vec4 bedzie szybszy niz vec3, trza to bekmarknac}
  takeCareOfDataBufferSize(chunk_width*chunk_width*chunk_height*4);
  //generate some terrain
  offsetx:=-((active_chunks_w-1) *chunk_width*block_size)/ 4;
  offsetz:=-((active_chunks_h-1) *chunk_width*block_size)/ 4;
  for z:=0 to active_chunks_h-1 do
    for x:=0 to active_chunks_w-1 do begin
      chunks[x,z]:=tchunk.create(x,z);
      chunks[x,z].xId:=x;
      chunks[x,z].yId:=z;
      setVector(chunks[x,z].hitBoxSize,chunk_width2,chunk_height2,chunk_width2);
      chunks[x,z].center[0]:=offsetx+x*(chunk_width*block_size)/ 2;
      chunks[x,z].center[1]:=(chunk_height*block_size)/ 2;
      chunks[x,z].center[2]:=offsetz+z*(chunk_width*block_size)/2;
      {$IFNDEF SERVER}
      renderer.add(chunks[x,z].renderable);

     // chunks[x,z].renderable.scale:=2;//for some reason cubes created in geom are half sized
      chunks[x,z].renderable.translate(
      chunks[x,z].center[0]*2,
      0,
     // -chunk_height+1,
      chunks[x,z].center[2]*2);
      {$ENDIF}
    end;
end;

procedure TPlanetCore.saveMap(filename:string);
begin
  terrain.saveMap(filename);

end;

procedure TPlanetCore.loadMap(filename:string);
begin
  terrain.loadMap(filename);
end;

procedure TPlanetCore.addUpdateable(u: TUpdateable);
begin
  updateables.Add(u);
end;

procedure TPlanetCore.init(w,h:integer);
var
  ini:tinifile;
  s:string;
  i:integer;
begin
  randomize;
  editingSelectionArea:=-1;
  apppath:=extractfilepath(application.ExeName);
  ini:=tinifile.Create(appPath+'config.ini');
  maxMouseSelectDistance:=258;

  Options.renderer.ssao:=ini.ReadBool('renderer','ssao',false);
  Options.renderer.ssaoSamples:=ini.Readinteger('renderer','ssaoSamples',10);
  Options.renderer.ambientOcclusion:=ini.ReadBool('renderer','ao',false);
  options.renderer.aoRayLength:=ini.Readinteger('renderer','aoRayLength',16);
  options.renderer.aoSkosRayLength:=options.renderer.aoRayLength-options.renderer.aoRayLength div 4;
  options.renderer.aoStep:=ini.ReadFloat('renderer','aoStep',0.02);
  ini.free;

  //options.debug.orders:=true;
  oM:=TOrdersManager.Create;


  pathManager:=TPathManager.create;
  {$IFNDEF SERVER}
  matLib:=TMaterialLib.create;
  matlib.init(true);
  uiManager:=TuiManager.Create;
  uiManager.init;
  renderer:=trenderer.create;
  renderer.init(w,h,true);
  log:=@glLogMsg;
  meshManager:=TmeshManager.create;
  meshManager.loadModelsFromFolder(appPath+'objects'+DirectorySeparator);
  //alembik:=talembik.create;
  //alembik.loadFromFile(apppath+ 'objects'+DirectorySeparator+'alembik.wtf');
  {$ENDIF}
  camera:=tcamera.create;
  physicsEngine:=tPhysicsEngine.create;

  soundManager:=TSoundManager.create;
  soundManager.init;
  //selection gizmo
//  renderer.add(cube);

  terrain:=TTerrainGenerator.create;
  terrain.mapSizeCallback:=@SetMapSize;
  updateables:=TUpdateablesList.Create;
  //updateables.add(camera);
  updateables.add(terrain);

  Logic:=TGameLogic.create;

  game:=TGame.create(3);
  uiManager.mainMenu.LoadLevelCallback:=@game.loadGame;
  if not gameMode then uiManager.mainMenu.visible:=false;
  //initChunks;
  //loadMap(apppath+'terrain\test.terrain');
  //terrain.generateMap(0,0);
  if fileexists(apppath+'editor.ini') then begin
    ini:=tinifile.create(apppath+'editor.ini');
    terrain.mapFileName:=ini.ReadString('options','mapFile','');
    ini.free;
  end;


  //items manager
  ItemsManager:=TitemsManager.create;
  inputManager:=tInputManager.create;
  inputManager.init;
  network:=TNetworkManager.create;

  if gameMode then begin
     //game.loadGame(appPath+'saves'+DirectorySeparator+'map.terrain');
     //load menu map
     //game.loadGame(format('%s%s%s%s',[appPath,'terrain',DirectorySeparator,'menu.terrain']),false);
     s:=format('%s%s%s%s',[appPath,'terrain',DirectorySeparator,'menu.terrain']);
     terrain.loadMap(s);
     renderer.loadLights(s);
     logic.skipUpdate:=false;
     inputState:=iShoot;
     //unlock mouse
     inputManager.mouseLocked:=false;
  end else
  begin
    //editor mode. loading last map used?
    if (terrain.mapFileName<>'') and (FileExists(terrain.mapFileName)) then begin
       s:=terrain.mapFilename;
       terrain.loadMap(s);
       renderer.loadLights(s);
       s:=ChangeFileExt(s,'.actors');
       game.loadActorsFromFile(s);
       //oM.addOrder(orMapLoaded,1,0);
    end
    //init blank map
    else begin
       //setMapSize(32,128,4,4);
       //terrain.initMap(false);
       loadMap(appPath+'terrain\default.terrain');
    end;
    {$IFNDEF SERVER}
    renderer.blockBrush:=tblockbrush.create;
    {$ENDIF}
  end;
  //camera.setAboveGround(0);
   //load models

end;

destructor TPlanetCore.destroy;
var
  x,y:integer;
begin
  network.free;
  inputManager.free;
  itemsManager.free;
  camera.free;
  game.destroy;
  {$IFNDEF SERVER}
  //alembik.Free;
  uiManager.destroy;
  renderer.destroy;
  matLib.destroy;
  meshManager.free;
  {$ENDIF}

  pathManager.destroy;
  physicsEngine.destroy;
  soundManager.Destroy;
  oM.destroy;
  logic.destroy;
  updateables.destroy;
  terrain.destroy;
  for y:=0 to active_chunks_h-1 do
    for x:=0 to active_chunks_w-1 do begin
      //save map
     // terrain.saveChunk(@chunks[x,y],x,y);
      chunks[x,y].free;
  end;

  inherited;
end;

initialization
DefaultFormatSettings.DecimalSeparator:='.';
end.

