unit core_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,core_render,vectorGeometry,core_types,core_lights,
  Vectortypes,core_camera,core_utils,core_terrain_generator,core_block_brush,core_chunk,
   menu_rotate, core_orders, core_behaviour_tree, core_listofrecords, core_atmosphere,
  core_orders_manager, menu_pointLight, core_logic, core_editor_states,core_main
  ;

type

  { TEditor }

  TEditor = class
  private
    //for creating selection poly in esDrawSelection mode
    selectionStart:TAffineVector;
    //when drawing selection area it holds job id
    editingSelectionArea:integer;
    //flags for detecting two buttons pressed simultaneously
    leftBtnDown,rightBtnDown,middleBtnDown:boolean;
    procedure setSingleBlockBrush(AValue: eBlockTypes);
    procedure setState(AValue: eEditorStates);
  public
    //index in renderer.pointlightslist
    pointLight:TPointLight;
    fbrushBlockType:eBlockTypes;//current type of block used to create new ones
    fstate:eEditorStates;
    selectedUnitType:integer;
    procedure OnMouseClick(Button: integer;down:boolean;Shift: TShiftState; X, Y: Integer);
    procedure OnMouseMove(Shift: TShiftState; X, Y: Integer);
    procedure OnMouseWheel(Shift: TShiftState;WheelDelta: Integer; MousePos: TPoint);
    procedure OnKeyBoard( var Key: Word; down: boolean; Shift: TShiftState);
    procedure addPointLight;
    procedure undo;
    procedure redo;
    property state:eEditorStates read fstate write setState;
    property brushBlockType:eBlockTypes read fbrushBlockType write setSingleBlockBrush;
    constructor create;
    destructor destroy;override;
  end;

var
  Editor:TEditor;

implementation

{ TEditor }

procedure TEditor.setSingleBlockBrush(AValue: eBlockTypes);
begin
  state:=esSingleBlock;
  if fbrushBlockType=AValue then Exit;
  fbrushBlockType:=AValue;
end;

procedure TEditor.setState(AValue: eEditorStates);
begin
//  if fstate=avalue then exit;
  case state of
    esPointLight:begin
      if pointLight<>nil then renderer.removePointLight(pointLight);
    end;
    //if brush screation mode then clicking blocks list doesn't switch to single block mode
    esEditBrush: begin
      if avalue=esSingleBlock then exit;
    end;
    esActor: begin

    end;
    esPaintWithBrush:
      if (avalue<>esPaintWithBrush) and (avalue<>esPickBlockColor)
         and (avalue<>esPaintColors) then begin
      renderer.blockBrush.clear;
      renderer.gizmos.brush:=false;
    end;
  end;
  if (avalue=esPaintWithBrush) or (avalue=esPaintColors) then renderer.gizmos.brush:=true
  else renderer.gizmos.brush:=false;
  fstate:=AValue;
  if menuPointLight<>nil then menuPointLight.editorState:=avalue;
end;

procedure TEditor.OnMouseClick(Button: integer; down: boolean;
  Shift: TShiftState; X, Y: Integer);
var
  ch:TVector2i;
  pos,offset:TAffineVector;
  blk:tvector3b;
  i:integer;
  st:eEditorStates;
  blok:eBlockTypes;
  d:TBrushData;
  order:rorder;
begin
  //keyBindings.translateKey(integer(Button),action); //eh wtf
  //undo batch status
  if not down then inc(undoBatchID);
  //set mouse keys down flags
  if button=0 then leftBtnDown:=down;
  if button=1 then rightBtnDown:=down;
  if button=2 then middleBtnDown:=down;

  if ssShift in shift then camera.processMouse(down,shift,x,y);
  st:=state;
  //paint flat unit
  if (state=esActor) and (not down) then begin
     if (Button=0) then begin
         oM.addOrder(orCreateActor,logic.getNextFreeId,
                         selectedUnitType,-1,
                         renderer.selectionCube.position);
         //setRemoteBrain(logic.lastActorID);
     end;
  end;
  if (state=esDrawSelection) then begin
{     if (down) and (button=0) then  editingSelectionArea:=jobManager.startDrawingArea(renderer.selectionCube.position,renderer.selectionCube.position,jtDig);
     if (not down) and (button=0) then begin
         jobManager.finishDrawingArea(editingSelectionArea);
         editingSelectionArea:=-1;
          //oM.addJobArea(selectionStart,renderer.selectionCube.position,jtDig);
     end;
     }
  end;
  if (state=esSingleBlock) or (state=esEditBrush) then begin
       //left button adds block
       if (Button=0) and (not down)then begin
          setVector(pos,renderer.selectionCube.position);
          blk:=absolutePositionToChunkBlok(ch,pos);
          //core.chunks[ch[0],ch[1]].setBlockUndo(blk,brushBlockType);//refactoring
          chunksPointer^[ch[0],ch[1]].setBlockUndo(blk,brushBlockType);//refactoring
          terrain.cullBlocksInChunk(chunksPointer,ch[0],ch[1]);
          if state=esEditBrush then
             //renderer.blockBrush.addBlok(pos,core.chunks[ch[0],ch[1]].getBlock(blk));
             renderer.blockBrush.addBlok(pos,chunksPointer^[ch[0],ch[1]].getBlock(blk));
        end;
       //right button removes blok
       if (Button=1) and (not down)then begin
           setVector(pos,renderer.selectionCube.position);
           blk:=absolutePositionToChunkBlok(ch,pos);
           chunksPointer^[ch[0],ch[1]].setBlockUndo(blk,btNone);
           //terrain.cullBlocksInChunk(@core.chunks,ch[0],ch[1]);
           terrain.cullBlocksInChunk(chunksPointer,ch[0],ch[1]);
           if state=esEditBrush then
              renderer.blockBrush.removeBlok(pos);
       end;
       //middle button samples block
      if (Button=2) and (not down)then begin
          setVector(pos,renderer.selectionCube.position);
          blk:=absolutePositionToChunkBlok(ch,pos);
          //brushBlockType:=core.chunks[ch[0],ch[1]].getBlock(blk);
          brushBlockType:=chunksPointer^[ch[0],ch[1]].getBlock(blk);
      end;
  end;
  if (state=esPointLight) and (pointLight<>nil) and (not down) then begin
      if Button=0 then pointLight:=nil;
      if Button=2 then renderer.removePointLight(pointLight);
      menuPointLight.Hide;
      state:=esSelectMode;
      exit;
  end;
  if (state=esSelectMode) and (not down) then begin
     //check if user clicks a light gizmo
     if renderer.gizmos.lights then
        for i:=0 to renderer.pointLightsDS.Count-1 do begin
          if mouseHitboxIntersect(x,y,renderer.pointLightsDS[i].position,vector3fmake(1,1,1))
          then begin
             //set mode to light edit and set active light
             state:=esPointLight;
             pointLight:=renderer.pointLightsDS[i];
             exit;
          end;
        end;
  end;
  //paint brushing
  if ((state=esPaintWithBrush) or (state=esPaintColors)) and (renderer.blockBrush.data.count>0) and (not down) then begin
     setVector(pos,renderer.selectionCube.position);
     offset:=VectorSubtract(pos,renderer.blockBrush.data[0].position);
     offset[0]:=-offset[0]; offset[1]:=-offset[1]; offset[2]:=-offset[2];

     for i:=0 to renderer.blockBrush.data.Count-1 do begin
         d:=renderer.blockBrush.data[i];
         if d.typ=btNone then continue;
         blk:=absolutePositionToChunkBlok(ch,VectorSubtract(d.position,offset));
         //in paint colors mode we only change type of existing blocks
         if state=esPaintColors then begin
           if chunksPointer^[ch[0],ch[1]].getBlock(blk)=btNone then continue;
           if button=0 then
              chunksPointer^[ch[0],ch[1]].setBlockUndo(blk,fbrushBlockType);
           if button=2 then fbrushBlockType:=chunksPointer^[ch[0],ch[1]].getBlock(blk);
         end else
         begin  //in other modes we modify all blokcks
           if button=1 then//right button deletes blocks
            chunksPointer^[ch[0],ch[1]].setBlockUndo(blk,btNone)
           else if button=0 then //left click aints terrain
             chunksPointer^[ch[0],ch[1]].setBlockUndo(blk,d.typ)
           else if button=2 then //middle btn samples block from terrain
             begin
               blok:=chunksPointer^[ch[0],ch[1]].getBlock(blk);
               if blok<>btNone then d.typ:=blok;
               renderer.blockBrush.data[i]:=d;
             end;
         end;
         terrain.cullBlocksInChunk(chunksPointer,ch[0],ch[1]);
       end;
     end;

  if (state=esWaterEmitter) and (not down) then begin

     core.addUpdateable(
        TAutomataGenerator.create(mouseToArray(renderer.selectionCube.position),
                          @waterMap,7,1));
  end;
  if (state=esOxygenEmitter) and (not down) then begin
     core.addUpdateable(
        TAutomataGenerator.create(mouseToArray(renderer.selectionCube.position),
                          @oxygenMap,1,0.1));
  end;
end;

procedure TEditor.OnMouseMove(Shift: TShiftState; X, Y: Integer);
var o:rorder;
    i:integer;
begin
  if pointLight<>nil then begin
     pointLight.position:=renderer.selectionCube.position;
     pointLight.positionY:=pointLight.positionY+1;
  end;
  if editingSelectionArea>-1 then begin;
     i:=oM.addOrder;
     o:=om.queue[i];
     o.order:=orModifyJobArea;
     o.int1:=editingSelectionArea;
     o.int2:=1;
     o.v1:=renderer.selectionCube.position;
     om.queue[i]:=o;
  end
  //if (leftBtnDown) and (rightBtnDown) then
end;

procedure TEditor.OnMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint);
begin
  if (state=esPaintWithBrush) and (ssshift in shift)then
     renderer.blockBrush.rotate(wheelDelta>0);
end;

procedure TEditor.OnKeyBoard(var Key: Word; down: boolean; Shift: TShiftState);
begin
  if (key=32) then begin
     if state=esPointLight then menuPointLight.ShowAtCursor(pointLight);
     if state=esEditBrush then  renderer.gizmos.brush:=down;;
     if state=esEditFrame then  menuRotate.ShowAtCursor;
  end;
  if not (ssShift in shift) and (renderer.blockBrush<>nil) then begin
    if key=90 then renderer.blockBrush.rotaxis:=2;
    if key=89 then renderer.blockBrush.rotaxis:=1;
    if key=88 then renderer.blockBrush.rotaxis:=0;
  end;
end;

procedure TEditor.addPointLight;
begin
  state:=esPointLight;
  pointLight:=renderer.addPointLight;
end;

procedure TEditor.undo;
var ch:tvector2i;
    pos:tvector3b;
    fromBlok,toBlock:eBlockTypes;
    comID:integer;
    blockID:integer;
    s:string;
begin
  //roll back block changes using undoLog stringlist
  if undoLog.Count=0 then exit;
  try
  blockID:=-1;
  while true do begin
    s:=undoLog[undoLine];
    comID:=strtoint(eatstring(s,' '));
    if blockID=-1 then blockID:=comID else if blockID<>comID then break;
    pos[0]:=strtoint(eatstring(s,' '));
    pos[1]:=strtoint(eatstring(s,' '));
    pos[2]:=strtoint(eatstring(s,' '));
    ch[0]:=strtoint(eatstring(s,' '));
    ch[1]:=strtoint(eatstring(s,' '));
    fromBlok:=eBlockTypes(strtoint(eatstring(s,' ')));
    toBlock:=eBlockTypes(strtoint(eatstring(s,' ')));
    chunksPointer^[ch[0],ch[1]].setBlock(pos,fromBlok);
    terrain.cullBlocksInChunk(chunksPointer,ch[0],ch[1]);
    if undoline>0 then dec(undoLine) else break;
  end;
  except
    clearUndoHistory;
  end;
end;

procedure TEditor.redo;
var ch:tvector2i;
    pos:tvector3b;
    fromBlok,toBlock:eBlockTypes;
    comID:integer;
    blockID:integer;
    s:string;
begin
  //roll back block changes using undoLog stringlist
  if undoLog.Count=0 then exit;
  try
  blockID:=-1;
  while true do begin
    s:=undoLog[undoLine];
    comID:=strtoint(eatstring(s,' '));
    if blockID=-1 then blockID:=comID else if blockID<>comID then break;
    pos[0]:=strtoint(eatstring(s,' '));
    pos[1]:=strtoint(eatstring(s,' '));
    pos[2]:=strtoint(eatstring(s,' '));
    ch[0]:=strtoint(eatstring(s,' '));
    ch[1]:=strtoint(eatstring(s,' '));
    fromBlok:=eBlockTypes(strtoint(eatstring(s,' ')));
    toBlock:=eBlockTypes(strtoint(eatstring(s,' ')));
    chunksPointer^[ch[0],ch[1]].setBlock(pos,toBlock);
    terrain.cullBlocksInChunk(chunksPointer,ch[0],ch[1]);
    if undoline<undoLog.count-1 then inc(undoLine) else break;
  end;
  except
    clearUndoHistory;
  end;
end;

constructor TEditor.create;
begin
  selectedUnitType:=0;
  //initialize default brush type
  editingSelectionArea:=-1;
  brushBlockType:=btStone;
  state:=esNone;
  undoLog:=tstringlist.create;
  undoLine:=-1;
end;

destructor TEditor.destroy;
begin
  undolog.free;
  inherited destroy;
end;

end.

