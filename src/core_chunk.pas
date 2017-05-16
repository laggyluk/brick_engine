unit core_chunk;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,core_types,core_classes,
  //{$IFNDEF SERVER}
  renderable_object_types,
  //{$ENDIF}
  VectorTypes,VectorGeometry;

type

  { TChunk }

  TChunk = class(TUpdateable)
   private
   // fblocks: pBlocksBuffer;//full map of all blocks making the chunk
    fdataOffset:cardinal;//this chunk's start of data in world buffer
    procedure writeBlock(x,y,z:integer;value:TBlockType);
    function readBlock(x,y,z:integer):TBlockType;
    procedure writePBlock(x,y,z:integer;value:pBlockType);
    function readPBlock(x,y,z:integer):pBlockType;
   public
   //flag for culling
   reBuild:boolean;
  // {$IFNDEF SERVER}
   renderable:TTerrainChunk;//object responsible for drawing this thing
   //{$ENDIF}
   xID,yID:integer;//which chunk relative to camera this is
   hitBoxSize:TAffineVector;
   center:TVector3f; //center of object (and hitbox)
   constructor create(chx, chy: integer);
   destructor destroy;override;
   function getHandle:pointer;
   procedure update;override;
   procedure setBlock(blokPos:TVector3b;blockType:eBlockTypes);
   procedure setBlockUndo(blokPos:TVector3b;blockType:eBlockTypes);
   function getBlock(blokPos:TVector3b):eBlockTypes;
   procedure setBlock(x,y,z:byte;blockType:eBlockTypes);
   //check if there is a blok on ray's way. rayHit is point where ray intersect bounding box
   function checkRayBlockInteresct(rayStart,rayDir,rayHit:TAffineVector; var blk:TAffineVector):boolean;
   property blocks[indexX,indexY,indexZ:integer]:TBlockType read readBlock write writeBlock;
   property pBlocks[indexX,indexY,indexZ:integer]:pBlockType read readPBlock write writePBlock;
   //returns offset at which chunk data starts in world data array
   function getChunkDataStart:cardinal;
  end;

  pChunk = ^TChunk;
  TChunksArray = array  of array of TChunk;
  PChunksArray = ^TChunksArray;

  { TChunkCache }

  TChunkCache = class
   blocks:TBlocksBuffer;
   xID,yID:integer;//identifies which chunk on grid this cache corresponds to
  end;

  procedure takeCareOfDataBufferSize(size:cardinal);
    //plots 3d line. thus finds cubes that colide with line
  procedure line3D(x0,y0,z0,x1,y1,z1:integer;blok:eBlockTypes);
  //function chunksPointer(chX,chY,x,y,z:integer):eBlockTypes;
  function absolutePositionToChunkBlok(out ch:TVector2i;pos:TAffineVector):TVector3b;
  procedure SwapAB(var a,b :integer);
  procedure SwapAB(var a,b :single);
  //check if block if set at position, if so then iterate up and return height at collision
  function getBlockCollisionUp(pos:TAffineVector):single;
  //checks if actor of given height will fit on position. returns 0 if fits, more if some block
  function willActorFit(pos:TAffineVector;height:single):integer;
  //check if there is a ground to walk on and 4 free blocks upwards for actor to fit
  function blokWalkable(tmp:TVector3i;flyngAllowed:boolean):boolean;
  //fast, no checks
  function getBlockTypeAtFast(x,y,z:integer):eblocktypes;inline;
  //Ray-blocks intersection. 'block' holds collision blok coords; result = true if collision
  function rayBlocksIntersect(x0,y0,z0,x1,y1,z1:integer;var chunk:TVector2i;var block:TAffineVector;maxSteps:integer = 256; draw:boolean = false):boolean;

var
  chunkVertexBuffer: array of rBufferDataBit;
  chunksPointer:PChunksArray; //pointer to active chunks array for global use

implementation

function rayBlocksIntersect(x0,y0,z0,x1,y1,z1:integer;var chunk:TVector2i;
  var block: TAffineVector;maxSteps:integer = 256;draw:boolean = false): boolean;
var
    x, delta_x, step_x  :integer;
    y, delta_y, step_y  :integer;
    z, delta_z, step_z  :integer;
    swap_xy, swap_xz            :boolean;
    drift_xy, drift_xz          :integer;
    cx, cy, cz,stepCount     :integer;
    blk:TBlockType;
    a,b,c,chx,chy:cardinal;
begin
    //log('start: ' + inttostr(x0) + ', ' + inttostr(y0) + ', ' + inttostr(z0));
    //log('end: ' + inttostr(x1) + ', ' + inttostr(y1) + ', ' + inttostr(z1));
    stepCount:=0;
    result:=false;
    swap_xy := Abs(y1 - y0) > Abs(x1 - x0);
    if swap_xy then begin
        SwapAB(x0, y0);
        SwapAB(x1, y1);
    end;
    swap_xz := Abs(z1 - z0) > Abs(x1 - x0);
    if swap_xz then begin
        SwapAB(x0, z0);
        SwapAB(x1, z1);
    end;
    delta_x := Abs(x1 - x0);
    delta_y := Abs(y1 - y0);
    delta_z := Abs(z1 - z0);
    drift_xy  := delta_x div 2;
    drift_xz  := (delta_x div 2);
    step_x := 1;  if (x0 > x1) then  step_x := -1;
    step_y := 1;  if (y0 > y1) then  step_y := -1;
    step_z := 1;  if (z0 > z1) then  step_z := -1;
    y := y0;
    z := z0;
    x:= x0;
    while (stepCount<=maxSteps) do begin
        cx := x;    cy := y;    cz := z;
        if swap_xz then SwapAB(cx, cz);
        if swap_xy then SwapAB(cx, cy);
        inc(stepCount);
        //check if we are not outside of chunks interns
        { TODO -copti : that should never happen in calling routine anyways so this check shouldn't be here }
        if not ((cx>=world_width) or (cx<0) or
           (cy>=chunk_height) or (cy<0) or
           (cz>=world_depth) or (cz<0)) then
           begin
            //i guess center of world is 0,0 so..
            //central block is in center of chunk so..
            //those coords should be divded by chunks size
            a:=(cx mod chunk_width);
            //b:=abs(cy + chunk_height2);
            b:=cy;
            c:=(cz mod chunk_width);
            chx:=(cx div chunk_width);
            chy:=(cz div chunk_width);
            blk:=chunksPointer^[chx,chy].blocks[a,b,c];
            //blk:=getWorldArray(cx,cy,cz);
            if draw then begin
              chunksPointer^[chx,chy].blocks[a,b,c]:=byte(btIce);
              chunksPointer^[chx,chy].rebuild:=true;
            end;
            if eblockTypes(blk)<>btNone then begin
               block:=vector3fMake(a,b,c);
               //log(Format('hit: %d,%d,%d ', [a,b,c]));
               chunk[0]:=chx;
               chunk[1]:=chy;
               result:=true;
               break;
            end;
          end;
        //log('test:' + inttostr(a) + ', ' + inttostr(b) + ', ' + inttostr(c));
        //log(': ' + inttostr(cx) + ', ' + inttostr(cy) + ', ' + inttostr(cz));
        drift_xy := drift_xy - delta_y;
        drift_xz := drift_xz - delta_z;
        if drift_xy < 0 then begin
            y := y + step_y;
            drift_xy := drift_xy + delta_x;
        end;
        if drift_xz < 0 then begin
            z := z + step_z;
            drift_xz := drift_xz + delta_x;
        end;
        x:=x+step_x;
    end;
    //log('chunk: ' + inttostr(chx) + ', ' + inttostr(chy));
end;

function absolutePositionToChunkBlok(out ch: TVector2i;pos: TAffineVector):TVector3b;
begin
  ch[0]:=abs(round(pos[0]+world_width2) div( chunk_width));
  ch[1]:=abs(round(pos[2]+world_depth2) div ( chunk_width));
  if ch[0]>=active_chunks_w then ch[0]:=active_chunks_w-1;
  if ch[1]>=active_chunks_h then ch[1]:=active_chunks_h-1;
  result:=vector3bmake(abs(round(pos[0]+world_width2)) mod (chunk_width),
          abs(round(pos[1])),
          abs(round(pos[2]+world_depth2)) mod (chunk_width));
end;

function getBlockTypeAtFast(x, y, z: integer): eblocktypes;
begin
  //if (x>world_width-1) or (y>chunk_height-1) or (z>world_depth-1) or (x<0) or (y<0) or (z<0)
  //then  result:=btUndefined else
  result:=eblocktypes(chunksPointer^[x div chunk_width,z div chunk_width].blocks[x mod chunk_width,y,z mod chunk_width]);
end;



function willActorFit(pos: TAffineVector; height: single): integer;
var
  i,j,x,y,z,xx,zz:integer;
begin
  j:=-1;
  x:=round(pos[0]+world_width2);
  z:=round(pos[2]+world_width2);
  xx:=x div chunk_width;zz:=z div chunk_width;
  x:=x mod chunk_width;
  z:=z mod chunk_width;
  y:=round(pos[1]);
  for i:=0 to round(height)-1 do
    if not(eblocktypes(chunksPointer^[xx,zz].blocks[x,y+i,z]) in mineralsNonBlocking)
      then begin
       //inc(j);
       j:=i;
    end;
  result:=j+1;
end;

function blokWalkable(tmp:TVector3i;flyngAllowed:boolean):boolean;
var x,y,z,cx,cz:cardinal;
begin
  try
   { TODO -cbug : niech pathfinder ma jakis licznik i zwraca failed po ilustam krokach/czasie bez wyniku.
druga zecz to ze tu jest exception kiedy on jeszce dziala a chunki sa juz zwolnine }
  result:=false;
  if (tmp[0]>world_width-1) or (tmp[1]>chunk_height-4) or (tmp[2]>world_depth-1)
      or (tmp[0]<0) or (tmp[1]<1) or (tmp[2]<0) then
          result:=false else
  begin
    cx:=tmp[0] div chunk_width;
    cz:=tmp[2] div chunk_width;
    x:= tmp[0] mod chunk_width;
    y:=tmp[1];
    z:= tmp[2] mod chunk_width;
    if flyngAllowed then begin
      if (eblocktypes(chunksPointer^[cx,cz].blocks[x,y+3,z])in mineralsNonBlocking) and
        (eblocktypes(chunksPointer^[cx,cz].blocks[x,y+2,z])in mineralsNonBlocking) and
        (eblocktypes(chunksPointer^[cx,cz].blocks[x,y+1,z])in mineralsNonBlocking)
       and (eblocktypes(chunksPointer^[cx,cz].blocks[x,y,z])in mineralsNonBlocking)
       //doesn't need ground below if flying
       //and (eblocktypes(chunksPointer^[cx,cz].blocks[x,y-1,z])in mineralsNonBlocking)
       then result:=true
    end else
    if (eblocktypes(chunksPointer^[cx,cz].blocks[x,y+3,z])in mineralsNonBlocking) and
      (eblocktypes(chunksPointer^[cx,cz].blocks[x,y+2,z])in mineralsNonBlocking) and
      (eblocktypes(chunksPointer^[cx,cz].blocks[x,y+1,z])in mineralsNonBlocking)
     and (eblocktypes(chunksPointer^[cx,cz].blocks[x,y,z])in mineralsNonBlocking)
     and (eblocktypes(chunksPointer^[cx,cz].blocks[x,y-1,z])in mineralsWalkable)
     then result:=true
  end;
  except
    log('blokWalkable:: out of bounds!');
  end;
end;


function getBlockCollisionUp(pos: TAffineVector): single;
var
  ch:TVector2i;
  blokpos:TVector3b;
  flag:boolean;
begin
  blokpos:=absolutePositionToChunkBlok(ch,pos);
  //jezeli jest juz tu jakis blok to sprawdzaj w gore
  //flag:=not (eblocktypes(chunksPointer^[ch[0],ch[1]].blocks[blokPos[0],blokPos[1],blokPos[2]]) in mineralsNonBlocking);
  //jezeli nie ma to szukaj w dol
  //true znaczy blok pelny
  flag:=true;
  if flag then
  while not (eblocktypes(chunksPointer^[ch[0],ch[1]].blocks[blokPos[0],blokPos[1],blokPos[2]]) in mineralsNonBlocking)
        and (blokPos[1]>0) do
          if flag then inc(blokPos[1]) else dec(blokPos[1]);
  //until (blok=btNone) or (blokPos[1]>=0);
  result:=blokPos[1];
end;

procedure SwapAB(var a,b :integer);
var
    c   :integer;
begin
    c := a;
    a := b;
    b := c;
end;

procedure SwapAB(var a, b: single);
var
    c   :single;
begin
    c := a;
    a := b;
    b := c;
end;

procedure line3D(x0,y0,z0,x1,y1,z1:integer;blok:eBlockTypes);
var
    x, delta_x, step_x  :integer;
    y, delta_y, step_y  :integer;
    z, delta_z, step_z  :integer;
    swap_xy, swap_xz            :boolean;
    drift_xy, drift_xz          :integer;
    cx, cy, cz,stepCount        :integer;
    a,b,c,chx,chy:cardinal;
    len:single;
begin
    stepCount:=0;
    len:=VectorDistance(vector3fmake(x0,y0,z0),vector3fmake(x1,y1,z1));
    swap_xy := Abs(y1 - y0) > Abs(x1 - x0);
    if swap_xy then begin
        SwapAB(x0, y0);
        SwapAB(x1, y1);
    end;
    swap_xz := Abs(z1 - z0) > Abs(x1 - x0);
    if swap_xz then begin
        SwapAB(x0, z0);
        SwapAB(x1, z1);
    end;
    delta_x := Abs(x1 - x0);
    delta_y := Abs(y1 - y0);
    delta_z := Abs(z1 - z0);
    drift_xy  := delta_x div 2;
    drift_xz  := (delta_x div 2);
    step_x := 1;  if (x0 > x1) then  step_x := -1;
    step_y := 1;  if (y0 > y1) then  step_y := -1;
    step_z := 1;  if (z0 > z1) then  step_z := -1;
    y := y0;
    z := z0;
    x:= x0;
    while (stepCount<=len) do begin
        cx := x;    cy := y;    cz := z;
        if swap_xz then SwapAB(cx, cz);
        if swap_xy then SwapAB(cx, cy);
        inc(stepCount);
        //check if we are not outside of chunks interns
        { TODO -copti : that should never happen in calling routine anyways so this check shouldn't be here }
        if not ((cx>=world_width) or (cx<0) or
           (cy>=chunk_height) or (cy<0) or
           (cz>=world_depth) or (cz<0)) then
           begin
             if (cx=x1) and (cy=y1) and (cz=z1) then break;
             a:=(cx mod chunk_width);
             //b:=abs(cy + chunk_height2);
             b:=cy;
             c:=(cz mod chunk_width);
             chx:=(cx  div chunk_width);
             chy:=(cz  div chunk_width);
             chunksPointer^[chx,chy].blocks[a,b,c]:=byte(blok);
//            chunksPointer^[chx,chy].blocks[a,b,c]:=byte(blok);
          end;
        //log('test:' + inttostr(a) + ', ' + inttostr(b) + ', ' + inttostr(c));
        //log(': ' + inttostr(cx) + ', ' + inttostr(cy) + ', ' + inttostr(cz));
        drift_xy := drift_xy - delta_y;
        drift_xz := drift_xz - delta_z;
        if drift_xy < 0 then begin
            y := y + step_y;
            drift_xy := drift_xy + delta_x;
        end;
        if drift_xz < 0 then begin
            z := z + step_z;
            drift_xz := drift_xz + delta_x;
        end;
        x:=x+step_x;
    end;
    //log('chunk: ' + inttostr(chx) + ', ' + inttostr(chy));
end;

//common buffer used by chunks to transfer data to their render objects.
//hopefully data type size doesn't changes across os'es. so it does for sure.

procedure takeCareOfDataBufferSize(size:cardinal);
begin
 if length(chunkVertexBuffer)<=size then   //expand transfer buffer size
   setlength(chunkVertexBuffer,length(chunkVertexBuffer)+
   chunk_width*chunk_width*chunk_height*sizeof(single)*vert_vec_size);
end;

{ TChunk }

procedure TChunk.writeBlock(x, y, z: integer; value: TBlockType);
begin
  world[fdataOffset+(x+(z*chunk_width))+y*chunk_xy_size]:=value;
  //world[fdataOffset+(x+(z+y*chunk_width)*chunk_width)]:=value;
  //fblocks^[x,y,z]:=value;
end;

function TChunk.readBlock(x, y, z: integer): TBlockType;
begin
  //result:=fblocks^[x,y,z];
  result:=world[fdataOffset+(x+(z*chunk_width))+y*chunk_xy_size];
end;

procedure TChunk.writePBlock(x, y, z: integer; value: pBlockType);
begin
  //fblocks^[x,y,z]:=value^;

  world[fdataOffset+(x+(z*chunk_width))+y*chunk_xy_size]:=value^;
end;

function TChunk.readPBlock(x, y, z: integer): pBlockType;
begin
  //result:=@fblocks[x,y,z];
  result:=@world[fdataOffset+(x+(z*chunk_width))+y*chunk_xy_size];
end;

function TChunk.getChunkDataStart: cardinal;
begin
  result:=fdataOffset;
end;

constructor TChunk.create(chx,chy:integer);
var i:integer;
begin
  inherited create();
  //setlength(fblocks,chunk_width,chunk_height,chunk_width);
  //i:=length(world);
  fdataOffset:=(chx*chunk_size)+((chy*active_chunks_w)*chunk_size);
  //fblocks:=@world[i];
  //copy terrain data to chunk
  {$IFNDEF SERVER}
  renderable:=TTerrainChunk.create(@chunkVertexBuffer,0);
  {$ENDIF}
 // renderable.isvisible:=true;
end;

destructor TChunk.destroy;
begin
  {$IFNDEF SERVER}
  renderable.free;
  {$ENDIF}
  inherited;
end;

function TChunk.getHandle: pointer;
begin
  result:=self;
end;

procedure TChunk.update;
begin

end;

procedure TChunk.setBlock(blokPos: TVector3b; blockType: eBlockTypes);
begin
   if (blokPos[0]>chunk_width-1) or (blokPos[2]>chunk_width-1) or (blokPos[1]>chunk_height-1) then exit;
   //eBlockTypes(blocks[blokPos[0],blokPos[1],blokPos[2]]):=blockType;
   blocks[blokPos[0],blokPos[1],blokPos[2]]:=byte(blockType);
  //vbo is updated outside since chunk doesn't have access to neighbour chunks
end;

procedure TChunk.setBlockUndo(blokPos: TVector3b; blockType: eBlockTypes);
var i:integer;
begin
  //jezeli bylo wcisniete undo to marker bedzie przed koncem pliku
  //w takim wypadku kasujemy to co jest dalej
  if (undoline>-1) and (undoLine<undoLog.count-1) then begin
     undoLog.BeginUpdate;
     for i:=undoLog.count-1 downto undoLine+1 do undoLog.Delete(i);
     undoLog.endUpdate;
  end;
  //could have field for distinguish block/lights/etc operations
  undoLog.add(format('%d %d %d %d %d %d %d %d',[undoBatchID,blokPos[0],blokPos[1],blokPos[2],
  xID,yID,
  integer(getBlock(blokPos)),
  integer(blockType)]));
  inc(undoLine);
  //for batch undos like brush paint, all commands will have same id.
  //for single block operations each will be different
  //if not undoBatch then inc(undoBatchID);
  setBlock(blokPos,blockType);
end;

function TChunk.getBlock(blokPos: TVector3b): eBlockTypes;
begin
  result:=eBlockTypes(blocks[blokPos[0],blokPos[1],blokPos[2]]);
end;

procedure TChunk.setBlock(x, y, z: byte; blockType: eBlockTypes);
var
  xx,yy,zz:cardinal;
begin
  blocks[x,y,z]:=byte(blockType);
end;

function TChunk.checkRayBlockInteresct(rayStart, rayDir, rayHit: TAffineVector;
  var blk:TAffineVector): boolean;
var
  x,y:integer;
begin
  //hitBoxSize ;
end;


end.

