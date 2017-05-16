unit core_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,dglOpenGL,VectorGeometry, fgl, VectorTypes;

const
  pi:double = 3.14159265358979323846264338327950288;
  pi_half:double = 1.5707963267948966192313216916398;
  pi_2:double = 6.283185307179586476925286766559;
  //pi_4:double = pi_2 / 2;
  //fixed limit of visible lights in forward rendering
  MAX_POINT_LIGHTS  = 2;
  MAX_SPOT_LIGHTS = 2;

  vert_vec_size = 4;//vectors holding vertexes and colors could be different in size

  block_size = 1;// block dimension for opengl visualisation
  block_size2 = 0.5;

  destProximity = 0.3;
  //
  colorSelection : TVector4f = (0.1,0.1,0.3,0.5);
  colorGray: TVector4f = (0.5,0.5,0.5,0.5);
  colorJobArea: TVector4f = (0.1,0.1,0.3,0.5);
  ////////////////////////////////////networking//////////////////////////////

  //how often check for incoming packets  (in s)
  refreshInterval = 0.0333333;

  //data seize needed for actor's properties in history snapshot
  //id 2b, position 12b, velocity 12b, actor type 1b, rotY 2b, hitbox 2x12b
  snapEntitySize = 53;
  //how many snapshots back in time will be saved
  historyPageCount = 255;
  snapPageSize = 128*snapEntitySize; //max 128* entity in snapshot.

var
  active_chunks_w:integer = 2;
  active_chunks_h:integer = 2;//map size
  chunk_width:integer = 64;  //
  chunk_width2:integer;// = chunk_width div 2;
  chunk_widthf:single;// = chunk_width * block_size;
  chunk_width2f:single;// = chunk_width * block_size div 2;

  chunk_height:integer = 128;//255?

  chunk_height2:integer;// = chunk_height div 2;
  chunk_heightf:single;// = chunk_height *block_size;
  chunk_height2f:single;// = (chunk_height *block_size) div 2;
  chunk_size:integer;// = chunk_width*chunk_width*chunk_height;
  chunk_xy_size:integer; //chunk_width*chunk_width;
  world_width:integer;// = active_chunks_w*chunk_width;
  world_width2:integer;// = world_width div 2;
  world_depth:integer;// = active_chunks_h*chunk_width; //atually world depth?
  world_depth2:integer;// = world_depth div 2;
  world_xy_size:integer;
  //totalTime  for time compenstation in actors that are not updated every frame
  totalTime:extended;
  //updated each network thread loop
  netFrame:cardinal;
  //let the center of object be in its center, not at 0,0
  //those are bounduaries for table iteration

  //direction vectors
  fwdVector,upVector,rightVector:TAffineVector;
  //game/editor
  gameMode:boolean;
  multiPlayerMode:boolean;
  //limit pathfinding search
  maxPathSteps:cardinal = 1024000;
  //ignore signals from analog input below this
  analogNoiseLevel:single = 0.1;
  stickSensitivityL:single = 0.5;
  stickSensitivityR:single = 0.8;
  mouseSensitivityX:single = 0.1;
  mouseSensitivityY:single = 3.1;
  mouseInvertY :boolean = false;
  //when true, skip level rendering and show some image, console msgs or sth
  showLoadingScreen:boolean = true;

type

  pBytes = ^TBytes;

  TLogProcedure = procedure(const s:string);

  TBlockType = byte; //defines size of data that will be defining each block
                         //that way in case it will need increasing beyond type capacity it will
                         // be easier. sort of
  pBlockType = ^TBlockType;
  //TBlocksBuffer = array [0..chunk_width-1]of array [0..chunk_height-1]
  //         of array [0..chunk_width-1]  of TBlockCoreType;
  TBlocksBuffer = array of array of array of TBlockType;
  PBlocksBuffer = ^TBlocksBuffer;

  TBlockMutator = procedure(block: pBlockType) of object;

  TListOfIntegers = specialize TFPGList <integer>;

  TDataBuffer  =array of single;
  TFrust  =array [0..23] of single;
  PFrust = ^TFrust;

  PDataBuffer = ^TDataBuffer;

  rBufferDataBit = packed record
    x,y,z,w,
    r,g,b,a:single;
  end;

  pChunk = pointer;

  rOptions = record
    renderer:record
      ssao:boolean;
      ssaoSamples:integer;
      ambientOcclusion:boolean;
      aoRayLength:integer;
      aoSkosRayLength:integer; //sqrt(aoRayLength)?
      aoStep:single;
    end;
    debug: record
      orders:boolean;//print order messages
      drawPaths:boolean;
    end;
  end;

  tPointf = record
    x,y:single;
  end;

  tPacketFlags = bitpacked record
    Bit0,
    Bit1,
    Bit2,
    Bit3,
    Bit4,
    Bit5,
    Bit6,
    Bit7: Boolean;
  end;

  {$PACKENUM 1}
  //new block needs to be added here. color def is in terrain\blockColorDefs
  eBlockTypes = (
    btNone,btUndefined,btStone,
    btYellowFlower,
    btWoodBrown,btWoodLight,btFloraLight,btFlora,btBlackRock,
    btQuartzite, btGneiss,btTourmaline,btPotassiumChloride,btRedRock,btNeodymium,
    btTitanium,btIce,btIron,btRedSlab, btCeramic,//theres' no lava on mars
    btSoil,btPlastic,btCopper,btGold, btQuartz, btAluminium,
    btFiberOil, //do produkcji plastyku, zamiast ropy naftowej ;p
    btLazure,btPink,btRadioActive,btNavy,btLime,btOlive,btGreenDark,
    btOrange,btMint,btBlueBright,btPlatinium,btSheisse,btMortadele,
    btYellowDark,btYellowMed,btOrangeDark,btSettingSun,btGalactic,
    bt1,bt2,bt3,bt4,bt5,bt6,bt7,bt8,bt9,bt10,bt11,bt12,bt13,bt14,bt15,bt16,
    bt17,bt18,bt19,bt20,bt21,bt22,bt23,bt24,bt25,bt26,bt27,bt28,bt29,bt30,bt31,
    bt32,bt33,bt34,bt35,bt36,bt37,bt38,bt39,bt40,bt41,bt42,bt43,bt44,bt45,bt46,
    bt47,bt48,bt49,bt50,bt51,bt52,bt53,bt54,bt55,bt56,bt57,bt58,bt59, bt60,
    bt61,bt62,bt63,bt64,bt65,bt66,bt67,bt68,bt69,bt70,bt71,bt72,bt73,bt74,bt75,bt76,
    bt77,bt78,bt79,bt80,bt81,bt82,bt83,bt84,bt85,bt86,bt87,bt88,bt89,
    bt90,bt91,bt92,bt93,bt94,bt95,bt96,bt97,bt98,bt99,
    bt100,bt101,bt102,bt103,bt104,bt105,bt106,bt107,bt108,bt109,
    bt110,bt111,bt112,bt113,bt114,bt115,bt116,bt117,bt118,bt119,
    bt120,bt121,bt122,bt123,bt124,bt125,bt126,bt127,bt128,bt129,
    bt130,bt131,bt132,bt133,bt134,bt135,bt136,bt137,bt138,bt139,
    bt140,bt141,bt142,bt143,bt144,bt145,bt146,bt147,bt148,bt149,
    bt150,bt151,bt152,bt153,bt154,bt155,bt156,bt157,bt158,bt159,
    bt160,bt161,bt162,bt163,bt164,bt165,bt166,bt167,bt168,bt169,
    bt170,bt171,bt172,bt173,bt174,bt175,bt176,bt177,bt178,bt179,
    bt180,bt181,bt182,bt183,bt184,bt185,bt186,bt187,bt188,bt189,
    bt190,bt191,//bt192,bt193,bt194,bt195,bt196,bt197,bt198,bt199,
//    bt200,bt201,bt202,bt203,bt204,bt205,bt206,bt207,bt208,bt209,
    btZizanium,
    btInvisibleBlocking,
    btWater1,btWater2,btWater3,btWater4,btWater5,btWater6,btWater7,
    btOxy1,btOxy2,btOxy3,btOxy4,btOxy5,btOxy6,btOxy7,btOxy8,btOxy9,btOxy10,
    btMax);

  sMinerals = set of eBlockTypes;
  //add all the sets to sets map, since it's not possible to convert string to set name
  TMineralSetsMap = specialize TFPGMap<string,sMinerals>;
  eResouceTypes = (rtEnergy,rtOxygen,rtFluid);

  //list of items that can be constructed
  eItemTypes = (
             itNone,
             itAirlock,
             itComputer,
             itCircuitBoard,
             itWeedSeed,
             itStashPortal,
             itHugeCPU,
             itFiberPlastic,
             itMediumCPU);

  //tryby pracy kamery
  eCameraModes = (
             cmFree, //default in editor
             cm90,   //follow player, rotates in 90 degree jumps
             cmFollow, //follows player, right stick orbits cam
             cm3rdPerson, //3rd person mouse look
             cmEdge,//camera moves left,right,up,down along map edge, fallout shelter style
             cmMax
             );

  //nie dzialaja czyzby?
  function getWorldArray(x,y,z:cardinal):eBlockTypes;
  function getWorldCentered(x,y,z:integer):eBlockTypes;
  function getWorldArrayF(ix,iy,iz:single):eBlockTypes;
  function getWorldCenteredF(ix,iy,iz:single):eBlockTypes;
  procedure clearUndoHistory;

var
  //world data
  world: array of TBlockType;
  //those are set in initialization
  mineralsRare: sMinerals;
  mineralsSurface: sMinerals;
  mineralsCommon: sMinerals;
  mineralsPlants: sMinerals;
  mineralsAll: sMinerals;
  //you can plant trees and stuff on it
  mineralsEarth: sMinerals;
  //deep/rare minerals
  mineralsDeep: sMinerals;
  mineralsDiggable: sMinerals;
  //can walk on them. solid ground
  mineralsWalkable: sMinerals;
  //can wolk through them
  mineralsNonBlocking:sMinerals;
  //set of blocks defining water
  mineralsWater:sMinerals;
  mineralsOxygen: sMinerals;
  //block types that are not rendered
  mineralsInvisible: sMinerals;
  //same, but used in cullChunk so it can be cleared to enable debug visualization
  // and then restored by copying from mineralsInvisible
  mineralsDontRender: sMinerals;
  mineralSetsMap:TMineralSetsMap;
  log:TLogProcedure; //pointer to procedure used for debug text output
  log_Add:TLogProcedure; //zenGL logging routine alias
  appPath:string; //path to application exe folder
  deltaT:single; //delta time for logic
  //global frame counter
  globalFrame:cardinal;
  //window dimensions. 1st two are half size and second2 are w & h
  viewport:tvector4i;
  //options used by rendered, should be set prior to initialization
  Options:rOptions;
  worldCameraMatrix:TMatrix;//holds projection matrix*world matrix, for optimization. calculated on calcCulling
  //maybe colors could be adjusted for different 'climats' or planet types
  //colors definitions are loaded from ini
  blockColors:array [btNone..btMax] of TVector4f;
  //defines depth on which mineral is most dense. from center? 127
  mineralDeepness:array [btNone..btMax] of smallint;
  {max. shouldn't be used }
  //flags that camera moved and transforms need to be updated on some objects
  worldMoved:boolean;
  //paths
  modelsFolder:string;
  undoLog:tstringlist;
  //current position in history file
  undoLine:integer;
  //distinguish undo blocks
  undoBatchID:integer;
  //fix one ctrl-z being ignored when undo direction changes
  undoLast:boolean;
implementation

procedure clearUndoHistory;
begin
  undoLog.clear;
  undoLine:=-1;
  undoBatchID:=0;
end;

function getWorldArray(x, y, z: cardinal): eBlockTypes;
var
    xx,zz,c,b,chx,chy:cardinal;
begin
  result:=btNone;
  if (x<0) or (z<0) or (y<0) then
     exit;
  if (x>=world_width) or (y>=chunk_height) or (z>=world_depth) then
     exit;
  xx:=x mod chunk_width;
  zz:=z mod chunk_width;
  chx:=x div chunk_width;
  chy:=z div chunk_width;
  c:=(xx + (zz * chunk_width) + y * chunk_xy_size)+(chx*chunk_size)+((chy*active_chunks_w)*chunk_size);
  result:=eBlockTypes(world[c]);
end;

function getWorldArrayF(ix, iy, iz: single): eBlockTypes;
begin
  result:=getWorldArray(round(ix),round(iy),round(iz));
end;

function getWorldCentered(x, y, z: integer): eBlockTypes;
var c,b:cardinal;
    xx,yy,zz,chx,chy:integer;
begin
  x:=(x+world_width2);
  z:=(z+world_depth2);
  xx:=x mod chunk_width;
  zz:=z mod chunk_width;
  yy:=round(y);
  chx:=x div chunk_width;
  chy:=z div chunk_width;
  c:=(xx + (zz * chunk_width) + yy * chunk_xy_size)+(chx*chunk_size)+((chy*active_chunks_w)*chunk_size);
  result:=eBlockTypes(world[c]);
end;

function getWorldCenteredF(ix, iy, iz: single): eBlockTypes;
begin
  result:=getWorldCentered(round(ix),round(iy),round(iz));
end;

procedure setWorldCentered(x, y, z: integer; value: eBlockTypes);
var c,b:cardinal;
    xx,yy,zz,chx,chy:integer;
begin
  x:=(x+world_width2);
  z:=(z+world_depth2);
  xx:=x mod chunk_width;
  zz:=z mod chunk_width;
  yy:=round(y);
  chx:=x div chunk_width;
  chy:=z div chunk_width;
  c:=(xx + (zz * chunk_width) + yy * chunk_xy_size)+(chx*chunk_size)+((chy*active_chunks_w)*chunk_size);
  world[c]:=byte(value);
end;

initialization
  mineralsWater:=[btWater1,btWater2,btWater3,btWater4,btWater5,btWater6,btWater7];
  mineralsOxygen:=[btOxy1 .. btOxy10];
  mineralsRare:= [btNeodymium,btTitanium];
  mineralsSurface:= [btGneiss,btStone,btRedRock,btIce];
  mineralsPlants:= [btWoodBrown,btFloraLight];
  mineralsDeep:= [btIron];
  mineralsCommon:= [btStone .. btZizanium]-mineralsWater-mineralsOxygen;
  mineralsCommon:= mineralsCommon - (mineralsRare - mineralsPlants);
  mineralsAll:= [btStone .. btZizanium];
  mineralsDiggable:= (mineralsAll - mineralsWater)-mineralsOxygen;
  //define mineral contrentration levels
  mineralDeepness[btStone]:=127;
  //mineralDeepness[btWater]:=127;//wtf
  mineralDeepness[btQuartzite]:=130;//up is -20 or +20?
  mineralDeepness[btNeodymium]:=70;
  mineralDeepness[btTourmaline]:=100;
  mineralDeepness[btPotassiumChloride]:=110;
  mineralDeepness[btIron]:=10;
  mineralsWalkable:= (mineralsAll - mineralsWater)-mineralsOxygen;
  mineralsNonBlocking:=[btNone]+mineralsOxygen;
  mineralsInvisible:=[btNone,btInvisibleBlocking]; //+mineralsOxygen;
  mineralsDontRender:=mineralsInvisible;
  mineralsEarth:=[btSoil];

  MineralSetsMap:=TMineralSetsMap.create;
  MineralSetsMap.Add('mineralsWater',mineralsWater);
  MineralSetsMap.Add('mineralsOxygen',mineralsOxygen);
  MineralSetsMap.Add('mineralsRare',mineralsRare);
  MineralSetsMap.Add('mineralsSurface',mineralsSurface);
  MineralSetsMap.Add('mineralsDeep',mineralsDeep);
  MineralSetsMap.Add('mineralsCommon',mineralsCommon);
  MineralSetsMap.Add('mineralsDiggable',mineralsDiggable);
  MineralSetsMap.Add('mineralsWalkable',mineralsWalkable);
  MineralSetsMap.Add('mineralsNonBlocking',mineralsNonBlocking);
  MineralSetsMap.Add('mineralsInvisible',mineralsInvisible);
  MineralSetsMap.Add('mineralsAll',mineralsAll);
  MineralSetsMap.Add('mineralsDontRender',mineralsDontRender);
  MineralSetsMap.Add('mineralsEarth',mineralsEarth);

  //Options.debug.drawPaths:=true;
  setVector(fwdVector,1,0,0);
  setVector(upVector,0,1,0);
  rightVector:=VectorCrossProduct(fwdVector,upVector);

finalization
  MineralSetsMap.free;
end.

