unit core_game;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, core_types, core_terrain_generator,
  VectorGeometry, core_player, core_actor, core_orders,
  core_render,core_atmosphere, ui_main_menu,
  core_orders_manager;

type


  { TGame }

  TGame = class
   public
    aliens:TPlayer;
    procedure setEnemies(p,pp:tplayer);
    //loads actors when map is loaded.
    //also sets controlledActorID for player1 if one of actors has name=player1
    procedure loadActorsFromFile(const filename: string);
    procedure loadKnownItems(const path:string);
    procedure saveGame(const path: string);
    procedure loadGame(path: string);
    //procedure
    constructor create(playersCount:byte);
    destructor destroy;override;
  end;

var
  game:tgame;

implementation

{ TGame }

procedure TGame.setEnemies(p, pp: tplayer);
begin
  p.enemies+=[pp.playerID];
  pp.enemies+=[p.playerID];
  p.friends-=[pp.playerID];
  pp.friends-=[p.playerID];
  p.neutrals-=[pp.playerID];
  pp.neutrals-=[p.playerID];
end;

procedure TGame.saveGame(const path:string);
begin
  terrain.saveMap(path+'map.terrain');
  renderer.saveLights(path+'map.terrain');
end;

procedure TGame.loadGame(path: string);
var i:integer;
begin
  log(format('loading map: %s',[ExtractFileName(path)]));
  //maybe here we can tell if it's single or multiplayer mode?
  //difference being that in multi, client shouldn't load map actors?
  //only terrain
  //multiPlayerMode could be simpy set when selectig it in main menu
  multiPlayerMode:=false;//multiMode;
  terrain.loadMap(path);

  renderer.loadLights(path);
  //dont load in multi client. actors will be spawned with commands from server
  path:=ChangeFileExt(path,'.actors');
  if multiPlayerMode=false then begin
      loadActorsFromFile(path);
      //oM.addOrder(orToggleBrain,player1.actorID,0);
      //camera follow actor
      //oM.addCameraMode(player1.actorID,cmEdge);
  end else begin
    //in multiplayer there is no default player1 actor as we didn't load him from file
    //so we need to first login to remote server and get him or he needs to be already assigned when we get here
    om.addOrder(orLogin,0,0);
  end;
  //camera.setAboveGround(40);
  //loadKnownItems(extractfilepath(path)+'knownItems.txt');
  //get control over actor0

end;

procedure TGame.loadActorsFromFile(const filename: string);
var
  i,j,actorID,faction,typ:integer;
  position:TAffineVector;
  str:tstringlist;
  a:tactor;
  o:rorder;
  key,value,name:string;
begin
  if not fileexists(filename) then begin
      log('achtung! TGame.loadActorsFromFile:: file doesn''t exist: '+filename);
      exit;
  end;
  str:=TStringList.create;
  str.LoadFromFile(filename);
  i:=0;actorID:=-1;typ:=-1;faction:=0;name:='noname';
  //reset id counter
  //lastActorID:=0;
  while i<str.Count do begin
    str.GetNameValue(i,key,value);
    if key='actorID' then begin
       actorID:=strtoint(value);
    end else
    if key='name' then begin
       Name:=value;
    end else
    if key='posX' then begin
       position[0]:=strtofloat(value);
    end else
    if key='posY' then begin
       position[1]:=strtofloat(value);
    end else
    if key='posZ' then begin
       position[2]:=strtofloat(value);
    end else
    if key='faction' then begin
       faction:=strtoint(value);
    end else
    if key='rotY' then begin
       faction:=strtoint(value);
    end else
    if key='typ' then begin
       typ:=strtoint(value);
    end else
    //u.position[0]:=f1;u.position[1]:=f2;u.position[2]:=f3;
    if value='[/actor]' then begin
       //actors.Add(a.actorID,a);
        //retain id links
        //if lastActorID<a.actorID then lastActorID:=a.actorID;
        //add an order and mark it as processed by logic
        j:=oM.addOrder;
        o:=om.queue[j];
        //skip second create on logic update
        //o.eatenLogic:=true;
        o.order:=orCreateActor;
        o.int1:=actorID;
        o.int2:=typ;
        o.int3:=faction;
        o.v1:=position;
        om.queue[j]:=o;
        if name=player1.name then player1.actorID:=actorID;
    end;
    inc(i);
  end;
  str.free;
end;

procedure TGame.loadKnownItems(const path: string);
var
  str:TStringList;
  i:integer;
begin
  str:=TStringList.create;
  str.LoadFromFile(path);
  for i:=0 to str.count-1 do begin
  //  alembik.getBlueprint(eItemTypes(GetEnumValue(typeinfo(eItemTypes),str[i]))).known:=true;
  end;
  str.free;
end;

constructor TGame.create(playersCount: byte);
var i:integer;
begin
  setlength(players,playersCount);
  for i:=0 to length(players)-1 do players[i]:=TPlayer.create(i);
  //if single player?
  player1:=players[0];
  aliens:=players[1];
  setEnemies(player1,aliens);
  player1.resources[byte(btStone)]:=100;
  player1.resources[byte(btRedSlab)]:=128;
  player1.resources[byte(btIron)]:=13;
  player1.resources[byte(btCopper)]:=25;
  oM.addOrder(orCameraMode,-1,integer(eCameraModes.cmEdge));
end;

destructor TGame.destroy;
var i:integer;
begin
  for i:=0 to length(players)-1 do players[i].destroy;
  inherited;
end;

end.

