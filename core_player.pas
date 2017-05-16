unit core_player;

{$mode objfpc}{$H+}

interface

uses
  fgl, VectorGeometry;

type

  eFactionAttitudes = (faOwn,faFriendly,faNeutral,faEnemy);

  sPlayers = set of byte;
  //for stash portals: id,position
  TAffineVectorMap = specialize TFPGMap<integer,TAffineVector>;
  { TPlayer }

  TPlayer = class
    login:string[16];
    name:string;
    playerID:byte;
    //relations with other factions. set of playerID
    enemies: sPlayers;
    friends: sPlayers;
    neutrals: sPlayers;
    //avaliable resources
    resEnergy:integer;
    resOxygen:integer;
    //clips
    ammo:integer;
    //magazyn surowcow
    resources:array [0..255] of integer;
    //lista stash portali for quick lookup
    stashPortals:TAffineVectorMap;
    //actorID that this player is controlling currently. msg's from input are sent
    //to it
    actorID:integer;
    //id of input device?joystick? actually not needed if no 2players on same machine..
    controllerID:integer;
    //ip that is allowed to connect to this slot.one time and no reconnect?
    ip:string[16];
    constructor create(playerFaction:byte);
    destructor destroy;override;
    //returns nearest portal position in var and id or -1 if no portals found
    function getNearestStashPortal(const origin: TAffineVector;
      var location: TAffineVector): integer;
  end;
  function getEnemies(faction: byte): sPlayers;

var
  players:array of TPlayer;
  player1:TPlayer;

implementation

function getEnemies(faction: byte): sPlayers;
begin
  result:=players[faction].enemies;
end;


{ TPlayer }

constructor TPlayer.create(playerFaction: byte);
begin
  playerID:=playerFaction;
  stashPortals:=TAffineVectorMap.create;
  ammo:=132;
  name:='player1';
  login:='player1';
  //weapon use
  //stash.newItem(itStashPortal);
end;

destructor TPlayer.destroy;
begin
  stashPortals.free;
  inherited destroy;
end;

function TPlayer.getNearestStashPortal(const origin:TAffineVector;var location: TAffineVector): integer;
var i:integer;
    f:single;
    minDistance:single;
begin
  result:=-1;
  minDistance:=MaxSingle;
  for i:=0 to stashPortals.Count-1 do begin
      f:=VectorDistance(origin, stashPortals.data[i]);
      if f<minDistance then begin
        location := stashPortals.data[i];
        minDistance:=f;
        result:=stashportals.Keys[i];
      end;
  end;
end;

end.

