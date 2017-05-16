unit core_tcp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, core_actor_definitions, fgl;

const
   maxPlayers = 8;
   //how many seconds till match start after majority is rady
   gameAutoInitTime = 25;
   gameInitShortTime = 1;
type

  {$packenum 1}
  eTcpMessages = (
                  mmNone,
                  mmLogin,
                  mmPing,
                  mmHost,
                  mmFind,
                  mmExitRoom,
                  msDisconnect,
                  mmStartGameHost,
                  mmJoinGame,
                  mmRoomState,
                  mmPlayerInfo
                  );

  eRoomStates = (rsBeforeGame,rsInGame,rsAfterGame);

  tLogin = string[16];
  tIp = array[0..3] of byte;
  pLogin = ^TLogin;

  //what player is wearing for the match
  rPlayerInfo = packed record
    character:eActorTypes;
    weapon1:eWeaponTypes;
    weapon2:eWeaponTypes;
    weapon1Mod1:eWeaponMod;
    weapon1Mod2:eWeaponMod;
    weapon2Mod1:eWeaponMod;
    weapon2Mod2:eWeaponMod;
    //login:tLogin;
  end;

  rMapInfo = packed record
    //serves as planet coords/name
    mapX,mapY:integer;
    difficulty:byte;
    serverIp:tIp;
  end;

  //structure for holding info about players in a room. used in tcp gameroom module
  TPlayerInfo = packed record
    login:tlogin;
    qport:word;
    info:rPlayerInfo;
    ip:tIP;
  end;
  TPlayerInfoArray = array of TPlayerInfo;

  procedure convertIp(const from:string;var toIp:tIp);
  procedure convertIp(const from:tIp;var toStr:shortstring);

implementation

function eatString(var s: string;separator:char = ';'):string;
var i:integer;
begin
  result:='';
  if s='' then exit;
  for I := 1 to length(s) do if s[i]=separator then break;
  if s[i]<>separator then begin
    result:=copy(s,1,i);
    s:='';
  end
  else begin
      result:=copy(s,1,i-1);
      s:=copy(s,i+1,length(s)-i);
  end;
end;

procedure convertIp(const from: string; var toIp: tIp);
var s:string;
begin
  s:=from;
  if s='' then s:='0.0.0.0';
  toIp[0]:=strtoint( eatString(s,'.'));
  toIp[1]:= strtoint( eatString(s,'.'));
  toIp[2]:= strtoint( eatString(s,'.'));
  toIp[3]:= strtoint( s);
end;

procedure convertIp(const from: tIp;var toStr: shortstring);
begin
  toStr:=format('%d.%d.%d.%d',[from[0],from[1],from[2],from[3]]);
end;



end.

