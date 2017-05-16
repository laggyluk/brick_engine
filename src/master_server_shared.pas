unit master_server_shared;

{$mode objfpc}{$H+}

interface

uses core_actor_definitions;

//client has to 'ping' serwer every n seconds or will be dropped
const max_ping_time = 30 ;

type

  {$packenum 1}
  emsStates = (
              //responses
              msServerDown,//couldn't connect to server
              msLoggedIn,//login ok, we are in
              msLoginFailed,//auth failed
              msDisconnected,//connection to master server lost
              msSearchingForGame,//searching for a game
              msPong,//response for ping from serv
              msRoomInfo,//notify that next we'll be sending room info
              msPlayerInfo,
              //requests
              msLogin ,
              msFindGame,
              msLogOut,
              msHostGame,
              msPing, //must be sent from client on interval or will be dropped. server resopnds with pong
              msCancelSearch,//client doesn't want to search for a gama anymore
              msExitRoom//send to server to leave room/ receive on client followed by seat byte when someone left the room
              );

  rSessionSettings = packed record
    difficulty:byte;
    //map world coordinates 123x554
    mapX,mapY:integer;
    privateRoom:boolean;
  end;

  //player info for others to see
  rPlayerInfo = packed record
    character:eActorTypes;//chosen character
    weapon1:eWeaponTypes;
    weapon2:eWeaponTypes;
    weapon1Mod1:eWeaponMod;
    weapon1Mod2:eWeaponMod;
    weapon2Mod1:eWeaponMod;
    weapon2Mod2:eWeaponMod;
    armorMod:eArmorMods;
  end;

  function eatString(var s: string;separator:char = ';'):string;

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


end.

