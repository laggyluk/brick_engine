unit server_server;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  rPlayerInfo = record
    login:string;
  end;

  //when player hosts a game, this is created and added to servers list
  TGameRoom = class
    mapName:string;
    players:array of rPlayerInfo;
  end;

  //has list of game rooms. probably should have static size and restart game on new match
  //to keep top perforrmance. but then what about loading map and resulting spike?
  TServer = class

  end;

implementation

end.

