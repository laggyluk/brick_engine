unit szmaster_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  blcksock, winsock, Synautil, fgl, ssl_openssl, mysql55conn, sqldb,
  master_server_shared, LCLIntf, ExtCtrls, core_actor_definitions;

type

  eRoomStates = (rsBeforeGame,rsInGame,rsAfterGame);

  { TGameRoom }

  TGameRoom = class
    settings:rSessionSettings;
    //player at seat 0 is room owner
    players:array [0..3] of pointer;
    state:eRoomStates;
    function countPlayers:integer;
    //when someone leaves let others know
    procedure leave(who:pointer);
    procedure addPlayer(who:pointer);
  end;

  { TClientThread }

  TClientThread = class(TThread)
  private
    Sock:TTCPBlockSocket;
    state:emsStates;
    lastSeen:integer;
    room:TGameRoom;
    procedure sendRoomInfoToRemoteClient;
  public
    login:string[16];
    //match settings
    settings:rSessionSettings;
    playerInfo:rPlayerInfo;
    //when client is joined to room this flags that he should send
    //room info to remote client;
    notifyRoomJoin:boolean;
    Constructor Create (hsock:tSocket);
    Destructor Destroy; override;
    procedure Execute; override;
    procedure leaveRoom;
  end;

  TClientList = specialize TFPGList<TClientThread>;
  //TGameRoomList = specialize TFPGList<TGameRoom>;

  { TMServerThread }

  TMServerThread = class(TThread)
  private
    Sock:TTCPBlockSocket;
    clients:TClientList;
    //once a while we wanna get rid of closed clients
    lastCleanUp:cardinal;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Execute; override;
    function roomCount:integer;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    roomsLabel: TLabel;
    select: TButton;
    connectBtn: TButton;
    memoLog: TMemo;
    MySqlConnection: TMySQL55Connection;
    Query: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure selectClick(Sender: TObject);
    procedure connectBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    server:TMServerThread;
  public
    { public declarations }
  end;

  procedure log(const s:string);

var
  Form1: TForm1;
  searchingQueue:tthreadList;
  gameRooms:tthreadList;

implementation
var froomCount:integer;

{$R *.lfm}

{ TGameRoom }

function TGameRoom.countPlayers: integer;
var i:integer;
begin
  result:=0;
  for i:=0 to length(players)-1 do if players[i]<>nil then inc(result);
end;

procedure TGameRoom.leave(who: pointer);
var j,i:integer;
    player:tclientThread;
begin
  for i:=0 to length(players)-1 do if players[i]=who then players[i]:=nil;
  //move players to new positions
  for i:=0 to length(players)-1 do
    for j:=i downto 0 do
      if (players[j]<>nil) and (players[i]=nil) then begin
         players[i]:=players[j];
         players[j]:=nil;
      end;

  //now notify about seats change
  for i:=0 to length(players)-1 do if players[i]<>nil then begin
    player:=tclientThread(players[i]);
    player.sock.SendByte(byte(msExitRoom));
    player.sock.SendByte(byte(i));
  end;
end;

procedure TGameRoom.addPlayer(who: pointer);
var i:integer;
begin
  for i:=0 to length(players)-1 do if players[i]=nil then players[i]:=who;
end;

{ TForm1 }

procedure TForm1.selectClick(Sender: TObject);
begin
  query.Open;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  server.Terminate;
  server.WaitFor;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TClientThread(gameRooms.LockList.Items[0]).sendRoomInfoToRemoteClient;
  gameRooms.UnlockList;
end;

procedure TForm1.connectBtnClick(Sender: TObject);
begin
  MySqlConnection.Connected:=true;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  server:=TMServerThread.create;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  roomsLabel.Caption:=format('rooms: %d',[server.roomCount]);
end;

procedure log(const s: string);
begin
  form1.memoLog.lines.add(format('%s> %s',[datetimetostr(now),s]));
end;

{ TMServerThread }

constructor TMServerThread.Create;
begin
  sock:=TTCPBlockSocket.create;
  gameRooms:=TThreadlist.create;
  searchingQueue:=tthreadList.create;
  FreeOnTerminate:=true;
  inherited create(false);
end;

destructor TMServerThread.Destroy;
begin
  gameRooms.free;
  searchingQueue.free;
  Sock.free;
  inherited Destroy;
end;

procedure TMServerThread.Execute;
var
  ClientSock:TSocket;
  i,j,k:integer;
  rooms:tlist;
  room:TGameRoom;
  client:TClientThread;
  finders:tlist;
  lastRoomServed:integer;
begin
  clients:=TClientList.create;
  lastRoomServed:=0;
  with sock do
    begin
      CreateSocket;
      setLinger(true,10000);
      bind('0.0.0.0','440');
      listen;
      repeat
        if terminated then break;
        if canread(1000) then
          begin
            ClientSock:=accept;
            if lastError=0 then begin
              clients.Add(TClientThread.create(ClientSock));
            end;
          end;
          //od czasu do czasu usuwaj tez zamkniete clienty
          if GetTickCount-lastCleanUp>10000 then begin
             lastCleanUp:=GetTickCount;
             for i:=clients.Count-1 downto 0 do if clients[i].Terminated then
               begin
                 clients[i].free;
                 clients.delete(i);
               end;
          end;
          //match ppl
          rooms:=gameRooms.LockList;
          finders:=searchingQueue.LockList;
          if lastRoomServed>finders.Count-1 then lastRoomServed:=finders.count-1;
          for j:=finders.Count-1 downto 0 do begin
            client:=tclientThread(finders[j]);
            for i:=lastRoomServed downto 0 do begin
              room:=tgameRoom(rooms[i]);
              //private rooms should be handled somwhere else
              if room.settings.privateRoom then continue;
              k:=room.countPlayers;
              if k<4 then begin
                 room.addPlayer(@client);
                 client.room:=room;
                 client.notifyRoomJoin:=true;
                 searchingQueue.Remove(client);
                 log(format('%s joined room %d',[client.login,i]));
                 //remove room? not rly, rather move to another list
                 //cause we want to verify match outcome
                 {if k>2 then begin
                   gameRooms.Remove(room);
                   dec(froomCount);
                 end;}
                 break;
              end;
            end;
          end;
          dec(lastRoomServed);
          if lastRoomServed<=0 then lastRoomServed:=rooms.Count-1;
          gameRooms.UnlockList;
          searchingQueue.UnlockList;
      until false;
    end;
  for i:=0 to clients.Count-1 do begin
    if clients[i]<>nil then clients[i].terminate;
  end;
  clients.free;
end;

function TMServerThread.roomCount: integer;
begin
  result:=froomCount;
end;

{ TClientThread }

procedure TClientThread.sendRoomInfoToRemoteClient;
var i:integer;
    player:TClientThread;
    lgn:string[16];
begin
  notifyRoomJoin:=false;
  if room=nil then exit;
  log(format('sending room info to %s',[login]));
  //tell the client that we'll be transfering room info
  sock.SendByte(byte(msRoomInfo));
  sock.SendBuffer(@room.settings,sizeof(room.settings));
  //then send players info
  //sock.SendByte(room.countPlayers);
  with room do begin
  for i:=0 to length(players)-1 do begin
    player:=nil;
    //sock.SendByte(byte(i));
    //first login if exists
    if players[i]=nil then lgn:='n/a' else begin
      player:=TClientThread(players[i]);
      lgn:=player.login;
    end;
    sock.SendString(lgn+crlf);
    //then rest of info, weapons etc
    if player<>nil then begin
       sock.SendBuffer(@player.PlayerInfo,sizeof(rPlayerInfo));
    end;
    //
  end;
  end;
end;

constructor TClientThread.Create(hsock: tSocket);
begin
  sock:=TTCPBlockSocket.create;
  Sock.socket:=HSock;
 // FreeOnTerminate:=true;
  inherited create(false);
end;

destructor TClientThread.Destroy;
begin
  Sock.free;
  inherited Destroy;
end;

procedure TClientThread.Execute;
var
  timeout: integer;
  s: string;
  method, uri, protocol: string;
  size: integer;
  x, n: integer;
  loginOk: boolean;
  request:emsStates;
  pass:string[32];
  p:pointer;
begin
  lastSeen:=max_ping_time;
  timeout := 120000;
  Room:=nil;
  // Note: There's no need for installing a client certificate in the
  //       webbrowser. The server asks the webbrowser to send a certificate but
  //       if nothing is installed the software will work because the server
  //       doesn't check to see if a client certificate was supplied. If you
  //       want you can install:
  //
  //       file: c_cacert.p12
  //       password: c_cakey
  //
  Sock.SSL.CertCAFile := ExtractFilePath(ParamStr(0)) + 's_cabundle.pem';
  Sock.SSL.CertificateFile := ExtractFilePath(ParamStr(0)) + 's_cacert.pem';
  Sock.SSL.PrivateKeyFile := ExtractFilePath(ParamStr(0)) + 's_cakey.pem';
  Sock.SSL.KeyPassword := 's_cakey';
  Sock.SSL.verifyCert := True;

  try
    if (not Sock.SSLAcceptConnection) or
       (Sock.SSL.LastError <> 0) then
    begin
      log('Error while accepting SSL connection: ' + Sock.SSL.LastErrorDesc);
      Exit;
    end;
  except
    log('Exception while accepting SSL connection');
    Exit;
  end;

  //get login and pass
  s := sock.RecvString(timeout);
  login:= eatString(s);
  pass:= eatString(s);
  log(format('%s: %s logged in',[sock.GetRemoteSinIP,login]));
  //check with database?
  //no :P
  loginOk:=true;
  //if login ok then start loop
  if loginOk=true then begin
    sock.sendstring('ok' + CRLF);
    while not terminated do begin
      //wait for some incoming data in a loop
//      if sock.canread(max_ping_time*1000) then begin
       if notifyRoomJoin then sendRoomInfoToRemoteClient;

       request:=emsStates(sock.RecvByte(1000));
       if sock.LastError=0 then begin
         case request of
           msLogOut:begin
             leaveRoom;
             log(format('%s logged out',[login]));
             sock.CloseSocket;
             terminate;
           end;
           msFindGame:begin
             log(format('%s is searching for a game to join',[login]));
             sock.RecvBuffer(@settings,sizeof(settings));
             searchingQueue.add(self);
           end;
           msHostGame:begin
             log(format('%s is hosting a game',[login]));
             sock.RecvBuffer(@settings,sizeof(settings));
             //create a game room
             room:=tgameroom.create;
             p:=pointer(self);
             room.players[0]:=p;
             inc(froomCount);
             //if not settings.privateRoom then
             gameRooms.Add(room);
           end;
           //client changed mind and wants out from queue
           msCancelSearch:begin
             searchingQueue.remove(self);
           end;
           msExitRoom:begin
             //transfer game room ownage or close room
             leaveRoom;
           end;
           //just a ping, do nothin
           msPing:begin
             log(format('%s ping',[login]));
             sock.SendByte(byte(msPong));
           end;
           //never should happen here, login is in server thread
           msLogin:begin
             leaveRoom;
             log('login in wrong place');
           end;

          end;
          lastSeen:=max_ping_time;
          //check if in meantime we joined a room
         end
         //if nothing received for given time then terminate client
      else begin
        dec(lastSeen);
        if lastSeen<=0 then begin
          leaveRoom;
          log(format('%s connection died',[login]));
          sock.CloseSocket;
          terminate;
        end;
      end;
    end;
  end
  else
    sock.sendstring('.' + CRLF);

  log(format('%s terminating',[login]));
end;

procedure TClientThread.leaveRoom;
var p:pointer;
begin
  if room=nil then exit;
  log(format('%s leaving room',[login]));
  //room owner?
  p:=pointer(self);
  if room.players[0]=p then
     //close room if we are owner and only player
     if room.countPlayers<=1 then begin
        gameRooms.remove(room);
        dec(froomCount);
        FreeAndNil(room);
     end
     else
  else begin
   //don't free the room since don't own it but just nil reference
   room.leave(@self);
   room:=nil;
  end;
end;

end.

