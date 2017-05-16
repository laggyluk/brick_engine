unit planet_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, DCPsha256, lNetComponents, lNet, core_tcp, ghashmap, strutils,
  math, fgl, LCLIntf, core_actor_definitions;

type

  //forward decl
  TGameRoom = class;
  { TClient }

  TClient = class
   public
    loggedIn:boolean;
    login:tLogin;
    room:TGameRoom;
    //ready for a game in room
    ready:boolean;
    socket:TLSocket;
    //is randomized upon entering the room and used as client port in core_network
    qport:word;
    ip:array[0..3] of byte;
    //gear weared for next match
    info:rPlayerInfo;
    //if is on searching for quickmatch list
    searching:boolean;
    procedure exitRoom;
    constructor create(asocket:TLSocket);
  end;

  //TClientList = specialize TFPGList <TClient>;
  TClientList = TThreadList;
  { TGameRoom }

  TGameRoom = class
   private
    readyTimer:integer;
    lastReadyTimer:cardinal;
    buf:array [0.. 10+maxPlayers*sizeof(rPlayerInfo)] of byte;
   public
    roomId:cardinal;
    //room owner.he's also on 'seats' list
    owner:TClient;
    //list of players in a room
    seats:TClientList;
    isPublic:boolean;
    mapInfo:rMapInfo;
    //how many players can be here
    maxSeats:byte;
    state:eRoomStates;
    function count:integer;
    procedure addClient(player:TClient);
    procedure leaveRoom(player:TClient);
    //check if everyone is ready for game. if not then resets the time
    procedure update;
    //tells the client to start playing
    procedure startGame(player: tclient);
    constructor create(leOwner:TClient;sPublic:boolean);
    destructor destroy;override;
  end;

  TRoomList = TThreadList;//specialize TFPGList <TGameRoom>;

  { TIpClientMap }

  TIpClientMap=class
  public
    class function hash ( a : TLSocket; b : SizeUInt ) : SizeUInt ;
  end;

  TClientMap=specialize THashMap<TLSocket , TClient , TIpClientMap >;

  { TRoomThread }

  TRoomThread = class(TThread)
    procedure execute;override;
    constructor create;
    destructor destroy;override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    clientsLabel: TLabel;
    privateRoomLabels: TLabel;
    publicRoomsLabel: TLabel;
    sha: TDCP_sha256;
    logMemo: TMemo;
    tcp: TLTCPComponent;
    statsTimer: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure statsTimerTimer(Sender: TObject);
    procedure tcpAccept(aSocket: TLSocket);
    procedure tcpDisconnect(aSocket: TLSocket);
    procedure tcpError(const msg: string; aSocket: TLSocket);
    procedure tcpReceive(aSocket: TLSocket);
  private
    { private declarations }
    //buffer for incoming messages
    tcpBuf: array [0..255] of byte;
    matcher:TRoomThread;
  public
    { public declarations }
  end;

  procedure log(const s:string);

var
  Form1: TForm1;
  publicRooms:TRoomList;
  privateRooms:TRoomList;
  searchers:TThreadList;
  publicRoomCount,privateRoomCount:integer;
  clients:TClientMap;

implementation
var lastRoomId:integer;
    //client after joining room is already ready
    debugReady:boolean = true;

{$R *.lfm}

{ TClient }

procedure TClient.exitRoom;
begin
  searching:=false;
  if room<>nil then room.leaveRoom(self);
end;

constructor TClient.create(asocket: TLSocket);
var s:string;
begin
  room:=nil;
  ready:=false;
  socket:=asocket;
  info.character:=eStrach;
  convertIp(asocket.PeerAddress,ip);
end;

{ TRoomThread }

procedure TRoomThread.execute;
var
  klienty,pokoje:tlist;
  i,j:integer;
  player:TClient;
begin
  while not terminated do begin
     klienty:=searchers.LockList;
     pokoje:=publicRooms.LockList;
     for j:=0 to pokoje.Count-1 do with TGameRoom(pokoje[j]) do begin
       for i:=0 to klienty.Count-1 do begin
          player:=TClient(klienty[i]);
          if count<maxSeats then begin
             addClient(player);
             searchers.Remove(player);
          end;
       end;
       //update room
       update;
     end;
     publicRooms.UnlockList;
     searchers.UnlockList;
     sleep(1);
  end;
end;

constructor TRoomThread.create;
begin
  FreeOnTerminate:=false;
  inherited create(false);
end;

destructor TRoomThread.destroy;
begin
  inherited destroy;
end;

{ TIpClientMap }

class function TIpClientMap.hash(a: TLSocket; b: SizeUInt): SizeUInt;
begin
  hash:= cardinal(a) mod b;
end;

{ TGameRoom }

function TGameRoom.count: integer;
begin
  result:=seats.LockList.Count;
  seats.UnlockList;
end;

procedure TGameRoom.addClient(player: TClient);
var i:integer;
    lista:tlist;
    c:cardinal;
begin
  player.room:=self;
  player.qport:=random(High(word));
  log(format('%s joining room %d',[player.login,roomid]));
  ///////////first send info about room state to new player
  //command type
  buf[0]:=byte(mmRoomState);
  //info about map and server ip
  move(MapInfo, buf[1],sizeof(rMapInfo));
  c:=sizeof(rMapInfo)+1;
  //port assigned to this client
  move(player.qport,buf[c],2);
  inc(c,2);
  move(player.info,buf[c],sizeof(rPlayerInfo));
  inc(c,sizeof(rPlayerInfo));
  lista:=seats.LockList;
  //player count
  buf[c]:=lista.Count;
  inc(c);
  //info about other players in a room
  for i:=0 to lista.Count-1 do with tclient(lista[i]) do begin
      buf[c]:=length(login);
      move(login[1],buf[c+1],length(login));
      c+=length(login)+1;
      move(ip,buf[c],sizeof(ip));
      inc(c,sizeof(ip));
      move(info,buf[c],sizeof(rPlayerInfo));
      inc(c,sizeof(rPlayerInfo));
  end;
  //state of the game. player will start the game immiedietly if rsInGame
  buf[c]:=byte(state);
  inc(c);
  player.socket.Send(buf,c);
  //////////then let other ppl in the room know that new player entered
  buf[0]:=byte(mmPlayerInfo);
  buf[1]:=length(player.login);
  move(player.login[1],buf[2],length(player.login));
  c:=2+length(player.login)+1;
  move(player.ip,buf[c],sizeof(player.ip));
  inc(c,sizeof(player.ip));
  move(player.info,buf[c],sizeof(rPlayerInfo));
  inc(c,sizeof(rPlayerInfo));
  move(player.qport,buf[c],2);
  inc(c,2);
  player.socket.PeerAddress;
  for i:=0 to lista.count-1 do with tclient(lista[i]) do begin
      socket.Send(buf,c);
  end;
  seats.UnlockList;
  seats.Add(player);
end;

procedure TGameRoom.leaveRoom(player: TClient);
var
   lista:tlist;
   seatCount:Integer;
begin
  log(format('%s leaving room %d',[player.login,roomid]));
  lista:=seats.LockList;
  if lista.IndexOf(player)>-1 then
     lista.Remove(player);
  seatCount:=lista.Count;
  seats.UnlockList;
  if seatCount=0 then begin
    if isPublic then begin
       publicRooms.Remove(self);
       dec(publicRoomCount);
    end
    else begin
       privateRooms.Remove(self);
       dec(privateRoomCount);
    end;
    free;
  end;
end;

procedure TGameRoom.update;
var i,j:integer;
    t:cardinal;
    lista:tlist;
begin
  case state of
    rsBeforeGame:begin
      j:=0;
      lista:=seats.LockList;
      for i:=0 to lista.count-1 do if tclient(lista[i]).ready then inc(j);
      //move the timer if atleast half is ready
      if j>lista.count div 2 then begin
        //speed up if everyone is ready
        if (j=lista.count) and (readyTimer>gameInitShortTime) then readyTimer:=gameInitShortTime;
        t:=GetTickCount;
        if t-lastReadyTimer>1000 then begin
           lastReadyTimer:=t;
           dec(readyTimer);
           log(format('room %d tick %d',[roomId,readyTimer]));
        end;
      end
      else begin
        lastReadyTimer:=GetTickCount;
        readyTimer:=gameAutoInitTime;
      end;
      if readyTimer<=0 then begin
        log(format('room %d starting game',[roomId]));
        state:=rsInGame;
        for i:=0 to lista.count-1 do begin
          startGame(tclient(lista[i]));
        end;
      end;
      seats.UnlockList;
    end;
  end;
end;

procedure TGameRoom.startGame(player:tclient);
var
   i:integer;
   s:string;
   c:cardinal;
begin
  if owner=player then
    buf[0]:= byte(mmStartGameHost)
    else buf[0]:= byte(mmJoinGame);
    player.socket.Send(buf,1);
end;

constructor TGameRoom.create(leOwner: TClient; sPublic: boolean);
var
   prv:string;
begin
  readyTimer:=gameAutoInitTime;
  seats:=TClientList.create;
  //seats.Add(leOwner);
  leOwner.room:=self;
  owner:=leOwner;
  isPublic:=sPublic;
  roomId:=lastRoomId;

  maxSeats:=3;

  mapInfo.mapx:=0;
  mapInfo.mapy:=0;
  mapInfo.difficulty:=3;

  convertIp(owner.socket.PeerAddress,mapInfo.serverIp);
  if isPublic then prv:='public' else prv:='private';
  addClient(leowner);
  log(format('%s created %s room %d',[leOwner.login,prv,roomid]));
  inc(lastRoomId);
end;

destructor TGameRoom.destroy;
begin
  log(format('closing room %d',[roomID]));
  seats.free;
  inherited destroy;
end;

{ TForm1 }

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  randomize;
  matcher:=TRoomThread.create;
  clients:=TClientMap.create;
  privateRooms:=TRoomList.create;
  publicRooms:=TRoomList.create;
  searchers:=TThreadList.create;
  sha.Init;
  tcp.Listen(440);
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  iterator: TClientMap.TIterator;
  i:integer;
  lista:tlist;
begin
  matcher.Terminate;
  matcher.WaitFor;
  matcher.free;
  //lista:=searchers.LockList;
  //for i:=0 to lista.count-1 do TClient(lista[i]).Free;
  searchers.free;
  lista:=privateRooms.LockList;
  for i:=0 to lista.count-1 do tgameroom(lista[i]).Free;
  privateRooms.free;
  lista:=publicRooms.LockList;
  for i:=0 to lista.count-1 do tgameroom(lista[i]).Free;
  publicRooms.free;

  if clients.size>0 then begin
    iterator:=clients.Iterator;
    repeat
      iterator.value.free;
    until not iterator.next;
    iterator.destroy;
  end;
  clients.free;
end;

procedure TForm1.statsTimerTimer(Sender: TObject);
begin
  publicRoomsLabel.Caption:=format('public rooms: %d',[publicRoomCount]);
  privateRoomLabels.Caption:=format('private rooms: %d',[privateRoomCount]);
  clientsLabel.Caption:=format('clients: %d',[clients.size]);
end;

procedure TForm1.tcpAccept(aSocket: TLSocket);
var
  s:string;
begin
  log(format('%s> connected',[asocket.PeerAddress]));
  clients[asocket]:=tclient.create(asocket);
end;

procedure TForm1.tcpDisconnect(aSocket: TLSocket);
var
  client:tclient;
begin
  client:=clients[asocket];
  client.exitRoom;
  log(format('%s> disconnected',[client.login]));
  clients.delete(asocket);
end;

procedure TForm1.tcpError(const msg: string; aSocket: TLSocket);
begin
  log(format('achtung! %s> %s',[aSocket.PeerAddress,msg]));
end;

procedure TForm1.tcpReceive(aSocket: TLSocket);
var
  s:string[16];
  s2:string;
  hbuf:array[0..31] of byte;
  i,j:integer;
  got:integer;
  gr:TGameRoom;
  client:TClient;
begin
  got:=aSocket.Get(tcpBuf,Length(tcpBuf));
  if got > 0 then begin
    case eTcpMessages(tcpBuf[0]) of
      mmLogin:begin
            setString(s,@tcpBuf[2],min(tcpBuf[1],16));
            log(format('%s> login: %s',[asocket.PeerAddress,s]));
            //start of pass hash in buf
            j:=tcpBuf[1]+2;
            //get hashed pass from database and compare with one received from client
      /////////////move(hashedPassFromDb[1],tcpBuf[127],32);/////////////////

            {s2:='passE';
            sha.Init;
            sha.Update(s2[1],length(s2));
            //let's use tcp buffer at offset for hash compare
            sha.Final(tcpBuf[127]);
            s2:='local: ';
            for i:=0 to 31 do s2:=s2+' '+inttostr(tcpBuf[127+i]);
            log(s2);
            s2:='remo: ';
            for i:=0 to 31 do s2:=s2+' '+inttostr(tcpBuf[i+j]);
            log(s2);  }
            //compare hashes
            {for i:=0 to (sha.HashSize div 8)-1 do begin
              if tcpBuf[j+i]<> tcpBuf[127+i] then begin
                log(format('%s> pass fail',[aSocket.PeerAddress]));
                aSocket.Disconnect(true);
                //add some fail count?
                exit;
              end;
            end;}
            //log out previous one
            //pass is ok
            log(format('%s> logged in as %s',[aSocket.PeerAddress,s]));
            with clients[aSocket] do begin
               loggedIn:=true;
               login:=s;
            end;
      end;
      mmFind: begin
            client:=clients[aSocket];
            log(format('%s> searches for a match',[asocket.PeerAddress]));
            client.searching:=true;
            searchers.Add(client);
      end;
      mmHost:begin
         client:=clients[aSocket];
         if client.searching then client.exitRoom;
         //we let client start the game?
         if debugReady then client.ready:=true;
         //log(format('%s> creating public room',[client.login]));
         //receive room init data?
         gr:=TGameRoom.create(client,true);
         publicRooms.Add(gr);
         inc(publicRoomCount);
      end;
      mmExitRoom:clients[asocket].exitRoom;
      mmPing:log('ping');
    end;
  end;
end;

procedure log(const s: string);
begin
  form1.logMemo.lines.add(format('%s> %s',[timeToStr(now),s]));
end;

end.

