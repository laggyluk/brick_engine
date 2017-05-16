//handle client-server communication
unit core_network;

{$mode objfpc}{$H+}

{$I '.\TicketSpinlock\defines.inc'}

interface

uses
  {$IFDEF Unix}cthreads,{$ENDIF}
  {$IF defined(Windows32) or  defined(Windows64) }
  windows,
  {$IFEND}
  fifoqueue_mpmc,
  Classes, SysUtils, core_types, lnet, core_orders, core_orders_manager,
  core_utils, core_player, core_camera, VectorGeometry, core_logic,
  gvector,garrayutils, core_tcp,
  core_actor_definitions, core_physics, core_actor;

const
  //typical mtu is 1500
  maxPacketSize = 1300;
  //max packet size - sizeof(port)-sizeof(tick)
  maxPacketData = maxPacketSize-1-sizeof(word)-sizeof(cardinal);
  //header is port, id, ack and flags. rest is data and dataSize
  metaHeaderSize = 10;

  //assign different priorities to entities relevant to clients
  //saves bandwidth but instead there's some additional memory shuffling and shit
  useRelevance = false;
  //when generating new random port for client, this will be added to randomized number
  //it's last digit must be 0
  randomPortBase = 32000;
type

  eGameStates = (gsPreLogin, gsLoggedIn, gsDropped, gsGame);

  //set it through lobby before loading a map so we know what to give to players
  rEntity = packed record
    id:word;
    //this will be dec every net update and when 0 sent, then set again to priority
    //so priority is co ile udpatow jest uwzgledniana ta jednostka
    lastUpdate:byte;
    priority:byte;
    pos,rot:TAffineVector;
  end;
  pEntity = ^rEntity;

  //maps entities (actorID's) to priority for each player
  type

  { TeGreater }

 generic TeGreater<rEntity>=class
    class function c(a,b:rEntity):boolean;inline;
  end;
  vectorlli = specialize TVector<rEntity>;
  eGreater = specialize TeGreater<rEntity>;
  sortlli = specialize TOrderingArrayUtils<vectorlli, rEntity, eGreater>;
  TRelevanceMap = vectorlli;

  { TClient }

  TClient = class
   public
    actorId:integer;//assigned actor
    ip:string[16];
    login:string[16];
    //on connect randomize and send this number to client. will be used to distinguish
    //packets from different players
    port:word;
    //id of last packet that was confirmed for this client
    lastAck:cardinal;
    gameState:eGameStates;
    //flags if old input was sent and can be sent again.
    //otherwise would send every framerather than on fixed interval
    stateSent:boolean;
    //some other shit
    characterType:eActorTypes;
    //guess this should go to array with states in time
    position,target:TAffineVector;
    //actual queue limit should be packet size?
    sendQueue:Tfifoqueue_MPMC;
    //priorities of other entities refresh, relevant to this actor
    relevance:TRelevanceMap;
    constructor create;
    destructor destroy;override;
  end;
  pClient = ^TClient;

  //paczka z inputem playera
  rClientPacket = record
    //state of movement 'stick'
    xAxis,yAxis:TFixedPoint;
    keys1:tPacketFlags; //8 keys state. which actually is movement + action + 3 skills
    //aim rotation in angles
    //angleX,
    //angleY,
    //angleZ:Smallint;//2 byte precision?
    target:TAffineVector;
  end;

  { TODO -copti : we could measure actual max size of client packet, probably will never be that big so we could separate, smaller record for it to save mem }
  rMetaPacket = packed record
    port:word;
    //tick this was sent in
    tick:cardinal;
    //id of last acknowledged 'reliable' packet
    ack:cardinal;
    data:array [0..maxPacketData] of byte;
    //need set how much data is actually used
    dataSize:Word;
  end;
  pMetaPacket = ^rMetaPacket;

  {rNetworkOrder = record
   case typ:orders of
    orMapLoaded,orDestroyActor: (
      int1:integer
      );
    orCreateActor: (int2,int3:integer);
  end;}

  { TNetThread }

  TNetThread = class (TThread)
   protected
    currentClientPacket:rMetaPacket;
    //pause when switching modes from host to client or sth
    paused:boolean;
    fLastSend:single;
    procedure execute; override;
  end;

  //this is for speeding up search of history frame. instead could iterate directly over bucket
  rSnapInfo = packed record
    //gFrame this snapshot was taken
    frameID:cardinal;
    //number of entities present in this frame
    entities:word;
    //start offset in buffer
    offset:cardinal;
  end;

  { TNetworkManager }

  TNetworkManager = class
  private
    lastSnapshotTime:single;
    //this is memory pool that must be enough to fit all entities across n snapshots
    fBucket:TBytes;
    //array holds start adresses of snapshots data start in bucket
    fSnapInfo:array [0..historyPageCount-1] of rSnapInfo;
    //index of last taken snapshot in fSnapInfo
    fLastSnap:integer;
    function getPlayerCount: integer;
   protected
    //if any outgoing order needs ack then whole sent packet will be flaged so
    sendAck:boolean;
    //server/client mode
    isServer:boolean;
    //addres to which we are sending input
    remoteAddr:string;
    net:TNetThread;
    player:TCLient;//points to 'self' in players array
    //map to load, set by lobby
    mapFile:string;
   public
    udpPort:integer;
    udp:TLudp;//socket
    //populated in inputManager update, holds most recent keyboard/mouse state
    playerInputState:rOrder;
    //connected players.
    //should be set prior to starting map for verification and gear setup
    clients:array of TClient;
    //creates snapshot of entities state in world and saves it in history buffer
    //if flag true then won't advance position in buffer but use current slot
    procedure makeSnapShot;
    procedure setMode(serverMode:boolean);
    procedure update;
    Procedure OnServerError(const msg: string; aSocket: TLSocket);
    Procedure OnServerConnect(aSocket: TLSocket);
    Procedure OnReceive(aSocket: TLSocket);
    Procedure OnServerDisconnect(aSocket: TLSocket);
    Procedure OnLocalError(const msg: string; aSocket: TLSocket);
    Procedure OnLocalConnect(aSocket: TLSocket);
    Procedure OnLocalDisconnect(aSocket: TLSocket);
    procedure updateClientMode;
    procedure updateServerMode;
    //set player slots to given number
    procedure initializeGame(roomData:rMapInfo;playerData:TPlayerInfoArray);
    function findPlayerByLogin(const logn:string):integer;
    //removes current packet from queue and sends new one
    procedure sendTo(client: TClient; const packetRef: pMetaPacket; ack: cardinal);
    //return number of clients
    property  clientCount:integer read getPlayerCount;
    //when client disconnects we can re use the client slot for someone else
    procedure reUseSlot(indx:integer);
    constructor create;
    destructor destroy;override;
  end;

var
  network:TNetworkManager;

implementation
var metaP:rMetaPacket;

{ TeGreater }

class function TeGreater.c(a, b: rEntity): boolean;
begin
  result:=b.priority>a.priority;
end;

{ TClient }


constructor TClient.create;
begin
  sendQueue:=Tfifoqueue_MPMC.create;
  actorId:=-1;
  relevance:=TRelevanceMap.create;
end;

destructor TClient.destroy;
begin
  relevance.free;;
  sendqueue.free;
  inherited destroy;
end;

{ TNetThread }

procedure TNetThread.execute;
var m:pMetaPacket;
    i:integer;
begin
  while (not Terminated) do begin
   if paused then begin
      sleep(1);
      continue;
   end;
   with network do if fLastSend<=0 then
   begin
     for i:=0 to length(clients)-1 do with clients[i] do begin
        m:=pMetaPacket(sendQueue.first);
        if m<>nil then begin
           udp.Send(m^,m^.dataSize+metaHeaderSize,ip);
           //log('send');
           //remove packet if it doesn't need ack or was acked
           {if (m^.ack=0) or (m^.ack=lastAck) then begin
              sendQueue.pop(tobject(m));
              freemem(m,m^.dataSize+metaHeaderSize);
           end;}
           clients[i].stateSent:=true;
        end;
      end;
      fLastSend:=refreshInterval;
    end else fLastSend-=deltaT;
    network.udp.CallAction;
    inc(netFrame);
    sleep(0);
    //sleep(refreshInterval);
  end;
end;

{ TNetworkManager }

function TNetworkManager.getPlayerCount: integer;
begin
  result:=length(clients);
end;

procedure TNetworkManager.setMode(serverMode: boolean);
var i:integer;
begin
  //if serverMode then remoteAddr:=
  isServer:=serverMode;
  remoteAddr:='localhost';
  multiPlayerMode:=true;
  if serverMode then begin
     udp.Listen(udpPort);
     log('TNetworkManager::starting server');
     //free camera for testing
     //oM.addCameraMode(player1.actorID,cmFree);
     //enable physics
     //oM.addOrder(orMapLoaded,3,0);
  end else begin
    log('TNetworkManager::joing game');
    udp.Connect;
  end;
  //SetLength(cons,clientCount);
  //for i:=0 to clientCount-1 do begin
  //
  //end;
  net.paused:=false;
end;

procedure TNetworkManager.OnReceive(aSocket: TLSocket);
var
  s: string[32];
  o:rorder;
  sm:smallint;
  inp:rClientPacket;
  buf:array[0..maxPacketSize-1] of byte;
  received:integer;
  pFlags:tPacketFlags;
  indx,i:integer;
  pID,pACK:cardinal;
  pPort,w:word;
  pClient:integer;
begin
  received:=aSocket.Get(buf,maxPacketSize);
  //log('receive');
  if received > 0 then begin
     //indx:=0;
     //read packet
     pPort:=buf[0];
     //get client out of port number
     pClient:=pPort-((pPort div 10)*10);
     //frame this was sent in
     pID:=buf[2];
     //last acknowledget frame
     pACK:=buf[6];
     //determine order type
     o.init;
     o.order:=orders(buf[metaHeaderSize]);
     if isServer then begin
      if (clients[pClient].lastAck<pID) then
      case o.order of //string start         //string length info
        orLogin:begin
         if buf[metaHeaderSize+1]<16 then begin
            //setlength(s,buf[metaHeaderSize+1]);
            move(buf[metaHeaderSize+1],s,buf[metaHeaderSize+1]+1);
            //player login is now i 's'
            //check if login is valid and respond
            { TODO -csec : some semi password assigned in lobby for better security }
            i:=findPlayerByLogin(s);
            if i>-1 then begin
              log(format('player "%s" knocks',[s]));
              //clients[i].lastAck:=pACK;
              //check if player is already logged in to prevent login spam
              if clients[i].gameState=gsPreLogin then begin
                clients[i].gameState:=gsLoggedIn;
                //generate port to use. least significant port digit is clients index in clients array
                //for now port is assigned on network create
                //pPort:=((random(5000)+randomPortBase) div 10)*10;
                //pPort+=i ;
                clients[i].actorId:=logic.getNextFreeId;
                //orders will be propagated to client in networkManager.update
                //create new actor
                //at some spawn position
                with getActorCommonProperties(clients[i].characterType) do begin
                  om.addOrder(orCreateActor,
                              clients[i].actorId,
                              integer(clients[i].characterType),
                              vector3fmake(0,32,0),
                              Vector3fMake(width,height,depth)
                              );
                end;
                //directly send or add this to queue?
                //oM.addOrder(orLoggedIn,clients[i].actorId,pPort);
                clients[i].lastAck:=pID;
                metaP.data[0]:=byte(orLoggedIn);
                w:=clients[i].actorId;
                move(w,metaP.data[1],2);
                move(pPort,metaP.data[3],2);
                metaP.dataSize:=5;
                sendTo(clients[i],@metaP,pID);
                //set port AFTER sending that packet cause other side doesn't know it's port yet
                clients[i].port:=pPort;
                log(format('player "%s" logged in. assigned port: %u, actorID: %d',[s,pPort,clients[i].actorID]));
              end else begin
                log(format('achtung! player "%s" already logged in',[s]));
                //re-use player slot for someone else?
                //but then what about reconnects?
                //we can't refuse here, client will send many of those before he gets ack
                //om.addOrder(orConnectionRefused,clients[i].actorId,0);
              end;
            end else log(format('OnServerReceive:: player %s not invited',[s]));
         end else log('OnServerReceive:: login too long!');
        end;
        orDisconnect:begin
         //client got bored with us and closed the game
         reUseSlot(pClient);
         //kill the dumped actor
         om.addOrder(orDestroyActor,clients[pClient].actorId,0);
         log(format('player diconnected: %s',[clients[pclient].login]));
        end;
        orClientReady:begin
          clients[pClient].gameState:=gsGame;
          clients[pClient].lastAck:=pID;
        end;
        {orMapLoad:begin
          log('client loading map');
        end;
        orMapLoaded:begin
          //client loaded map and is ready to receive game state.
          //actually it's ready right from beginning but still delta might be less that way
          log(format('client %s ready',[clients[pClient].login]));
          clients[pClient].gameState:=gsGame;
        end;}

        //this is regullar packet sent during game play
        orControllerState:begin
         //convert client input to order
         move(buf[metaHeaderSize+1],inp,sizeof(inp));
         { TODO -csec : sanity check }
         o.int1:=clients[pclient].actorId;
         //'joystick' axes
         //FixToFloat(pFixedPoint(@buf[1+metaHeaderSize])^);
         o.v1[0]:=FixToFloat(inp.xAxis);
         o.v1[1]:=FloatToFix(inp.yAxis);
         //camera target
         move(buf[5+metaHeaderSize],clients[pclient].target,sizeof(TAffineVector));
         //buttons state flags
         if inp.keys1.Bit0 then o.int2:=bitSet(o.int2,1);
         if inp.keys1.Bit1 then o.int2:=bitSet(o.int2,2);
         if inp.keys1.Bit2 then o.int2:=bitSet(o.int2,3);
         if inp.keys1.Bit3 then o.int2:=bitSet(o.int2,4);
         if inp.keys1.Bit4 then o.int3:=bitSet(o.int3,1);
         if inp.keys1.Bit5 then o.int3:=bitSet(o.int3,2);
         if inp.keys1.Bit6 then o.int3:=bitSet(o.int3,3);
         if inp.keys1.Bit7 then o.int3:=bitSet(o.int3,4);
         //add order to queue
         om.addOrder(o);
        end;
      end;
    end else
    //we are a client receiving game states from server
    begin
     if (player.lastAck<pID)// and (pPort=0) or (pport=clients[pClient].port)
     then
     case o.order of
        orLoggedIn:begin
          //we got assigned this actor
          //here we know our clients new actorID but don't set it in
          //player1.actorID cause it might do some shit like list out of bounds
          //let's wait for orCreateActor with this id
          move(w,buf[1+metaHeaderSize],2);
          player.actorId:=w;
          move(player.port,buf[4+metaHeaderSize],2);
          //this should stop sending login requests to server
          player.lastAck:=pID;
          //send confirmation and load map? maybe sending orMapLoad will serve as keep alive
          metap.tick:=netFrame;
          //no need to set metaP port because we are only client here?
          metaP.data[0]:=byte(orClientReady);
          metaP.dataSize:=1;
          sendTo(player,@metaP,pID);
          player.gameState:=gsGame;
          //terrain.loadMap(mapfile);
          //oM.addOrder(orMapLoaded,3,0);
        end;
        orDisconnect:begin
         //server is shuting down
         log('server ended game');
         net.paused:=true;
         udp.Disconnect();
        end;
        //incoming world state (delta)
        orSnapShot:begin
          indx:=metaHeaderSize+1;
          while indx+metaHeaderSize<received do begin
            move(buf[indx],w,2);
            o.int1:=w;
            //if actor exists then move him. if doesn't then create
            if PhysicsEngine.indexof(o.int1)>-1 then begin
               o.order:=orWantMove;
               o.int2:=0;
               //position
               move(buf[indx+2],o.v1,12);
               //skip rest. velo maybe is used to calc rot. but then we send rotY too..
               indx+=29;
               //send order
               om.addOrder(o);
            end else begin
             //create actor if he doesn't exist
               o.order:=orCreateActor;
               //position
               move(buf[indx+2],o.v1,12);
               //skip velo
               //type
               o.int2:=buf[indx+26];
               //why o why there's size in v2 needed?
               o.v2:=getActorCommonProperties(eActorTypes(o.int2)).size;
               indx+=29;
               //send order
               om.addOrder(o);
               if o.int1=player.actorId then
                  om.addCameraMode(o.int1,cm3rdPerson);
               oM.addOrder(orToggleBrain,o.int1,0);
            end;
          end;
        end;
        //cases end
     end;
   end;
  end;
end;

procedure TNetworkManager.update;
var i:integer;
    o:rorder;
begin
  if not multiPlayerMode then begin
    for i:=0 to om.queue.Count-1 do begin
     o:=om.queue[i];
     o.eatenFlags.eatenNetwork:=true;
     om.queue[i]:=o;
    end;
    exit;
  end;
  //decide what needs to be sent and to who
  if isServer then begin
    //construct a world snapshot for every connected player
    //which would be own position and actors that are visible.
    //that is within frustum and raycasable
    updateServerMode;
  end else begin
    //send input if we are a client
    //sendOrder(playerInputState);
    updateClientMode;
  end;
  //moved to net
  //local.CallAction;
  //server.CallAction;
end;

procedure TNetworkManager.makeSnapShot;
var offset:cardinal;
    i,j,k:integer;
    u:TFlatPhysicsUnit;
    a:TActor;
    w:word;
    d:single;
    e:rEntity;
begin
  if (fLastSnap+1>length(fSnapInfo)) then fLastSnap:=0 else inc(fLastSnap);
  //exit if snap was already taken in this frame
  if fSnapInfo[fLastSnap].frameID=netFrame then exit;
  //it might not be worth to do that prioritizing actually..
  if useRelevance then for j:=0 to clientCount-1 do clients[j].relevance.Clear;

  with fSnapInfo[fLastSnap] do begin
    frameID:=netFrame;
    //data start in bucket is index*snapPageSize
    offset:=(snapPageSize*fLastSnap);
    //build the snapshot
    PhysicsEngine.instaHitSnapshot(nil,nil,0,true,@fBucket[0],offset);
    move(fBucket[offset+4],entities,2);
    //compute priotity of this entity for each client
    //could be fixed if entity type is a vehicle. if there were vehicles
    if useRelevance then begin
      for j:=0 to clientCount-1 do begin
          d:=VectorDistance(clients[j].position,u.position);
          if d>0 then begin
            e.id:=u.actorID;
            e.lastUpdate:=0;
            if d<20 then e.priority:=3 else e.priority:=2;
            //if vehicle then = 1
            //and could add some priority if entity within frustum
            clients[j].relevance.PushBack(e);
          end;
      end;
      //sort entities by pririty
      for j:=0 to clientCount-1 do sortlli.Sort(clients[j].relevance,clients[j].relevance.Size);
    end;
    //then should add info about changed blocks if any
  end;
end;

procedure TNetworkManager.updateServerMode;
var
  i,j,indx:integer;
  o:rOrder;
  ignoreMovement:boolean;
  c,step,bstep,d:cardinal;
  w:word;
const serverOrders : set of orders = [
                                   orMapLoad
                                   ,orLoggedIn
                                   //,orWantShoot,
                                   //,orWantReload
                                   ];
begin
  for i:=0 to om.queue.Count-1 do begin
   o:=om.queue[i];
   //if o.eatenFlags.eatenNetwork then continue;
   o.eatenFlags.eatenNetwork:=true;
   om.queue[i]:=o;
 {  if o.order in serverOrders then begin
     indx:=0;//need to zero it when new packet is used
     metaP.tick:=netFrame;
     metaP.port:=clients[0].port;
     //zero the ack flag
     metaP.ack:=0;
     ignoreMovement:=true;
     with o do begin
       case order of
         //need to send info to client that login is ok
        { orLoggedIn:begin
           metaP.data[indx]:=byte(order);
           metaP.dataSize:=1;//length of login string + 1 byte for order type
           ignoreMovement:=true;
           //clients[int1].
         end;
          }
         orMapLoad:;
       end;
     end;
   end;}
  end;
  //some orders are from clients and need to be 'added' to orders queue.
  //Some others are from server logic and need to be sent to clients?
  //or maybe not if we gonna build snapshot from 'world state' then we need to
  //create that delta snapshot, pack and sent. It will be 'translated' to orsers on
  //other side?
  //or just pack world state in bunch of orders

  //build snapshot
  if lastSnapshotTime<=0 then begin
     makeSnapShot;
      //create delta for each client and send
    for i:=0 to clientCount-1 do
     if clients[i].gameState=gsGame then with clients[i] do begin
        //for now just send last snapshot
        metaP.data[0]:=byte(orSnapShot);
        if useRelevance then begin
          for j:=0 to relevance.Size-1 do begin

          end;
        end else begin
          //if not using relevence mode
          //that is all entities in (currently) last snapshot. no delta yet
          c:=1;
          step:=29;//how much data is sent per entity
          //interval in bucket between entities
          bstep:=snapEntitySize;
          //first bytes are frameID and entities count
          d:=fSnapInfo[fLastSnap].offset+6;
          for j:=0 to fSnapInfo[fLastSnap].entities-1 do with fSnapInfo[fLastSnap] do begin
             move(fBucket[d],metaP.data[c],29);
             //skip hitbox 24b
             c+=step;
             d+=bstep;
          end;
          //add voxel changes?
          //send
          metaP.dataSize:=c;
          sendTo(clients[i],@metaP,lastAck);
        end;
        //sendTo(clients[i],@metaP,pID);
     end;
     lastSnapshotTime:=refreshInterval;
  end else lastSnapshotTime-=deltaT;


  //server.Send(inp,sizeof(inp),remoteAddr);
end;

procedure TNetworkManager.updateClientMode;
var
  inpt:rClientPacket;
  i:integer;
  o:rOrder;
  pm:pMetaPacket;
  ignoreMovement:boolean;
  //offset in current packet being build
  indx:integer;
  //here are orders that client can send to server
const clientOrders : set of orders = [
                                   orMapLoad
                                   ,orMapLoaded
                                   ,orLogin
                                   //,orWantShoot,
                                   //,orWantReload
                                   ];
begin
  //need to pack data before sending, usual rOrder is a waste of bandwidth
  //so process the queue, build packet from it and then send together with input data
  //if game state requires it

  //niektore komendy wymusza ignorowanie inputa wiec mozna go niewysylac
  ignoreMovement:=false;
  for i:=0 to om.queue.Count-1 do begin
   o:=om.queue[i];
   if o.eatenFlags.eatenNetwork then continue;
   o.eatenFlags.eatenNetwork:=true;
   om.queue[i]:=o;
   //nothing more to do if not playing remote game?
   if not multiPlayerMode then exit;
   //setup the default packet if order is networkable
   if o.order in clientOrders then begin
     indx:=0;//need to zero it when new packet is used
     metaP.tick:=netFrame;
     metaP.port:=clients[0].port;
     //zero the ack flag
     metaP.ack:=0;
     ignoreMovement:=true;
   end;
   with o do begin
     case order of
       //login is commenced before map load as handshake with server
       orLogin:begin //send login string
         metaP.data[indx]:=byte(order);
         move(player1.login,metaP.data[indx+1],length(player1.login)+1);
         metaP.dataSize:=length(player1.login)+2;//length of login string + 1 byte for order type
         sendTo(player,@metaP,player.lastAck);
         ignoreMovement:=true;
       end;
       //onMapLoad powinno ladowac mape a jakis call back czy terrain powinien puszczac map loaded
       //when map is loaded send notification and start pumping input packets
       {orMapLoaded:begin
        player.gameState:=gsGame;
        metaP.data[indx]:=byte(order);
        metaP.dataSize:=1;
        sendTo(player,@metaP,player.lastAck);
        ignoreMovement:=true;
       end
       }
       {orCreateActor:begin
        //actorID=Smallint,actorType=byte,faction=shortint
        metaP.data[indx]:=smallInt(int1);
        metaP.data[indx+2]:=byte(int2);
        metaP.data[indx+3]:=shortint(int3);
        //position v1. cast to smallint
        metaP.data[indx+4]:=smallint(round(v1[0]));
        //y position can be casted to byte as 255 is max
        metaP.data[indx+6]:=byte(round(v1[1]));
        metaP.data[indx+7]:=smallint(round(v1[2]));
        //v2 size. Shortints are enough
        metaP.data[indx+9]:=Shortint(round(v2[0]));
        metaP.data[indx+11]:=Shortint(round(v2[1]));
        metaP.data[indx+13]:=Shortint(round(v2[2]));
        //advnce index. size-1 cause later there's post inc
        inc(indx,14);}
       end;
     //since orControllerState is not used then it means that every other
     //add packet to send queue
     //don't wanna make packets with irrevelant orders
    { if o.order in clientOrders then begin
        pm:=GetMem(metaP.dataSize+metaHeaderSize);//le bullshit? maybe should use pool to avoid fragmentation
        pm^.dataSize:=metaP.dataSize;
        move(metaP,pm^,metaHeaderSize+metaP.dataSize);
        //add input info to packet if it's ok
        if not ignoreMovement then begin
          inpt.xAxis:=FloatToFix(playerInputState.v1[0]);
          inpt.yAxis:=FloatToFix(playerInputState.v1[1]);
          //camera angles
          //inpt.angleY:=FloatToFix(camera.inputRot);
          inpt.target:=camera.target;
          inpt.keys1.Bit0:=bitGet(playerInputState.int2,1);
          inpt.keys1.Bit1:=bitGet(playerInputState.int2,2);
          inpt.keys1.Bit2:=bitGet(playerInputState.int2,3);
          inpt.keys1.Bit3:=bitGet(playerInputState.int2,4);
          inpt.keys1.Bit4:=bitGet(playerInputState.int3,1);
          inpt.keys1.Bit5:=bitGet(playerInputState.int3,2);
          inpt.keys1.Bit6:=bitGet(playerInputState.int3,3);
          inpt.keys1.Bit7:=bitGet(playerInputState.int3,4);
          move(inpt,pm^.data[metaHeaderSize+pm^.dataSize+1],sizeof(inpt));
          pm^.dataSize+=sizeof(inpt);
        end;
        if not clients[player1.actorID].sendQueue.Push(tobject(pm)) then
           log('achtung! TNetworkManager::sendQueue overflow');
     end;
     }
   end;
   //send input to server
   if (player.gameState=gsGame) and (player.stateSent) then begin
      inpt.xAxis:=FloatToFix(playerInputState.v1[0]);
      inpt.yAxis:=FloatToFix(playerInputState.v1[1]);
      //camera angles
      //inpt.angleY:=FloatToFix(camera.inputRot);
      inpt.target:=camera.target;
      inpt.keys1.Bit0:=bitGet(playerInputState.int2,1);
      inpt.keys1.Bit1:=bitGet(playerInputState.int2,2);
      inpt.keys1.Bit2:=bitGet(playerInputState.int2,3);
      inpt.keys1.Bit3:=bitGet(playerInputState.int2,4);
      inpt.keys1.Bit4:=bitGet(playerInputState.int3,1);
      inpt.keys1.Bit5:=bitGet(playerInputState.int3,2);
      inpt.keys1.Bit6:=bitGet(playerInputState.int3,3);
      inpt.keys1.Bit7:=bitGet(playerInputState.int3,4);
      metaP.data[0]:=byte(orControllerState);
      move(inpt,metaP.data[1],sizeof(inpt));
      metaP.dataSize:=sizeof(inpt)+1;
      sendTo(player,@metaP,player.lastAck)
   end;
   //if o.order in clientOrders then addOrder(@o,true);
  end;
end;

procedure TNetworkManager.initializeGame(roomData: rMapInfo;
  playerData: TPlayerInfoArray);
var i:integer;
begin
  //mapFile:=format('%s%s%s%s',[apppath,'world',DirectorySeparator,'test.terrain']);
  mapFile:=format('%dx%d.terrain',[roomData.mapX,roomData.mapY]);
  netFrame:=1;
  fLastSnap:=-1;
  for i:=0 to length(clients)-1 do clients[i].free;
  //copy data from room
  setlength(clients,length(playerData));
  for i:=0 to length(clients)-1 do begin
     clients[i]:=tclient.create;
     with clients[i] do begin
       login:=playerData[i].login;
       characterType:=eActorTypes(playerData[i].info.character);
       port:=playerData[i].qport;
       convertIp(playerData[i].ip,ip);
     end;
  end;
  //set player reference slot that has same login as game.player1
  i:=findPlayerByLogin(player1.login);
  if i<0 then log('achtung! player1 not found in game definition')
  else player:=clients[i];
end;

function TNetworkManager.findPlayerByLogin(const logn: string): integer;
var i:integer;
begin
   result:=-1;
   for i:=0 to length(clients)-1 do if clients[i].login=logn then result:=i;
end;

procedure TNetworkManager.sendTo(client: TClient; const packetRef: pMetaPacket;
  ack: cardinal);
var
  m:pMetaPacket;
begin
  //pop existing packet?
  if client.sendqueue.first<>nil then begin
     client.sendqueue.pop(tobject(m));
     freemem(m,m^.dataSize+metaHeaderSize);
  end;
  { TODO -copti : allocating mem each time packet is send... maybe could use a pool }
  m:=GetMem(packetRef^.dataSize+metaHeaderSize);
  m^.dataSize:=packetRef^.dataSize;
  move(packetRef^,m^,metaHeaderSize+packetRef^.dataSize);
  m^.tick:=netFrame;
  m^.ack:=ack;
  m^.port:=client.port;
  client.sendQueue.push(tobject(m));
  client.stateSent:=false;
end;

procedure TNetworkManager.reUseSlot(indx: integer);
begin
  { TODO : get new player from lobby }
  with clients[indx] do begin
    actorId:=-1;
    port:=0;
    gameState:=gsPreLogin;
  end;
end;

procedure TNetworkManager.OnServerError(const msg: string; aSocket: TLSocket);
begin
  log(format('%s %s',['TNetworkManager::OnOurError:',msg]));
end;

procedure TNetworkManager.OnServerConnect(aSocket: TLSocket);
begin
  log('on our connect');
end;

procedure TNetworkManager.OnServerDisconnect(aSocket: TLSocket);
begin
  log('on server disconnect');
end;

procedure TNetworkManager.OnLocalError(const msg: string; aSocket: TLSocket);
begin
  log(format('%s %s',['TNetworkManager::OnLocalError:',msg]));
end;

procedure TNetworkManager.OnLocalConnect(aSocket: TLSocket);
begin

  log('on server connect');
end;

procedure TNetworkManager.OnLocalDisconnect(aSocket: TLSocket);
begin
  log('on local disconnect');
end;

constructor TNetworkManager.create;
begin
  setlength(fBucket,historyPageCount*snapPageSize);
  //should be set somwhere else

  udpPort:= 20452;
  //initialize server component
  udp:=TLUdp.Create(nil);
  udp.OnError := @OnServerError;
  udp.OnReceive := @OnReceive;
  udp.OnDisconnect := @OnServerDisconnect;
  udp.Port:=udpPort;

  net:=TNetThread.create(false);
  net.paused:=true;
  //sendQueue.Capacity:=64;
end;

destructor TNetworkManager.destroy;
var i:integer;
begin
  //notify others that game is being shut down
  if multiPlayerMode then begin
    if isServer then for i:=0 to clientCount-1 do begin
       player:=clients[i];
       metaP.data[0]:=byte(orDisconnect);
       metaP.dataSize:=1;
       sendTo(player,@metaP,netFrame);//a to czasem nie server powinien wysylac?
    end else begin
      metaP.port:=player.port;
      metaP.tick:=netFrame;
      metaP.ack:=netFrame;
      metaP.data[0]:=byte(orDisconnect);
      metaP.dataSize:=1;
      sendTo(player,@metaP,netFrame);
    end;
  end;
  sleep(200);
  net.paused:=true;
  net.terminate;
  net.WaitFor;

  udp.Disconnect();
  sleep(100);

  for i:=0 to length(clients)-1 do clients[i].free;
  udp.Free;
  net.free;
  inherited destroy;
end;

end.

