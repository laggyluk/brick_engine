unit ui_main_menu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ui_menu_template, BitmapLabel, core_game, core_types, core_ui,
  core_input, core_network, core_player, DCPsha256, lNetComponents, fgl,
  core_logic, core_tcp, core_orders_manager, core_orders, VectorGeometry,
  core_actor_definitions, lNet, math;

type

  { TmenuForm }

  TmenuForm = class(TmenuTemplate)
    BitmapLabel1: TBitmapLabel;
    addressE: TComboBox;
    sha: TDCP_sha256;
    loginBtn: TBitmapLabel;
    loginE: TEdit;
    tcp: TLTCPComponent;
    passE: TEdit;
    singleBtn: TBitmapLabel;
    joinBtn: TBitmapLabel;
    hostBtn: TBitmapLabel;
    topBar1: TBitmapLabel;
    procedure FormDestroy(Sender: TObject);
    procedure hostBtnClick(Sender: TObject);
    procedure loginBtnClick(Sender: TObject);
    procedure singleBtnClick(Sender: TObject);
    procedure joinBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tcpConnect(aSocket: TLSocket);
    procedure tcpDisconnect(aSocket: TLSocket);
    procedure tcpError(const msg: string; aSocket: TLSocket);
    procedure tcpReceive(aSocket: TLSocket);
  private
    { private declarations }
    buf:array [0..255] of byte;
    function playerExists(const l:tlogin):integer;
  public
    { public declarations }
    me:TPlayerInfo;
    mapInfo:rMapInfo;
    //MapInfo:rMapInfo;
    seats:TPlayerInfoArray;
    procedure startHostGame;
    procedure joinGame;
  end;

var
  menuForm: TmenuForm;

implementation

{$R *.lfm}

{ TmenuForm }

procedure TmenuForm.FormCreate(Sender: TObject);
begin
  inherited;
  lockWidth:=true;
  lockHeight:=true;
  logine.Text:=format('demo%d',[random(99999)]);
end;

procedure TmenuForm.tcpConnect(aSocket: TLSocket);
var i,count:cardinal;
    s:string;
begin
  log('connected to planet');
  fillchar(buf,length(Buf),0);
  //set own ip, wtf?
  convertIp(asocket.LocalAddress,me.ip);
  //encode which command this is
  Buf[0]:=byte(mmLogin);
  //length of login
  count := min(length(logine.text),16);
  Buf[1]:=count;
  //copy login
  s:=lowercase(logine.Text);
  move(s[1],Buf[2],count);
  inc(count,2);
  //len of pass
  sha.Init;
  s:=passe.text;
  sha.Update(s[1],length(s));
  sha.Final(Buf[count]);
  //move(s[1],Buf[count],length(s));
  i:=sha.HashSize div 8;
  //i:=length(s);
  inc(count,i);
  tcp.Send(Buf,count);
end;

procedure TmenuForm.tcpDisconnect(aSocket: TLSocket);
begin
  log('disconnected from planet');
end;

procedure TmenuForm.tcpError(const msg: string; aSocket: TLSocket);
begin
  log('achtung! tcp:' + msg);
end;

procedure TmenuForm.tcpReceive(aSocket: TLSocket);
var
  s:string;
  got,i:integer;
  c:cardinal;
begin
  got:=aSocket.Get(Buf[0],length(Buf));
  if got>0 then begin
    //log(format('%s> %s',[asocket.PeerAddress,s]));
    case eTcpMessages(Buf[0]) of
      mmStartGameHost:startHostGame;
      mmJoinGame:joinGame;
      mmRoomState:begin
        move(buf[1],MapInfo,sizeof(rMapInfo));
        c:=sizeof(rMapInfo)+1;
        //port assigned to this client
        move(buf[c],me.qport,2);
        inc(c,2);
        //loadout
        move(buf[c],me.info,sizeof(rPlayerInfo));
        inc(c,sizeof(rPlayerInfo));
        //how many players? +1 for self at end of table
        SetLength(seats,min(buf[c],maxPlayers-1)+1);
        inc(c);
        //info about other players in a room
        for i:=0 to length(seats)-2 do with seats[i] do begin
            setString(login,@buf[c+1],buf[c]);
            c+=length(login)+1;
            move(buf[c],ip,sizeof(ip));
            inc(c,sizeof(ip));
            move(buf[c],info,sizeof(rPlayerInfo));
            inc(c,sizeof(rPlayerInfo));
        end;
        //copy 'own' data to last seat for iterating convenievne
        move(me,seats[length(seats)-1],sizeof(TPlayerInfo));
        //state of the game. player will start the game immiedietly if rsInGame
        if eRoomStates(buf[c])=rsInGame then joinGame;
      end;
      mmPlayerInfo:begin
        //if player already exists on the list then update, oterhwise add
        SetString(s,@buf[2],buf[1]);
       // move(buf[2],s[1],length(s));
        i:=playerExists(s);
        if i=-1 then begin
           i:=length(seats);
           setlength(seats,length(seats)+1);
        end;
        with seats[i] do begin
          //buf[1]:=length(login);
          login:=s;
          c:=2+length(login)+1;
          move(buf[c],ip,sizeof(ip));
          inc(c,sizeof(ip));
          move(buf[c],info,sizeof(rPlayerInfo));
          inc(c,sizeof(rPlayerInfo));
          move(buf[c],qport,2);
        end;
      end;
    end;
  end;
end;

function TmenuForm.playerExists(const l: tlogin): integer;
var i:integer;
begin
  result:=-1;
  for i:=0 to length(seats)-1 do if seats[i].login=l then begin
    result:=i;
    break;
  end;
end;

procedure TmenuForm.startHostGame;
var i:integer;
begin
  player1.login:=me.login;
  network.initializeGame(mapInfo,seats);
  //multi mode, hosting a game
  game.loadGame(format('%s%s%s%dx%d%s',[appPath,'world',DirectorySeparator,mapInfo.mapX,mapInfo.mapY,'.terrain']),true);
  //lock mouse
  inputManager.mouseLocked:=true;
  //show crosshairs
  uiManager.toggleCrosshair(true);
  //tell network that we are hosting
  network.setMode(true);

  //spawn host's actor. probably should be done elswhere..
  i:=logic.getNextFreeId;
  network.clients[0].actorId:=i;
  network.clients[0].port:=10;
  player1.actorID:=i;
  om.addOrder(orCreateActor,
              i,
              integer(network.clients[0].characterType),
              vector3fmake(0,32,0),
              getActorCommonProperties(network.clients[0].characterType).size
             );
  oM.addOrder(orToggleBrain,player1.actorID,0);
  oM.addCameraMode(player1.actorID,cm3rdPerson);
  close;
end;

procedure TmenuForm.joinGame;
begin
  player1.login:=me.login;
  network.initializeGame(mapInfo,seats);
  //first set player name to player2 cause server is already player1
  game.loadGame(format('%s%s%s%dx%d%s',[appPath,'world',DirectorySeparator,mapInfo.mapX,mapInfo.mapY,'.terrain']),true);
  //lock mouse
  inputManager.mouseLocked:=true;
  //show crosshairs
  uiManager.toggleCrosshair(true);
  //tell network that we are joining
  network.setMode(false);
  close;
end;

procedure TmenuForm.singleBtnClick(Sender: TObject);
begin
  //single player mode
  game.loadGame(format('%s%s%s%s',[appPath,'saves',DirectorySeparator,'map.terrain']),false);
  //lock mouse
  inputManager.mouseLocked:=true;
  //show crosshairs
  uiManager.toggleCrosshair(true);
  close;
end;

procedure TmenuForm.hostBtnClick(Sender: TObject);
var i:integer;
begin
  //create game room and put self in the first slot
  setlength(seats,1);
  move(me,seats[0],sizeof(me));
  Buf[0]:=byte(mmHost);
  tcp.Send(Buf,1);
end;

procedure TmenuForm.FormDestroy(Sender: TObject);
var i:integer;
begin
//  for i:=0 to length(seats)-1 do seats[i].free;
  //seats.free;
end;

procedure TmenuForm.loginBtnClick(Sender: TObject);
begin
  me.login:=lowercase(logine.Text);
 // moje ip?
  //me.ip;
  //me.info;
  tcp.Connect(addresse.Text,440);
  log('connecting..');
end;

procedure TmenuForm.joinBtnClick(Sender: TObject);
begin
  //send find match request
  Buf[0]:=byte(mmFind);
  tcp.Send(Buf,1);
end;


end.

