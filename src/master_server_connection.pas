unit master_server_connection;
//thread based connection to server. can do one action at a time,
//returns result via callback

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,blcksock, synautil, synsock, ssl_openssl, ssl_openssl_lib,
  master_server_shared,ExtCtrls;

const timeout = 120000;//for login
      //masterAddress = 'laggyluk.no-ip.biz';
      masterAddress = '192.168.1.132';
      masterPort = '440';

type

  TMasterServerCallback = procedure(leState:emsStates) of object;
  { TMasterServerConnection }

  TMasterServerConnection = class(TThread)
  private
    Socket:TTCPBlockSocket;
    fcallback:TMasterServerCallback;
    //flags if socket is opened
    connected:boolean;
    //flags what needs to be done
    faction:boolean;
    fstate:emsStates;
    flogin,fpass:string;
    pingTimer:TTimer;
    pingCount:integer;
    settings:rSessionSettings;
    procedure pingTimerTimer(Sender: TObject);
  public
    Constructor Create(leCallback:TMasterServerCallback);
    Destructor Destroy; override;
    procedure Execute; override;
    procedure login(const lgn,pass:string);
    procedure hostGame(lesettings: rSessionSettings);
    procedure joinGame(lesettings: rSessionSettings);
    procedure ping;
    procedure logOut;
  end;

var
  playersInfo:array [0..3] of record
    login:string[16];
    info:rPlayerInfo;
  end;

implementation

{ TMasterServerConnection }

procedure TMasterServerConnection.pingTimerTimer(Sender: TObject);
begin
   if connected then begin
       inc(pingCount);
       if pingCount>max_ping_time*0.85 then begin
          pingCount:=0;
          ping;
       end;
    end
    else pingCount:=0;
end;

constructor TMasterServerConnection.Create(leCallback: TMasterServerCallback);
begin
  fcallback:=leCallback;
  socket:=TTCPBlockSocket.create;
  Socket.SSL.CertCAFile := ExtractFilePath(ParamStr(0)) + 's_cabundle.pem';
  Socket.SSL.CertificateFile := ExtractFilePath(ParamStr(0)) + 's_cacert.pem';
  Socket.SSL.KeyPassword := 's_cakey';
  FreeOnTerminate:=true;
  pingTimer:=ttimer.create(nil);
  pingTimer.interval:=1000;
  pingTimer.OnTimer:=@pingTimerTimer;
  pingTimer.enabled:=true;
  inherited create(false);
end;

destructor TMasterServerConnection.Destroy;
begin
  Socket.free;
  inherited Destroy;
end;

procedure TMasterServerConnection.Execute;
var
  result:string;
  s:string;
  i:integer;
begin
  try
  repeat
    if terminated then break;
    if faction then begin
       faction:=false;
       result:='';
       //case request of
       case fstate of
         //try to login to master server
         mslogin:begin
           connected:=false;
           //create ssl connection
           Socket.Connect(masterAddress, masterPort); // connect to the host
           Socket.SSLDoConnect; // start SSL connection; only server has a certificate
           if Socket.LastError = 0 then
           begin
             Socket.SendString(format('%s;%s%s',[flogin,fpass,CRLF]));
             result:=socket.RecvString(timeout);
           end else begin
             fstate:=msServerDown;
           end;
           if result='ok' then begin
              fstate:=msLoggedIn;
              connected:=true;
           end else begin
              fstate:=msLoginFailed;
           end;

         end;
         //search for a game
         msFindGame:if connected then begin
           fstate:=msSearchingForGame;
           Socket.SendByte(byte(msFindGame));
           socket.SendBuffer(@settings,sizeof(rSessionSettings));
         end else
           fstate:=msDisconnected;

         //host a game
         msHostGame:if connected then begin
            fstate:=msHostGame;
            Socket.SendByte(byte(msHostGame));
            socket.SendBuffer(@settings,sizeof(rSessionSettings));
            //fstate:=emsStates(socket.RecvByte(timeout));
         end else
           fstate:=msDisconnected;
         //report to ser so it doen't disconnect
         msPing:if connected then begin
           Socket.SendByte(byte(msPing));
           fstate:=emsStates(socket.RecvByte(timeout));
         end else
           fstate:=msDisconnected;
         msLogOut:begin
           socket.sendByte(byte(msLogOut));
           fstate:=msLogOut;
         end;
         //incoming stuff like map name etc
         msRoomInfo:begin
           socket.RecvBuffer(@settings,sizeof(settings));
           //fstate:=msRoomInfo;
           //login
           for i:=0 to length(playersInfo)-1 do begin
              playersInfo[i].login:=socket.RecvString(timeout);
              //rest of data if login ok
              if playersInfo[i].login<>'n/a' then
                 socket.RecvBuffer(@playersInfo[i].info,sizeof(rPlayerInfo));
           end;
           fstate:=msPlayerInfo;
         end;
       end;
       fcallback(fstate);
    end;
    //here we need to query server for match
    //check if connected to master server every now and then.
    //if not then stop game
    //if socket.CanRead(1500) then socket.RecvString(timeout);
    sleep(1000);
 until false;
 except
//    log('some socket error')
 end;
end;

procedure TMasterServerConnection.login(const lgn, pass: string);
begin
  fstate:=mslogin;
  flogin:=lgn;
  fpass:=pass;
  faction:=true;
end;

procedure TMasterServerConnection.hostGame(lesettings: rSessionSettings);
begin
  settings:=lesettings;
  fstate:=msHostGame;
  faction:=true;
end;

procedure TMasterServerConnection.joinGame(lesettings: rSessionSettings);
begin
  settings:=lesettings;
  fstate:=msFindGame;
  faction:=true;
end;

procedure TMasterServerConnection.ping;
begin
  fstate:=msPing;
  faction:=true;
end;

procedure TMasterServerConnection.logOut;
begin
  fstate:=msLogOut;
  faction:=true;
end;

end.

