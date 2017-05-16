unit test_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, master_server_connection, master_server_shared;

type

  { TForm1 }

  TForm1 = class(TForm)
    difficulty: TEdit;
    joinBtn: TButton;
    Edit1: TEdit;
    host: TButton;
    loginBtn: TButton;
    Memo1: TMemo;
    pingTimer: TTimer;
    difficultyBar: TScrollBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure hostClick(Sender: TObject);
    procedure joinBtnClick(Sender: TObject);
    procedure loginBtnClick(Sender: TObject);
    procedure pingTimerTimer(Sender: TObject);
    procedure difficultyBarChange(Sender: TObject);
  private
    { private declarations }
    ms:TMasterServerConnection;
    loginOk:boolean;
    //for pinging serv onece a while
    pingCount:integer;
    settings:rSessionSettings;
  public
    { public declarations }
    procedure log(const s:string);
    procedure status(leState:emsStates);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.loginBtnClick(Sender: TObject);
begin
  ms:=TMasterServerConnection.create(@status);
  ms.login(edit1.Text,'zsmest');
end;

procedure TForm1.pingTimerTimer(Sender: TObject);
begin
  exit;
  if loginOk then begin
     inc(pingCount);
     if pingCount>max_ping_time*0.85 then begin
        pingCount:=0;
        ms.ping;
     end;
  end
  else pingCount:=0;
end;

procedure TForm1.difficultyBarChange(Sender: TObject);
begin
  difficulty.text:=inttostr(difficultyBar.Position);
  settings.difficulty:=difficultyBar.Position;
end;

procedure TForm1.hostClick(Sender: TObject);
begin
  if loginok then ms.hostGame(settings) else log('login first');
end;

procedure TForm1.joinBtnClick(Sender: TObject);
begin
  if loginok then ms.joinGame(settings) else log('login first');
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
   if loginOk then ms.logOut;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  randomize;
  edit1.Text:='test'+inttostr(random(100));
end;

procedure TForm1.log(const s: string);
begin
  memo1.lines.add(format('%s> %s',[timetostr(now),s]));
end;

procedure TForm1.status(leState: emsStates);
begin
  case (leState) of
   msServerDown: log('master server down!');
   msLoggedIn:begin
      log('logged in');
      loginOk:=true;
      //pingTimer.enabled:=true;
   end;
   msLoginFailed:begin
      log('login failed :(');
      loginOk:=false;
   end;
   msDisconnected:begin
      log('master server connection lost!');
      loginOk:=false;
   end;
   msHostGame:log('hosting a game');
   msSearchingForGame:log('searching for a game');
   msPong:begin
       log('pong from server');
   end;
   msRoomInfo:begin
       log('room info updated');
       //now to do something with that info..
   end;
   msPlayerInfo:log('player info received');
   //else
     //log('unhandled reponse')
  end;
end;

end.

