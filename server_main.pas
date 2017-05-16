unit server_main;

{$mode objfpc}{$H+}

interface

uses
  windows,Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  core_main, core_types, core_network;

type

  { TForm1 }

  TForm1 = class(TForm)
    addGameBtn: TButton;
    Label1: TLabel;
    lista: TListBox;
    Memo1: TMemo;
    procedure addGameBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnAppIdle(Sender: TObject; var Done: Boolean);
  private
    { private declarations }
    StartTime:cardinal;
  public
    { public declarations }
  end;

  procedure sout(const s:string);

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.addGameBtnClick(Sender: TObject);
begin
  //start a dedicated server
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  log:=@sout;
  log_add:=@sout;
  DecimalSeparator:='.';
  Application.AddOnIdleHandler(@OnAppIdle);
  gameMode:=true;
  core:=TPlanetCore.Create;
  //screen width and height doesn't really matter on dedicated serv
  core.init(320,240);
  network.setMode(true);

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  core.free;
end;

procedure TForm1.OnAppIdle(Sender: TObject; var Done: Boolean);
begin
  core.update((GetTickCount-StartTime)/ 1000);
  StartTime:= GetTickCount;
  //DrawTime:= GetTickCount - StartTime;
  Done :=  false ;
end;

procedure sout(const s: string);
begin
  form1.memo1.lines.add(s);
end;

end.

