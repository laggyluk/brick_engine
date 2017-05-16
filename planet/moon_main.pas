unit moon_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, lNetComponents, Forms, Controls, Graphics,
  Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    hostE: TComboBox;
    loginE: TEdit;
    loginBtn: TButton;
    logMemo: TMemo;
    passE: TEdit;
    tcp: TLTCPComponent;
    procedure loginBtnClick(Sender: TObject);
  private
    { private declarations }
    procedure log(const s:string);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.loginBtnClick(Sender: TObject);
begin
  if tcp.Connect(hoste.Text,440) then log('connected')
  else log('disconnected');
end;

procedure TForm1.log(const s: string);
begin
  logMemo.Lines.add(format('%s> %s',[timetostr(now),s]));
end;

end.

