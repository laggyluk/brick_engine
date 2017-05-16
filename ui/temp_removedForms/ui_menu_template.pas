unit ui_menu_template;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, BitmapLabel,
  core_ui;

type

  { TmenuTemplate }

  TmenuTemplate = class(TForm)
    BitmapLabel2: TBitmapLabel;
    leftBorder: TBitmapLabel;
    quitBtn: TBitmapLabel;
    resizeBtn: TBitmapLabel;
    rightBorder: TBitmapLabel;
    topBar: TBitmapLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure quitBtnClick(Sender: TObject);
    procedure resizeBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure resizeBtnMouseLeave(Sender: TObject);
    procedure resizeBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure topBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure topBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure topBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
    MouseIsDown: boolean;
    PX, PY: integer;
  public
    { public declarations }
    lockWidth:boolean;
    lockHeight:boolean;
  end;

var
  menuTemplate: TmenuTemplate;

implementation

{$R *.lfm}

{ TmenuTemplate }

procedure TmenuTemplate.FormActivate(Sender: TObject);
begin
  uiManager.paintFormButtons(self);
  uiManager.currentForm:=self;
  showmessage('wtf');
end;

procedure TmenuTemplate.FormCreate(Sender: TObject);
begin
  color:=backgroundColorGrey;
  lockWidth:= true;
  lockHeight:= false;
end;

procedure TmenuTemplate.quitBtnClick(Sender: TObject);
begin
  close;
end;

procedure TmenuTemplate.resizeBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TmenuTemplate.resizeBtnMouseLeave(Sender: TObject);
begin
  MouseIsDown:=False;
end;

procedure TmenuTemplate.resizeBtnMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  dx,dy:integer;
begin
  if MouseIsDown then begin
     if Height+ (Y - PY)<64 then exit;
     dx:= (X - PX);
     dy:= (Y - PY);
     if lockWidth then dx:=0;
     if lockHeight then dy:=0;
     SetBounds(Left , Top , Width+ dx, Height+ dy);
   end;
end;

procedure TmenuTemplate.topBarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
      MouseIsDown := True;
      PX := X;
      PY := Y;
    end;
end;

procedure TmenuTemplate.topBarMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if MouseIsDown then begin
     SetBounds(Left + (X - PX), Top + (Y - PY), Width, Height);
   end;
end;

procedure TmenuTemplate.topBarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseIsDown:=False;
end;

end.

