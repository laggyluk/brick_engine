unit ui_plan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, 
    ui_menu_template, BitmapLabel;

type

  { TmenuPlan }

  TmenuPlan = class(TmenuTemplate)
    BitmapLabel1: TBitmapLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure resizeBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure resizeBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure resizeBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure topBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure topBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure topBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  menuPlan: TmenuPlan;

implementation

{$R *.lfm}

{ TmenuPlan }

procedure TmenuPlan.FormCreate(Sender: TObject);
begin
  inherited;
end;

procedure TmenuPlan.resizeBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TmenuPlan.resizeBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

procedure TmenuPlan.resizeBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TmenuPlan.topBarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TmenuPlan.topBarMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

procedure TmenuPlan.topBarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;


procedure TmenuPlan.FormActivate(Sender: TObject);
begin
  inherited;
end;

end.

