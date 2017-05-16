unit toolbar_lights;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ColorBox, Buttons, ExtCtrls, Spin, ComCtrls, core_types, TypInfo, types,
  FPimage, core_terrain_generator,core_render,VectorGeometry,core_editor,core_main,
  brickUIMain;

type

  { TtoolbarLights }

  TtoolbarLights = class(TForm)
    addPointLightBtn: TBitBtn;
    colorPicker: TColorDialog;
    sun: TShape;
    sunAmbient: TFloatSpinEdit;
    sunAmbientSlider: TTrackBar;
    sunDiffuseSlider: TTrackBar;
    sunDifuse: TFloatSpinEdit;
    sunDirection: TShape;
    initTimer: TTimer;
    procedure addPointLightBtnClick(Sender: TObject);
    procedure initTimerTimer(Sender: TObject);
    procedure sunAmbientChange(Sender: TObject);
    procedure sunAmbientSliderChange(Sender: TObject);
    procedure sunDiffuseSliderChange(Sender: TObject);
    procedure sunDifuseChange(Sender: TObject);
    procedure sunDirectionMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sunDirectionMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure sunDirectionMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sunMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sunMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure sunMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
    updateFlag:boolean;
    dirDrag:boolean;
    procedure setUpSun;
  public
    { public declarations }
  end;

var
  toolbarLights: TtoolbarLights;

implementation

var
  blockIntColors:array [btNone..btMax] of TFPColor;

{$R *.lfm}

{ TtoolbarLights }

procedure TtoolbarLights.addPointLightBtnClick(Sender: TObject);
begin
  editor.addPointLight;
  ui.BringToFront;
end;

procedure TtoolbarLights.initTimerTimer(Sender: TObject);
begin
  initTimer.Enabled:=false;
  sunAmbientSliderChange(nil);
  sunDiffuseSliderChange(nil);
end;

procedure TtoolbarLights.sunAmbientChange(Sender: TObject);
begin
  if updateFlag then exit;
  updateFlag:=true;
  sunAmbientSlider.Position:=round(sunAmbient.Value*sunDiffuseSlider.Max);
  setupSun;
  updateFlag:=false;
end;

procedure TtoolbarLights.sunAmbientSliderChange(Sender: TObject);
begin
  if updateFLag then exit;
  updateFlag:=true;
  sunAmbient.Value:=sunAmbientSlider.Position / 10;
  setUpSun;
  updateFlag:=false;
end;

procedure TtoolbarLights.sunDiffuseSliderChange(Sender: TObject);
begin
  if updateFLag then exit;
    updateFlag:=true;
    sunDifuse.Value:=sunDiffuseSlider.Position / 10;
    setUpSun;
    updateFlag:=false;
end;

procedure TtoolbarLights.sunDifuseChange(Sender: TObject);
begin
  if updateFlag then exit;
  updateFlag:=true;
  sunDiffuseSlider.Position:=round(sunDifuse.Value*sunDiffuseSlider.Max);
  setupSun;
  updateFlag:=false;
end;

procedure TtoolbarLights.sunDirectionMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   sunMouseDown(sender,button,shift,sunDirection.left,sunDirection.top);
end;

procedure TtoolbarLights.sunDirectionMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  sunMouseMove(sender,shift,sunDirection.left,sunDirection.top);
end;

procedure TtoolbarLights.sunDirectionMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    dirDrag:=false;
end;

procedure TtoolbarLights.sunMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if button = mbright then exit;
  dirDrag:=true;
end;

procedure TtoolbarLights.sunMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
 if dirDrag then begin
    sunDirection.top:=y;
    sunDirection.Left:=x;
    setUpSun;
 end;
end;

procedure TtoolbarLights.sunMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  dirDrag:=false;
  if button=mbRight then if colorPicker.Execute then begin
     sun.Brush.Color:=colorPicker.Color;
     setUpSun;
  end;
  ui.BringToFront;
end;

procedure TtoolbarLights.setUpSun;
var
  col:tcolor;
  x,y,w,h:integer;
begin
  w:= sun.left+ (sun.Width div 2);
  x:= (sun.left+sunDirection.left + sunDirection.Width div 2) -w;
  h:= sun.top+ (sun.height div 2);
  y:= (sun.top+sunDirection.top + sunDirection.height div 2) -h;
  renderer.setDirectionalLight(
          //color
          vector3fmake(
          red(sun.brush.Color) / 255,
          green(sun.brush.Color) / 255,
          blue(sun.brush.Color) / 255),       //direction
                  vectornormalize(vector3fmake(
                  x,-8.0,y)),
                  sunAmbient.Value,
                  sunDifuse.value);
end;

end.

