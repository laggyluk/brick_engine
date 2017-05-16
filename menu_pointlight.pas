unit menu_pointLight;

{$mode objfpc}{$H+}

interface

uses

  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls,  core_utils, core_types, core_lights, core_editor_states;

type

  { TmenuPointLight }

  TmenuPointLight = class(TForm)
    attenConst: TTrackBar;
    attenExp: TTrackBar;
    attenLinear: TTrackBar;
    ColorDialog1: TColorDialog;
    diffuse: TTrackBar;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure diffuseChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    editorState:eEditorStates;
    pointLight:TPointLight;
    { public declarations }
    procedure ShowAtCursor(_pointLight:TPointLight);
    procedure setlight;
  end;

var
  menuPointLight: TmenuPointLight;

implementation

{$R *.lfm}

{ TmenuPointLight }

procedure TmenuPointLight.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if key=32 then hide;
end;

procedure TmenuPointLight.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (button=mbright) and (ColorDialog1.Execute) then begin
     color:=ColorDialog1.Color;
     setlight;
  end;
end;

procedure TmenuPointLight.diffuseChange(Sender: TObject);
begin
  setLight;
  label1.Caption:='Diffuse strength: '+ floattostr(diffuse.Position/10);
  label4.Caption:='Attenuation const: '+ floattostr(attenConst.Position/10);
  label2.Caption:='Attenuation linear: '+ floattostr(attenLinear.Position/10);
  label3.Caption:='Attenuation exp: '+ floattostr(attenExp.Position/10);
end;

procedure TmenuPointLight.FormActivate(Sender: TObject);
var
  c:tcolor;
begin
  if editorState=esPointLight then begin
     c:=vecToColor(pointLight.color);
     color:=c;
     diffuse.Position:=round(pointLight.diffuseIntensity*10);
     attenLinear.Position:=round(pointLight.attenuation.linear*10);
     attenConst.Position:=round(pointLight.attenuation.constant*10);
     attenExp.Position:=round(pointLight.attenuation.exp*10);
  end;
end;

procedure TmenuPointLight.ShowAtCursor(_pointLight: TPointLight);
var
  p:tpoint;
begin
  p:=getMousePosition;
  left:=p.x-width div 2;
  top:=p.y+100;
  pointLight:=_pointLight;
  show;
end;

procedure TmenuPointLight.setlight;
begin
  if editorState=esPointLight then pointLight.setUp(
      colortovec3(color),
      pointLight.position,
      diffuse.Position / 10,
      attenConst.Position / 10,
      attenLinear.Position / 10,
      attenExp.position / 10);
end;

end.

