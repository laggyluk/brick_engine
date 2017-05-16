unit toolbar_log;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics,
  StdCtrls, ExtCtrls, core_types, core_main, core_render,
  core_utils, typinfo, VectorGeometry, core_orders, core_orders_manager;

type

  { TlogForm }

  TlogForm = class(TForm)
    CheckBox1: TCheckBox;
    input: TEdit;
    im: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    kloc: TLabel;
    Memo1: TMemo;
    Panel2: TPanel;
    radarTimer: TTimer;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure inputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure radarTimerTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  logForm: TlogForm;

implementation

{$R *.lfm}

{ TlogForm }

procedure TlogForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if sender=logform then CloseAction:=TCloseAction.caHide
  else application.Terminate;
end;

procedure TlogForm.FormCreate(Sender: TObject);
begin
  radarTimer.Enabled:=CheckBox1.checked;
end;

procedure TlogForm.inputKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var o:orders;
    c,s,p1,p2,p3,p4:string;
begin
  s:=input.Text;
  if key=13 then begin
    c:=eatString(s,' ');
    p1:=eatString(s,' ');
    p2:=eatString(s,' ');
    p3:=eatString(s,' ');
    p4:=eatString(s,' ');
    if p1='' then p1:='0';
    if p2='' then p2:='0';
    if p3='' then p3:='0';
    if p4='' then p4:='0';

    o:=orders( GetEnumValue(typeinfo(orders),c));
    oM.addOrder(o,strtoint(p1),strtoint(p2),strtoint(p3),vector3fmake(0,0,0));
  end;
end;

procedure TlogForm.CheckBox1Change(Sender: TObject);
begin
  radarTimer.Enabled:=CheckBox1.Checked;
end;

procedure TlogForm.radarTimerTimer(Sender: TObject);
var
  w,h,x,y:integer;
  blok:eBlockTypes;
  v:taffinevector;
begin
  w:=im.Canvas.width div (active_chunks_w+4);
  //w:=w-2;
  for y:=0 to active_chunks_h-1 do
    for x:=0 to active_chunks_w-1 do
      if core.chunks[x,y].renderable.isVisible then begin
        im.canvas.brush.color:=clWhite;
        im.canvas.pen.color:=clgreen;
        im.Canvas.Rectangle(x*(w+2),y*(w+2),x*(w+2)+w,y*(w+2)+w)
      end else begin
        im.canvas.brush.color:=clBlack;
        im.canvas.pen.color:=clred;
        im.Canvas.Rectangle(x*(w+2),y*(w+2),x*(w+2)+w,y*(w+2)+w)
      end;
  //selection block coords
  label1.caption:=floattostr(renderer.selectionCube.position[0]);
  label2.caption:=floattostr(renderer.selectionCube.position[1]);
  label3.caption:=floattostr(renderer.selectionCube.position[2]);

  //label1.caption:=floattostr(v[0]);
  //label2.caption:=floattostr(v[1]);
  //label3.caption:=floattostr(v[2]);
  setvector(v,renderer.selectionCube.position);
  //blok:=getBlockTypeAt(renderer.selectionCube.position);
  blok:=getWorldArray(round(v[0]+world_width2),round(v[1]),round(v[2]+world_depth2));
  kloc.Caption:=EnumToStrIng(typeinfo(eBlockTypes),integer(blok));
  //cam coordinates
  //label1.caption:=floattostr(camera.position[0]);
  //label2.caption:=floattostr(camera.position[1]);
  //label3.caption:=floattostr(camera.position[2]);
  //draw camera
  //x:=im.Canvas.width div round((camera.position[0]+world_width2) / active_chunks_w) ;
  //y:=(im.Canvas.height div round((camera.position[2]+world_depth2) / active_chunks_h));
  //im.canvas.pen.color:=clyellow;
  //im.Canvas.Rectangle(x*(w+2),y*(w+2),x*(w+2)+w,y*(w+2)+w)
end;

end.

