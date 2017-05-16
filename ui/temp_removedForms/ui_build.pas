unit ui_build;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, BitmapLabel, core_ui_controls, core_ui, core_utils,
  core_game, core_types, VectorTypes, core_block_brush, core_render, core_main,
  core_player;
type

  { TmenuBuild }

  TmenuBuild = class(TForm)
    arrowMat: TBitmapLabel;
    quitBtn: TBitmapLabel;
    BitmapLabel2: TBitmapLabel;
    arrowBlu: TBitmapLabel;
    leftBorder1: TBitmapLabel;
    topSepartor: TBitmapLabel;
    leftBorder: TBitmapLabel;
    resizeBtn: TBitmapLabel;
    rightBorder: TBitmapLabel;
    topBar: TBitmapLabel;
    materialsCol: TBitmapLabel;
    BlueprintsCol: TBitmapLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure quitBtnClick(Sender: TObject);
    procedure BlueprintsColClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure materialsColClick(Sender: TObject);
    procedure resizeBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { private declarations }
    //currently selected brush. filled with

    //currently selected material
    materialBlock:eBlockTypes;
  public
    { public declarations }
    blockBrush:TBlockBrush;
    procedure populateList;
  end;

var
  menuBuild: TmenuBuild;
  MouseIsDown: boolean;
  PX, PY: integer;

implementation

{$R *.lfm}

{ TmenuBuild }

procedure TmenuBuild.FormCreate(Sender: TObject);
begin
  color:=backgroundColorGrey;
  blockBrush:=TBlockBrush.create;
end;

procedure TmenuBuild.FormDestroy(Sender: TObject);
begin
  blockBrush.free;
end;

procedure TmenuBuild.quitBtnClick(Sender: TObject);
begin
  close;
end;

procedure TmenuBuild.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TmenuBuild.FormActivate(Sender: TObject);
begin
  uiManager.currentForm:=self;
end;

procedure TmenuBuild.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
      MouseIsDown := True;
      PX := X;
      PY := Y;
    end;
end;

procedure TmenuBuild.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if MouseIsDown then begin
     SetBounds(Left + (X - PX), Top + (Y - PY), Width, Height);
   end;
end;

procedure TmenuBuild.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseIsDown:=False;
end;

procedure TmenuBuild.FormPaint(Sender: TObject);
var
  x, y: Integer;
begin
  {
  y := 0;
  while y < Height do
   begin
     Canvas.Draw(0, y, vertFrame.picture.Bitmap);
     Canvas.Draw((width-(vertFrame.picture.width div 2)) div 2, y, vertFrame.picture.Bitmap);
     Canvas.Draw(width-vertFrame.picture.width, y, vertFrame.picture.Bitmap);
     y := y + vertFrame.picture.Bitmap.Height;
   end;
   }
end;

procedure TmenuBuild.materialsColClick(Sender: TObject);
begin
  arrowMat.Top:=(sender as TBitmapLabel).Top;
  arrowMat.visible:=true;
  materialBlock:=strToMineral('bt'+(sender as TBitmapLabel).txt[0]);
  if blockBrush.data.Count>0 then
     blockBrush.fill(materialBlock);
end;

procedure TmenuBuild.BlueprintsColClick(Sender: TObject);
begin
  arrowBlu.Top:=(sender as TBitmapLabel).Top;
  arrowBlu.Visible:=true;
  blockBrush.load(apppath+'saves'+DirectorySeparator+(sender as TBitmapLabel).txt[0]+'.blu');
  if materialBlock<>btNone then blockBrush.fill(materialBlock);
  renderer.blockBrush:=blockBrush;
  renderer.gizmos.brush:=true;
  core.inputState:=iBuild;
end;

procedure TmenuBuild.resizeBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
const
  lockWidth = true;
  lockHeight = false;
begin
  if MouseIsDown then begin
     if Height+ (Y - PY)<64 then exit;
     if lockWidth then
        SetBounds(Left , Top , Width, Height+ (Y - PY)) else
     if lockHeight then
        SetBounds(Left , Top , Width+ (X - PX), Height)
     else
     SetBounds(Left, Top, Width + (X - PX), Height + (Y - PY));
   end;
end;

procedure TmenuBuild.populateList;
var
  i,c,j:integer;
  s:string;
  b:TBitmapLabel;
  str:tstringlist;
begin
  //populate owned block types list
  c:=materialsCol.top+materialsCol.Height+10;
  j:=0;
  for i:=0 to 255 do if player1.resources[i]>0 then begin
      s:=EnumToStrIng(typeinfo(eBlockTypes),i);
      s:=copy(s,3,length(s));
      b:=TBitmapLabel.create(self);
      //add material name label
      with b do begin
        parent:=self;
        top:=c+j*26;
        b.Height:=24;
        b.width:=materialsCol.width-26;
        left:=materialsCol.left;
        txt.add(s);
        uiManager.paintBmpButton(b,true);
        b.SendToBack;
        b.OnClick:=@materialsColClick;
      end;
      //add block color
      b:=TBitmapLabel.create(self);
      with b do begin
        parent:=self;
        top:=c-2+j*26;
        b.Height:=24;
        b.width:=16;
        left:=materialsCol.left+materialsCol.Width-14;
        txt.add('^');
        b.fontColor:=vecToColor(blockColors[eBlockTypes(i)]);
        uiManager.paintBmpButton(b,false);
        b.SendToBack;
        if menuBuild.Height-b.Top+b.Height<20 then break;//koniec strony
      end;
      inc(j);
  end;
  //populate blueprints list. from folder contents
  s:=apppath+'saves'+DirectorySeparator;
  str:=findallfiles(s,'*.blu',false);
  j:=0;
  for i:=0 to str.count-1 do begin
    b:=TBitmapLabel.create(self);
    //add material name label
    with b do begin
      parent:=self;
      top:=c+j*26;
      b.Height:=24;
      b.width:=materialsCol.width;
      left:=BlueprintsCol.left;
      s:=extractfilename(str[i]);
      txt.add(copy(s,1,length(s)-4));
      uiManager.paintBmpButton(b,true);
      b.SendToBack;
      b.OnClick:=@BlueprintsColClick;
      inc(j);
    end;
  end;
  str.free;
  arrowBlu.Visible:=false;
  arrowMat.Visible:=false;
end;

end.

