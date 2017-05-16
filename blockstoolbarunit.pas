unit blocksToolbarUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ColorBox, Buttons, ExtCtrls, Spin, ComCtrls, Grids, core_types, TypInfo,
  types, FPimage, core_terrain_generator, core_render, VectorGeometry,
  core_editor, core_main, core_utils, LCLIntf, brickUImain, core_editor_states;

type

  { TToolbarBlocks }

  TToolbarBlocks = class(TForm)
    loadPalette: TOpenDialog;
    refreshChunksBtn1: TBitBtn;
    saveBlockColorsBtn: TBitBtn;
    colorPicker: TColorDialog;
    colorList: TListBox;
    colorGrid: TStringGrid;
    saveBlockColorsBtn1: TBitBtn;
    loadPaletteBtn: TBitBtn;
    refreshChunksBtn: TBitBtn;
    savePalette: TSaveDialog;
    procedure addPointLightBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure colorGridClick(Sender: TObject);
    procedure colorGridDblClick(Sender: TObject);
    procedure colorGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure colorGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure colorListDblClick(Sender: TObject);
    procedure colorListDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure colorListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure refreshChunksBtn1Click(Sender: TObject);
    procedure refreshChunksBtnClick(Sender: TObject);
    procedure saveBlockColorsBtn1Click(Sender: TObject);
    procedure loadPaletteBtnClick(Sender: TObject);
    procedure saveBlockColorsBtnClick(Sender: TObject);
    procedure colorGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    { private declarations }
    updateFlag:boolean;
    dirDrag:boolean;
  public
    { public declarations }
    procedure vibrate;
    procedure populateColorList; //populate list with color names and values
  end;

var
  toolbarBlocks: TtoolbarBlocks;
  mx,my:integer;

implementation
//uses brickUIMain;

var
  blockIntColors:array [btNone..btMax] of TFPColor;

{$R *.lfm}

{ toolbarBlocks }

procedure ttoolbarBlocks.colorListDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
   b:TBlockType;
begin
  with Control as TListBox do
  begin
    canvas.FillRect(ARect);
    b:=TBlockType(index+2);
    canvas.TextOut(ARect.Left + 2, ARect.Top, copy(Items[Index],3,length(Items[Index])-2));
    canvas.Brush.Color:=FPColorToTColor(blockIntColors[eBlockTypes(b)]);
    canvas.FillRect(ARect.Right-14,arect.top+2,arect.right-2,arect.Bottom-2);
//    Canvas.Font.Color := TColor(Items.Objects[Index]);
  end;
end;

procedure ttoolbarBlocks.colorListDblClick(Sender: TObject);
var
  b:TBlockType;
begin
  if colorPicker.Execute then begin
     b:=TBlockType(colorList.ItemIndex+2);
    //blockIntColors[eblocktypes(colorList.ItemIndex+2)]:=TColorToFPColor(colorPicker.Color);
     blockColors[eBlockTypes(b)][0]:=red(colorPicker.Color) / 255;
     blockColors[eBlockTypes(b)][1]:=green(colorPicker.Color) / 255;
     blockColors[eBlockTypes(b)][2]:=blue(colorPicker.Color) / 255;
    // blockColors[eBlockTypes(b)][3]:=1;
     populateColorList;
  end;
end;

procedure ttoolbarBlocks.addPointLightBtnClick(Sender: TObject);
begin
  editor.addPointLight;
  ui.BringToFront;
end;

procedure TToolbarBlocks.Button1Click(Sender: TObject);
begin

end;

procedure TToolbarBlocks.colorGridClick(Sender: TObject);
begin
  if editor.state=esPickBlockColor then begin
      renderer.blockBrush.fill(eblocktypes(mx+my*16));
      editor.state:=esPaintWithBrush;
  end else
      editor.brushBlockType:=eblocktypes(mx+my*16);
  ui.BringToFront;
end;

procedure TToolbarBlocks.colorGridDblClick(Sender: TObject);
var
  b:TBlockType;
begin
  //pick color
  if colorPicker.Execute then begin
     b:=TBlockType(mx+my*16);
    //blockIntColors[eblocktypes(colorList.ItemIndex+2)]:=TColorToFPColor(colorPicker.Color);
     blockColors[eBlockTypes(b)][0]:=red(colorPicker.Color) / 255;
     blockColors[eBlockTypes(b)][1]:=green(colorPicker.Color) / 255;
     blockColors[eBlockTypes(b)][2]:=blue(colorPicker.Color) / 255;
     //blockColors[eBlockTypes(b)][3]:=1;
     populateColorList;
     colorgrid.Clean;
  end;
end;

procedure TToolbarBlocks.colorGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TToolbarBlocks.colorGridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  colorgrid.MouseToCell(x,y,x,y);
  mx:=x;my:=y;
  colorGrid.hint:= GetEnumName(typeinfo(eBlockTypes),integer(x+y*16));
end;

procedure ttoolbarBlocks.colorListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

  if editor.state=esPickBlockColor then begin
      renderer.blockBrush.fill(eblocktypes(colorList.ItemIndex+2));
      editor.state:=esPaintWithBrush;
  end else
      editor.brushBlockType:=eblocktypes(colorList.ItemIndex+2);
  ui.BringToFront;
end;

procedure TToolbarBlocks.FormCreate(Sender: TObject);
begin

end;

procedure TToolbarBlocks.refreshChunksBtn1Click(Sender: TObject);
begin
  editor.state:=esPaintColors;
  ui.BringToFront;
end;

procedure TToolbarBlocks.refreshChunksBtnClick(Sender: TObject);
begin
  terrain.cullAllChunks;
end;

procedure ttoolbarBlocks.saveBlockColorsBtnClick(Sender: TObject);
begin
  savepalette.InitialDir:=apppath+DirectorySeparator+'palettes';
  if savePalette.execute then begin
    terrain.saveBlockColors(savepalette.filename);
  end;
  ui.BringToFront;
end;

function GiveRainbowColor(iMin, iMax, i: Integer): TColor;
var
  m: Double;
  r, g, b, mt: Byte;
begin
  m := (i - iMin)/(iMax - iMin + 1) * 6;
  mt := (round(frac(m)*$FF));
  case Trunc(m) of
  0: begin
      R := $FF;
      G := mt;
      B := 0;
    end;
  1: begin
      R := $FF - mt;
      G := $FF;
      B := 0;
    end;
  2: begin
      R := 0;
      G := $FF;
      B := mt;
    end;
  3: begin
      R := 0;
      G := $FF - mt;
      B := $FF;
    end;
  4: begin
      R := mt;
      G := mt-$ff;
      B := $F0;
    end;
  5: begin
      R := mt;
      G := mt;
      B := mt;
    end;
  end; // case
  Result := rgb(R,G,B);
end;

procedure TToolbarBlocks.colorGridPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var bl:eblockTypes;
begin
  if not (gdfixed in aState) then begin
    bl:=eBlockTypes(aCol+aRow*16);
{    if (bl>btGalactic) and (bl<btZizanium) then
       colorGrid.Canvas.Brush.Color:=GiveRainbowColor(integer(btGalactic),integer(btZizanium),integer(bl))
     else}
       colorGrid.Canvas.Brush.Color:=vectocolor(blockColors[bl]);
  end;
end;

procedure TToolbarBlocks.saveBlockColorsBtn1Click(Sender: TObject);
var bl:eblockTypes;
    aCol,aRow:integer;
begin
  for bl:=bt1 to bt191 do
    blockColors[bl]:=colorToVec4(GiveRainbowColor(integer(bt1),integer(bt191),integer(bl)));
  populateColorList;
  colorGrid.Clean;
  ui.BringToFront;
end;

procedure TToolbarBlocks.loadPaletteBtnClick(Sender: TObject);
begin
  if loadPalette.execute then begin;
    terrain.loadBlockColors(loadpalette.FileName);
    //refresh list and grid
    populateColorList;
    colorGrid.Clean;
    terrain.cullAllChunks;
  end;
  ui.BringToFront;
end;


procedure TToolbarBlocks.vibrate;
var x,y,i:integer;
begin
  x:=left;
  y:=top;
  for i:=0 to 4 do begin
    left:=x+random(5);
    top:=y+random(5);
    Application.ProcessMessages;
    sleep(80);
  end;
  top:=y;
  left:=x;
end;

procedure ttoolbarBlocks.populateColorList;
var
  s:string;
  b:TBlockType;
begin
  colorList.Clear;
  for b:=TBlockType(btNone)+2 to TBlockType(btMax)-1 do begin
    s := GetEnumName(TypeInfo(eBlockTypes),TBlockType(b));
    colorList.AddItem(s,nil);
//    c:=blockColors[eBlockTypes(b)][0];
    blockIntColors[eBlockTypes(b)].red:=round(65535*blockColors[eBlockTypes(b)][0]);
    blockIntColors[eBlockTypes(b)].green:=round(65535*blockColors[eBlockTypes(b)][1]);
    blockIntColors[eBlockTypes(b)].blue:=round(65535*blockColors[eBlockTypes(b)][2]);
    blockIntColors[eBlockTypes(b)].alpha:=1;
  end;
end;

end.

