unit core_ui_controls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,core_texture, core_ui_base, math;

type

  //class for system log displaying
  { TUILog }
  //keeps track of all* the lines we have added but displays only portion
  TUILog = class (TUICanvas)
   private

    fTxt:TUIText;
    lines:TStringlist;
    //for some bug reason we need rect as root?
    rect:TUIRect;
    procedure truncTextForDisplay;
   public
    //after exceeding this lines will be cleared;
    maxLines:integer;
    //how many lines can be displayed at a time
    displayedLines:integer;
    procedure Add(const s:string);
    procedure AddLines(strings:TStringList);
    procedure Update;override;
    constructor Create(_atlas: TTextureAtlas);
    destructor Destroy;override;
  end;

  { TUIButton }

  TUIButton = class (TUIRect)
   public
    caption:TUIText;
    constructor Create(_parent: TUITransform; _atlas: TTextureAtlas; _x, _y,
      _width, _height: single; fontSize: single=3);
  end;

implementation

{ TUIButton }

constructor TUIButton.Create(_parent: TUITransform; _atlas: TTextureAtlas;_x,_y,
  _width, _height: single;fontSize:single = 3);
begin
  inherited create(_parent);
  sprite:=@_atlas.data[_atlas.getIndexOf('rect')];
  setColor(0.3,0.3,0.3,1);
  hitTest:=true;
  width:=_width;
  height:=_height;
  x:=_x;
  y:=_y;
//  rect.OnMouseClick:=@ClickTest;
  caption:=TUIText.create(_parent,_atlas);
  caption.width:=_width*0.9;
  caption.height:=_height*2.5;
  caption.x:=_x+0.02;
  caption.y:=_y+0.01;
  //caption.autoSize:=false;
  //caption.fontSize:=fontSize;
end;

{ TUILog }

procedure TUILog.truncTextForDisplay;
var
  i:integer;
  str:tstringlist;
begin
  if lines.count> maxLines then begin
     str:=tstringlist.create;
     for i:=max(0,lines.count-displayedLines-1) to lines.count-1 do
       str.add(lines[i]);
     lines.text:=str.text;
     str.free;
  end;
  //need to move the uitext up
  ftxt.lines.clear;
  //say hello to memory fragmentation
  //for i:=lines.Count-1 downto max(0,lines.count-displayedLines) do
  for i:=max(0,lines.Count-displayedLines) to lines.Count-1 do
      ftxt.lines.Add(lines[i]);
  ftxt.BuildText;
  //ftxt.text:=lines.Text;
end;

procedure TUILog.Add(const s: string);
begin
  lines.add(s);
  dirty:=true;
end;

procedure TUILog.AddLines(strings: TStringList);
begin
  lines.Append(strings.text);
  dirty:=true;
end;

procedure TUILog.Update;
begin
  //inherited Update;
  if dirty then begin
    truncTextForDisplay;
    build;
  end;
end;

constructor TUILog.Create(_atlas:TTextureAtlas);
begin
  inherited create(nil);
  width:=1;
  height:=1;
  enabled:=false;
  fTxt:=TUIText.create(self,_atlas);
  ftxt.autoSize:=false;
  ftxt.fontSize:=2.5;
  ftxt.x:=-0.495;
  ftxt.y:=-0.3;
  ftxt.width:=1;
  ftxt.height:=1;
  lines:=TStringlist.create;
  displayedLines:=8;
  maxLines:=128;
end;

destructor TUILog.Destroy;
begin
  lines.free;
  inherited Destroy;
end;



end.

