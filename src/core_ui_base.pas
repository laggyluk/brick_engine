unit core_ui_base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stdctrls, core_types, vectorTypes,VectorGeometry,core_texture,
  dglOpenGL, core_material_library,fgl, core_listOfRecords ;

type

  TUIVertex = record
    Pos: TAffineVector;
    UV: TVector2f;
    Col: TVector4f;
  end;

  TUIVertexBuffer = array of TUIVertex;
  //callback definition for mouse events
  TUIOnMouseClick = procedure (sender:tobject;button: integer;down:boolean;Shift: TShiftState; X, Y: Integer) of object;

  //base for uiRect and uiCanvas
  { TUITransform }
  TUITransform = class
   protected
    //position in parent, -0.5,+0.5 range where -0.5,+0.5 is top left corner
    fPosition:TAffineVector;
    //how much parent space it should occupy (-1,1 range or sth). z is not used
    fSize:TAffineVector;
    kids:TList;
    parent:TUITransform;
    //how many verticles are required for this control to be drawn
    //(?including children?)
    //in case of simple rect it will be 4/6, in case of more complicated
    //descendant, with text for example it will be letter count * 4/6
    vertCount:integer;
    //where verts are starting for this element in buffer
    vertStart:integer;
    //size and position getters and setters
    function GetPosProp(AIndex: Integer): single;
    function GetSizeProp(AIndex: Integer): single;
    procedure SetPosProp(AIndex: Integer; AValue: single);
    procedure SetSizeProp(AIndex: Integer; AValue: single);
    procedure SetPosition(AValue: TAffineVector);
    procedure SetSize(AValue: TAffineVector);
   public
    //position 'on screen' in gl units
    absolutePosition:TAffineVector;
    absoluteSize:TAffineVector;
    color:TVector4f;
    //when disabled, input events will be ignored. probably would be better off as color toggling property
    enabled:boolean;
    dirty:boolean;
    //should be tested for mouse clicks? when not visible also shouldn't be hitested.
    //all hittest enabled items are added to list on uicanvas
    hitTest:boolean;
    name:string;
    //when not visible it won't render but still occupy space
    visible:boolean;
    //assign callback functions
    OnMouseClick:TUIOnMouseClick;
    procedure AddKid(kid:TUITransform);
    //descendants use this to 'draw' themself into vertexBuffer which later is used for rendering
    procedure FillBuffer(var buf:TUIVertexBuffer; var count:integer);virtual;
    procedure Render;virtual;
    //returns vert count for self and children
    function GetVertCount:integer;
    procedure SetColor(r,g,b,a:single);
    //canvass calls it to get all elements valid for hit testing
    procedure registerForHitTest(toList:TList);
    property height:single index 1 read GetSizeProp write SetSizeProp;
    property width:single index 0 read GetSizeProp write SetSizeProp;
    property position: TAffineVector read fPosition write SetPosition;
    property size: TAffineVector read fSize write SetSize;
    property x:single index 0 read GetPosProp write SetPosProp;
    property y:single index 1 read GetPosProp write SetPosProp;
    constructor Create(_parent:TUITransform);
    destructor Destroy;override;
  end;

  { TUIRect }
  //base class for ui controls
  TUIRect = class (TUITransform)
   public
     sprite:pSprite;
     procedure FillBuffer(var buf:TUIVertexBuffer; var count:integer);override;
     constructor Create(_parent:TUITransform);
     destructor Destroy;override;
  end;

  { TUIText }

  TUIText = class (TUITransform)
   private
    //font atlas
    atlas:TTextureAtlas;
    fFontheight:single;
    //text length in 'atlas pixels'
    fTextWidth:single;
    //used to scale text to our desire, use with autosize disabled
    fFontScale:single;
    function GetFontSize: single;
    function GetText: string;
    procedure SetFontSize(AValue: single);
    procedure SetText(AValue: string);
   public
    //when enabled text will stretch to container size, when disabled it will use fixed size
    autoSize:boolean;
    //text is held here. if you alter lines don't forget to call BuildText
    lines:TStringList;
    //alignment:TAlignment;//not implemented :P
    //hieght of used font in pixels
    property fontHeight:single read fFontheight;
    //fontSize, use with autosize disabled
    property fontSize: single read GetFontSize write SetFontSize;
    property text:string read GetText write SetText;
    //calculates stuff, call after modyfing lines directly. changing .text property does it automatically
    procedure BuildText;
    procedure FillBuffer(var buf:TUIVertexBuffer; var count:integer);override;
    constructor Create(_parent:TUITransform;fontAtlas:TTextureAtlas);
    destructor Destroy;override;
  end;

  { TUICanvas }
  //handles opengl part of drawing it's children. canvas itself is invisible
  //(although it has size and position?)
  //if it's supposed to host movable window then hitBox must be 'fullscreen' hitbox[0,0,1,1]
  TUICanvas = class (TUITransform)
   protected
     //do we need all those transformations?
     //even mvp should be set only once if we don't need scaling and rotation
     mvp:tmatrix;
     //transformM,scaleM,rotationM,translateM:tmatrix;
     vao:GLuint;
     vbo:GLuint;
     hitTestingKids:TList;
   public
     //calculate transform. use only when position or sth changes
     procedure Build;
     //checks hit test on list-registered children, when canvas is .disabled it will skip the check entirely
     function checkKidsHitTest(mX, mY: Integer): TUITransform;
     procedure Render;override;
     procedure Update;virtual;//and kids
     //initialize stuff
     constructor Create(_parent:TUITransform);
     destructor Destroy;override;
  end;

implementation
const
  screenGlUnits  = 2; //screen width/size (not in pixels but gl space)
var
  vertexBuffer:TUIVertexBuffer;

{ TUIText }

function TUIText.GetText: string;
begin
  result:=lines.Text;
end;

function TUIText.GetFontSize: single;
begin
  result:=1/fFontScale*1000;
end;

procedure TUIText.SetFontSize(AValue: single);
begin
  fFontScale:=1/AValue * 1000;
end;

procedure TUIText.SetText(AValue: string);
begin
  lines.Text:=avalue;
  BuildText;
end;

procedure TUIText.BuildText;
var
  q,i,j:integer;
  xx,yoff:single;
begin
  //count verts neded
  vertCount:=0;
  fTextWidth:=0;
  for i:=0 to lines.count-1 do vertCount+= length(lines[i])*6;
  //count text width in 'atlas pixels'
  for j:=0 to lines.Count-1 do begin
    xx:=0;
    for i:=1 to length(lines[j]) do begin
      q:=atlas.getIndexOf(copy(lines[j],i,1));
      if q=-1 then xx+=5 //skip letters that are not in atlas
      else with atlas.data[q] do begin
        xx:=xx+w+1;
      end;
    end;
    if xx>fTextWidth then fTextWidth:=xx;
  end;
  fTextWidth-=1;
  dirty:=false;
end;

procedure TUIText.FillBuffer(var buf: TUIVertexBuffer; var count: integer);
var
  i,j,q,k:integer;
  xoff,yoff:single;
begin
  vertStart:=count;
  yoff:=0;
  for j:=0 to lines.Count-1 do begin
     xoff:=0;
     for i:=1 to length(lines[j]) do begin
        q:=atlas.getIndexOf(copy(lines[j],i,1));
        if q=-1 then xoff+=4 //skip letters that are not in atlas
        else with atlas.data[q] do begin
          with buf[0+count] do begin
            //xyz
            Pos[0]:=xoff;
            Pos[1]:=yoff+h;// screenGlUnits * absoluteSize[1];
            pos[2]:=0;
            //uv
            UV[0]:=(x) / atlaswidth;
            UV[1]:=(y)/ atlasheight;
            //color
            move(color,col,sizeof(col));
          end;
          with buf[1+count] do begin
            Pos[0]:=xoff;
            Pos[1]:=yoff;
            pos[2]:=0;
            UV[0]:=(x) / atlaswidth;
            UV[1]:=(y+h)/ atlasheight;
            move(color,col,sizeof(col));
          end;
          with buf[2+count] do begin
            Pos[0]:=xoff+w;
            Pos[1]:=yoff+h;
            pos[2]:=0;
            UV[0]:=(x+w) / atlaswidth;
            UV[1]:=(y)/ atlasheight;
            move(color,col,sizeof(col));
          end;
          with buf[3+count] do begin
            Pos[0]:=xoff;
            Pos[1]:=yoff;
            pos[2]:=0;
            UV[0]:=(x) / atlaswidth;
            UV[1]:=(y+h)/ atlasheight;
            move(color,col,sizeof(col));
          end;
          with buf[4+count] do begin
            Pos[0]:=xoff+w;
            Pos[1]:=yoff;
            pos[2]:=0;
            UV[0]:=(x+w) / atlaswidth;
            UV[1]:=(y+h)/ atlasheight;
            move(color,col,sizeof(col));
          end;
          with buf[5+count] do begin
            Pos[0]:=xoff+w;
            Pos[1]:=yoff+h;
            pos[2]:=0;
            UV[0]:=(x+w) / atlaswidth;
            UV[1]:=(y)/ atlasheight;
            move(color,col,sizeof(col));
          end;
          xoff:=xoff+w+1;
          for k:=0 to 5 do begin
            //scale down to screen size
            if autosize then scaleVector(buf[k+count].Pos,screenGlUnits / fTextWidth )
            else scaleVector(buf[k+count].Pos,screenGlUnits / fFontScale);
            //scale down to parent
            scaleVector(buf[k+count].Pos, absoluteSize );
            //move to screen position
            AddVector(buf[k+count].Pos,absolutePosition);

          end;
          inc(count,6);
        end;
     end;
     yoff-=atlas.data[q].h;
  end;
  {
  v:=absolutePosition;
  for i:=0 to 5 do begin
      AddVector(buf[i+count].Pos,v);
      //v1:=absolutePosition;
  end;
   }
  inc(count,vertCount);
  for i:=0 to kids.count-1 do TUITransform(kids[i]).FillBuffer(buf,count);
  dirty:=false;
end;

constructor TUIText.Create(_parent: TUITransform; fontAtlas: TTextureAtlas);
begin
  inherited Create(_parent);
  fontSize:=4;
  autoSize:=true;
  width:=1;
  height:=1;
  color[0]:=1;
  color[1]:=1;
  color[2]:=1;
  color[3]:=1;
  atlas:=fontAtlas;
  //all letters have same height so pick first one
  ffontheight:=atlas.data[atlas.getIndexOf('a')].h;
  lines:=TStringList.create;;
end;

destructor TUIText.Destroy;
begin
  lines.free;
  inherited Destroy;
end;

{ TUITransform }

procedure TUITransform.SetPosition(AValue: TAffineVector);
begin
  fPosition:=AValue;
  if parent<>Nil then begin
     //absolutePosition[0]:=parent.absolutePosition[0] + parent.absoluteSize[0] * abs(avalue[0]);
     //absolutePosition[1]:=parent.absolutePosition[1] + parent.absoluteSize[1] * abs(avalue[1]);
     //absolutePosition:=parent.absolutePosition;
     absolutePosition[0]:=parent.absolutePosition[0] + avalue[0]*2 * parent.absoluteSize[0];
     absolutePosition[1]:=parent.absolutePosition[1] + avalue[1]*2 * parent.absoluteSize[1];
  end
  else absolutePosition:=avalue;
  dirty:=true;
end;

procedure TUITransform.SetPosProp(AIndex: Integer; AValue: single);
begin
  fPosition[aindex]:=avalue;
  if parent<>Nil then
     //absolutePosition[aindex]:=parent.absolutePosition[aindex] + parent.absoluteSize[aindex] * abs(AValue)
     absolutePosition[aindex]:=parent.absolutePosition[aindex] + avalue*2 * parent.absoluteSize[aindex]
  else absolutePosition[aindex]:=avalue;
  dirty:=true;
end;

procedure TUITransform.SetSize(AValue: TAffineVector);
begin
  fSize:=AValue;
  if parent<>Nil then begin
     absoluteSize[0]:=avalue[0] * parent.absoluteSize[0];
     absoluteSize[1]:=avalue[1] * parent.absoluteSize[1];
  end
  else absoluteSize:=avalue;
  dirty:=true;
end;

procedure TUITransform.SetSizeProp(AIndex: Integer; AValue: single);
begin
  fSize[aindex]:=avalue;
  if parent<>Nil then
     absoluteSize[aindex]:=avalue * parent.absoluteSize[aindex]
  else absoluteSize[aindex]:=avalue;
  dirty:=true;
end;

function TUITransform.GetPosProp(AIndex: Integer): single;
begin
  result:=fPosition[AIndex];
end;

function TUITransform.GetSizeProp(AIndex: Integer): single;
begin
  result:=fSize[AIndex];
end;

procedure TUITransform.AddKid(kid: TUITransform);
begin
  kids.add(kid);
end;

procedure TUITransform.FillBuffer(var buf: TUIVertexBuffer; var count: integer);
begin
  //override me
  log('TUITransform.FillBuffer:: override me');
end;

procedure TUITransform.Render;
var
  i:integer;
begin
  if visible then begin
     glDrawArrays(GL_TRIANGLEs, vertStart,vertCount);
     for i:=0 to kids.count-1 do TUITransform(kids[i]).Render;
  end;
end;

function TUITransform.GetVertCount: integer;
var
  i:integer;
begin
  result:=vertCount;
  for i:=0 to kids.count-1 do inc(result,TUITransform(kids[i]).GetVertCount);
end;

procedure TUITransform.SetColor(r, g, b, a: single);
begin
  color[0]:=r;color[1]:=g;color[2]:=g;color[3]:=a;
end;

procedure TUITransform.registerForHitTest(toList: TList);
var
  i:integer;
begin
  if hitTest then toList.add(self);
  for i:=0 to kids.count-1 do TUITransform(kids[i]).registerForHitTest(toList);
end;

constructor TUITransform.Create(_parent: TUITransform);
begin
  parent:=_parent;
  kids:=tlist.create;
  dirty:=true;
  hittest:=false;
  visible:=true;
  enabled:=true;
  fsize[2]:=0;//z is not used but init it anyways
  absolutePosition[2]:=0;
  fPosition[2]:=0;
  if parent<>nil then parent.AddKid(self);
  //init pos from parrent that way
  x:=0;y:=0;
end;

destructor TUITransform.Destroy;
var
  i:integer;
begin
  for i:=0 to kids.count-1 do begin
    TUITransform(kids[i]).Free;
  end;
  kids.free;
  inherited Destroy;
end;

{ TUICanvas }
procedure TUICanvas.Build;
var
  i,used:integer;
begin
  //ask children how much verts they need
  vertCount:=0;
  hitTestingKids.clear;
  for i:=0 to kids.Count-1 do begin
     vertCount:= vertCount + TUITransform(kids[i]).GetVertCount;
     TUITransform(kids[i]).registerForHitTest(hitTestingKids);
  end;
  if length(vertexBuffer)<vertCount then setlength(vertexBuffer,vertCount*2);
  //supply our vertexBuf to children so the can fill it in
  used:=0;
  for i:=0 to kids.count-1 do TUITransform(kids[i]).FillBuffer(vertexBuffer,used);
  //upload data
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glBufferData(GL_ARRAY_BUFFER,used*sizeof(TUIVertex),@vertexBuffer[0],GL_DYNAMIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER,0);
  dirty:=false;
end;

function TUICanvas.checkKidsHitTest(mX, mY: Integer
  ): TUITransform;
var
  i:integer;
  sx,xx,yy,sy:single;
  e:TUITransform;
begin
  result:=nil;
  if not enabled then exit;
  //calculate gl screen size to pixels ratio
  sx:= (((screenGlUnits / viewport[2]) * mx));// -1);// * 0.5;
  sy:= 2-(((screenGlUnits / viewport[3]) * my));// -1);// * 0.5;

  for i:=0 to hitTestingKids.count-1 do begin
    e:=TUITransform(hitTestingKids[i]);
    if not e.enabled then continue;
    with e do begin
      xx:=absolutePosition[0]+1;
      yy:=absolutePosition[1]+1;
      if (xx<=sx) and (xx+absoluteSize[0]*2>=sx) and
         (yy<=sy) and (yy+absoluteSize[1]*2>=sy)
      //and (absolutePosition[1]>=sy) and (absolutePosition[1]+absoluteSize[1]>=sy)
      then
         result:=TUITransform(hitTestingKids[i]);//maybe we could break here but some kid can be on top
    end;
  end;

end;

procedure TUICanvas.Render;
var
  i:integer;
begin
  //this call probably should be used only once as there is no mvp modification anywhere
  glUniformMatrix4fv(matlib.shaderUI.mvp, 1, bytebool(GL_FALSE), @mvp);
  //notice how we dont update position
  //can ignore in 2d?
  //glUniformMatrix4fv(matlib.shaderUI.m_WorldMatrixLocation, 1, bytebool(GL_FALSE), @transformM);
  glBindVertexArray(vao);
//  glDrawArrays(GL_TRIANGLEs, 0,vertCount);
  for i:=0 to kids.count-1 do TUITransform(kids[i]).Render;
  glBindVertexArray(0);
end;

procedure TUICanvas.Update;
var
  i:integer;
begin
  //for i:=0 to kids.count-1 do TUITransform(kids[i]).Update;
end;

constructor TUICanvas.Create(_parent:TUITransform);
begin
  inherited Create(_parent);
  mvp:=IdentityHmgMatrix;
//  setlength(vertexBuffer,6);
  //set it to full screen by default
  x:=0;
  y:=0;
  width:=1;
  height:=1;
  absoluteSize[0]:=1;
  absoluteSize[1]:=1;
  hitTestingKids:=TLIst.create;
  glGenBuffers(1,@vbo);
  glGenVertexArrays(1, @vao);
  //setup vao
  glBindVertexArray(vao);
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  glEnableVertexAttribArray(2);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glVertexAttribPointer(0, 3, GL_FLOAT, bytebool(GL_FALSE),sizeof(TUIVertex), nil);
//  glVertexAttribPointer(1, 2, GL_FLOAT, bytebool(GL_FALSE),sizeof(TUIVertex), @vertexBuffer[0].UV);
  glVertexAttribPointer(1, 2, GL_FLOAT, bytebool(GL_FALSE), sizeof(TUIVertex), pointer(3*sizeof(single)));
  glVertexAttribPointer(2, 4, GL_FLOAT, bytebool(GL_FALSE), sizeof(TUIVertex), pointer(5*sizeof(single)));

  glBindVertexArray(0);
  glDisableVertexAttribArray(0);
  glDisableVertexAttribArray(1);
  glDisableVertexAttribArray(2);
  glBindBuffer(GL_ARRAY_BUFFER,0);

end;

destructor TUICanvas.Destroy;
begin
  hitTestingKids.free;
  glDeleteBuffers(1,@vbo);
  glDeleteVertexArrays(1,@vao);
  inherited destroy;
end;

{ TUIRect }

procedure TUIRect.FillBuffer(var buf: TUIVertexBuffer; var count:integer);
var
  i:integer;
  v,v1:taffinevector;
begin
  if sprite<>nil then begin
    vertStart:=count;
    //dane z kolorem sa zjebane wiec wez to sprawdz to
    with sprite^ do begin
      with buf[0+count] do begin
       //xyz
       Pos[0]:=0;
       Pos[1]:= screenGlUnits * absoluteSize[1];
       pos[2]:=0;
       //uv
       UV[0]:=(x) / atlaswidth;
       UV[1]:=(y)/ atlasheight;
       //color
       move(color,col,sizeof(col));
      end;
      with buf[1+count] do begin
       Pos[0]:=0;
       Pos[1]:=0;
       pos[2]:=0;
       UV[0]:=x / atlaswidth;
       UV[1]:=(y+h)/ atlasheight;
       move(color,col,sizeof(col));
      end;
      with buf[2+count] do begin
       Pos[0]:=screenGlUnits* absoluteSize[0];
       Pos[1]:=screenGlUnits* absoluteSize[1];
       Pos[2]:=0;
       UV[0]:=(x+w) / atlaswidth;
       UV[1]:=(y)/ atlasheight;
       move(color,col,sizeof(col));
      end;
      with buf[3+count] do begin
       Pos[0]:=0;
       Pos[1]:=0;
       Pos[2]:=0;
       UV[0]:=x / atlaswidth;
       UV[1]:=(y+h)/ atlasheight;
       move(color,col,sizeof(col));
      end;
      with buf[4+count] do begin
       Pos[0]:= screenGlUnits* absoluteSize[0];
       Pos[1]:=0;
       Pos[2]:=0;
       UV[0]:=(x+w) / atlaswidth;
       UV[1]:=(y+h)/ atlasheight;
       move(color,col,sizeof(col));
      end;
      with buf[5+count] do begin
       Pos[0]:= screenGlUnits* absoluteSize[0];
       Pos[1]:= screenGlUnits* absoluteSize[1];
       Pos[2]:=0;
       UV[0]:=(x+w) / atlaswidth;
       UV[1]:=(y)/ atlasheight;
       move(color,col,sizeof(col));
      end;
     end;
     //at this point we have a huge sprite originating in centre of screen and growing tleft and top, each pixel being 1 gl unit
     //need to scale it to given size
     //v:=VectorScale(position,2);
     //v:=vectorScale( absolutePosition,0.5);
  //   v1:=position;
  //   v:=vectorScale(absolutePosition,1);
     v:=absolutePosition;
     for i:=0 to 5 do begin
         AddVector(buf[i+count].Pos,v);
         //v1:=absolutePosition;
     end;

     inc(count,vertCount);
   end;
   for i:=0 to kids.count-1 do TUITransform(kids[i]).FillBuffer(buf,count);
end;

constructor TUIRect.Create(_parent:TUITransform);
begin
  inherited Create(_parent);
  sprite:=nil;
  width:=1;
  height:=1;
  //(3 pos, 2 uv,4 color) *6 but we now use TUIVertex
  vertCount:=6;
  color[0]:=1;
  color[1]:=1;
  color[2]:=1;
  color[3]:=1;
end;

destructor TUIRect.Destroy;
var
  i:integer;
begin
  inherited Destroy;
end;

initialization
setlength(vertexBuffer,16384);
end.

