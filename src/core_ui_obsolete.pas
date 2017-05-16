unit core_ui_obsolete;
//this unit is first iteration of opengl ui and is such crap! do not try to extend it, use core_ui_controls instead

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,dglOpenGL,
  core_texture, core_material_library, fgl, core_listofrecords,
  VectorGeometry;

const
  //some colors
   frameColor =$005AB429;
   fillColor = $00A7E492;
   labelColor = $0012FC41;
   fontColorGreen = $0012FC41;
   fontColorRed = $001212FC;
   //backgroundColorGrey = $00444646;
   backgroundColorGrey = $000C0C0C;

type

  { TUIElement }

     TUIElement = class
      protected
        atlas:TTextureAtlas;
        fPosition:TAffineVector;
        mvp,transformM,scaleM,rotationM,translateM:tmatrix;
        vao:GLuint;
        vbo:GLuint;
        fVertCount:integer;
      public
        visible:boolean;
        //calculate transform. use only when position or sth changes
        procedure update;
        procedure render;virtual;
        //initialize stuff
        constructor create(leAtlas: TTextureAtlas);
        procedure setScale(x,y,z:single);
        procedure setPosition(x, y, z: single);
        destructor destroy;override;
     end;
     pUIElement = ^TUIElement;

     TUIElemetsList = specialize TFPGList <TUIElement>;

     { TUISprite }
     //2d sprite for ui elements
     TUISprite = class (TUIElement)
       indexInAtlas:integer;
       constructor create(leAtlas: TTextureAtlas; InAtlasindex: integer; r: single=1;
         g: single=1; b: single=1; a: single=1);
     end;

     //gizmo for displaying text
     TListofColours = specialize TFPGList <rColour>;

    { TUIStrings }
    TUIStrings = class (TUIElement)
     protected
      lines:TStringList;
      colours:TListofColours;
      fontheight:single;
      dirtyTxt:boolean;
     public
      //how many lines of text to render, starting from the end
      visibleLines:integer;
      //add line of text
      procedure render;override;
      procedure add(const s:string;r:single=1;g:single=1;b:single=1;a:single=1);
      procedure buildText;
      constructor create(leAtlas:TTextureAtlas);
      destructor destroy;override;
    end;
    TGLStringsList = specialize TFPGList<TUIStrings>;

implementation
var
   vbobuf:array of GLfloat;

{ TUISprite }

constructor TUISprite.create(leAtlas: TTextureAtlas; InAtlasindex: integer;r:single=1;g:single=1;b:single=1;a:single=1);
const quadSize = 54;
var
  colour:rColour;
begin
  inherited create(leAtlas);
  indexInAtlas:=InAtlasindex;
  fvertCount:=6;
  if length(vbobuf)<quadSize then setlength(vbobuf,quadSize);
  //create quad from atlas params
  with atlas.data[indexInAtlas] do begin
    //xyz
    vbobuf[0]:=0;
    vbobuf[1]:=h;
    vbobuf[2]:=0;
    vbobuf[3]:=(x) / atlas.width;
    vbobuf[4]:=(y)/ atlas.height;
    //color
    vbobuf[5]:=r;
    vbobuf[6]:=g;
    vbobuf[7]:=b;
    vbobuf[8]:=a;

    vbobuf[9]:=0;
    vbobuf[10]:=0;
    vbobuf[11]:=0;
    vbobuf[12]:=x / atlas.width;
    vbobuf[13]:=(y+h)/ atlas.height;
    vbobuf[14]:=r;
    vbobuf[15]:=g;
    vbobuf[16]:=b;
    vbobuf[17]:=a;

    vbobuf[18]:=w;
    vbobuf[19]:=h;
    vbobuf[20]:=0;
    vbobuf[21]:=(x+w) / atlas.width;
    vbobuf[22]:=(y)/ atlas.height;
    vbobuf[23]:=r;
    vbobuf[24]:=g;
    vbobuf[25]:=b;
    vbobuf[26]:=a;

    vbobuf[27]:=0;
    vbobuf[28]:=0;
    vbobuf[29]:=0;
    vbobuf[30]:=x / atlas.width;
    vbobuf[31]:=(y+h)/ atlas.height;
    vbobuf[32]:=r;
    vbobuf[33]:=g;
    vbobuf[34]:=b;
    vbobuf[35]:=a;

    vbobuf[36]:=w;
    vbobuf[37]:=0;
    vbobuf[38]:=0;
    vbobuf[39]:=(x+w) / atlas.width;
    vbobuf[40]:=(y+h)/ atlas.height;
    vbobuf[41]:=r;
    vbobuf[42]:=g;
    vbobuf[43]:=b;
    vbobuf[44]:=a;

    vbobuf[45]:=w;
    vbobuf[46]:=h;
    vbobuf[47]:=0;
    vbobuf[48]:=(x+w) / atlas.width;
    vbobuf[49]:=(y)/ atlas.height;
    vbobuf[50]:=r;
    vbobuf[51]:=g;
    vbobuf[52]:=b;
    vbobuf[53]:=a;
  end;
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glBufferData(GL_ARRAY_BUFFER,quadSize*sizeof(glfloat),@vbobuf[0],GL_DYNAMIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER,0);
end;

{ TUIElement }

procedure TUIElement.update;
begin
  MatrixMultiply(scaleM,rotationM,transformM);
  MatrixMultiply(transformM,translateM,transformM);
  mvp:=transformM;
  //MatrixMultiply(transformM,worldCameraMatrix,mvp);
end;

procedure TUIElement.render;
begin
  glUniformMatrix4fv(matlib.shaderUI.mvp, 1, bytebool(GL_FALSE), @mvp);
  //can ignore in 2d?
  //glUniformMatrix4fv(matlib.shaderUI.m_WorldMatrixLocation, 1, bytebool(GL_FALSE), @transformM);
  glBindVertexArray(vao);
  glDrawArrays(GL_TRIANGLEs, 0,fvertCount);
  glBindVertexArray(0);
end;

constructor TUIElement.create(leAtlas: TTextureAtlas);
begin
  scaleM:=IdentityHmgMatrix;
  rotationM:=IdentityHmgMatrix;
  translateM:=IdentityHmgMatrix;
  transformM:=IdentityHmgMatrix;
  visible:=true;
  atlas:=leAtlas;
  glGenBuffers(1,@vbo);
  glGenVertexArrays(1, @vao);
  //setup vao
  glBindVertexArray(vao);
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  glEnableVertexAttribArray(2);
  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glVertexAttribPointer(0, 3, GL_FLOAT, bytebool(GL_FALSE),9*sizeof(single), nil);
  glVertexAttribPointer(1, 2, GL_FLOAT, bytebool(GL_FALSE), 9*sizeof(single), pointer(3*sizeof(single)));
  glVertexAttribPointer(2, 4, GL_FLOAT, bytebool(GL_FALSE), 9*sizeof(single), pointer(5*sizeof(single)));
  glBindVertexArray(0);
  glDisableVertexAttribArray(0);
  glDisableVertexAttribArray(1);
  glDisableVertexAttribArray(2);
  glBindBuffer(GL_ARRAY_BUFFER,0);
end;

procedure TUIElement.setScale(x, y, z: single);
begin
  scaleM[0,0]:=x;
  scaleM[1,1]:=y;
  scaleM[2,2]:=z;
end;

procedure TUIElement.setPosition(x, y, z: single);
begin
  translateM[3,0]:=x;
  translateM[3,1]:=y;
  translateM[3,2]:=z;
  setVector(fposition,x,y,z);
end;

destructor TUIElement.destroy;
var
  i:integer;
begin
  glDeleteBuffers(1,@vbo);
  glDeleteVertexArrays(1,@vao);
  inherited destroy;
end;

{ TUIStrings }

procedure TUIStrings.render;
begin
  if dirtyTxt then buildText;
  inherited render;
end;

procedure TUIStrings.add(const s: string;r:single=1;g:single=1;b:single=1;a:single=1);
var color:rColour;
begin
  color.r:=r;color.a:=a;color.b:=b;color.g:=g;
  //add to stringlist
  lines.Add(s);
  colours.add(color);
  if lines.Count<=visibleLines then begin
     setPosition(fposition[0],fposition[1]+fontheight*scaleM[1][1],fposition[2]);
     update;
  end;
  dirtyTxt:=true;
end;

procedure TUIStrings.buildText;
const quadSize = 54;
var
  i,j,q,verts,startLine:integer;
  c:cardinal;
  xoff,yoff:single;
  colour:rColour;
begin
  dirtyTxt:=false;
  //first count how many verts will be needed
  verts:=0;
  if lines.count>visibleLines then startLine:=lines.count-visibleLines else
    startLine:=0;
  for i:=startLine to lines.Count-1 do begin
     verts+=length(lines[i]);
  end;

  fvertCount:=verts*6;
  //6 xyz verts + 2 uv's per letter quad
  if length(vbobuf)<quadSize*verts then setlength(vbobuf,verts*quadSize*2);
  yoff:=0;
  c:=0;
  for j:=startLine to lines.Count-1 do begin
     xoff:=0;
     for i:=1 to length(lines[j]) do begin
        colour:=colours[j];
        q:=atlas.getIndexOf(copy(lines[j],i,1));
        if q=-1 then xoff+=5 //skip letters that are not in atlas
        else with atlas.data[q] do begin
          vbobuf[0+c]:=xoff;
          vbobuf[1+c]:=yoff+h;
          vbobuf[2+c]:=0;
          vbobuf[3+c]:=(x) / atlas.width;
          vbobuf[4+c]:=(y)/ atlas.height;
          move(colour,vbobuf[5+c],sizeof(rColour));

          vbobuf[9+c]:=xoff;
          vbobuf[10+c]:=yoff;
          vbobuf[11+c]:=0;
          vbobuf[12+c]:=x / atlas.width;
          vbobuf[13+c]:=(y+h)/ atlas.height;
          move(colour,vbobuf[14+c],sizeof(rColour));

          vbobuf[18+c]:=xoff+w;
          vbobuf[19+c]:=yoff+h;
          vbobuf[20+c]:=0;
          vbobuf[21+c]:=(x+w) / atlas.width;
          vbobuf[22+c]:=(y)/ atlas.height;
          move(colour,vbobuf[23+c],sizeof(rColour));

          vbobuf[27+c]:=xoff;
          vbobuf[28+c]:=yoff;
          vbobuf[29+c]:=0;
          vbobuf[30+c]:=x / atlas.width;
          vbobuf[31+c]:=(y+h)/ atlas.height;
          move(colour,vbobuf[32+c],sizeof(rColour));

          vbobuf[36+c]:=xoff+w;
          vbobuf[37+c]:=yoff;
          vbobuf[38+c]:=0;
          vbobuf[39+c]:=(x+w) / atlas.width;
          vbobuf[40+c]:=(y+h)/ atlas.height;
          move(colour,vbobuf[41+c],sizeof(rColour));

          vbobuf[45+c]:=xoff+w;
          vbobuf[46+c]:=yoff+h;
          vbobuf[47+c]:=0;
          vbobuf[48+c]:=(x+w) / atlas.width;
          vbobuf[49+c]:=(y)/ atlas.height;
          move(colour,vbobuf[50+c],sizeof(rColour));
          xoff:=xoff+w+1;
          inc(c,quadSize);
        end;
     end;
     yoff-=atlas.data[q].h;
     fontheight:=atlas.data[q].h;
  end;

  glBindBuffer(GL_ARRAY_BUFFER,vbo);
  glBufferData(GL_ARRAY_BUFFER,c*sizeof(single),@vbobuf[0],GL_DYNAMIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER,0);
end;

constructor TUIStrings.create(leAtlas: TTextureAtlas);
begin
  inherited;
  lines:=tstringlist.create;
  colours:=TListofColours.create;
  visibleLines:=10;
end;

destructor TUIStrings.destroy;
begin
  lines.free;
  colours.free;
  inherited destroy;
end;

end.

