unit core_texture;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,dglOpenGL,Graphics,core_types,core_utils,VectorTypes,
  VectorGeometry;

type

  rSprite = record
     name:string;
     x,y,w,h:single;
     atlasWidth, atlasHeight:single;
  end;
  pSprite = ^rSprite;

  { TTexture }

  TTexture = class
    private
     textureTarget:GLenum;
     textureObj:GLuint;
    public
     img:tpicture;
     filename:string;
    //load tex either from given path or from one used in create. wtf
    function Load(ffile: string='';blur: boolean=true): boolean;
    procedure Bind(textureUnit:glenum);
    constructor create(target:glenum);
    destructor destroy;override;
  end;

  { TTextureAtlas }

  TTextureAtlas = class
    texture:TTexture;
    //loaded texture path
    name:string[32];
    //atlas texture size
    width,height:single;
    //'subtextures'
    data: array of rSprite;
    procedure load(const filename: string;blur: boolean=true);
    procedure Bind(textureUnit:glenum);
    function getIndexOf(texName:string):integer;
    function getNameOf(index:integer):string;
    procedure getRect(index:integer; var rect:tvector4f);
    constructor create(target:glenum);
    destructor destroy;override;
  end;

implementation

{ TTextureAtlas }

procedure TTextureAtlas.load(const filename: string; blur: boolean);
var
  str:tstringlist;
  i:integer;
  s:string;
begin
  str:=tstringlist.Create;
  name:=copy(filename,1,LastDelimiter('.',filename)-1);
  name:=copy(name,LastDelimiter('\',name)+1,length(name));
  try
    s:=copy(filename,1,LastDelimiter('.',filename))+'png';
    texture.Load(s,blur);
    str.LoadFromFile(filename);
    //one sub texture per line so total count is = count
    setlength(data,str.Count);
    for i:=0 to str.Count-1 do begin
      s:=str[i];
      data[i].name:=eatstring(s,' ');
      data[i].x:=strtofloat(eatstring(s,' '));
      data[i].y:=strtofloat(eatstring(s,' '));
      data[i].w:=strtofloat(eatstring(s,' '));
      data[i].h:=strtofloat(eatstring(s,' '));
      data[i].atlasHeight:=texture.img.height;
      data[i].atlasWidth:=texture.img.width;
    end;
    width:=texture.img.Width;
    height:=texture.img.Height;
  finally
    str.free;
  end;
end;

//returns value from 'bla bla=value'
function getValue(s:string):integer;
begin
  result:=strtoint(copy(s,LastDelimiter('=',s)+1,length(s)));
end;

function eatValue(var s:string):integer;
var i:integer;
begin
  result:=strtoint(copy(s,LastDelimiter('=',s)+1,length(s)));
  i:=LastDelimiter('=',s);
  while (i>-1) and (s[i]<>' ') do i-=1;
  while (i>-1) and (s[i]=' ') do i-=1;
  s:=copy(s,1,i);
end;


procedure TTextureAtlas.Bind(textureUnit: glenum);
begin
  texture.Bind(textureUnit);
end;

function TTextureAtlas.getIndexOf(texName: string): integer;
var
  i:integer;
begin
  result:=-1;
  for i:=0 to length(data)-1 do if data[i].name=texName then begin
    result:=i;
    break;
  end;
end;

function TTextureAtlas.getNameOf(index: integer): string;
begin
  result:=data[index].name;
end;

procedure TTextureAtlas.getRect(index: integer; var rect: tvector4f);
begin
  rect[0]:=data[index].x;
  rect[1]:=data[index].y;
  rect[2]:=data[index].w;
  rect[3]:=data[index].h;
end;

constructor TTextureAtlas.create(target:glenum);
begin
  texture:=ttexture.create(target);
end;

destructor TTextureAtlas.destroy;
begin
  texture.destroy;
  inherited;
end;

{ TTexture }

function TTexture.Load(ffile:string = '';blur:boolean=true): boolean;
var
  s:string;
begin
  result:=true;
  if ffile='' then ffile:=filename;
  try
    img.LoadFromFile(ffile);
    filename:=ffile;
    glGenTextures(1, @textureObj);
    glBindTexture(textureTarget, textureObj);
    glTexImage2D(textureTarget, 0, GL_RGBA, img.Width, img.Height,0, GL_BGRA, GL_UNSIGNED_BYTE, img.PNG.RawImage.Data);
    if blur then begin
      glTexParameterf(textureTarget, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameterf(textureTarget, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    end else begin
      glTexParameterf(textureTarget, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexParameterf(textureTarget, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    end;
//    s:=gluErrorString(glGetError());
  except
    log('exception while loading texture: '+ffile);
    result:=false;
  end;
end;

procedure TTexture.Bind(textureUnit: glenum);
begin
  glActiveTexture(TextureUnit);
  glBindTexture(textureTarget, textureObj);
end;

constructor TTexture.create(target:glenum);
begin
  img:=tpicture.create;
  textureTarget:=target;
end;

destructor TTexture.destroy;
begin
  glDeleteTextures(1, @textureObj);
  img.Free;
  inherited;
end;

end.

