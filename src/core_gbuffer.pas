unit core_gBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,dglOpenGL,core_types;

  const GBUFFER_DIFFUSE_TEXTURE_UNIT=0;
        GBUFFER_NORMAL_TEXTURE_UNIT=1;
        GBUFFER_POSITION_TEXTURE_UNIT = 2;
        GBUFFER_DEPTH_TEXTURE_UNIT=3;

type

  	GBUF_TEX_TYPE  = (
		GBUF_TEX_TYPE_COL,
		GBUF_TEX_TYPE_NORM,
                GBUF_TEX_TYPE_POS,
		GBUFFER_NUM_TEXTURES);

  { TGBuffer }

  TGBuffer = class
      m_fbo,m_depthTexture,m_finalTexture,m_ssaoTexture:GLuint;
      initialized:boolean;
      DrawBuffers:array [0..2] of glenum;
    public
      //POSITION,DIFFUSE,NORMAL
      m_textures: array[GBUF_TEX_TYPE_COL..GBUF_TEX_TYPE_POS] of GLuint;
      procedure StartFrame;
      procedure BindForGeomPass;
      procedure BindForStencilPass;
      procedure BindForLightPass;
      procedure BindForFinalPass;
      procedure BindForSsaoPass;
      function init(WindowWidth, WindowHeight: integer): boolean;
      destructor destroy;
  end;

var
  GBuffer:TGBuffer;

implementation

{ TGBuffer }

procedure TGBuffer.StartFrame;
begin
     glBindFramebuffer(GL_DRAW_FRAMEBUFFER, m_fbo);
     glDrawBuffer(GL_COLOR_ATTACHMENT5);
     glClear(GL_COLOR_BUFFER_BIT );
end;

procedure TGBuffer.BindForGeomPass;
begin
  glDrawBuffers(3, DrawBuffers);
end;

procedure TGBuffer.BindForSsaoPass;
const
  DrawBuffer:glenum = (GL_COLOR_ATTACHMENT4);
begin
  glDrawBuffers(1,@DrawBuffer);

  glActiveTexture(GL_TEXTURE0+0);    //byc moze to 0 a moze 3
  glBindTexture(GL_TEXTURE_2D, m_depthTexture);

  glActiveTexture(GL_TEXTURE0+1);
  glBindTexture(GL_TEXTURE_2D, m_textures[GBUF_TEX_TYPE_COL]);

end;

procedure TGBuffer.BindForStencilPass;
begin
  // must disable the draw buffers
  glDrawBuffer(GL_NONE);
end;

procedure TGBuffer.BindForLightPass;
var
  i:cardinal;
begin
 glDrawBuffer(GL_COLOR_ATTACHMENT5);

 glActiveTexture(GL_TEXTURE0 + GBUFFER_POSITION_TEXTURE_UNIT);
 glBindTexture(GL_TEXTURE_2D, m_textures[GBUF_TEX_TYPE_POS]);
 glActiveTexture(GL_TEXTURE0 + GBUFFER_DIFFUSE_TEXTURE_UNIT);
 //glBindSampler
 //sao is already mixed with color
 if options.renderer.ssao then glBindTexture(GL_TEXTURE_2D, m_ssaoTexture) else
    glBindTexture(GL_TEXTURE_2D, m_textures[GBUF_TEX_TYPE_COL]);
 glActiveTexture(GL_TEXTURE0 + GBUFFER_NORMAL_TEXTURE_UNIT);
 glBindTexture(GL_TEXTURE_2D, m_textures[GBUF_TEX_TYPE_NORM]);
  //ssao tex
  //glActiveTexture(GL_TEXTURE3);
  //glBindTexture(GL_TEXTURE_2D, m_ssaoTexture);
end;

procedure TGBuffer.BindForFinalPass;
begin
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
  glBindFramebuffer(GL_READ_FRAMEBUFFER, m_fbo);
  glReadBuffer(GL_COLOR_ATTACHMENT5);
  ////////////////debuuug ssaooo///////////////////////////////////////////////
  //glReadBuffer(GL_COLOR_ATTACHMENT4);
end;

function TGBuffer.init(WindowWidth, WindowHeight: integer): boolean;
var
    i:GBUF_TEX_TYPE;
    status:glenum;
    s:string;
    e:integer;
    c:cardinal;
begin
//  initialized:=true;
  //free textures?
  // delete existing fbo, textures, and render buffer in case we are regenerating at new size
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
  glDeleteTextures(1, @m_finalTexture);
  glDeleteTextures(1, @m_ssaoTexture);
  glDeleteTextures(1, @m_depthTexture);
  glDeleteTextures(3, m_textures);
  glDeleteFramebuffers(1, @m_fbo);
  // Create the FBO
  glGenFramebuffers(1, @m_fbo);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, m_fbo);
  // Create the gbuffer textures
  glGenTextures(length(m_textures), m_textures);
  glGenTextures(1, @m_depthTexture);
  glGenTextures(1, @m_ssaoTexture);
  glGenTextures(1, @m_finalTexture);

  //position,color, normal tex
  for i:=GBUF_TEX_TYPE_col to GBUF_TEX_TYPE_POS do begin
    glBindTexture(GL_TEXTURE_2D, m_textures[i]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB32F, WindowWidth, WindowHeight, 0, GL_RGB, GL_FLOAT, nil);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    c:=GL_COLOR_ATTACHMENT0 +cardinal(i);
    glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, c , GL_TEXTURE_2D, m_textures[i], 0);
  end;
  // depth
  glBindTexture(GL_TEXTURE_2D, m_depthTexture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_BASE_LEVEL, 0);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_NONE);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  //glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH32F_STENCIL8, WindowWidth, WindowHeight, 0, GL_DEPTH_COMPONENT, GL_FLOAT, nil);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH32F_STENCIL8, WindowWidth, WindowHeight, 0, GL_DEPTH_STENCIL, GL_FLOAT_32_UNSIGNED_INT_24_8_REV, nil);
  //s:=gluErrorString(glGetError());
  glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D, m_depthTexture, 0);
  //s:=gluErrorString(glGetError());
  //ambient occlusion result
  glBindTexture(GL_TEXTURE_2D, m_ssaoTexture);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB32F, WindowWidth, WindowHeight, 0, GL_RGB, GL_FLOAT, nil);
  glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT4 , GL_TEXTURE_2D, m_ssaoTexture, 0);
  //s:=gluErrorString(glGetError());
  //final
  glBindTexture(GL_TEXTURE_2D, m_finalTexture);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, WindowWidth, WindowHeight, 0, GL_RGB, GL_FLOAT, nil);
  glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT5, GL_TEXTURE_2D, m_finalTexture, 0);
  //s:=gluErrorString(glGetError());
  Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);

  if (Status <> GL_FRAMEBUFFER_COMPLETE) then begin
      if status = GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT then log('FBO Incomplete attachment') else
      if status = GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT then log('FBO INCOMPLETE_MISSING_ATTACHMENT') else
      if status = GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER then log('FBO INCOMPLETE_DRAW_BUFFER')else
      if status = GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER then log('FBO INCOMPLETE_READ_BUFFER') else
      if status = GL_FRAMEBUFFER_UNSUPPORTED then log('FBO UNSUPPORTED') else
         log('some GBuffer error, I''m blind!');
      result:=false;
      //exit;
  end;
  DrawBuffers[0]:=GL_COLOR_ATTACHMENT2;
  DrawBuffers[1]:=GL_COLOR_ATTACHMENT0;
  DrawBuffers[2]:=GL_COLOR_ATTACHMENT1;

  // restore default FBO
  //glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
  result:=true;
  //glStencilMask($FFFFFFFF);
end;

destructor TGBuffer.destroy;
begin
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
  glDeleteTextures(1,@m_depthTexture);
  glDeleteTextures(length(m_textures), m_textures);
  glDeleteTextures(1, @m_finalTexture);
  glDeleteTextures(1, @m_ssaoTexture);
  glDeleteFramebuffers(1, @m_fbo);
end;


end.

