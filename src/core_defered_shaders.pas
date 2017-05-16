unit core_defered_shaders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,core_shader_program,dglOpenGL,VectorGeometry,VectorTypes,core_lights,
  core_gBuffer,core_types,core_camera;

type

  rAtten = record
    Constant:GLint;
    Linear:GLint;
    Exp:GLint;
  end;

  { TShaderChunkDS } //ds is for deferred mode shaders

  TShaderChunkDS = class (TProgram)
    unifMVP,
    m_WorldMatrixLocation:glint;
    //ghostUnif:glint;
    procedure use;
    procedure init(prg:gluint);override;
  end;

  TLightDS = class (TProgram)
    unifMvp:glint;//model matrix
    m_posTextureUnitLocation:glint;
    m_normalTextureUnitLocation:glint;
    m_colorTextureUnitLocation:glint;
    m_eyeWorldPosLocation:glint;
    m_matSpecularIntensityLocation:glint;
    m_matSpecularPowerLocation:glint;
    m_screenSizeLocation:glint;
    Color:glint;
    AmbientIntensity:glint;
    DiffuseIntensity:glint;
    Direction:glint;
    Position:GLint;
    Atten:rAtten;
  end;

  { TShaderPointLightDS }

  TShaderPointLightDS = class(TLightDS)
    //point light
    procedure use;
    procedure init(prg:gluint);
    procedure setLight(light:PPointLight);
    constructor create;
  end;

  { TShaderDirLightQuadDS }   //full screen quad for directional light

  TShaderDirLightQuadDS = class (TLightDS)
    mat:TMatrix;
    procedure use;
    procedure init(prg:gluint);
    procedure setLight(light:pDirLight);
    constructor create;
  end;

  { TNullDS }

  TNullDS = class (tprogram)
    gWVP:glint;
    procedure use;
    procedure init(prg:gluint);
  end;

  { TShaderSSAO }

  TShaderSSAO = class (TProgram)
    bgl_DepthTexture,
    bgl_RenderedTexture,
    gScreenSize,
    samplesCount,zfar:glint;
    mat:tmatrix;
    procedure use;
    procedure init(prg:gluint);
  end;

  //for flat people

  { TShaderFlatDS }

  TShaderFlatDS = class (TProgram)
    mvp, textureUnif,texSize,texRect,
    m_WorldMatrixLocation:glint;
    mat:TMatrix;
    procedure use;
    procedure init(prg:gluint);
    constructor create;
  end;

  { TShaderUI }

  TShaderUI = class (TProgram)
    mvp, textureUnif,
    m_WorldMatrixLocation:glint;
    mat:TMatrix;
    procedure use;
    procedure init(prg:gluint);
    constructor create;
  end;

implementation

{ TShaderUI }

procedure TShaderUI.use;
begin
    glUseProgram(id);
end;

procedure TShaderUI.init(prg: gluint);
begin
    glUseProgram(prg);
    id:=prg;
    MVP:=GetUniformLocation(prg,'mvp');
//    m_WorldMatrixLocation:=GetUniformLocation(prg,'gWorld');
    textureUnif:=GetUniformLocation(prg,'uTexture');
    glUniform1i(textureUnif, 0);
    mat:=IdentityHmgMatrix;
end;

constructor TShaderUI.create;
begin

end;

{ TShaderFlatDS }

procedure TShaderFlatDS.use;
begin
  glUseProgram(id);
end;

procedure TShaderFlatDS.init(prg: gluint);
//var s:string;
begin
  glUseProgram(prg);
  id:=prg;
  MVP:=GetUniformLocation(prg,'mvp');
  m_WorldMatrixLocation:=GetUniformLocation(prg,'gWorld');
  textureUnif:=GetUniformLocation(prg,'uTexture');
  texSize:=GetUniformLocation(prg,'texSize');
  texRect:=GetUniformLocation(prg,'texRect');

  glUniform1i(textureUnif, 0);

  mat:=IdentityHmgMatrix;
 // s:=gluErrorString(glGetError());
end;

constructor TShaderFlatDS.create;
begin

end;

{ TShaderSSAO }

procedure TShaderSSAO.use;
begin
  glUseProgram(id);
  if resizeDirty then begin
     glUniform1i(bgl_DepthTexture, 0);
     glUniform1i(bgl_RenderedTexture, 1);
     glUniform2f(gScreenSize,round(viewport[2]),round(viewport[3]));
     glUniform1i(samplesCount, options.renderer.ssaoSamples);
     glUniform1f(zfar,camera.zFar);
     resizeDirty:=false;
  end;
end;

procedure TShaderSSAO.init(prg: gluint);
begin
   glUseProgram(prg);
   id:=prg;
   bgl_DepthTexture:=GetUniformLocation(prg,'bgl_DepthTexture');
   bgl_RenderedTexture:=GetUniformLocation(prg,'bgl_RenderedTexture');
   gScreenSize:=GetUniformLocation(prg,'gScreenSize');
   zfar:=GetUniformLocation(prg,'zfar');
   samplesCount:=GetUniformLocation(prg,'samples');
   mat:=IdentityHmgMatrix;
   //set uniforms
end;

{ TNullDS }

procedure TNullDS.use;
begin
    glUseProgram(id);
end;

procedure TNullDS.init(prg: gluint);
begin
    glUseProgram(prg);
    id:=prg;
    gWVP:=GetUniformLocation(prg,'gWVP');
end;

{ TShaderPointLightDS }

procedure TShaderPointLightDS.use;
begin
  glUseProgram(id);
  if resizeDirty then begin
     //setup texture uniforms
     //position
     glUniform1i(m_posTextureUnitLocation, GBUFFER_POSITION_TEXTURE_UNIT);
     //clor
     glUniform1i(m_colorTextureUnitLocation, GBUFFER_DIFFUSE_TEXTURE_UNIT);
     //normal
     glUniform1i(m_normalTextureUnitLocation,GBUFFER_NORMAL_TEXTURE_UNIT);
     //screen size  uniform  0,0 here :(
     glUniform2f(m_screenSizeLocation, viewport[2], viewport[3]);
     //glUniformMatrix4fv(unifCameraToClipMatrix, 1, bytebool(GL_FALSE), @camera.projectionMatrix);
     resizeDirty:=false;
  end;
  //glUniformMatrix4fv(unifMvp, 1, bytebool(GL_FALSE), @camera.WorldToCameraMatrix);
end;

procedure TShaderPointLightDS.init(prg: gluint);
var
   s:string;
begin
  glUseProgram(prg);
  id:=prg;
  //point light
  unifMvp:=GetUniformLocation(prg,'mvp');
  Color := GetUniformLocation(prg,'gPointLight.Base.Color');
  AmbientIntensity := GetUniformLocation(prg,'gPointLight.Base.AmbientIntensity');
  Position := GetUniformLocation(prg,'gPointLight.Position');
  DiffuseIntensity := GetUniformLocation(prg,'gPointLight.Base.DiffuseIntensity');
  Atten.Constant := GetUniformLocation(prg,'gPointLight.Atten.Constant');
  Atten.Linear := GetUniformLocation(prg,'gPointLight.Atten.Linear');
  Atten.Exp := GetUniformLocation(prg,'gPointLight.Atten.Exp');
  //light
  m_posTextureUnitLocation:=GetUniformLocation(prg,'gPositionMap');
  m_colorTextureUnitLocation:=GetUniformLocation(prg,'gColorMap');
  m_normalTextureUnitLocation:=GetUniformLocation(prg,'gNormalMap');
  m_eyeWorldPosLocation:=GetUniformLocation(prg,'gEyeWorldPos');
  m_matSpecularIntensityLocation:=GetUniformLocation(prg,'gMatSpecularIntensity');
  m_matSpecularPowerLocation:=GetUniformLocation(prg,'gSpecularPower');
  m_screenSizeLocation:=GetUniformLocation(prg,'gScreenSize');
  //s:=gluErrorString(glGetError());
end;

procedure TShaderPointLightDS.setLight(light: PPointLight);
begin
  glUniform3f(Color, Light^.Color[0], Light^.Color[1], Light^.Color[2]);
  glUniform1f(AmbientIntensity, Light^.AmbientIntensity);
  glUniform1f(DiffuseIntensity, Light^.DiffuseIntensity);
  glUniform3f(Position, Light^.Position[0], Light^.Position[1], Light^.Position[2]);
  glUniform1f(Atten.Constant, Light^.Attenuation.Constant);
  glUniform1f(Atten.Linear, Light^.Attenuation.Linear);
  glUniform1f(Atten.Exp, Light^.Attenuation.Exp);
end;

constructor TShaderPointLightDS.create;
begin

end;

{ TShaderDirLightQuadDS }

procedure TShaderDirLightQuadDS.use;
begin
  glUseProgram(id);
  if resizeDirty then begin
     resizeDirty:=false;
     glUniform1i(m_posTextureUnitLocation, GBUFFER_POSITION_TEXTURE_UNIT);
     //clor
     glUniform1i(m_colorTextureUnitLocation, GBUFFER_DIFFUSE_TEXTURE_UNIT);
     //normal
     glUniform1i(m_normalTextureUnitLocation,GBUFFER_NORMAL_TEXTURE_UNIT);

    { glUniform1i(m_posTextureUnitLocation, 2);
     glUniform1i(m_colorTextureUnitLocation, 0);
     glUniform1i(m_normalTextureUnitLocation,1);
     }
     //screen size  uniform  0,0 here :(
     glUniform2f(m_screenSizeLocation, viewport[2], viewport[3]);
 //    glUniformMatrix4fv(gWVP, 1, bytebool(GL_FALSE), @mat);
 //		SetDirectionalLight(m_dirLight);
  end;
  //glUniformMatrix4fv(gWVP, 1, bytebool(GL_FALSE), @mat);
  //full screen quad in camera space or whatever
end;

procedure TShaderDirLightQuadDS.init(prg: gluint);
begin
    glUseProgram(prg);
    id:=prg;

    mat:=IdentityHmgMatrix;
    m_posTextureUnitLocation:=GetUniformLocation(prg,'gPositionMap');
    m_colorTextureUnitLocation:=GetUniformLocation(prg,'gColorMap');
    m_normalTextureUnitLocation:=GetUniformLocation(prg,'gNormalMap');
    m_eyeWorldPosLocation:=GetUniformLocation(prg,'gEyeWorldPos');
    m_matSpecularIntensityLocation:=GetUniformLocation(prg,'gMatSpecularIntensity');
    m_matSpecularPowerLocation:=GetUniformLocation(prg,'gSpecularPower');
    m_screenSizeLocation:=GetUniformLocation(prg,'gScreenSize');
    Color := GetUniformLocation(prg,'gDirectionalLight.Base.Color');
    AmbientIntensity := GetUniformLocation(prg,'gDirectionalLight.Base.AmbientIntensity');
    Direction := GetUniformLocation(prg,'gDirectionalLight.Direction');
    DiffuseIntensity := GetUniformLocation(prg,'gDirectionalLight.Base.DiffuseIntensity');
    //i think mvp mat needs to be set only once?

end;

procedure TShaderDirLightQuadDS.setLight(light: pDirLight);
begin
    glUniform3f(Color, light^.Color[0], light^.Color[1], light^.Color[2]);
    glUniform1f(AmbientIntensity, Light^.AmbientIntensity);
    //Vector3f Direction = Light.Direction;
    //Direction.Normalize();
    glUniform3f(Direction, light^.Direction[0], light^.Direction[1], light^.Direction[2]);
    glUniform1f(DiffuseIntensity, Light^.DiffuseIntensity);
end;

constructor TShaderDirLightQuadDS.create;
begin

end;

{ TShaderChunkDS }

procedure TShaderChunkDS.use;
var
   dir:TAffineVector;
begin
  glUseProgram(id);
//  glUniformMatrix4fv(unifWorldToCameraMatrix, 1, bytebool(GL_FALSE), @camera.WorldToCameraMatrix);
  //VectorSubtract(camera.position,camera.target,dir);
  //NormalizeVector(dir);
end;

procedure TShaderChunkDS.init(prg: gluint);
begin
    glUseProgram(prg);
    id:=prg;
    unifMVP:=GetUniformLocation(prg,'mvp');
    m_WorldMatrixLocation:=GetUniformLocation(prg,'gWorld');
//    ghostUnif:=GetUniformLocation(prg,'ghost');
end;

end.

