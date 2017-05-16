unit core_forward_shaders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,core_shader_program,dglOpenGL,core_camera,core_lights,core_types,
  VectorGeometry;


type
  //forward mode shaders
  { TShaderForwardLighting }

  TShaderForwardLighting = class (TProgram)
    unifMVP:glint;
    m_WVPLocation,
    m_WorldMatrixLocation,
    m_samplerLocation,
    m_eyeWorldPosLocation,
    m_matSpecularIntensityLocation,
    m_matSpecularPowerLocation,
    m_numPointLightsLocation,
    m_numSpotLightsLocation:GLint;
    m_dirLightLocation : record
        Color,
        AmbientIntensity,
        DiffuseIntensity,
        Direction:glint;
    end;
    m_pointLightsLocation: array [0..MAX_POINT_LIGHTS-1] of record
        Color,
        AmbientIntensity,
        DiffuseIntensity,
        Position:glint;
        Atten : record
            Constant,
            Linear,
            Exp:glint;
        end;
    end;
    m_spotLightsLocation: array [0..MAX_SPOT_LIGHTS-1] of record
        Color,
        AmbientIntensity,
        DiffuseIntensity,
        Position,
        Direction,
        Cutoff:glint;
        Atten : record
          Constant,
          Linear,
          Exp:glint;
        end;
    end;
    procedure use;
    procedure init(prg:gluint);
    procedure resize;
    procedure setDirectionalLight(light:pDirLight);
    procedure SetEyeWorldPos(const EyeWorldPos:taffinevector);
    procedure SetMatSpecularIntensity(Intensity:single);
    procedure SetMatSpecularPower(Power:single);
    procedure SetPointLights(NumLights:cardinal; pLights:pPointLights);
    procedure SetSpotLights(NumLights:cardinal;  pLights:pSpotLights);
    procedure SetWorldMatrix(mat:Pmatrix);
  end;

implementation

{ TShaderForwardLighting }

procedure TShaderForwardLighting.use;
begin
    glUseProgram(id);
    //glUniformMatrix4fv(unifWorldToCameraMatrix, 1, bytebool(GL_FALSE), @camera.WorldToCameraMatrix);

end;

procedure TShaderForwardLighting.init(prg: gluint);
var
  i:cardinal;
  name:string;
begin
    glUseProgram(prg);
    id:=prg;
    unifMVP:=GetUniformLocation(prg,'mvp');
    m_WorldMatrixLocation := GetUniformLocation(id,'gWorld');
    m_eyeWorldPosLocation := GetUniformLocation(id,'gEyeWorldPos');
    m_dirLightLocation.Color := GetUniformLocation(id,'gDirectionalLight.Base.Color');
    m_dirLightLocation.AmbientIntensity := GetUniformLocation(id,'gDirectionalLight.Base.AmbientIntensity');
    m_dirLightLocation.Direction := GetUniformLocation(id,'gDirectionalLight.Direction');
    m_dirLightLocation.DiffuseIntensity := GetUniformLocation(id,'gDirectionalLight.Base.DiffuseIntensity');
    m_matSpecularIntensityLocation := GetUniformLocation(id,'gMatSpecularIntensity');
    m_matSpecularPowerLocation := GetUniformLocation(id,'gSpecularPower');
    m_numPointLightsLocation := GetUniformLocation(id,'gNumPointLights');
    m_numSpotLightsLocation := GetUniformLocation(id,'gNumSpotLights');

    for i:=0 to length(m_pointLightsLocation)-1 do begin
        name:=format('gPointLights[%d].Base.Color',[i]);
        m_pointLightsLocation[i].Color := GetUniformLocation(id,name);

        name:=format('gPointLights[%d].Base.AmbientIntensity', [i]);
        m_pointLightsLocation[i].AmbientIntensity := GetUniformLocation(id,Name);

        name:=format('gPointLights[%d].Position', [i]);
        m_pointLightsLocation[i].Position := GetUniformLocation(id,Name);

        name:=format('gPointLights[%d].Base.DiffuseIntensity', [i]);
        m_pointLightsLocation[i].DiffuseIntensity := GetUniformLocation(id,Name);

        name:=format('gPointLights[%d].Atten.Constant', [i]);
        m_pointLightsLocation[i].Atten.Constant := GetUniformLocation(id,Name);

        name:=format('gPointLights[%d].Atten.Linear', [i]);
        m_pointLightsLocation[i].Atten.Linear := GetUniformLocation(id,Name);

        name:=format('gPointLights[%d].Atten.Exp', [i]);
        m_pointLightsLocation[i].Atten.Exp := GetUniformLocation(id,Name);
    end;
    for i:=0 to length(m_spotLightsLocation)-1 do begin
        name:=format('gSpotLights[%d].Base.Base.Color', [i]);
        m_spotLightsLocation[i].Color := GetUniformLocation(id,Name);

        name:=format('gSpotLights[%d].Base.Base.AmbientIntensity', [i]);
        m_spotLightsLocation[i].AmbientIntensity := GetUniformLocation(Name);

        name:=format('gSpotLights[%d].Base.Position', [i]);
        m_spotLightsLocation[i].Position := GetUniformLocation(Name);

        name:=format('gSpotLights[%d].Direction', [i]);
        m_spotLightsLocation[i].Direction := GetUniformLocation(Name);

        name:=format('gSpotLights[%d].Cutoff', [i]);
        m_spotLightsLocation[i].Cutoff := GetUniformLocation(Name);

        name:=format('gSpotLights[%d].Base.Base.DiffuseIntensity', [i]);
        m_spotLightsLocation[i].DiffuseIntensity := GetUniformLocation(Name);

        name:=format('gSpotLights[%d].Base.Atten.Constant', [i]);
        m_spotLightsLocation[i].Atten.Constant := GetUniformLocation(Name);

        name:=format('gSpotLights[%d].Base.Atten.Linear', [i]);
        m_spotLightsLocation[i].Atten.Linear := GetUniformLocation(Name);

        name:=format('gSpotLights[%d].Base.Atten.Exp', [i]);
        m_spotLightsLocation[i].Atten.Exp := GetUniformLocation(Name);
    end;
end;

procedure TShaderForwardLighting.resize;
begin
  glUseProgram(id);
//  glUniformMatrix4fv(unifCameraToClipMatrix, 1, bytebool(GL_FALSE), @camera.projectionMatrix);
end;

procedure TShaderForwardLighting.setDirectionalLight(light: pDirLight);
begin
  glUniform3f(m_dirLightLocation.Color, Light^.Color[0], Light^.Color[1], Light^.Color[2]);
  glUniform1f(m_dirLightLocation.AmbientIntensity, Light^.AmbientIntensity);
  //Vector3f Direction = Light.Direction;
  //Direction.Normalize();
  glUniform3f(m_dirLightLocation.Direction, light^.Direction[0], light^.Direction[1], light^.Direction[2]);
  glUniform1f(m_dirLightLocation.DiffuseIntensity, Light^.DiffuseIntensity);
end;

procedure TShaderForwardLighting.SetEyeWorldPos(const EyeWorldPos: taffinevector);
begin
  glUniform3f(m_eyeWorldPosLocation, EyeWorldPos[0], EyeWorldPos[1], EyeWorldPos[2]);
end;

procedure TShaderForwardLighting.SetMatSpecularIntensity(Intensity: single);
begin
  glUniform1f(m_matSpecularIntensityLocation, Intensity);
end;

procedure TShaderForwardLighting.SetMatSpecularPower(Power: single);
begin
  glUniform1f(m_matSpecularPowerLocation, Power);
end;

procedure TShaderForwardLighting.SetPointLights(NumLights: cardinal;
  pLights: pPointLights);
var
  i:cardinal;
begin
  glUniform1i(m_numPointLightsLocation, NumLights);

  for i := 0 to NumLights-1 do begin
      glUniform3f(m_pointLightsLocation[i].Color, pLights^[i].Color[0], pLights^[i].Color[1], pLights^[i].Color[2]);
      glUniform1f(m_pointLightsLocation[i].AmbientIntensity, pLights^[i].AmbientIntensity);
      glUniform1f(m_pointLightsLocation[i].DiffuseIntensity, pLights^[i].DiffuseIntensity);
      glUniform3f(m_pointLightsLocation[i].Position, pLights^[i].Position[0], pLights^[i].Position[1], pLights^[i].Position[2]);
      glUniform1f(m_pointLightsLocation[i].Atten.Constant, pLights^[i].Attenuation.Constant);
      glUniform1f(m_pointLightsLocation[i].Atten.Linear, pLights^[i].Attenuation.Linear);
      glUniform1f(m_pointLightsLocation[i].Atten.Exp, pLights^[i].Attenuation.Exp);
  end;
end;

procedure TShaderForwardLighting.SetSpotLights(NumLights: cardinal;
  pLights: pSpotLights);
var
  i:cardinal;
begin
  glUniform1i(m_numSpotLightsLocation, NumLights);

  for i:=0 to NumLights-1 do begin
      glUniform3f(m_spotLightsLocation[i].Color, pLights^[i].Color[0], pLights^[i].Color[1], pLights^[i].Color[2]);
      glUniform1f(m_spotLightsLocation[i].AmbientIntensity, pLights^[i].AmbientIntensity);
      glUniform1f(m_spotLightsLocation[i].DiffuseIntensity, pLights^[i].DiffuseIntensity);
      glUniform3f(m_spotLightsLocation[i].Position,  pLights^[i].Position[0], pLights^[i].Position[1], pLights^[i].Position[2]);
      //Vector3f Direction = pLights^[i].Direction;
      //Direction.Normalize();
      glUniform3f(m_spotLightsLocation[i].Direction, pLights^[i].Direction[0], pLights^[i].Direction[1], pLights^[i].Direction[2]);
      glUniform1f(m_spotLightsLocation[i].Cutoff, cos(degtorad(pLights^[i].Cutoff)));
      glUniform1f(m_spotLightsLocation[i].Atten.Constant, pLights^[i].Attenuation.Constant);
      glUniform1f(m_spotLightsLocation[i].Atten.Linear,   pLights^[i].Attenuation.Linear);
      glUniform1f(m_spotLightsLocation[i].Atten.Exp,      pLights^[i].Attenuation.Exp);
  end;
end;

procedure TShaderForwardLighting.SetWorldMatrix(mat: Pmatrix);
begin
  glUniformMatrix4fv(m_WorldMatrixLocation, 1, bytebool(GL_TRUE), @mat);
end;

end.





