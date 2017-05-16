unit core_lights;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,VectorGeometry,dglOpenGL,fgl,math,core_types;

type
    eLightTypes = (ltDirectional, ltCube, ltSpot);

    rAttenuation = record
       constant:single;
       linear:single;
       exp:single;
    end;


    TBaseLight = class
      color:TAffineVector;
      ambientIntensity:single;
      diffuseIntensity:single;
      //flags if light should be saved with level
      staticLight:boolean;
    end;

    //point light cube is iterated through list o lights and transformed
    //through scale and position if it changes, if not then it uses transform matrix of this
    //light instance

    { TDirectionalLight }

    TDirectionalLight = class (TBaseLight)
      direction:TAffineVector;
      procedure setUp(col,dir:TAffineVector;ambientIntens,diffuseIntens:single);
      constructor create(col,dir:TAffineVector;ambientIntens,diffuseIntens:single);
    end;
    pDirLight = ^TDirectionalLight;

    { TPointLight }

    TPointLight = class  (TBaseLight)
    private
        fposition:TAffineVector;
        procedure setPosition(v:TAffineVector);overload;
        procedure setPosition(x,y,z:single);overload;
    public
      size:single; //nie ma?

      halfRadius:single;
      dirty:boolean;//if flagged then calculate transform, if not then use this matrix
      transform:tmatrix;
      attenuation:rAttenuation;
      procedure setUp(col,pos:TAffineVector;diffuse,attenuationConst,attenuationLinear,attenuationExp:single);
      constructor create(col, pos: TAffineVector; diffuse, attenuationConst,
                         attenuationLinear, attenuationExp: single);
      procedure calcPointLightCube;
      property position:taffinevector read fposition write setPosition;
      property positionX:single read fposition[0] write fposition[0];
      property positionY:single read fposition[1] write fposition[1];
      property positionZ:single read fposition[2] write fposition[2];
    end;

    PPointLight = ^TPointLight;
    TPointLights = array [0..MAX_POINT_LIGHTS-1] of TPointLight;
    pPointLights = ^TPointLights;

    { TSpotLight }

    TSpotLight = class (TPointLight)
      cutoff: single;
      direction:TAffineVector;
      constructor create(col, pos,dir: TAffineVector; cutof, diffuse, attenuationConst,
                         attenuationLinear, attenuationExp: single);
      procedure setUp(col,pos,dir:TAffineVector;cutof,diffuse,attenuationConst,
                         attenuationLinear,attenuationExp:single);overload;
    end;
    TSpotLights = array [0..MAX_SPOT_LIGHTS-1] of TSpotLight;
    pSpotLights = ^TSpotlights;

    TListOfPointLights = specialize TFPGList<TPointLight>;
    TListOfDirLights = specialize TFPGList<TDirectionalLight>;

implementation

{ TSpotLight }

constructor TSpotLight.create(col, pos, dir: TAffineVector; cutof, diffuse,
  attenuationConst, attenuationLinear, attenuationExp: single);
begin
  setUp(col,pos,dir,cutof,diffuse,attenuationConst,attenuationLinear,attenuationExp);
end;

procedure TSpotLight.setUp(col, pos,dir: TAffineVector; cutof, diffuse,
  attenuationConst, attenuationLinear, attenuationExp: single);
begin
  inherited setup(col, pos,diffuse, attenuationConst,attenuationLinear, attenuationExp);
  cutoff:=cutof;
  direction:=dir;
end;

{ TDirectionalLight }

procedure TDirectionalLight.setUp(col, dir: TAffineVector; ambientIntens,
  diffuseIntens: single);
begin
  color:=col;
  direction:=dir;
  ambientIntensity:=ambientIntens;
  diffuseIntensity:=diffuseIntens;
end;

constructor TDirectionalLight.create(col, dir: TAffineVector; ambientIntens,
  diffuseIntens: single);
begin
  setUp(col,dir,ambientIntens,diffuseIntens);
end;


{ TPointLight }

procedure TPointLight.setUp(col, pos: TAffineVector; diffuse, attenuationConst,
  attenuationLinear, attenuationExp: single);
begin
  dirty:=true;
  color:=col;
  position:=pos;
  diffuseIntensity:=diffuse;
  attenuation.constant:=attenuationConst;
  attenuation.linear:=attenuationLinear;
  attenuation.exp:=attenuationExp;
  calcPointLightCube;
end;

constructor TPointLight.create(col, pos: TAffineVector; diffuse, attenuationConst,
  attenuationLinear, attenuationExp: single);
begin
  transform:=IdentityHmgMatrix;
  setUp(col, pos,diffuse, attenuationConst,attenuationLinear, attenuationExp);
end;

procedure TPointLight.calcPointLightCube;
var
  c,MaxChannel:single;
begin
  //original was made for sphere so maybe it should be taken into acount
 	MaxChannel := max(max(Color[0], Color[1]), Color[2]);
 	c := MaxChannel * diffuseIntensity;
 	size:= (8.0 * sqrt(c) + 1.0)*1.2;
end;

procedure TPointLight.setPosition(v: TAffineVector);
begin
  dirty:=true;
  fposition:=v;
end;

procedure TPointLight.setPosition(x, y, z: single);
begin
  dirty:=true;
  fposition:=vector3fmake(x,y,z);
end;

end.

