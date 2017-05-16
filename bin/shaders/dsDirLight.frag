#version 330 core
layout(location = 0) out vec4 outputColor;

struct BaseLight
{
    vec3 Color;
    float AmbientIntensity;
    float DiffuseIntensity;
};

struct DirectionalLight
{
    BaseLight Base;
    vec3 Direction;
};

struct Attenuation
{
    float Constant;
    float Linear;
    float Exp;
};

uniform sampler2D gColorMap;
uniform sampler2D gNormalMap;
uniform sampler2D gPositionMap;

uniform DirectionalLight gDirectionalLight;
uniform vec3 gEyeWorldPos;
uniform float gMatSpecularIntensity;
uniform float gSpecularPower;
uniform vec2 gScreenSize;

vec4 CalcLightInternal(BaseLight Light,
					   vec3 LightDirection,
					   vec3 WorldPos,
					   vec3 Normal)
{
    vec4 AmbientColor = vec4(Light.Color, 1.0f) * Light.AmbientIntensity;
    float DiffuseFactor = dot(Normal, -LightDirection);

    vec4 DiffuseColor  = vec4(0.0f, 0.0f, 0.0f, 0.0f);
    vec4 SpecularColor = vec4(0.0f, 0.0f, 0.0f, 0.0f);

    if (DiffuseFactor > 0) {
        DiffuseColor = vec4(Light.Color, 1.0f) * Light.DiffuseIntensity * DiffuseFactor;

        vec3 VertexToEye = normalize(gEyeWorldPos - WorldPos);
        vec3 LightReflect = normalize(reflect(LightDirection, Normal));
        float SpecularFactor = dot(VertexToEye, LightReflect);
        SpecularFactor = pow(SpecularFactor, gSpecularPower);
        if (SpecularFactor > 0) {
            SpecularColor = vec4(Light.Color, 1.0f) * gMatSpecularIntensity * SpecularFactor;
        }
    }

    return (AmbientColor + DiffuseColor + SpecularColor);
}

vec4 CalcDirectionalLight(vec3 WorldPos, vec3 Normal)
{
    return CalcLightInternal(gDirectionalLight.Base,
							 gDirectionalLight.Direction,
							 WorldPos,
							 Normal);
}

vec2 CalcTexCoord()
{
    return gl_FragCoord.xy / gScreenSize;
}

void main()
{ 
    vec2 TexCoord = CalcTexCoord();
	vec3 WorldPos = texture(gPositionMap, TexCoord).xyz;
	vec3 Color = texture(gColorMap, TexCoord).xyz;
	//Color = texture(gColorMap, TexCoord);
    vec3 Normal = texture(gNormalMap, TexCoord).xyz;
	//substract ssao result from original color
	outputColor = vec4(Color, 1.0) * CalcDirectionalLight(WorldPos, Normal);
//	-texture(gSSAOMap, TexCoord)).xyz;
	
	//Normal = normalize(Normal);
	//Color = mix(Color,SSAO,1);	
	//outputColor = vec4(Color, 1.0f);
    //outputColor = vec4(WorldPos, 1.0f);
    //outputColor = vec4(1,1,1,1);
}