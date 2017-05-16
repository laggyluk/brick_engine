#version 330 core
layout(location = 0) out vec4 WorldPos;
layout(location = 1) out vec3 Color;
layout(location = 2) out vec3 Normal;

in vec4 worldPos;
in vec3 colour;
in vec3 normal;	
in vec2 texCoord;	

uniform sampler2D uTexture;

const vec4 fogcolor = vec4(0.1,0.1,0.3,0);
const float fogdensity = .000005;

void main()
{ 
  //float z = gl_FragCoord.z / gl_FragCoord.w;
  //float fog = clamp(exp(-fogdensity * z * z), 0.2, 1);
  WorldPos = worldPos;	
  vec4 col = texture( uTexture, texCoord );
  if(col.a < 0.5) discard;  
  Color = col.xyz;
  //discard transparent fragments
  
  //oData.Color = mix(fogcolor.xyz, iData.colour, fog).xyz;
  Normal = normal;    
}