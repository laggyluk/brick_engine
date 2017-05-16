#version 330 core
layout(location = 0) out vec4 WorldPos;
layout(location = 1) out vec3 Color;
layout(location = 2) out vec3 Normal;

in  vec4 worldPos;
smooth in vec3 colour;
in  vec3 normal;


const vec4 fogcolor = vec4(0.1,0.1,0.3,0);
const float fogdensity = .000005;

void main()
{ 	
	//outputColor = oColor;
  float z = gl_FragCoord.z / gl_FragCoord.w;
  float fog = clamp(exp(-fogdensity * z * z), 0.2, 1);
  //gl_FragColor = mix(fogcolor, oData.oColor, fog);

  
  WorldPos = worldPos;	
  //Color =  colour;
  Color = mix(fogcolor.xyz, colour, fog).xyz;
  Normal = normal;  
  //spit.Diffuse =  mix(fogcolor, oData.Color, fog).xyz;
  	  
}
