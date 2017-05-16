#version 330 core
out	vec4 Color;     
in	vec2 texCoord;
in vec4 leColor;	

uniform sampler2D uTexture;

void main()
{ 
  vec4 col = texture( uTexture, texCoord ) ;
  if(col.a < 0.1) discard;
  col = mix(col,leColor,0.5f);  
  Color = col; 
}