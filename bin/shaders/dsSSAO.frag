#version 330 core
/*
SSAO GLSL shader v1.2
assembled by Martins Upitis (martinsh) (devlog-martinsh.blogspot.com)
original technique is made by Arkano22 (www.gamedev.net/topic/550699-ssao-no-halo-artifacts/)

changelog:
1.2 - added fog calculation to mask AO. Minor fixes.
1.1 - added spiral sampling method from here:
(http://www.cgafaq.info/wiki/Evenly_distributed_points_on_sphere)
*/
layout(location = 0) out vec4 outputColor;

uniform sampler2D bgl_DepthTexture;
uniform sampler2D bgl_RenderedTexture;
uniform int samples; //ao sample count
uniform vec2 gScreenSize;
uniform float zfar;

#define PI    3.14159265

float width = gScreenSize[0]; //texture width
float height = gScreenSize[1]; //texture height


//------------------------------------------
//general stuff

//make sure that these two values are the same for your camera, otherwise distances will be wrong.

float znear = 0.1; //Z-near
//float zfar = 300.0; //Z-far moved to uniforms

//user variables
float radius = 3.0; //ao radius
float aoclamp = 0.25; //depth clamp - reduces haloing at screen edges
bool noise = true; //use noise instead of pattern for sample dithering
float noiseamount = 0.0002; //dithering amount

float diffarea = 0.4; //self-shadowing reduction
float gdisplace = 0.4; //gauss bell center

bool mist = false; //use mist?
float miststart = 0.0; //mist start
float mistend = 16.0; //mist end

bool onlyAO = false; //use only ambient occlusion pass?
float lumInfluence = 0.8; //how much luminance affects occlusion

vec2 texCoord;
//--------------------------------------------------------
vec2 CalcTexCoord()
{
    return gl_FragCoord.xy / gScreenSize;
}

//--------------------------------------------------------

vec2 rand(vec2 coord) //generating noise/pattern texture for dithering
{
	float noiseX = ((fract(1.0-coord.s*(width/2.0))*0.25)+(fract(coord.t*(height/2.0))*0.75))*2.0-1.0;
	float noiseY = ((fract(1.0-coord.s*(width/2.0))*0.75)+(fract(coord.t*(height/2.0))*0.25))*2.0-1.0;
	
	if (noise)
	{
		noiseX = clamp(fract(sin(dot(coord ,vec2(12.9898,78.233))) * 43758.5453),0.0,1.0)*2.0-1.0;
		noiseY = clamp(fract(sin(dot(coord ,vec2(12.9898,78.233)*2.0)) * 43758.5453),0.0,1.0)*2.0-1.0;
	}
	return vec2(noiseX,noiseY)*noiseamount;
}

float doMist()
{
	float zdepth = texture(bgl_DepthTexture,texCoord.xy).x;
	float depth = -zfar * znear / (zdepth * (zfar - znear) - zfar);
	return clamp((depth-miststart)/mistend,0.0,1.0);
}

float readDepth(vec2 coord) 
{
	if (texCoord.x<0.0 || texCoord.y<0.0) return 1.0;
	return (2.0 * znear) / (zfar + znear - texture(bgl_DepthTexture, coord ).x * (zfar-znear));
}

int far = 0;
float compareDepths(float depth1, float depth2)
{   	
	float garea = 2.0; //gauss bell width    
	float diff = (depth1 - depth2)*100.0; //depth difference (0-100)
	//reduce left bell width to avoid self-shadowing 
	if (diff<gdisplace)
	{
		garea = diffarea;
	}else{
		far = 1;
	}
	
	float gauss = pow(2.7182,-2.0*(diff-gdisplace)*(diff-gdisplace)/(garea*garea));
	return gauss;
}   

float calAO(float depth, float dw, float dh)
{   
	float dd = (1.0-depth)*radius;
	
	float temp = 0.0;
	float temp2 = 0.0;
	float coordw = texCoord.x + dw*dd;
	float coordh = texCoord.y + dh*dd;
	float coordw2 = texCoord.x - dw*dd;
	float coordh2 = texCoord.y - dh*dd;
	
	vec2 coord = vec2(coordw , coordh);
	vec2 coord2 = vec2(coordw2, coordh2);
	
	far = 0; //global var
	temp = compareDepths(depth, readDepth(coord));
	//DEPTH EXTRAPOLATION:
	if (far > 0)
	{
		temp2 = compareDepths(readDepth(coord2),depth);
		temp += (1.0-temp)*temp2;
	}
	return temp;
	//return readDepth(coord2);
} 

void main(void)
{
	float debug;
	texCoord = CalcTexCoord();
	vec2 noise = rand(texCoord); 
	float depth = readDepth(texCoord);
	
	float w = (1.0 / width)/clamp(depth,aoclamp,1.0)+(noise.x*(1.0-noise.x));
	float h = (1.0 / height)/clamp(depth,aoclamp,1.0)+(noise.y*(1.0-noise.y));
	
	float pw;
	float ph;
	
	float ao = 0;
	
	float dl = PI*(3.0-sqrt(5.0));
	float dz = 1.0/float(samples);
	float l = 0.0;
	float z = 1.0 - dz/2.0;
	
	for (int i = 0; i <= samples; i ++)
	{     
		float r = sqrt(1.0-z);		
		pw = cos(l)*r;
		ph = sin(l)*r;
		debug = calAO(depth,pw*w,ph*h);
		ao += debug;        
		z = z - dz;
		l = l + dl;
	}
	
	ao = ao / float(samples);
	ao = 1.0-ao;	
	
	if (mist)
	{
		ao = mix(ao, 1.0,doMist());
	}
	
	vec3 color = texture(bgl_RenderedTexture,texCoord).rgb;
	
	vec3 lumcoeff = vec3(0.299,0.587,0.114);
	float lum = dot(color.rgb, lumcoeff);
	vec3 luminance = vec3(lum, lum, lum);
	//ao = debug;
	vec3 final = vec3(color*mix(vec3(ao),vec3(1.0),luminance*lumInfluence));//mix(color*ao, white, luminance)
	
	if (onlyAO)
	{
		final = vec3(mix(vec3(ao),vec3(1.0),luminance*lumInfluence)); //ambient occlusion only
	}
	//outputColor = vec4(mix(vec3(debug,debug,debug),vec3(color),0.8),1);
	outputColor = vec4(final,1);
	//outputColor = color - vec4(debug,debug,debug,1); 
	//outputColor = vec4(1.0,1.0,1.0,1.0);
}