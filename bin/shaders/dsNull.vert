#version 330 core
layout (location = 0) in vec3 position;

uniform mat4 gWVP;

void main()
{
    gl_Position = gWVP * vec4(position, 1.0);
}
