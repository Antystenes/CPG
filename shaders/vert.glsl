#version 330 core

uniform mat4 camera;
uniform mat4 proj;
uniform mat4 pos;
uniform mat4 rot;


layout(location = 0) in vec3 vertexPosition_modelspace;
layout(location = 1) in vec3 vertexNormal;
layout(location = 2) in vec2 vertexUV;
layout(location = 3) in vec3 vertex_col;

out vec2 v_uv;
out vec3 v_col;
out vec3 light_ray;
out vec3 v_normal;
out vec3 eye_direction;

vec4 light = vec4(3,200,5,1);

void main(){
  mat4 posMat = pos * rot;
  vec4 position = posMat * vec4(vertexPosition_modelspace,1.0);

  vec3 light2 = (camera * light).xyz;

  light_ray = normalize(light2 - position.xyz);

  vec4 view_position = camera * position;

  eye_direction = normalize(-view_position.xyz);

  vec3 normal = (camera * posMat * vec4(vertexNormal,0)).xyz;

  // float light_distance = dot(light_ray, light_ray);
  // float light_bounce = clamp( dot(light_ray, vertexNormal),0,1);

  v_uv = vertexUV;
  v_col = vertex_col;
  v_normal = normalize(normal);
  // diffuse = light_power * light_bounce/light_distance;
  gl_Position = proj * view_position;
}
