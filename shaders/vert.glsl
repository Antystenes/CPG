#version 330 core

uniform mat4 camera;
uniform mat4 proj;
uniform mat4 pos;
uniform mat4 rot;


layout(location = 0) in vec3 vertexPosition_modelspace;
layout(location = 1) in vec3 vertexNormal;
layout(location = 2) in vec2 vertexUV;
layout(location = 3) in vec3 vertex_col;
layout(location = 4) in vec3 vertex_tangent;
layout(location = 5) in vec3 vertex_bitangent;

out vec2 v_uv;
out vec3 v_col;
out vec3 light_ray;
out vec3 v_normal;
out vec3 eye_direction;
out mat3 TBN;

vec4 light = vec4(10,100,100,1);

void main(){



  mat4 posMat = pos * rot;
  mat4 MV     = camera * posMat;

  vec3 T      = (MV * vec4(vertex_tangent,0)).xyz;
  vec3 B      = (MV * vec4(vertex_bitangent,0)).xyz;
  vec3 normal = (MV * vec4(vertexNormal,0)).xyz;

  mat3 TBN    = transpose(mat3(T, B, normal));

  vec4 position = posMat * vec4(vertexPosition_modelspace,1.0);

  vec3 light2 = (camera * light).xyz;

  vec4 view_position = camera * position;

  light_ray = TBN * normalize(light2 - view_position.xyz);

  eye_direction = TBN * normalize(-view_position.xyz);


  // float light_distance = dot(light_ray, light_ray);
  // float light_bounce = clamp( dot(light_ray, vertexNormal),0,1);

  v_uv = vertexUV;
  v_col = vertex_col;
  v_normal = normalize(TBN * normal);
  // diffuse = light_power * light_bounce/light_distance;
  gl_Position = proj * view_position;
}
