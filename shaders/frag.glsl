#version 330 core

in vec2 v_uv;
in vec3 v_col;
// in vec3 v_normal;
in vec3 light_ray;
in vec3 eye_direction;

out vec3 color;
uniform sampler2D texSampler;
uniform sampler2D normSampler;

float light_power = 1;

vec3 v_normal   = normalize(texture(normSampler, v_uv).rgb * 2.0 - 1.0);

vec3 reflection = reflect(-light_ray, v_normal); 

void main(){
  float light_distance = dot(light_ray, light_ray);
  float light_bounce = clamp( dot(light_ray, v_normal),0,1);

  float diffuse = light_power * light_bounce / light_distance;

float specular_coeff = light_power * pow(clamp( dot(eye_direction, reflection),0,1),5) / light_distance;

  vec3 ambient = vec3(0.1,0.1,0.1);
  color = texture( texSampler, v_uv ).rgb * (diffuse + specular_coeff + ambient);
}
// * texture( texSampler, v_uv).rgb
