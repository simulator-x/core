#version 100
precision highp float;

uniform sampler2D sceneMap;
uniform float posX;
uniform float posY;
uniform float time;
uniform vec4 color;

varying vec2 texCoord;

void main() 
{ 
  //vec4 color = vec4(1.0,0.0,0.0,0.0);
  float colorFactor = 0.0;
  vec3 shockParams = vec3(10.0, 0.8, 0.1);
  vec2 center = vec2(posX, 1.0-posY);
  vec2 newTexCoord = texCoord;
  vec2 dis = texCoord - center;
  float distancee = length(vec2(dis.x*(16.0/9.0), dis.y));
  if ( (distancee <= (time + shockParams.z)) && 
       (distancee >= (time - shockParams.z)) ) 
  {    
    float diff = (distancee - time); 
    float powDiff = 1.0 - pow(abs(diff*shockParams.x), 
                                shockParams.y); 
    float diffTime = diff  * powDiff; 
    vec2 diffUV = normalize(texCoord - center); 
    colorFactor = (1.0 - (abs(time - distancee) / shockParams.z)) * (1.0 - distance(texCoord, center));
    newTexCoord = texCoord + (diffUV * diffTime);
  } 
  gl_FragColor = texture2D(sceneMap, newTexCoord) + (color * colorFactor);
}


