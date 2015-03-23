#version 100
precision highp float;

uniform sampler2D jvr_Texture0;
uniform sampler2D jvr_Texture1;

uniform float alpha;

varying vec2 texCoord;

void main (void){
    vec4 overlay = texture2D(jvr_Texture1, texCoord);
    gl_FragColor = mix(texture2D(jvr_Texture0, texCoord), vec4(overlay.xyz, alpha), overlay.a);
}
