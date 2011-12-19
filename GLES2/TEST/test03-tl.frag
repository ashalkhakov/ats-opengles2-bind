// Texture mapping with lighting.
uniform sampler2D tex;
uniform vec4 kd; // ignored

varying vec2 v_tc;
varying vec3 v_l, v_n;

void main (void) {
  gl_FragColor = texture2D (tex, v_tc) * min(1.0, max (dot (normalize (v_n), normalize (v_l)), 0.0));
}