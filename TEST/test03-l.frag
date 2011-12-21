// Simple Lambert-style lighting.
uniform vec4 kd;
varying	vec3 v_l, v_n;

void main (void) {
  gl_FragColor = kd * min (1.0, max (dot (normalize (v_n), normalize (v_l)), 0.0));
}