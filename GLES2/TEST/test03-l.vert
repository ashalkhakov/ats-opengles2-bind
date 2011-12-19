// Simple Lambert-style lighting.
uniform mat4 mv, mvp;     // model-view, model-view-projection
uniform mat3 nm;          // normal matrix
uniform vec4 lpos, epos;  // light and eye positions

attribute vec4 pos;       // vertex position
attribute vec3 norm;      // vertex normal

varying vec3 v_l, v_n;

void main (void) {
  vec3 p = vec3 (mv * pos);
  v_l = normalize (vec3 (lpos) - p); // direction to light source
  v_n = normalize (nm * norm); // surface normal
  gl_Position = mvp * pos;
}