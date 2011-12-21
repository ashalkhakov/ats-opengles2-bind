// Texture mapping with lighting.
uniform mat4 mv, mvp;    // model-view, model-view-projection
uniform mat3 nm;         // normal matrix
uniform vec4 lpos, epos; // light and eye positions

attribute vec4 pos;
attribute vec3 norm;
attribute vec2 texcoord;

varying vec2 v_tc;
varying vec3 v_l, v_n;

void main (void) {
  vec3 p = vec3 (mv * pos); // position in world space
  v_l = normalize (vec3 (lpos) - p); // direction to light source
  v_n = normalize (nm * norm); // surface normal
  v_tc = texcoord;
  gl_Position = mvp * pos;
}
