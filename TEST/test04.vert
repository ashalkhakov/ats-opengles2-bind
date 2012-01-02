// Level set contour texturing, Stefan Gustavson 2009
// This code is in the public domain.
// Slightly modified by Artyom Shalkhakov in 2011
// for OpenGL ES 2.0

uniform mat4 mvp;
attribute vec4 pos;
attribute vec2 texcoord;

uniform sampler2D gradtexture, reftexture;
uniform float texw, texh;
varying float onestepu, onestepv;
varying vec2 st;

void main( void )
{
  // Get the texture coordinates
  st = texcoord;
  onestepu = 1.0 / texw; // Saves a division in the fragment shader
  onestepv = 1.0 / texh;
  gl_Position = mvp * pos;
}
