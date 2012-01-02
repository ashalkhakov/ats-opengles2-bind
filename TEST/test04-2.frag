#extension GL_OES_standard_derivatives : enable

// Level set contour texturing, Stefan Gustavson 2009
// This code is in the public domain.

uniform sampler2D gradtexture, reftexture;
uniform float texw, texh;
varying float onestepu, onestepv;
varying vec2 st;

void main( void )
{
  // Scale texcoords to range ([0,texw], [0,texh])
  vec2 uv = st * vec2(texw, texh);

  // Compute texel-local (u,v) coordinates for the four closest texels
  vec2 uv00 = floor(uv - vec2(0.5)); // Lower left corner of lower left texel
  vec2 uvthis = floor(uv); // Lower left corner of texel containing (u,v)
  vec2 uvlerp = uv - uv00 - vec2(0.5); // Texel-local lerp blends [0,1]

  // Perform explicit texture interpolation of a,b,c coefficients.
  // This has several benefits: it works around the currently very bad
  // texture interpolation precision in ATI hardware, it makes it possible
  // to use local uv coordinates for each texel and thus make the texture
  // tile and easy to crop and edit, and a non-linear interpolating ramp
  // can be used for improved smoothness.

  // Center st00 on lower left texel and rescale to [0,1] for texture lookup
  vec2 st00 = (uv00  + vec2(0.5)) * vec2(onestepu, onestepv);

  // Compute g_u, g_v, D coefficients from four closest 8-bit RGBA texels
  vec4 rawtex00 = texture2D(gradtexture, st00);
  vec4 rawtex10 = texture2D(gradtexture, st00 + vec2(onestepu, 0.0));
  vec4 rawtex01 = texture2D(gradtexture, st00 + vec2(0.0, onestepv));
  vec4 rawtex11 = texture2D(gradtexture, st00 + vec2(onestepu, onestepv));

  // Restore the value for gu, gv and D from their 8-bit and 16-bit encodings
  vec3 gugvD00 = vec3(2.0*(rawtex00.rg-0.50196), 2040.0*(rawtex00.a-0.50196)+7.96875*rawtex00.b);
  vec3 gugvD10 = vec3(2.0*(rawtex10.rg-0.50196), 2040.0*(rawtex10.a-0.50196)+7.96875*rawtex10.b);
  vec3 gugvD01 = vec3(2.0*(rawtex01.rg-0.50196), 2040.0*(rawtex01.a-0.50196)+7.96875*rawtex01.b);
  vec3 gugvD11 = vec3(2.0*(rawtex11.rg-0.50196), 2040.0*(rawtex11.a-0.50196)+7.96875*rawtex11.b);

  // Translate D to extrapolate F from neighboring texels to local uv
  vec2 uvoffset = uvthis - uv00; // 0 or 1 depending on (u,v) quadrant
  gugvD00.z -= dot(vec2(0.0, 0.0)-uvoffset, gugvD00.xy);
  gugvD10.z -= dot(vec2(1.0, 0.0)-uvoffset, gugvD10.xy);
  gugvD01.z -= dot(vec2(0.0, 1.0)-uvoffset, gugvD01.xy);
  gugvD11.z -= dot(vec2(1.0, 1.0)-uvoffset, gugvD11.xy);

  // Interpolate gugvD between four closest texels
  vec2 uvlocal = fract(uv); // Texel-local uv coordinates [0,1]
  // Use a cubic interpolation ramp for a smoother contour
  uvlerp = uvlerp * uvlerp * (3.0 - 2.0 * uvlerp); // 3*uv^2 - 2*uv^3
  // Interpolate along v
  vec3 gugvD0 = mix(gugvD00, gugvD01, uvlerp.y);
  vec3 gugvD1 = mix(gugvD10, gugvD11, uvlerp.y);
  // Interpolate along u
  vec3 gugvD = mix(gugvD0, gugvD1, uvlerp.x);

  // Compute  F = g_u*u + g_v*v + D
  float F = dot(gugvD, vec3(uvlocal, 1.0));
  // Compute length of local gradient transformed to fragment space
  // to perform high quality anisotopic analytic antialiasing
  float aastep = length(vec2(dot(dFdx(uv), gugvD.xy), dot(dFdy(uv), gugvD.xy)));
  // 'pattern' is 1 where F>0, 0 where F<0, with proper AA around F=0.
  float pattern = smoothstep(-aastep, aastep, F);

  // 'bitmap' is an ordinary texture for comparison.
  float bitmap = texture2D(reftexture, st00+uvoffset*vec2(onestepu, onestepv)).r;

  // Final fragment color
  gl_FragColor = vec4(bitmap, pattern, bitmap, 1.0);
}
