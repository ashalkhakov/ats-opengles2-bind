//
// Author: Artyom Shakhakov (artyom DOT shalkhakov AT gmail DOT com)
// Time: May, 2011
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no need for staloading at run-time

(* ****** ****** *)

staload "contrib/GLES2/SATS/gl2.sats"

fun shader_from_source {l:agz} (x: !GLshader, src: !strptr l): void
fun shader_from_file (x: !GLshader, name: string): void

// loads a texture from a file of specified name (wrt to current workdir);
// does no mip-mapping
// NOTE: wrapping is set to [GL_REPEAT], filtering is [GL_LINEAR]
fun texture_from_file {l:agz} (x: !GLtexture1, filename: !strptr l): void

// similar to [texture_from_file], but will also report dimensions;
// allows wrapping/filtering to be specified
fun texture_from_file_dim {l:agz} (
  x: !GLtexture1, filename: !strptr l, w: &size_t? >> size_t, h: &size_t? >> size_t
, filt: GLint, wrap: GLint
): void

(* ****** ****** *)

(* end of [util.sats] *)
