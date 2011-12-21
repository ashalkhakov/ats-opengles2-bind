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

fun texture_from_file (x: !GLtexture1, filename: !strptr1): void

(* ****** ****** *)

(* end of [util.sats] *)
