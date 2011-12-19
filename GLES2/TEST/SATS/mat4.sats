//
// Author: Artyom Shakhakov (artyom DOT shalkhakov AT gmail DOT com)
// Time: June, 2011
//
(*
** Miscellaneous matrix-related functions for use with OpenGL ES.
*)
(* ****** ****** *)

staload "contrib/GLES2/SATS/gl2.sats"

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no staloading at run-time

(* ****** ****** *)

typedef GLmat4 = @[GLfloat][16]

// determinant of [m]
fun mat_det (m: &GLmat4):<> GLfloat

// [m] becomes the identity matrix
fun mat_id (m: &GLmat4? >> GLmat4):<> void

// transpose [m] in-place
// [m' := transpose (m)]
fun mat_transp (m: &GLmat4):<> void

// compute the inverse of [m], placing it into [n]
// [n := inverse(m)]
fun mat_invert_into (m: &GLmat4, n: &GLmat4? >> GLmat4):<> void

// multiplication of two matrices (in-place)
// [m' := m * n]
fun mat_mult (m: &GLmat4, n: &GLmat4):<> void

// concatenate a rotation about arbitrary axis
// given by (x,y,z) by angle [a] onto [m]
fun mat_rot (
  m: &GLmat4, a: GLfloat, x: GLfloat, y: GLfloat, z: GLfloat
) :<> void

// concatenate a translation by (x,y,z) onto [m]
fun mat_trn (m: &GLmat4, x: GLfloat, y: GLfloat, z: GLfloat):<> void

// [m] becomes a perspective projection matrix
fun mat_persp (
  fovy: float, aspect: float, near: float, far: float
, m: &GLmat4? >> GLmat4
) :<> void

// for debugging
fun mat_prerr (m: &GLmat4): void
overload prerr with mat_prerr

(* ****** ****** *)

(* end of [mat4.sats] *)
