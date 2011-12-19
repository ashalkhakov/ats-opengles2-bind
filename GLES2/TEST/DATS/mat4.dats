//
// Author: Artyom Shakhakov (artyom DOT shalkhakov AT gmail DOT com)
// Time: June, 2011
//
(* ****** ****** *)

staload "libc/SATS/math.sats"

staload "libats/SATS/biarray.sats"

staload "contrib/GLES2/SATS/gl2.sats"

staload "mat4.sats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no need for dynloading at run-time

(* ****** ***** *)

implement mat_det (m) = let
  #define F float_of_GLfloat; #define G GLfloat_of_float
in
  G (
    (F m.[4+0] * F m.[4+1] - F m.[0+1] * F m.[4+0]) * (F m.[8+2] * F m.[12+3] - F m.[8+3] * F m.[12+2]) -
    (F m.[4+0] * F m.[4+2] - F m.[0+2] * F m.[4+0]) * (F m.[8+1] * F m.[12+3] - F m.[8+3] * F m.[12+1]) +
    (F m.[4+0] * F m.[4+3] - F m.[0+3] * F m.[4+0]) * (F m.[8+1] * F m.[12+2] - F m.[8+2] * F m.[12+1]) +
    (F m.[4+1] * F m.[4+2] - F m.[0+2] * F m.[4+1]) * (F m.[8+0] * F m.[12+3] - F m.[8+3] * F m.[12+0]) -
    (F m.[4+1] * F m.[4+3] - F m.[0+3] * F m.[4+1]) * (F m.[8+0] * F m.[12+2] - F m.[8+2] * F m.[12+0]) +
    (F m.[4+2] * F m.[4+3] - F m.[0+3] * F m.[4+2]) * (F m.[8+0] * F m.[12+1] - F m.[8+1] * F m.[12+0])
  )
end // end of [mat_det]

(* ****** ***** *)

implement mat_mult (m, n) = let
  fun go .< >. (b: &GLmat4, a: &GLmat4):<> void = let
    #define F float_of
    #define G GLfloat_of_float
    var !p = @[GLfloat][16](G 0.0f)
    val ai0 = F a.[0] and ai1 = F a.[1] and ai2 = F a.[2] and ai3 = F a.[3]
    val () = !p.[0] := G (ai0 * F b.[0] + ai1 * F b.[4] + ai2 * F b.[8] + ai3 * F b.[12]);
    val () = !p.[1] := G (ai0 * F b.[1] + ai1 * F b.[5] + ai2 * F b.[9] + ai3 * F b.[13]);
    val () = !p.[2] := G (ai0 * F b.[2] + ai1 * F b.[6] + ai2 * F b.[10] + ai3 * F b.[14]);
    val () = !p.[3] := G (ai0 * F b.[3] + ai1 * F b.[7] + ai2 * F b.[11] + ai3 * F b.[15])

    val ai0 = F a.[4] and ai1 = F a.[5] and ai2 = F a.[6] and ai3 = F a.[7]
    val () = !p.[4] := G (ai0 * F b.[0] + ai1 * F b.[4] + ai2 * F b.[8] + ai3 * F b.[12]);
    val () = !p.[5] := G (ai0 * F b.[1] + ai1 * F b.[5] + ai2 * F b.[9] + ai3 * F b.[13]);
    val () = !p.[6] := G (ai0 * F b.[2] + ai1 * F b.[6] + ai2 * F b.[10] + ai3 * F b.[14]);
    val () = !p.[7] := G (ai0 * F b.[3] + ai1 * F b.[7] + ai2 * F b.[11] + ai3 * F b.[15])  

    val ai0 = F a.[8] and ai1 = F a.[9] and ai2 = F a.[10] and ai3 = F a.[11]
    val () = !p.[8] := G (ai0 * F b.[0] + ai1 * F b.[4] + ai2 * F b.[8] + ai3 * F b.[12]);
    val () = !p.[9] := G (ai0 * F b.[1] + ai1 * F b.[5] + ai2 * F b.[9] + ai3 * F b.[13]);
    val () = !p.[10] := G (ai0 * F b.[2] + ai1 * F b.[6] + ai2 * F b.[10] + ai3 * F b.[14]);
    val () = !p.[11] := G (ai0 * F b.[3] + ai1 * F b.[7] + ai2 * F b.[11] + ai3 * F b.[15])

    val ai0 = F a.[12] and ai1 = F a.[13] and ai2 = F a.[14] and ai3 = F a.[15]
    val () = !p.[12] := G (ai0 * F b.[0] + ai1 * F b.[4] + ai2 * F b.[8] + ai3 * F b.[12]);
    val () = !p.[13] := G (ai0 * F b.[1] + ai1 * F b.[5] + ai2 * F b.[9] + ai3 * F b.[13]);
    val () = !p.[14] := G (ai0 * F b.[2] + ai1 * F b.[6] + ai2 * F b.[10] + ai3 * F b.[14]);
    val () = !p.[15] := G (ai0 * F b.[3] + ai1 * F b.[7] + ai2 * F b.[11] + ai3 * F b.[15])
  in
    array_ptr_copy_tsz (!p, b, size1_of_int1 16, sizeof<GLfloat>)
  end // end of [go]
in
  go (m, n)
end // end of [mat_mult]

(* ****** ****** *)

// originally from www.geometrictools.com/LibMathematics/Algebra/Wm5Matrix4.inl
// Copyright (c) 1998-2011
// Distributed under the Boost Software License, Version 1.0.
// http://www.boost.org/LICENSE_1_0.txt
// http://www.geometrictools.com/License/Boost/LICENSE_1_0.txt
implement mat_invert_into (m, n) = let
  #define F float_of_GLfloat; #define G GLfloat_of_float
  val a0 = F m.[ 0]*F m.[ 5] - F m.[ 1]*F m.[ 4]
  val a1 = F m.[ 0]*F m.[ 6] - F m.[ 2]*F m.[ 4]
  val a2 = F m.[ 0]*F m.[ 7] - F m.[ 3]*F m.[ 4]
  val a3 = F m.[ 1]*F m.[ 6] - F m.[ 2]*F m.[ 5]
  val a4 = F m.[ 1]*F m.[ 7] - F m.[ 3]*F m.[ 5]
  val a5 = F m.[ 2]*F m.[ 7] - F m.[ 3]*F m.[ 6]
  val b0 = F m.[ 8]*F m.[13] - F m.[ 9]*F m.[12]
  val b1 = F m.[ 8]*F m.[14] - F m.[10]*F m.[12]
  val b2 = F m.[ 8]*F m.[15] - F m.[11]*F m.[12]
  val b3 = F m.[ 9]*F m.[14] - F m.[10]*F m.[13]
  val b4 = F m.[ 9]*F m.[15] - F m.[11]*F m.[13]
  val b5 = F m.[10]*F m.[15] - F m.[11]*F m.[14]
  val det = a0*b5 - a1*b4 + a2*b3 + a3*b2 - a4*b1 + a5*b0
  // FIXME: breaking the type guarantee
  val () = $effmask_all (assert_errmsg (abs det > 1e-6f, "[mat_invert_into]: degenerate matrix"))
  val invDet = 1.0f / det

  prval pf_arr = view@ (n)
  prval pf_out = biarray_v_nil {GLfloat} ()
  var p = &n

  prval (pf_at, pf_arr) = array_v_uncons {GLfloat?} (pf_arr)
  val () = !p := G (( F m.[ 5]*b5 - F m.[ 6]*b4 + F m.[ 7]*b3) * invDet)
  prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
  val () = p := p+sizeof<GLfloat>

  prval (pf_at, pf_arr) = array_v_uncons {GLfloat?} (pf_arr)
  val () = !p := G ((~F m.[ 1]*b5 + F m.[ 2]*b4 - F m.[ 3]*b3) * invDet)
  prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
  val () = p := p+sizeof<GLfloat>

  prval (pf_at, pf_arr) = array_v_uncons {GLfloat?} (pf_arr)
  val () = !p := G (( F m.[13]*a5 - F m.[14]*a4 + F m.[15]*a3) * invDet)
  prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
  val () = p := p+sizeof<GLfloat>

  prval (pf_at, pf_arr) = array_v_uncons {GLfloat?} (pf_arr)
  val () = !p := G ((~F m.[ 9]*a5 + F m.[10]*a4 - F m.[11]*a3) * invDet)
  prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
  val () = p := p+sizeof<GLfloat>

  prval (pf_at, pf_arr) = array_v_uncons {GLfloat?} (pf_arr)
  val () = !p := G ((~F m.[ 4]*b5 + F m.[ 6]*b2 - F m.[ 7]*b1) * invDet)
  prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
  val () = p := p+sizeof<GLfloat>

  prval (pf_at, pf_arr) = array_v_uncons {GLfloat?} (pf_arr)
  val () = !p := G (( F m.[ 0]*b5 - F m.[ 2]*b2 + F m.[ 3]*b1) * invDet)
  prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
  val () = p := p+sizeof<GLfloat>

  prval (pf_at, pf_arr) = array_v_uncons {GLfloat?} (pf_arr)
  val () = !p := G ((~F m.[12]*a5 + F m.[14]*a2 - F m.[15]*a1) * invDet)
  prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
  val () = p := p+sizeof<GLfloat>

  prval (pf_at, pf_arr) = array_v_uncons {GLfloat?} (pf_arr)
  val () = !p := G (( F m.[ 8]*a5 - F m.[10]*a2 + F m.[11]*a1) * invDet)
  prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
  val () = p := p+sizeof<GLfloat>

  prval (pf_at, pf_arr) = array_v_uncons {GLfloat?} (pf_arr)
  val () = !p := G (( F m.[ 4]*b4 - F m.[ 5]*b2 + F m.[ 7]*b0) * invDet)
  prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
  val () = p := p+sizeof<GLfloat>

  prval (pf_at, pf_arr) = array_v_uncons {GLfloat?} (pf_arr)
  val () = !p := G ((~F m.[ 0]*b4 + F m.[ 1]*b2 - F m.[ 3]*b0) * invDet)
  prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
  val () = p := p+sizeof<GLfloat>

  prval (pf_at, pf_arr) = array_v_uncons {GLfloat?} (pf_arr)
  val () = !p := G (( F m.[12]*a4 - F m.[13]*a2 + F m.[15]*a0) * invDet)
  prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
  val () = p := p+sizeof<GLfloat>

  prval (pf_at, pf_arr) = array_v_uncons {GLfloat?} (pf_arr)
  val () = !p := G ((~F m.[ 8]*a4 + F m.[ 9]*a2 - F m.[11]*a0) * invDet)
  prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
  val () = p := p+sizeof<GLfloat>

  prval (pf_at, pf_arr) = array_v_uncons {GLfloat?} (pf_arr)
  val () = !p := G ((~F m.[ 4]*b3 + F m.[ 5]*b1 - F m.[ 6]*b0) * invDet)
  prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
  val () = p := p+sizeof<GLfloat>

  prval (pf_at, pf_arr) = array_v_uncons {GLfloat?} (pf_arr)
  val () = !p := G (( F m.[ 0]*b3 - F m.[ 1]*b1 + F m.[ 2]*b0) * invDet)
  prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
  val () = p := p+sizeof<GLfloat>

  prval (pf_at, pf_arr) = array_v_uncons {GLfloat?} (pf_arr)
  val () = !p := G ((~F m.[12]*a3 + F m.[13]*a1 - F m.[14]*a0) * invDet)
  prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
  val () = p := p+sizeof<GLfloat>

  prval (pf_at, pf_arr) = array_v_uncons {GLfloat?} (pf_arr)
  val () = !p := G (( F m.[ 8]*a3 - F m.[ 9]*a1 + F m.[10]*a0) * invDet)
  prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
  val () = p := p+sizeof<GLfloat>

  prval () = array_v_unnil {GLfloat?} (pf_arr)
  prval () = view@ n := array_v_of_biarray_v (pf_out)
in
  // nothing
end // end of [mat_invert_into]

(*
implement mat_invert_into (m, n) = let
  #define F float_of_GLfloat; #define G GLfloat_of_float
  // TODO: calculate the adjoint to [m] into [n]
  // TODO: calculate the determinant (as inverse == 1/det * adjoint, adjoint * m == identity * det, so this calculates the det)
  // TODO: multiply all elements of [n] by determinant
in
end // end of [mat_invert_into]
*)
(*
// Adapted from code contributed to Mesa by David Moore (Mesa 7.6 under SGI Free License B - which is MIT/X11-type)
// added helper for common subexpression elimination by eihrul, and other optimizations by div0
int Matrix4x4_Invert_Full (matrix4x4_t *out, const matrix4x4_t *in1)
{
        float det;

        // this seems to help gcc's common subexpression elimination, and also makes the code look nicer
        float   m00 = in1->m[0][0], m01 = in1->m[0][1], m02 = in1->m[0][2], m03 = in1->m[0][3],
                m10 = in1->m[1][0], m11 = in1->m[1][1], m12 = in1->m[1][2], m13 = in1->m[1][3],
                m20 = in1->m[2][0], m21 = in1->m[2][1], m22 = in1->m[2][2], m23 = in1->m[2][3],
                m30 = in1->m[3][0], m31 = in1->m[3][1], m32 = in1->m[3][2], m33 = in1->m[3][3];

        // calculate the adjoint
        out->m[0][0] =  (m11*(m22*m33 - m23*m32) - m21*(m12*m33 - m13*m32) + m31*(m12*m23 - m13*m22));
        out->m[0][1] = -(m01*(m22*m33 - m23*m32) - m21*(m02*m33 - m03*m32) + m31*(m02*m23 - m03*m22));
        out->m[0][2] =  (m01*(m12*m33 - m13*m32) - m11*(m02*m33 - m03*m32) + m31*(m02*m13 - m03*m12));
        out->m[0][3] = -(m01*(m12*m23 - m13*m22) - m11*(m02*m23 - m03*m22) + m21*(m02*m13 - m03*m12));
        out->m[1][0] = -(m10*(m22*m33 - m23*m32) - m20*(m12*m33 - m13*m32) + m30*(m12*m23 - m13*m22));
        out->m[1][1] =  (m00*(m22*m33 - m23*m32) - m20*(m02*m33 - m03*m32) + m30*(m02*m23 - m03*m22));
        out->m[1][2] = -(m00*(m12*m33 - m13*m32) - m10*(m02*m33 - m03*m32) + m30*(m02*m13 - m03*m12));
        out->m[1][3] =  (m00*(m12*m23 - m13*m22) - m10*(m02*m23 - m03*m22) + m20*(m02*m13 - m03*m12));
        out->m[2][0] =  (m10*(m21*m33 - m23*m31) - m20*(m11*m33 - m13*m31) + m30*(m11*m23 - m13*m21));
        out->m[2][1] = -(m00*(m21*m33 - m23*m31) - m20*(m01*m33 - m03*m31) + m30*(m01*m23 - m03*m21));
        out->m[2][2] =  (m00*(m11*m33 - m13*m31) - m10*(m01*m33 - m03*m31) + m30*(m01*m13 - m03*m11));
        out->m[2][3] = -(m00*(m11*m23 - m13*m21) - m10*(m01*m23 - m03*m21) + m20*(m01*m13 - m03*m11));
        out->m[3][0] = -(m10*(m21*m32 - m22*m31) - m20*(m11*m32 - m12*m31) + m30*(m11*m22 - m12*m21));
        out->m[3][1] =  (m00*(m21*m32 - m22*m31) - m20*(m01*m32 - m02*m31) + m30*(m01*m22 - m02*m21));
        out->m[3][2] = -(m00*(m11*m32 - m12*m31) - m10*(m01*m32 - m02*m31) + m30*(m01*m12 - m02*m11));
        out->m[3][3] =  (m00*(m11*m22 - m12*m21) - m10*(m01*m22 - m02*m21) + m20*(m01*m12 - m02*m11));

        // calculate the determinant (as inverse == 1/det * adjoint, adjoint * m == identity * det, so this calculates the det)
        det = m00*out->m[0][0] + m10*out->m[0][1] + m20*out->m[0][2] + m30*out->m[0][3];
        if (det == 0.0f)
                return 0;

        // multiplications are faster than divisions, usually
        det = 1.0f / det;

        // manually unrolled loop to multiply all matrix elements by 1/det
        out->m[0][0] *= det; out->m[0][1] *= det; out->m[0][2] *= det; out->m[0][3] *= det;
        out->m[1][0] *= det; out->m[1][1] *= det; out->m[1][2] *= det; out->m[1][3] *= det;
        out->m[2][0] *= det; out->m[2][1] *= det; out->m[2][2] *= det; out->m[2][3] *= det;
        out->m[3][0] *= det; out->m[3][1] *= det; out->m[3][2] *= det; out->m[3][3] *= det;

        return 1;
}
*)

(* ****** ****** *)

implement mat_rot (m, a, x, y, z) = let
  fn go (
      m: &GLmat4, a: GLfloat, x: GLfloat, y: GLfloat, z: GLfloat
    ) :<> void = let
    val a = float_of a
    val x = float_of x and y = float_of y and z = float_of z
    val s = sinf a and c = cosf a
    #define G GLfloat
    var !p_r = @[GLfloat](
      G (x * x * (1.0f - c) + c),     G (y * x * (1.0f - c) + z * s), G (x * z * (1.0f - c) - y * s), G 0.0f,
      G (x * y * (1.0f - c) - z * s), G (y * y * (1.0f - c) + c),     G (y * z * (1.0f - c) + x * s), G 0.0f, 
      G (x * z * (1.0f - c) + y * s), G (y * z * (1.0f - c) - x * s), G (z * z * (1.0f - c) + c), G 0.0f,
      G 0.0f, G 0.0f, G 0.0f, G 1.0f
    ) // end of [var]
  in
    mat_mult (m, !p_r)
  end // end of [go]
in
  go (m, a, x, y, z)
end // end of [mat_rot]

(* ****** ****** *)

implement mat_trn (m, x, y, z) = let
  fn go (m: &GLmat4, x: GLfloat, y: GLfloat, z: GLfloat):<> void = let
    val S = (GLfloat)1.0f and Z = (GLfloat)0.0f
    var !p_t = @[GLfloat](S,Z,Z,Z,  Z,S,Z,Z, Z,Z,S,Z, x,y,z,S)
    // S Z Z x
    // Z S Z y
    // Z Z S z
    // Z Z Z S
  in
    mat_mult (m, !p_t)
  end // end of [go]
in
  go (m, x, y, z)
end // end of [mat_trn]

(* ****** ****** *)

absviewtype arrinit_vt (a:viewt@ype, m:int, n:int, l:addr)

extern
fun arrinit_ptr_start {a:viewt@ype} {n:nat} {l:addr} (
  pf_arr: array_v (a?, n, l) | p_arr: ptr l
) :<> arrinit_vt (a, n, 0, l)

extern
fun{a:viewt@ype} arrinit_ptr_extend {m:nat} {n:nat | n < m} {l:addr} (
 ai: arrinit_vt (a, m, n, l)
, x: a
) :<> arrinit_vt (a, m, n+1, l)

extern
fun arrinit_ptr_end {a:viewt@ype} {n:nat} {l:addr} (
  ai: arrinit_vt (a, n, n, l)
) :<> (array_v (a, n, l) | void)

local

assume arrinit_vt (
  a:viewt@ype
, m:int
, n:int
, l:addr
) = [n <= m] [ofs:int] @{
  pf_ini= array_v (a, n, l) // array_v (a, n, l+ofs)
, pf_mul= MUL (n, sizeof a, ofs) // MUL (m-n, sizeof a, ofs)
, pf_rst= array_v (a?, m-n, l+ofs) // array_v (a?, m-n, l)
, p_rst= ptr (l+ofs)
} // end of [assume]

in // in of [local]

implement arrinit_ptr_start {a} {n} {l} (pf_arr | p_arr) = @{
  pf_ini= array_v_nil {a} ()
, pf_mul= mul_make {0,sizeof a} ()
, pf_rst= pf_arr
, p_rst= p_arr
} // end of [arrinit_ptr_start]

implement{a} arrinit_ptr_extend {m} {n} {l} (ai, x) = let
  prval (pf_at, pf_rst) = array_v_uncons {a?} (ai.pf_rst)
  var p = ai.p_rst
  val () = !p := x
in  @{
  pf_ini= array_v_extend {a} (ai.pf_mul, ai.pf_ini, pf_at)
, pf_mul= MULind (ai.pf_mul)
, pf_rst= pf_rst
, p_rst= p + sizeof<a>
}
end // end of [arrinit_ptr_extend]

implement arrinit_ptr_end {a} {n} {l} (ai) = let
  prval () = array_v_unnil {a?} (ai.pf_rst)
in
  (ai.pf_ini | ())
end // end of [arrinit_ptr_end]

end // end of [local]

implement mat_id (m) = let
  val Z = (GLfloat)0.0f
  val S = (GLfloat)1.0f
  infixl *.
  #define *. arrinit_ptr_extend
  val (pf_ai | ()) = arrinit_ptr_end (arrinit_ptr_start {GLfloat} (view@ m | &m)
    *. S *. Z *. Z *. Z
    *. Z *. S *. Z *. Z
    *. Z *. Z *. S *. Z
    *. Z *. Z *. Z *. S)
  prval () = view@ m := pf_ai
in
 // nothing
end // end of [mat_id]

implement mat_transp (m) = let
  var tmp: GLfloat // uninitialized
  macdef swap (x, y) = (tmp := ,(x); ,(x) := ,(y); ,(y) := tmp)
in
(* 0  4  8  12
   1  5  9  13
   2  6 10  14
   3  7 11  15 *)
  swap (m.[1], m.[4]);
  swap (m.[2], m.[8]);
  swap (m.[3], m.[12]);
  swap (m.[6], m.[9]);
  swap (m.[7], m.[13]);
  swap (m.[11], m.[14])
end // end of [mat_transp]

implement mat_persp (fovy, aspect, near, far, m) = let
  fn go (
      fovy: float, aspect: float, near: float, far: float
    , m: &GLmat4? >> GLmat4
    ):<> void = let
    val ymax = near * tanf (fovy * float_of M_PI / 180.0f)
    val ymin = ~ymax
    val xmin = ymin * aspect
    val xmax = ymax * aspect
    val Z = GLfloat 0.0f
    #define G GLfloat_of_float
    infixl *.
    #define *. arrinit_ptr_extend
    val (pf_ai | ()) = arrinit_ptr_end (arrinit_ptr_start {GLfloat} (view@ m | &m)
           *. G ((2.0f * near) / (xmax - xmin)) *. Z *. Z *. Z
           *. Z *. G ((2.0f * near) / (ymax - ymin)) *. Z *. Z
           *. G ((xmax + xmin) / (xmax - xmin)) *. G ((ymax + ymin) / (ymax - ymin)) *. G (~(far + near) / (far - near)) *. G ~1.0f
           *. Z *. Z *. G (~(2.0f * far * near) / (far - near)) *. Z)
    prval () = view@ m := pf_ai
  in
    // nothing
  end // end of [go]
in
  go (fovy, aspect, near, far, m)
end // end of [mat_persp]

(* ****** ****** *)

implement mat_prerr (m) = let
  #define F float_of_GLfloat
in
  prerr (F m.[0]); prerr " "; prerr (F m.[4]); prerr " "; prerr (F m.[8]); prerr " "; prerr (F m.[12]); prerr_newline ();
  prerr (F m.[1]); prerr " "; prerr (F m.[5]); prerr " "; prerr (F m.[9]); prerr " "; prerr (F m.[13]); prerr_newline ();
  prerr (F m.[2]); prerr " "; prerr (F m.[6]); prerr " "; prerr (F m.[10]); prerr " "; prerr (F m.[14]); prerr_newline ();
  prerr (F m.[3]); prerr " "; prerr (F m.[7]); prerr " "; prerr (F m.[11]); prerr " "; prerr (F m.[15]); prerr_newline ()
end // end of [mat_prerr]

(* ****** ****** *)

(* end of [mat4.dats] *)
