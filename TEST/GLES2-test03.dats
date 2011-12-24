(*
** In this demo, buffer objects are used for specifying
** mesh vertices as well as connectivity.
**
** New features in this example:
** - mesh loading (pass [-obj <filename>] as a parameter)
** - setting up modelview and projection matrices for 3D
** - (primitive) camera control: use arrows and 'a'/'z' to spin the camera
**   (mesh has fixed position and orientation)
** - multiple GLSL programs for supporting different
**   rendering modes (from solid and unlit to lit and texture-mapped)
**
** TODO:
** - animated lighting?
*)
//
// Author: Artyom Shakhakov (artyom DOT shalkhakov AT gmail DOT com)
// Time: June, 2011
// Modified in November-December, 2011
//

(* ****** ****** *)

staload "libc/SATS/math.sats" // for [M_PI]
staload _(*anonymous*) = "prelude/DATS/list_vt.dats"
staload _(*anonymous*) = "prelude/DATS/array.dats"
staload _(*anonymous*) = "prelude/DATS/reference.dats"

(* ****** ****** *)

staload "contrib/GLES2/SATS/gl2.sats"

(* ****** ******* *)

staload "SATS/obj.sats"
staload "SATS/mat4.sats"
staload "SATS/util.sats"

(* ****** ******* *)

staload V = "libats/SATS/vector.sats"
staload _(*anonymous*) = "libats/DATS/vector.dats"

stadef VSHELL = $V.VSHELL
stadef VSHELL0 = $V.VSHELL0
stadef VECTOR = $V.VECTOR
stadef VECTOR1 = $V.VECTOR1

staload H = "libats/SATS/hashtable_chain.sats"
staload _(*anonymous*) = "libats/DATS/hashtable_chain.dats"

stadef HASHTBLptr = $H.HASHTBLptr

(* ****** ******* *)

#define i2s size1_of_int1

(* ****** ****** *)

%{
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <EGL/egl.h>
%}

// HACKHACKHACK
%{^
extern ats_void_type mainats (
  ats_int_type argc, ats_ptr_type argv
);
%}

(* ****** ****** *)

var winx = 0
val (pf_view_winx | ()) =
  vbox_make_view_ptr {int} (view@ winx | &winx)
// end of [val]

var winy = 0
val (pf_view_winy | ()) =
  vbox_make_view_ptr {int} (view@ winy | &winy)
// end of [val]

(* ****** ****** *)

val attr_pos = (GLuint)(uint_of 0)
val attr_norm = (GLuint)(uint_of 1)
val attr_texc = (GLuint)(uint_of 2)

(* ****** ****** *)

var view_rotx = (GLfloat)0.0f
val (pf_view_rotx | ()) =
  vbox_make_view_ptr {GLfloat} (view@ view_rotx | &view_rotx)
// end of [prval]
var view_roty = (GLfloat)0.0f
val (pf_view_roty | ()) =
  vbox_make_view_ptr {GLfloat} (view@ view_roty | &view_roty)
// end of [prval]
var view_rotz = (GLfloat)0.0f
val (pf_view_rotz | ()) =
  vbox_make_view_ptr {GLfloat} (view@ view_rotz | &view_rotz)
// end of [prval]

extern
fun keypress (code: natLt 6): void = "keypress"
implement keypress (code) = let
  fn add {l:addr} (
      pf: vbox (GLfloat @ l)
    | p: ptr l, d: float
    ) :<!ref> void = let
    prval vbox pf_at = pf
  in
    !p := GLfloat (float_of !p + d)
  end // end of [add]
in
  case+ code of
  | 0 => add (pf_view_roty | &view_roty, 5.0f)
  | 1 => add (pf_view_roty | &view_roty, ~5.0f)
  | 2 => add (pf_view_rotx | &view_rotx, 5.0f)
  | 3 => add (pf_view_rotx | &view_rotx, ~5.0f)
  | 4 => add (pf_view_rotz | &view_rotz, ~5.0f)
  | 5 => add (pf_view_rotz | &view_rotz, 5.0f)
end // end of [keypress]

(* ****** ****** *)

dynload "obj_lats.dats"

(* ****** ****** *)

(*
// Some other time, perhaps...
extern
fun glVertexAttribPointer {a:t@ype} {n,k:nat} {ofs:int} (
  pf_mul: MUL (k, sizeof a, ofs)
| indx: GLuint
, size: GLint n
, type: GLenum_type a
, normalized: GLboolean
, stride: GLsizei ofs
, p: &GEVEC (a, n, k)
) : void
  = "mac#atsctrb_glVertexAttribPointer"
// end of [glVertexAttribPointer]
*)

(* ****** ****** *)

// program and uniform locations
// (each such uniform is a free variable)
viewtypedef pbind = @{
  p= GLprogram
  // NOTE: -1 means "does not exist in a program",
  // and passing it to the corresponding functions
  // will have no effect (incl. no errors raised by GL)
, l_mv= GLint   // model-view matrix
, l_mvp= GLint  // model-view-projection matrix
  // normal matrix (transpose of the inverse of the upper
  // leftmost 3x3 of model-view matrix)
, l_nm= GLint
, l_lpos= GLint
, l_epos= GLint
, l_kd= GLint  // constant diffuse color
, l_tex= GLint // texture unit (for sampler)
} // end of [pbind]

// we need flat matrices embedded in records
viewtypedef GLmat3 = @[GLfloat][9]
viewtypedef GLmat3_t = $extype_struct "GLmat3_t" of {
  m= @[GLfloat][9]
} // end of [GLmat3_t]
%{^
typedef struct GLmat3_s {
  GLfloat m[9];
} GLmat3_t ;
%} // end of [%{^]

viewtypedef GLmat4_t = $extype_struct "GLmat4_t" of {
  m= @[GLfloat][16]
} // end of [GLmat4_t]
%{^
typedef struct GLmat4_s {
  GLfloat m[16];
} GLmat4_t ;
%} // end of [%{^]

// uniform environment (mapping from uniform
// locations to values)
// NOTE: some locations are supplied with constants
// supplied from material, these are not listed here
viewtypedef uenv = @{
  mv= GLmat4_t
, mvp= GLmat4_t
, nm= GLmat3_t
, lpos= float3_t
, epos= float3_t
} // end of [uenv]

viewtypedef progs= @{
  sim= pbind    // simple: unlit and no texture mapping
, lit= pbind    // lit but no texture mapping
, tm= pbind     // unlit but texture-mapped
, lit_tm= pbind // lit and texture-mapped
} // end of [progs]

fun create_shaders (res: &progs? >> progs): void = let
  fn upload_program (vs: string, fs: string, res: &pbind? >> pbind): void = let
    var stat: GLint
    val fragShader = glCreateShader GL_FRAGMENT_SHADER
    val () = shader_from_file (fragShader, fs)
    val vertShader = glCreateShader GL_VERTEX_SHADER
    val () = shader_from_file (vertShader, vs)
    val program = glCreateProgram ()
  in
    glAttachShader (program, vertShader);
    glAttachShader (program, fragShader);
    glLinkProgram program;
    glGetProgramiv (program, GL_LINK_STATUS, stat);
    if int_of_GLint stat = 0 then let
      var !p_log with pf_log = @[byte][1000]()
      var len: GLsizei // uninitialized
      prval pf_len = Some_v (view@ (len))
      val () = glGetProgramInfoLog (pf_log, pf_len | program, (GLsizei)1000, &len, p_log)
      prval Some_v pf = pf_len
      prval () = view@ (len) := pf
      val () = begin
        print ("Error: linking:\n");
        fprint_strbuf (stdout_ref, !p_log);
        print_newline ()
      end // end of [begin]
      prval () = pf_log := bytes_v_of_strbuf_v (pf_log)
    in
      exit {void} (1)
    end; // end of [if]
    glUseProgram program;
    // bind attribute locations
    glBindAttribLocation (program, attr_pos, "pos");
    glBindAttribLocation (program, attr_norm, "norm");
    glBindAttribLocation (program, attr_texc, "texcoord");
    glLinkProgram program; // needed to put attribs into effect
    // get uniform locations
    res.l_mv := glGetUniformLocation (program, "mv");
    res.l_mvp := glGetUniformLocation (program, "mvp");
    res.l_nm := glGetUniformLocation (program, "nm");
    res.l_lpos := glGetUniformLocation (program, "lpos");
    res.l_epos := glGetUniformLocation (program, "epos");
    res.l_kd := glGetUniformLocation (program, "kd");
    res.l_tex := glGetUniformLocation (program, "tex");
    res.p := program;
    // shader objects are reference-counted
    glDeleteShader vertShader;
    glDeleteShader fragShader
  end // end of [upload_program]
in
  upload_program ("test03.vert", "test03.frag", res.sim);
  upload_program ("test03-l.vert", "test03-l.frag", res.lit);
  upload_program ("test02.vert", "test02.frag", res.tm);
  upload_program ("test03-tl.vert", "test03-tl.frag", res.lit_tm)
end // end of [create_shaders]

fun destroy_shaders (progs: progs): void = let
  fun f (x: pbind): void = glDeleteProgram (x.p)
in
  f (progs.sim);
  f (progs.lit);
  f (progs.tm);
  f (progs.lit_tm)
end // end of [destroy_shaders]

fn program_unapply (
  pbind: &pbind
, tex: !GLtexture0
): void =
  // TODO: glUseProgram (0)?
  if GLtexture_isnot_zero tex then begin
    glDisable GL_TEXTURE_2D
  end // end of [program_unapply]

fn program_apply (
  pbind: &pbind, ue: &uenv, kd: &float3_t, tex: !GLtexture0
): void = let
  fun umat4 (loc: GLint, m: &GLmat4): void = () where {
    prval pf_mat1 = array_v_sing (view@ m)
    val p = &m
    val () = glUniformMatrix4fv (loc, (GLsizei)1, GL_FALSE, !p)
    prval () = view@ m := array_v_unsing pf_mat1
  } // end of [umat4]
  fun umat3 (loc: GLint, m: &GLmat3): void = () where {
    prval pf_mat1 = array_v_sing (view@ m)
    val p = &m
    val () = glUniformMatrix3fv (loc, (GLsizei)1, GL_FALSE, !p)
    prval () = view@ m := array_v_unsing pf_mat1
  } // end of [umat3]
  fun uvec3 (loc: GLint, v: &float3_t): void =
    glUniform4f (loc, (GLfloat)v.0, (GLfloat)v.1, (GLfloat)v.2, (GLfloat)1.0f)
  // end of [uvec3]
in
  glUseProgram pbind.p;
  umat4 (pbind.l_mv, ue.mv.m);
  umat4 (pbind.l_mvp, ue.mvp.m);
  umat3 (pbind.l_nm, ue.nm.m);
  uvec3 (pbind.l_lpos, ue.lpos);
  uvec3 (pbind.l_epos, ue.epos);
  uvec3 (pbind.l_kd, kd);
  if GLtexture_isnot_zero tex then begin
    glEnable GL_TEXTURE_2D;
    glActiveTexture GL_TEXTURE0;
    glUniform1i (pbind.l_tex, (GLint)0);
    glBindTexture (GL_TEXTURE_2D, tex)
  end // end of [if]
end // end of [program_apply]

(* ****** ****** *)
// External function prototypes.
// This is basically how we upload data to the GL: via calling
// external functions introduced for our ad-hoc purposes.
// The GL API defines some entry points which are difficult to handle
// in ATS: while it may be done, the effort of programming against
// such bindings would be too high.

%{^
ats_GLuint_type cast_size_to_GLuint (ats_size_type x) { return x; }

void
glBufferData_convert (
  ats_GLenum_type target
, ats_GLenum_type type
, ats_size_type tsz
, ats_size_type sz
, ats_ptr_type data
, ats_GLenum_type usage
) {
  glBufferData (target, sz * tsz, (void *)data, usage);
  return;
} // end of [glBufferData_convert]

void glBufferDataF3 (
  ats_size_type n
, ats_ref_type data
, ats_GLenum_type usage
) {
  glBufferData (GL_ARRAY_BUFFER, n * sizeof(float) * 3, (void *)data, usage);
  return;
}

void glVertexAttribPointerBuffer (
  ats_GLuint_type indx
, ats_GLsizei_type size
, ats_GLenum_type type
, ats_GLsizei_type stride
, ats_GLsizeiptr_type pos
) {
  glVertexAttribPointer (indx, size, type, GL_FALSE, stride, (void *)pos);
  return;
} // end of [glVertexAttribPointerBuffer]

void glDrawElementsBuffer (
  ats_GLenum_type mode
, ats_GLsizei_type count
, ats_GLenum_type type
) {
  glDrawElements (mode, count, type, NULL);
  return;
} // end of [glDrawElementsBuffer]

%}

extern
fun cast_size_to_GLuint (x: size_t):<> GLuint = "cast_size_to_GLuint"

extern
fun glBufferData {n:nat} {a:t@ype} (
  target: GLenum, type: GLenum_type a
, tsz: size_t (sizeof a)
, sz: size_t n, data: &(@[a][n]), usage: GLenum
) : void
  = "glBufferData_convert"
// end of [fun]

extern
fun glBufferDataF3 {n:nat} (
  n: size_t n, p: &(@[float3_t][n])
, usage: GLenum
) : void
  = "glBufferDataF3"
// end of [fun]

extern
fun glVertexAttribPointerBuffer {a:t@ype} (
  indx: GLuint
, size: GLsizei
, type: GLenum_type a
, stride: GLsizei
, pos: GLsizeiptr
) : void
  = "glVertexAttribPointerBuffer"
// end of [glVertexAttribPointerBuffer]

extern
fun glDrawElementsBuffer {a:t@ype} (
  mode: GLenum, count: GLsizei, type: GLenum_type a
) : void
  = "glDrawElementsBuffer"
// end of [glDrawElementsBuffer]

(* ****** ****** *)

absviewtype gpumesh // abstract

local

// vertex structure
// (when the input mesh has no normals or no texture
// coordinates, some space is wasted)
typedef vert = (* $extype_struct "vert_type" of *) @{
  pos= float3_t // vertex position
, nrm= float3_t // vertex normal
, tc0= float2_t // texture coordinate for unit 0
} // end of [vert]

// index buffer
viewtypedef gpuidx = @{
  buf= GLbuffer  // buffer object (storage)
, mode= GLenum   // GL_TRIANGLES, ...
, count= GLsizei // total count of indices
// TODO: support this as well!
, lo= GLsizei    // minimum vertex index
, hi= GLsizei    // maximum vertex index
, type= [a:t@ype] GLenum_type a  // GL_UNSIGNED_INT, ...
} // end of [gpuidx]

// vertex buffer
viewtypedef gpuvrt = @{
  buf= GLbuffer                 // buffer object (storage)
, stride= GLsizei               // buffer stride (sizeof<vert>)
, type= [a:t@ype] GLenum_type a // GL_FLOAT
, ofspos= GLsizeiptr            // offset to position
, szpos= GLsizei                // position size (3)
, ofsnrm= GLsizeiptr            // offset to normal
, sznrm= GLsizei                // normal size (3)
, ofstc0= GLsizeiptr            // offset to texcoords for unit 0
, sztc0= GLsizei                // texcoords size (2)
// fixed layout:
// pos:@[GLfloat][3], nrm:@[GLfloat][3], tc0:@[GLfloat][2]
// where comma means "adjacent in memory"
// FIXME: do not forget to change this when you modify [vert]
, use_nrm= bool // normals present?
, use_tc0= bool // texture coordinates for MTU0 present?
} // end of [gpuvrt]

// all GPU surfaces share a single vertex buffer
viewtypedef gpusrf (nm:int) = @{
  mtl= sizeLt nm
, ib= gpuidx
} // end of [gpusrf]

viewtypedef gpumtl = @{
  id= strptr1        // for informational purposes
, kd= float3_t       // constant diffuse color
, map_kd= GLtexture0 // only diffuse mapping ATM
} // end of [gpumtl]

// upload the textures
fun mtllib_upload {nm:nat} (
  mtllib: &arrsz_vt (mtl, nm)
, mtls: &VSHELL0? >> VECTOR1 (gpumtl, nm)
) :<!exn,!ntm,~ref> void = let
  val () = $V.vector_initialize<gpumtl> (mtls, mtllib.2)
  val () = loop (mtllib.0 | mtls, mtllib.3, mtllib.2) where {
    fun loop {i:nat | i <= nm} {l:addr} .<i>. (
      pf_arr: !array_v (mtl, i, l)
    | mtls: &VECTOR (gpumtl, nm, nm-i) >> VECTOR (gpumtl, nm, nm)
    , p: ptr l, i: size_t i
    ) :<!exn,~ref> void =
      if i = 0 then let
        prval () = array_v_unnil {mtl} (pf_arr)
        prval () = pf_arr := array_v_nil {mtl} ()
      in
        // nothing
      end else let
        prval (pf_at, pf1_arr) = array_v_uncons {mtl} (pf_arr)
        var tex: GLtexture0 // uninitialized
        fn tex_upload (tex: &GLtexture0? >> GLtexture0, map: !strptr0):<> void =
          case+ :(tex: GLtexture0) => strptr_isnot_null map of
          | true => $effmask_all (glGenTexture tex; texture_from_file (tex, map))
          | false => tex := GLtexture_zero ()
        // end of [tex_upload]
        val () = tex_upload (tex, p->map_kd)
        val () = $V.vector_append<gpumtl> (mtls, @{id= strptr_dup p->id, kd= p->kd, map_kd= tex})
        val () = loop (pf1_arr | mtls, p+sizeof<mtl>, i-1)
        prval () = pf_arr := array_v_cons {mtl} (pf_at, pf1_arr)
      in
        // nothing
      end // end of [loop]
  } // end of [where]
in
  // nothing
end // end of [mtllib_upload]

// upload the whole mesh geometry
extern
fun geom_upload {nm,nv,nn,ntc,ns:nat} (
  invrt: &arrsz_vt (float3_t, nv)
, innrm: &arrsz_vt (float3_t, nn)
, intc0: &arrsz_vt (float2_t, ntc)
, insfs: &arrsz_vt (surf (nm, nv, nn, ntc), ns)
, outvbo: &gpuvrt? >> gpuvrt
, outsfs: &List_vt (gpusrf nm)? >> list_vt (gpusrf nm, ns)
) :<!exn,!ntm,~ref> void

implement geom_upload {nm,nv,nn,ntc,ns} (invrt, innrm, intc0, insfs, outvbo, outsfs) = let
  typedef key = faceidx (nv, nn, ntc)
  typedef itm = GLuint // not indexed

  val [l_ht:addr] ht = $effmask_all ($H.hashtbl_make_hint {key,itm} (hash, eq, invrt.2)) where {
    fn s2ul (x: size_t):<> ulint = ulint_of_int (int_of_size x)
    implement $H.hash_key<key> (x, _) = let
      fn step (seed: ulint, x: size_t):<> ulint =
        // taken from Boost.Functional.Hash
        (seed lxor (s2ul x))
        + (ulint_of_uint 0x9e3779b9u) + (seed << 6) + (seed >> 2)
      // end of [step]    
    in
      step (step (step (ulint_of_int 0, x.vidx), x.nidx), x.tidx)
    end // end of [$H.hash_key]
    implement $H.equal_key_key<key> (x1, x2, _) =
      x1.vidx = x2.vidx && x1.nidx = x2.nidx && x1.tidx = x2.tidx
    // end of [$H.equal_key_key]
    // don't need these (see specializations above)
    fn hash (x: key):<cloref> ulint = ulint_of_int 0
    fn eq (x1: key, x2: key):<cloref> bool = false
  } // end of [where]

  viewtypedef S (nf:int) = surf (nm, nv, nn, ntc, nf)
  // it is an array of surfaces
  dataview surfarr_v (int, int, int, addr) =
    | {nf,ni:nat} {m,n,k:nat} {l:addr} surfarr_v_cons (m+ni, max (n, ni), k+1, l) of (
        MUL (nf, 3, ni), S (nf) @ l, surfarr_v (m, n, k, l+sizeof surf0)
      ) // end of [surfarr_v_cons]
    | {l:addr} surfarr_v_nil (0, 0, 0, l)
  // end of [surfarr_v]

  prval pf_surfs = surfarr_v_of_array_v (insfs.0) where {
    extern prfun surfarr_v_of_array_v {k:int} {l:addr} (
      pf: array_v (surf (nm, nv, nn, ntc), k, l)
    ):<> [m,n:nat] surfarr_v (m, n, k, l)
  } // end of [where]

  var nidx = i2s 0 and idxm = i2s 0
  local
    fun loop {m,n:int} {i:nat} {a,b:nat} {l:addr} .<i>. (
      pf: !surfarr_v (m, n, i, l) | p: ptr l, i: size_t i
    , a: &size_t a >> size_t (a+m)
    , b: &size_t b >> size_t (max (b, n))
    ) :<> void = if i > 0 then let
      prval surfarr_v_cons (pf1_mul, pf_at, pf1) = pf
      val (pf2_mul | nidx1) = !p.faces.2 szmul2 3
      prval () = mul_isfun (pf1_mul, pf2_mul)
      prval () = mul_nat_nat_nat pf2_mul
      val () = a := a + nidx1
      val () = b := max (b, nidx1)
      val () = loop (pf1 | p+sizeof<surf0>, i-1, a, b)
      prval () = mul_nat_nat_nat pf1_mul
      prval () = pf := surfarr_v_cons (pf1_mul, pf_at, pf1)
    in
      // nothing
    end else let
      prval surfarr_v_nil () = pf
      prval () = pf := surfarr_v_nil () in
      // nothing
    end // end of [loop]
  in // of [local]
    val () = loop (pf_surfs | insfs.3, insfs.2, nidx, idxm)
  end // end of [local]
  stavar nidx:int; val nidx = nidx: size_t nidx
  stavar idxm:int; val idxm = idxm: size_t idxm

  viewtypedef HASHTBL = HASHTBLptr (key, itm, l_ht)
  viewtypedef VRT = arrsz_vt (float3_t, nv)
  viewtypedef NRM = arrsz_vt (float3_t, nn)
  viewtypedef TC0 = arrsz_vt (float2_t, ntc)

  // TODO, perhaps:
  //   ht: !HASHTBLptr (key, GLuintLt onv, l_ht) >> HASHTBLptr (key, GLuintLt onv', l_ht)
  //   out_verts: &VECTOR (vert, nf3, onv) >> VECTOR (vert, nf3, onv')
  // where [onv <= onv' <= nf3]
  // TODO: replace [vc] by [nf3]

  var out_idx: VSHELL0 // uninitialized
  val () = $V.vector_initialize<GLuint> (out_idx, idxm)
  var out_vrt: VSHELL0 // uninitialized
  val () = $V.vector_initialize<vert> (out_vrt, nidx)

  extern
  fun glBufferData {a:t@ype} {n:nat} {l:addr} (
    pf_data: !array_v (a, n, l)
  | target: GLenum, type: GLenum_type a
  , tsz: sizeof_t a
  , sz: size_t n, data: ptr l, usage: GLenum
  ) : void
    = "glBufferData_convert"
  // end of [fun]

  val () = loop0 (pf_surfs | ht, invrt, innrm, intc0, insfs.3, insfs.2, out_vrt, out_idx, outsfs) where {
    fun loop0 {m,n,k,vs:nat | m <= nidx-vs; n <= idxm} {l:addr} .<k>. (
      pf: !surfarr_v (m, n, k, l)
    | ht: !HASHTBL
    , invrt: &VRT, innrm: &NRM, intc0: &TC0
    , p: ptr l, i: size_t k
    , gpuvrt: &VECTOR (vert, nidx, vs) >> VECTOR (vert, nidx, vs')
    , gpuidx: &VECTOR (itm, idxm, 0)
    , gpu_sfs: &List_vt (gpusrf nm)? >> list_vt (gpusrf nm, k)
    ) :<!exn,~ref> #[vs':nat | vs'-vs <= m] void = if i > 0 then let
      prval surfarr_v_cons (pf_mul, pf_at, pf1) = pf
      stavar nf:int; stavar nidx1:int
      prval pf_mul = pf_mul: MUL (nf, 3, nidx1)
      val () = gpu_sfs := list_vt_cons {gpusrf nm} {0} (?, ?)
      val+ list_vt_cons (!p_gsrf, !p_gs) = gpu_sfs
      typedef T = triangle (nv, nn, ntc)

      fun loop1
        {nf',nidx':nat}
        {is:nat | is+nidx' <= idxm}
        {vs:nat | vs+nidx' <= nidx} {l:addr} .<nf'>. (
        pf_mul: MUL (nf', 3, nidx')
      , pf_tris: !array_v (T, nf', l)
      | ht: !HASHTBL
      , invrt: &VRT, innrm: &NRM, intc0: &TC0, p: ptr l, n: size_t nf'
      , gpuvrt: &VECTOR (vert, nidx, vs) >> VECTOR (vert, nidx, vs')
      , gpuidx: &VECTOR (itm, idxm, is) >> VECTOR (itm, idxm, is+nidx')
      ) :<!exn,~ref> #[vs':nat | vs' <= vs+nidx'] void = let
        fn idx_of_faceidx {vss:nat | vss < vs+3; vs+3 <= nidx} (
          ht: !HASHTBL
        , c: key
        , in_verts: &VRT, in_norms: &NRM, in_texcoords: &TC0
        , out_verts: &VECTOR (vert, nidx, vss) >> VECTOR (vert, nidx, vss')
        ):<> #[vss':nat | vss' <= vss+1] itm = let
          var res: itm // uninitialized
        in
          if :(res: itm) => $H.hashtbl_search<key,itm> (ht, c, res) then let
            prval () = opt_unsome {itm} (res) in
            res
          end else let
            prval () = opt_unnone {itm} (res)
            val (pf_verts | p_verts) = (in_verts.0 | in_verts.3)
            val (pf_norms | p_norms) = (in_norms.0 | in_norms.3)
            val (pf_texcoords | p_texcoords) = (in_texcoords.0 | in_texcoords.3)
            val v = @{
              pos= !p_verts.[c.vidx]
              // TODO: use NaNs instead of zeroes?
            , nrm= if in_norms.2 > 0 then !p_norms.[c.nidx] else @(0.0f, 0.0f, 0.0f)
            , tc0= if in_texcoords.2 > 0 then let
                val st = !p_texcoords.[c.tidx] in
                @(st.0, st.1)
              end else @(0.0f, 0.0f)
            } // end of [val]
            prval () = in_verts.0 := pf_verts
            prval () = in_norms.0 := pf_norms
            prval () = in_texcoords.0 := pf_texcoords
            val () = $V.vector_append<vert> (out_verts, v)
            val () = res := GLuint_of_uint (uint_of_size ($V.vector_size out_verts - 1))
          in
            $H.hashtbl_insert<key,itm> (ht, c, res); res
          end // end of [if]
        end // end of [idx_of_faceidx]
      in
        if n = 0 then let
          prval MULbas () = pf_mul
        in
          // nothing
        end else let
          prval () = mul_pos_pos_pos (pf_mul)
          prval MULind pf1_mul = pf_mul
          stavar nf1:int; stavar indx11:int
          prval pf1_mul = pf1_mul: MUL (nf1, 3, indx11)
          prval (pf_tri, pf1_tris) = array_v_uncons {T} (pf_tris)
          // handle the three indices
          val (a, b, c) = !p
          macdef ap (x) = $V.vector_append<itm> (
            gpuidx, idx_of_faceidx (ht, ,(x), invrt, innrm, intc0, gpuvrt)
          ) // end of [macdef]
        in
          ap a;
          ap b;
          ap c;
          loop1 (pf1_mul, pf1_tris | ht, invrt, innrm, intc0, p+sizeof<T>, n-1, gpuvrt, gpuidx);
          () where { prval () = pf_tris := array_v_cons (pf_tri, pf1_tris) };
        end // end of [if]
      end // end of [loop1]
      val () = p_gsrf->ib.lo := GLsizei_of_size1 ($V.vector_size gpuvrt);
      val () = loop1 {nf,nidx1} {0} (pf_mul, !p.faces.0
        | ht, invrt, innrm, intc0
        , !p.faces.3, !p.faces.2, gpuvrt, gpuidx)
      // end of [val]
      prval (pf2_mul, (pf1_arr, pf2_arr)) = (pf_mul, $V.vector_v_decode {itm} (pf_mul, pf_vec)) where {
        prval pf_vec = $V.VECTOR_decode {itm} (gpuidx)
        stavar m:int; stavar n:int; stavar l:addr
        prval pf_vec = pf_vec: $V.vector_v (itm, m, n, l)
        prval pf_mul = mul_istot {n,sizeof itm} ()
      } // end of [where]
    in
      $effmask_all (
        // this is highly unlikely to interact with any of our
        // shared resources
        glGenBuffer p_gsrf->ib.buf;
        glBindBuffer (GL_ELEMENT_ARRAY_BUFFER, p_gsrf->ib.buf);
        glBufferData (
          pf1_arr
        | GL_ELEMENT_ARRAY_BUFFER, GL_UNSIGNED_INT, sizeof<GLuint>
        , gpuidx.n, gpuidx.ptr, GL_STATIC_DRAW
        ));
      p_gsrf->ib.mode := GL_TRIANGLES;
      p_gsrf->ib.count := GLsizei_of_size1 gpuidx.n;
      p_gsrf->ib.type := GL_UNSIGNED_INT;
      p_gsrf->ib.hi := GLsizei_of_size1 ($V.vector_size gpuvrt);
      p_gsrf->mtl := p->mtl;
      () where { prval () = $V.VECTOR_encode {itm} ($V.vector_v_encode {itm} (pf2_mul, pf1_arr, pf2_arr) | gpuidx) };
      $V.vector_clear (gpuidx);
      loop0 (pf1
      | ht, invrt, innrm, intc0
      , p+sizeof<surf0>, i-1, gpuvrt, gpuidx, !p_gs);
      pf := surfarr_v_cons (pf_mul, pf_at, pf1);
      fold@ gpu_sfs
    end else let
      prval surfarr_v_nil () = pf
    in
      pf := surfarr_v_nil ();
      gpu_sfs := list_vt_nil ()
    end // end of [loop]
  } // end of [where]

  val () = () where {
    prval (pf_mul, (pf1_arr, pf2_arr)) = (pf_mul, $V.vector_v_decode {vert} (pf_mul, pf_vec)) where {
      prval pf_vec = $V.VECTOR_decode {vert} (out_vrt)
      stavar m:int; stavar n:int; stavar l:addr
      prval pf_vec = pf_vec: $V.vector_v (vert, m, n, l)
      prval pf_mul = mul_istot {n,sizeof vert} ()
    } // end of [where]
    val (pf1_mul | n8) = (out_vrt.n) szmul2 8
    prval (fpf, pf_arr) = __coerce (pf1_mul, pf1_arr) where {
      extern prfun __coerce {m,n:nat} {mn:int} {l:addr} (
        pf1_mul: MUL (m, n, mn), pf_arr: array_v (vert, m, l)
      ):<> (array_v (GLfloat, mn, l) -<lin,prf> array_v (vert, m, l), array_v (GLfloat, mn, l))
    } // end of [where]
    val () = begin
      $effmask_all (
        // this is highly unlikely to borrow any of our shared
        // resources
        glGenBuffer outvbo.buf;
        glBindBuffer (GL_ARRAY_BUFFER, outvbo.buf);
        glBufferData (
          pf_arr
        | GL_ARRAY_BUFFER, GL_FLOAT, sizeof<GLfloat>
        , n8, out_vrt.ptr, GL_STATIC_DRAW
        ));
      outvbo.stride := GLsizei_of_size1 (sizeof<GLfloat> * 8);
      outvbo.type := GL_FLOAT;
      outvbo.ofspos := GLsizeiptr_of_int1 0;
      outvbo.szpos := (GLsizei)3;
      outvbo.ofsnrm := GLsizeiptr_of_int1 (int1_of_size1 (sizeof<GLfloat>) * 3);
      outvbo.sznrm := (GLsizei)3;
      outvbo.ofstc0 := GLsizeiptr_of_int1 (int1_of_size1 (sizeof<GLfloat>) * 6);
      outvbo.sztc0 := (GLsizei)2;
      outvbo.use_nrm := innrm.2 > 0;
      outvbo.use_tc0 := intc0.2 > 0;
    end // end of [begin]
    prval () = pf1_arr := fpf (pf_arr)
    prval () = $V.VECTOR_encode {vert} ($V.vector_v_encode {vert} (pf_mul, pf1_arr, pf2_arr) | out_vrt)
  } // end of [where]
in
  $V.vector_uninitialize out_idx;
  $V.vector_uninitialize out_vrt;
  $H.hashtbl_free ht;
  () where {
    extern prfun array_v_of_surfarr_v {m,n,k:int} {l:addr} (
      pf: surfarr_v (m, n, k, l)
    ):<> array_v (surf (nm, nv, nn, ntc), k, l)
    prval () = insfs.0 := array_v_of_surfarr_v (pf_surfs)
  } // end of [where]
end // end of [geom_upload]

viewtypedef gpumsh0 = [n:int] @{
  mtllib= VSHELL0?
, progs= progs?
, vbo= gpuvrt?
, norms= bool?
, tex0s= bool?
, sfs= List_vt (gpusrf n)?
} // end of [gpumsh0]

viewtypedef gpumsh = [nm1,nm,ns:int] @{
  mtllib= VECTOR (gpumtl, nm1, nm)
, progs= progs
, vbo= gpuvrt
, norms= bool
, tex0s= bool
, sfs= list_vt (gpusrf nm, ns)
} // end of [gpumsh]

assume gpumesh = Option_vt (gpumsh)
val the_gpumesh = ref<gpumesh> (None_vt ())

fun gpumsh_free (g: &gpumsh >> gpumsh0):<!ntm,!exn,~ref> void = let
  stavar nm1:int; stavar nm:int
  var mtllib = g.mtllib: VECTOR (gpumtl, nm1, nm)
  stavar ns:int; var sfs = g.sfs: list_vt (gpusrf nm, ns)
  prval () = __assert () where {
    extern prfun __assert (): [nm >= 0; ns >= 0; nm1 >= 0] void
  } // end of [where]
  fn gpuvrt_free (x: &gpuvrt >> gpuvrt?):<> void = $effmask_all (glDeleteBuffer x.buf)
  fn tex_free (x: &GLtexture0 >> GLtexture0?):<> void =
    if :(x: GLtexture0?) => GLtexture_is_zero x then GLtexture_free_zero x else $effmask_all (glDeleteTexture x)
  // end of [tex_free]
in
  $V.vector_clear_fun<gpumtl> (mtllib, lam (x) => (strptr_free x.id; tex_free x.map_kd));
  $V.vector_uninitialize_vt<gpumtl> (mtllib);
  g.mtllib := mtllib;
  gpuvrt_free (g.vbo);
  $effmask_all (destroy_shaders (g.progs));
  list_vt_free_fun<gpusrf nm> (sfs, lam (x) => $effmask_all (glDeleteBuffer (x.ib.buf)))
end // end of [gpumsh_free]

in // in of [local]

fun the_gpumesh_destroy (): void = let
  val (vbox pf | p) = ref_get_view_ptr {gpumesh} (the_gpumesh)
in
  case+ !p of
  | ~Some_vt (p1) => let var p1 = p1 in ($effmask_all (gpumsh_free (p1)); !p := None_vt ()) end
  | None_vt () => fold@ !p
end // end of [the_gpumesh_destroy]

fun the_gpumesh_init {nm,nv,nn,ntc,ns:nat} (m: &mesh (nm, nv, nn, ntc, ns)): void = let
  val (vbox pf | p) = ref_get_view_ptr {gpumesh} (the_gpumesh)
  fn init (inm: &mesh (nm, nv, nn, ntc, ns), outm: &gpumsh0 >> gpumsh) :<!exn,!ntm,~ref> void = begin
    geom_upload {nm,nv,nn,ntc,ns} (inm.verts, inm.norms, inm.texcoords, inm.surfs, outm.vbo, outm.sfs);
    outm.norms := inm.norms.2 > 0;
    outm.tex0s := inm.texcoords.2 > 0;
    $effmask_all (create_shaders (outm.progs));
    mtllib_upload (inm.mtllib, outm.mtllib);
  end // end of [init]
in
  case+ !p of
  | Some_vt (!p1) => begin
      gpumsh_free (!p1);
      init (m, !p1);
      fold@ !p
    end // end of [begin]
  | ~None_vt () => let
      val () = !p := Some_vt {gpumsh} (?)
      val+ Some_vt (!p1) = !p
      prval () = __cast (!p1) where {
        extern castfn __cast (x: &gpumsh? >> gpumsh0):<> void
      } // end of [where]
    in
      init (m, !p1);
      fold@ !p
    end // end of [let]
end // end of [the_gpumesh_init]

fun the_gpumesh_draw (ue: &uenv): void = let
  fun loop {nm,ns:int} (
    ue: &uenv
  , progs: &progs, mtllib: &VECTOR1 (gpumtl, nm)
  , norms: bool, tex0s: bool
  , sfs: !list_vt (gpusrf nm, ns)
  ) : void = case+ sfs of
    | list_vt_cons (!p_srf, !p_sfs1) => fold@ sfs where {
      prval pf_mul = mul_istot {nm,sizeof gpumtl} ()
      prval pf_vec = $V.VECTOR_decode {gpumtl} (mtllib)
      prval (pf1_arr, pf2_arr) = $V.vector_v_decode {gpumtl} (pf_mul, pf_vec)
      val () = () where {
        val (pf1_mul | ofs) = p_srf->mtl szmul2 sizeof<gpumtl>
        prval (pf_at, fpf) = array_v_takeout {gpumtl} (pf1_mul, pf1_arr)
        val p = mtllib.ptr + ofs
        stavar l:addr; val p = p: ptr l
        val () = begin
          if :(pf_at: gpumtl @ l) => norms then begin
            if :(pf_at: gpumtl @ l) => tex0s && GLtexture_isnot_zero p->map_kd then begin
              program_apply (progs.lit_tm, ue, p->kd, p->map_kd);
              glBindBuffer (GL_ELEMENT_ARRAY_BUFFER, p_srf->ib.buf);
              glDrawElementsBuffer (p_srf->ib.mode, p_srf->ib.count, p_srf->ib.type);
              program_unapply (progs.lit_tm, p->map_kd)
            end else begin
              program_apply (progs.lit, ue, p->kd, p->map_kd);
              glBindBuffer (GL_ELEMENT_ARRAY_BUFFER, p_srf->ib.buf);
              glDrawElementsBuffer (p_srf->ib.mode, p_srf->ib.count, p_srf->ib.type);
              program_unapply (progs.lit, p->map_kd)
            end // end of [if]
          end else begin
            if :(pf_at: gpumtl @ l) => tex0s && GLtexture_isnot_zero p->map_kd then begin
              program_apply (progs.tm, ue, p->kd, p->map_kd);
              glBindBuffer (GL_ELEMENT_ARRAY_BUFFER, p_srf->ib.buf);
              glDrawElementsBuffer (p_srf->ib.mode, p_srf->ib.count, p_srf->ib.type);
              program_unapply (progs.tm, p->map_kd)
            end else begin
              program_apply (progs.sim, ue, p->kd, p->map_kd);
              glBindBuffer (GL_ELEMENT_ARRAY_BUFFER, p_srf->ib.buf);
              glDrawElementsBuffer (p_srf->ib.mode, p_srf->ib.count, p_srf->ib.type);
              program_unapply (progs.sim, p->map_kd)
            end // end of [if]
          end // end of [if]
        end // end of [val]
        prval () = pf1_arr := fpf (pf_at)
      } // end of [where]
      prval () = $V.VECTOR_encode {gpumtl} ($V.vector_v_encode {gpumtl} (pf_mul, pf1_arr, pf2_arr) | mtllib)
      val () = loop (ue, progs, mtllib, norms, tex0s, !p_sfs1)
    } // end of [where]
    | list_vt_nil () => fold@ sfs
  // end of [loop]
  val (vbox pf | p) = ref_get_view_ptr {gpumesh} (the_gpumesh)
in
  case+ !p of
  | Some_vt (!p1) => begin
      $effmask_all (
        glBindBuffer (GL_ARRAY_BUFFER, p1->vbo.buf);
        glVertexAttribPointerBuffer (attr_pos, p1->vbo.szpos, p1->vbo.type, p1->vbo.stride, p1->vbo.ofspos);
        glEnableVertexAttribArray attr_pos;
        glVertexAttribPointerBuffer (attr_norm, p1->vbo.sznrm, p1->vbo.type, p1->vbo.stride, p1->vbo.ofsnrm);
        glEnableVertexAttribArray attr_norm;
        glVertexAttribPointerBuffer (attr_texc, p1->vbo.sztc0, p1->vbo.type, p1->vbo.stride, p1->vbo.ofstc0);
        glEnableVertexAttribArray attr_texc;
        loop (ue, p1->progs, p1->mtllib, p1->norms, p1->tex0s, p1->sfs);
        glDisableVertexAttribArray attr_pos;
        glDisableVertexAttribArray attr_norm;
        glDisableVertexAttribArray attr_texc
      );
      fold@ (!p)
    end // end of [begin]
  | None_vt () => fold@ (!p)
end // end of [the_gpumesh_draw]

end // end of [local]

extern
fun init (filename: string): void = "init"
implement init (filename) = let
  var msh: mesh0 in // uninitialized
  mesh_from_file (filename, msh);
  print "loaded a mesh\n";
  printf ("%d verts, %d norms, %d texcoords, %d surfaces\n", @(
    int1_of_size1 msh.verts.2, int1_of_size1 msh.norms.2
  , int1_of_size1 msh.texcoords.2, int1_of_size1 msh.surfs.2
  ));
  the_gpumesh_init (msh);
  mesh_free msh;
  glEnable GL_DEPTH_TEST;
  glEnable GL_CULL_FACE;
  glClearColor ((GLclampf)0.3f, (GLclampf)0.3f, (GLclampf)0.9f, (GLclampf)0.0f);
  assert_errmsg (glGetError () = GL_NO_ERROR, "[init]: glGetError <> GL_NO_ERROR")
end // end of [init]

(* ****** ****** *)

extern
fun draw (): void = "draw"
implement draw () = let
  #define F float_of
  #define G GLfloat_of_float
  // NOTE: [proj] maps from world space to clip space
  var !p_proj = @[GLfloat][16]()
  val () = () where {
    val w = winx where { prval vbox pf_at = pf_view_winx }
    val h = winy where { prval vbox pf_at = pf_view_winy }
    val () = glViewport (GLint_of_int1 0, GLint_of_int1 0,
                         GLsizei_of_int1 (int1_of_int w), GLsizei_of_int1 (int1_of_int h))
    val () = mat_persp (30.0f, float_of w / float_of h, 1.0f, 300.0f, !p_proj)
    val () = mat_trn (!p_proj, G 0.0f, G 0.0f, G ~5.0f)
    val one = G 1.0f and zero = G 0.0f
    val () = begin
    mat_rot (!p_proj, G (2.0f * F M_PI * F view_rotx / 360.0f), one, zero, zero) where {
      prval vbox pf = pf_view_rotx
    }; // end of [where]
    mat_rot (!p_proj, G (2.0f * F M_PI * F view_roty / 360.0f), zero, one, zero) where {
      prval vbox pf = pf_view_roty
    }; // end of [where]
    mat_rot (!p_proj, G (2.0f * F M_PI * F view_rotz / 360.0f), zero, zero, one) where {
      prval vbox pf = pf_view_rotz
    } // end of [where]
    end // end of [begin]
  } // end of [where]
  var uenv: uenv // uninitialized
  // NOTE: [mv] maps from object space to world space
  val () = mat_id uenv.mv.m
  // NOTE: [mvp] maps from object space to clip space
  val () = array_ptr_copy_tsz (!p_proj, uenv.mvp.m, size1_of_int1 16, sizeof<GLfloat>)
  val () = mat_mult (uenv.mvp.m, uenv.mv.m)
  // NOTE: this computation is unnecessary at the moment
  val () = nm_of_mv (uenv.mv.m, uenv.nm.m) where {
    staload "libats/SATS/biarray.sats"

    // the normal matrix is the transpose of inverse of model-view matrix
    fn mat3_of_mat4 (x: &GLmat4, y: &GLmat3? >> GLmat3):<> void = let
      prval pf_out = biarray_v_nil {GLfloat} ()
      prval pf_rst = view@ (y)
      var p = &y

      prval (pf_at, pf_rst) = array_v_uncons {GLfloat?} (pf_rst)
      val () = !p := x.[0]
      prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
      val () = p := p+sizeof<GLfloat>

      prval (pf_at, pf_rst) = array_v_uncons {GLfloat?} (pf_rst)
      val () = !p := x.[1]
      prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
      val () = p := p+sizeof<GLfloat>

      prval (pf_at, pf_rst) = array_v_uncons {GLfloat?} (pf_rst)
      val () = !p := x.[2]
      prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
      val () = p := p+sizeof<GLfloat>

      prval (pf_at, pf_rst) = array_v_uncons {GLfloat?} (pf_rst)
      val () = !p := x.[4+0]
      prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
      val () = p := p+sizeof<GLfloat>

      prval (pf_at, pf_rst) = array_v_uncons {GLfloat?} (pf_rst)
      val () = !p := x.[4+1]
      prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
      val () = p := p+sizeof<GLfloat>

      prval (pf_at, pf_rst) = array_v_uncons {GLfloat?} (pf_rst)
      val () = !p := x.[4+2]
      prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
      val () = p := p+sizeof<GLfloat>

      prval (pf_at, pf_rst) = array_v_uncons {GLfloat?} (pf_rst)
      val () = !p := x.[8+0]
      prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
      val () = p := p+sizeof<GLfloat>

      prval (pf_at, pf_rst) = array_v_uncons {GLfloat?} (pf_rst)
      val () = !p := x.[8+1]
      prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
      val () = p := p+sizeof<GLfloat>

      prval (pf_at, pf_rst) = array_v_uncons {GLfloat?} (pf_rst)
      val () = !p := x.[8+2]
      prval () = pf_out := biarray_v_snoc {GLfloat} (pf_out, pf_at)
      val () = p := p+sizeof<GLfloat>

      prval () = array_v_unnil {GLfloat?} (pf_rst)
      prval () = view@ y := array_v_of_biarray_v (pf_out)
    in
      // nothing
    end // end of [mat3_of_mat4]

    fn nm_of_mv (x: &GLmat4, y: &GLmat3? >> GLmat3): void = let
      var !tmp = @[GLfloat][16]() in
      mat_invert_into (x, !tmp);
      mat_transp (!tmp);
      mat3_of_mat4 (!tmp, y)
    end // end of [nm_of_mv]
  } // end of [where]
in
  uenv.lpos := @(0.0f, 5.0f, 0.0f);
  // TODO: send a correct eye position
  uenv.epos := @(0.0f, 0.0f, 0.0f);
  glClear (GL_COLOR_BUFFER_BIT lor GL_DEPTH_BUFFER_BIT);
  the_gpumesh_draw (uenv)
end // end of [draw]

(* ****** ****** *)

// new window size or exposure
extern
fun reshape {w,h:pos} (width: int w, height: int h): void = "reshape"
implement reshape (w, h) = begin
  winx := w where { prval vbox pf_at = pf_view_winx };
  winy := h where { prval vbox pf_at = pf_view_winy };
end // end of [reshape]

(* ****** ****** *)

implement main_dummy () = () // [mainats] is implemented externally

(* ****** ****** *)

%{$
/*
 * Create an RGB, double-buffered X window.
 * Return the window and context handles.
 */
static void
make_x_window(Display *x_dpy, EGLDisplay egl_dpy,
              const char *name,
              int x, int y, int width, int height,
              Window *winRet,
              EGLContext *ctxRet,
              EGLSurface *surfRet)
{
   static const EGLint attribs[] = {
      EGL_RED_SIZE, 1,
      EGL_GREEN_SIZE, 1,
      EGL_BLUE_SIZE, 1,
      EGL_DEPTH_SIZE, 1,
      EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT,
      EGL_NONE
   };
   static const EGLint ctx_attribs[] = {
      EGL_CONTEXT_CLIENT_VERSION, 2,
      EGL_NONE
   };
   int scrnum;
   XSetWindowAttributes attr;
   unsigned long mask;
   Window root;
   Window win;
   XVisualInfo *visInfo, visTemplate;
   int num_visuals;
   EGLContext ctx;
   EGLConfig config;
   EGLint num_configs;
   EGLint vid;

   scrnum = DefaultScreen( x_dpy );
   root = RootWindow( x_dpy, scrnum );

   if (!eglChooseConfig( egl_dpy, attribs, &config, 1, &num_configs)) {
      printf("Error: couldn't get an EGL visual config\n");
      exit(1);
   }

   //   assert(config);
   //   assert(num_configs > 0);

   if (!eglGetConfigAttrib(egl_dpy, config, EGL_NATIVE_VISUAL_ID, &vid)) {
      printf("Error: eglGetConfigAttrib() failed\n");
      exit(1);
   }

   /* The X window visual must match the EGL config */
   visTemplate.visualid = vid;
   visInfo = XGetVisualInfo(x_dpy, VisualIDMask, &visTemplate, &num_visuals);
   if (!visInfo) {
      printf("Error: couldn't get X visual\n");
      exit(1);
   }

   /* window attributes */
   attr.background_pixel = 0;
   attr.border_pixel = 0;
   attr.colormap = XCreateColormap( x_dpy, root, visInfo->visual, AllocNone);
   attr.event_mask = StructureNotifyMask | ExposureMask | KeyPressMask;
   mask = CWBackPixel | CWBorderPixel | CWColormap | CWEventMask;

   win = XCreateWindow( x_dpy, root, 0, 0, width, height,
		        0, visInfo->depth, InputOutput,
		        visInfo->visual, mask, &attr );

   /* set hints and properties */
   {
      XSizeHints sizehints;
      sizehints.x = x;
      sizehints.y = y;
      sizehints.width  = width;
      sizehints.height = height;
      sizehints.flags = USSize | USPosition;
      XSetNormalHints(x_dpy, win, &sizehints);
      XSetStandardProperties(x_dpy, win, name, name,
                              None, (char **)NULL, 0, &sizehints);
   }

   eglBindAPI(EGL_OPENGL_ES_API);

   ctx = eglCreateContext(egl_dpy, config, EGL_NO_CONTEXT, ctx_attribs );
   if (!ctx) {
      printf("Error: eglCreateContext failed\n");
      exit(1);
   }

   /* test eglQueryContext() */
   {
      EGLint val;
      eglQueryContext(egl_dpy, ctx, EGL_CONTEXT_CLIENT_VERSION, &val);
      //      assert(val == 2);
   }

   *surfRet = eglCreateWindowSurface(egl_dpy, config, win, NULL);
   if (!*surfRet) {
      printf("Error: eglCreateWindowSurface failed\n");
      exit(1);
   }

   /* sanity checks */
   {
      EGLint val;
      eglQuerySurface(egl_dpy, *surfRet, EGL_WIDTH, &val);
      //      assert(val == width);
      eglQuerySurface(egl_dpy, *surfRet, EGL_HEIGHT, &val);
      //      assert(val == height);
      //      assert(eglGetConfigAttrib(egl_dpy, config, EGL_SURFACE_TYPE, &val));
      //      assert(val & EGL_WINDOW_BIT);
   }

   XFree(visInfo);

   *winRet = win;
   *ctxRet = ctx;
}

// shamelessly stolen from SDL
static int X11_KeyRepeat(Display *display, XEvent *event) {
    XEvent peekevent;

    if (XPending(display)) {
        XPeekEvent(display, &peekevent);
        if ((peekevent.type == KeyPress) &&
            (peekevent.xkey.keycode == event->xkey.keycode) &&
            ((peekevent.xkey.time-event->xkey.time) < 2)) {
            return 1;
        }
    }
    return 0;
}

static void
event_loop(Display *dpy, Window win,
           EGLDisplay egl_dpy, EGLSurface egl_surf)
{
   while (1) {
      int redraw = 0;
      XEvent event;

      XNextEvent(dpy, &event);

      switch (event.type) {
      case Expose:
         redraw = 1;
         break;
      case ConfigureNotify:
         reshape(event.xconfigure.width, event.xconfigure.height);
         break;
      case KeyPress:
         {
            char buffer[10];
            int r, code;
            code = XLookupKeysym(&event.xkey, 0);
	    switch (code) {
	    case XK_Left:
	      keypress(0);
	      break;
	    case XK_Right:
	      keypress(1);
	      break;
	    case XK_Up:
	      keypress(2);
	      break;
	    case XK_Down:
	      keypress(3);
	      break;
	    default:
               r = XLookupString(&event.xkey, buffer, sizeof(buffer),
                                 NULL, NULL);
	       if (buffer[0] == 'a' || buffer[0] == 'A') {
		 keypress(4);
	       }
	       else if (buffer[0] == 'z' || buffer[0] == 'Z') {
		 keypress(5);
	       }
               else if (buffer[0] == 27) {
                  /* escape */
                  return;
               }
	       break;
            }
         }
         redraw = 1;
         break;
      default:
	; /*no-op*/
      }

      if (redraw) {
         draw();
         eglSwapBuffers(egl_dpy, egl_surf);
      }
   }
}

static void
usage(void)
{
   printf("Usage:\n");
   printf("  -display <displayname>  set the display to run on\n");
   printf("  -info                   display OpenGL renderer info\n");
   printf("  -obj <path>             load mesh in Wavefront OBJ format on the specified path\n");
}

ats_void_type mainats (
  ats_int_type argc, ats_ptr_type argv
) {
   const int winWidth = 800, winHeight = 600;
   Display *x_dpy;
   Window win;
   EGLSurface egl_surf;
   EGLContext egl_ctx;
   EGLDisplay egl_dpy;
   char *dpyName = NULL;
   GLboolean printInfo = GL_FALSE;
   EGLint egl_major, egl_minor;
   int i;
   const char *s;
   const char *meshname = "data/bunny.obj";

   for (i = 1; i < argc; i++) {
     if (strcmp(((char **)argv)[i], "-display") == 0) {
       dpyName = ((char **)argv)[i+1]; // FIXME: reading past end?
       i++;
     }
     else if (strcmp(((char **)argv)[i], "-info") == 0) {
       printInfo = GL_TRUE;
     }
     else if (strcmp(((char **)argv)[i], "-obj") == 0) {
       meshname = ((char **)argv)[i+1]; // FIXME: reading past end?
       i++;
     }
     else {
       usage();
       return;
     }
   }

   x_dpy = XOpenDisplay(dpyName);
   if (!x_dpy) {
      printf("Error: couldn't open display %s\n",
	     dpyName ? dpyName : getenv("DISPLAY"));
      return;
   }

   egl_dpy = eglGetDisplay(x_dpy);
   if (!egl_dpy) {
      printf("Error: eglGetDisplay() failed\n");
      return;
   }

   if (!eglInitialize(egl_dpy, &egl_major, &egl_minor)) {
      printf("Error: eglInitialize() failed\n");
      return;
   }

   s = eglQueryString(egl_dpy, EGL_VERSION);
   printf("EGL_VERSION = %s\n", s);

   s = eglQueryString(egl_dpy, EGL_VENDOR);
   printf("EGL_VENDOR = %s\n", s);

   s = eglQueryString(egl_dpy, EGL_EXTENSIONS);
   printf("EGL_EXTENSIONS = %s\n", s);

   s = eglQueryString(egl_dpy, EGL_CLIENT_APIS);
   printf("EGL_CLIENT_APIS = %s\n", s);

   make_x_window(x_dpy, egl_dpy,
                 "OpenGL ES 2.x textured quad", 0, 0, winWidth, winHeight,
                 &win, &egl_ctx, &egl_surf);

   XMapWindow(x_dpy, win);
   if (!eglMakeCurrent(egl_dpy, egl_surf, egl_surf, egl_ctx)) {
      printf("Error: eglMakeCurrent() failed\n");
      return;
   }

   if (printInfo) {
      printf("GL_RENDERER   = %s\n", (char *) glGetString(GL_RENDERER));
      printf("GL_VERSION    = %s\n", (char *) glGetString(GL_VERSION));
      printf("GL_VENDOR     = %s\n", (char *) glGetString(GL_VENDOR));
      printf("GL_EXTENSIONS = %s\n", (char *) glGetString(GL_EXTENSIONS));
   }

   init((ats_ptr_type)meshname);

   /* Set initial projection/viewing transformation.
    * We can't be sure we'll get a ConfigureNotify event when the window
    * first appears.
    */
   reshape(winWidth, winHeight);

   event_loop(x_dpy, win, egl_dpy, egl_surf);

   eglDestroyContext(egl_dpy, egl_ctx);
   eglDestroySurface(egl_dpy, egl_surf);
   eglTerminate(egl_dpy);


   XDestroyWindow(x_dpy, win);
   XCloseDisplay(x_dpy);

   return;
} // end of [mainats]
%} // end of [%{$]

(* end of [GLES2-test03.dats] *)
