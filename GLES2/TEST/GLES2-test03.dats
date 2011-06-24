(*
** In this demo, buffer objects are used for specifying
** mesh vertices as well as connectivity.
**
** New features in this example:
** - mesh loading
** - setting up modelview and projection matrices for 3D
** - (primitive) camera control:
**   use arrows to spin the mesh, 'a' to scale it up, 'z' to scale it down
*)

(*
roadblock: moving loaded mesh into GPU and rendering it?
moving loaded mesh into GPU
*)

staload "libc/SATS/math.sats"

staload _(*anonymous*) = "prelude/DATS/array.dats"

staload GA = "libats/SATS/genarrays.sats"
stadef GEVEC = $GA.GEVEC

staload "GLES2/SATS/gl2.sats"

staload "GLES2/TEST/SATS/util.sats"

staload "GLES2/TEST/SATS/obj.sats"

(* ****** ******* *)

#define SHADER_VERT "test03.vert"
#define SHADER_FRAG "test03.frag"

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

var u_matrix = (GLint)0
val (pf_u_matrix | ()) =
  vbox_make_view_ptr {GLint} (view@ u_matrix | &u_matrix)
// end of [val]

(* ****** ****** *)

var view_rotx = (GLfloat)0.0f
val (pf_view_rotx | ()) =
  vbox_make_view_ptr {GLfloat} (view@ view_rotx | &view_rotx)
// end of [prval]
var view_roty = (GLfloat)0.0f
val (pf_view_roty | ()) =
  vbox_make_view_ptr {GLfloat} (view@ view_roty | &view_roty)
// end of [prval]
var view_dist = (GLfloat)1.0f
val (pf_view_dist | ()) =
  vbox_make_view_ptr {GLfloat} (view@ view_dist | &view_dist)
// end of [prval]

extern
fun keypress {i:int | i >= 0; i <= 1} (state: int i, code: natLt 6): void = "keypress"
// state = 1 means key has been pressed
// state = 0 means key has been released
implement keypress (state, code) = let
  #define F float_of_GLfloat
in
  if state = 1 then begin
    case+ code of
    | 0 => (*left*) let
        prval vbox pf_aty = pf_view_roty
      in
        view_roty := (GLfloat)(F view_roty + 5.0f)
      end
    | 1 => (*right*) let
        prval vbox pf_aty = pf_view_roty
      in
        view_roty := (GLfloat)(F view_roty - 5.0f)
      end
    | 2 => (*up*) let
        prval vbox pf_atx = pf_view_rotx
      in
        view_rotx := (GLfloat)(F view_rotx + 5.0f)
      end
    | 3 => (*down*) let
        prval vbox pf_atx = pf_view_rotx
      in
        view_rotx := (GLfloat)(F view_rotx - 5.0f)
      end
    | 4 => (*increase scale*) let
        prval vbox pf_at = pf_view_dist
        val s = F view_dist + 0.25f
      in
        view_dist := (GLfloat)(if s > 10.0f then 10.0f else s)
      end
    | 5 => (*decrease scale*) let
        prval vbox pf_at = pf_view_dist
        val s = F view_dist - 0.25f
      in
        view_dist := (GLfloat)(if s < 0.1f then 0.1f else s)
      end
  end // end of [if]
end // end of [keypress]

(* ****** ****** *)

dynload "obj_lats.dats"

(* ****** ****** *)

// how to represent buffers in our app?
// fun buffers_of_mesh (msh: &mesh, vts: &)
// glVertexAttribPointer(3, GL_FLOAT, 8 * sizeof(float), 0)
// glVertexAttribPointer(3, GL_FLOAT, 8 * sizeof(float), 3)
// glVertexAttribPointer(2, GL_FLOAT, 8 * sizeof(float), 5)
// matrix (float, 8, n)

(*
fun glVertexAttribPointer {a:t@ype} {n:pos | n <= 4} {m:nat} {l:addr} (
  pf: !matrix_v (a, m, n, l)
| indx: GLuint
, size: GLint n
, type: GLenum_type a
, normalized: GLboolean
, stride: GLsizei 0
, p: ptr l
) : void
*)

// given vertices, normals, texcoords and faces,
// - if numverts = 0, there is something wrong!
//   else allocate verts: @[vec3][numverts] and initialize it in reverse
// - if numnorms > 0, allocate norms: @[vec3][numverts]
//    and map every face.nindex to face.vindex
//    vertex[vidx]
//    out.[vindex] := in.[nindex]
// - if numtexcoords > 0, allocate texcoords: @[vec2][numverts]
//    and map every face.tindex to face.vindex
//    out.[vindex] := in.[tindex]
// - triangles simply need to be copied to indices

// extern fun remap {nv,nn,nt,nf:nat} (m: !mesh (nv, nn, ntc, nf), )
// for each face [f] in [mesh.faces]:
//   for each face element [e] in [f]:
//     FIXME: does OBJ count from 1? it seems so
//     assert_errmsg (f.e.vindex < vindices, "something bad happened")
//     norms.[f.e.vindex] := mesh.norms.[f.e.nindex]
//     texcoords.[f.e.vindex] := mesh.texcoords.[f.e.tindex]
// we can also convert triangles to indices

(*
in this case, a VBO can be seen as a matrix (or rather, an array of arrays),
where each row is a vertex.
*)

// computing stride...
// (@(a, b) @ l | ptr l) -> (a @ l, b @ l+ofs, (a @ l, b @ l+ofs) -<prf> @(a, b) @ l | size_t ofs)
//
(*
absview deltarray_v (a:viewt@ype, n:int(*size*), d:int(*delta*), l:addr(*start*))
extern fun deltarray_of_array {a,b:viewt@ype} {n:nat} {l:addr} (
  pf_arr: array_v (@(a, b), n, l)
| ptr l
) :<> [ofs:nat] (
  deltarray_v (a, n, sizeof(@(a, b)), l)
  deltarray_v (b, n, sizeof(@(a, b)), l+ofs)
, size_t ofs
)
*)
//
// (array_v (@(a, b), n, l) | ptr l) -> (
//   arrayd_v (a, n, sizeof(@(a,b)), l)
// , arrayd_v (b, n, sizeof(@(a,b)), l+ofs)
// | size_t ofs
// )

// fun make_vbo (msh: !mesh):

(* ****** ****** *)

fun create_shaders (): void = let
   var stat: GLint
   var fragShader = glCreateShader GL_FRAGMENT_SHADER
   val () = shader_from_file (fragShader, SHADER_FRAG)
   var vertShader = glCreateShader GL_VERTEX_SHADER
   val () = shader_from_file (vertShader, SHADER_VERT)
   val program = glCreateProgram ()
   val () = glAttachShader (program, vertShader)
   val () = glAttachShader (program, fragShader)
   val () = glLinkProgram program
   val () = glGetProgramiv (program, GL_LINK_STATUS, stat)
   val () = if int_of_GLint stat = 0 then let
       var !p_log with pf_log = @[byte][1000]()
       var len: GLsizei // uninitialized
       prval pf_len = Some_v (view@ (len))
       val () = glGetProgramInfoLog (pf_log, pf_len | program, (GLsizei)1000, &len, p_log)
       prval Some_v pf = pf_len; prval () = view@ (len) := pf
       val () = begin
         print ("Error: linking:\n");
         fprint_strbuf (stdout_ref, !p_log);
         print_newline ()
       end // end of [begin]
       prval () = pf_log := bytes_v_of_strbuf_v (pf_log)
     in
       exit {void} (1)
     end // end of [if]
  val () = glUseProgram program
  val () = glBindAttribLocation (program, attr_pos, "pos")
  val () = glBindAttribLocation (program, attr_norm, "norm")
  val () = glLinkProgram program // needed to put attribs into effect
  val () = () where {
    val um = glGetUniformLocation (program, "mvp")
    prval vbox pf_u = pf_u_matrix
    val () = u_matrix := um
  } // end of [where]
  val () = glDeleteShader vertShader
  val () = glDeleteShader fragShader
  val () = let
      extern castfn __leak1 (x: GLprogram): void
    in
      __leak1 (program)
    end
in
  // nothing
end // end of [create_shaders]

(*
viewdef gpumesh (d:int) = @{
  buf= GLbuffer
, vsiz= size_t d
, pofs= sizeLt d
, nofs= sizeLt d
, tofs= sizeLt d
}

extern
fun mesh_reindex (
  m: &mesh
, buf: &GEVEC (GLfloat, n, d)
, d: &size_t? >> size_t d
, n: &size_t? >> size_t n
) :<> #[n,d:nat] void
*)

(*
how to load:
- create a chunk big enough to hold all the mesh data
- copy vertex positions, normals, etc. into the chunk
- then:
   glGenBuffers(1, &vbo);
   glBindBuffer(GL_ARRAY_BUFFER, vbo);
   glBufferData(GL_ARRAY_BUFFER, bytes, buffer, GL_STATIC_DRAW_ARB);
   glBindBufferB(GL_ARRAY_BUFFER, 0);
// in our case, the whole buffer consists of only floats:
// it can be represented by GEVEC (from libats/SATS/genarrays.sats)
// or by GEMAT, even

// renumbering the mesh:
// - forall triangle vertices:
//   v = vindex, t = tindex, n = nindex
//   tc = lookup (t, texcoords)
//   nm = lookup (n, norms)
//   vt = lookup (v, verts)
//   out.tc := tc; out.nm := nm; out.vt := vt
// the resulting array can be directly used for loading into a VBO

   glBindBufferARB(GL_ARRAY_BUFFER_ARB, model->vbo);

   glVertexPointer(3, GL_FLOAT, model->vertexSize * sizeof(float), (void * ) model->posOffset);
   glEnableVertexAttribArray(attr_pos);

   if (model->numnormals > 0) {
      glNormalPointer(GL_FLOAT, model->vertexSize * sizeof(float), (void * ) model->normOffset);
      glEnableVertexAttribArray(attr_norm);
   }

   if (model->numtexcoords > 0) {
      glTexCoordPointer(2, GL_FLOAT, model->vertexSize * sizeof(float), (void * ) model->texOffset);
      glEnableVertexAttribArray(attr_texcoord);
   }
   glDrawElements(GL_TRIANGLES,
     3 * group->numtriangles,
     GL_UNSIGNED_INT, group->triIndexes);

   glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
   glDisableVertexAttribArray(attr_pos);
   glDisableVertexAttribArray(attr_norm);
   glDisableVertexAttribArray(attr_texcoord);
*)

extern
fun init (filename: string): void = "init"
implement init (filename) = let
  var msh: mesh0 // uninitialized
  val () = mesh_from_file (filename, msh)
  val () = print "loaded a mesh\n"
  val () = begin
    // 
    printf ("%d verts, %d norms, %d texcoords, %d tris\n", @(
      int1_of_size1 msh.verts.2, int1_of_size1 msh.norms.2
    , int1_of_size1 msh.texcoords.2, int1_of_size1 msh.faces.2
    ))
  end
  val () = mesh_free msh
in
  create_shaders ();
  glEnable GL_DEPTH_TEST;
  glEnable GL_CULL_FACE;
  glClearColor ((GLclampf)0.3f, (GLclampf)0.3f, (GLclampf)0.9f, (GLclampf)0.0f);
  assert_errmsg (glGetError () = GL_NO_ERROR, "[init]: glGetError <> GL_NO_ERROR")
end // end of [init]

(* ****** ****** *)
// miscellaneous matrix-related functions

fn make_projection_matrix (
    fovy: float, aspect: float, near: float, far: float
  , m: &(@[GLfloat?][16]) >> @[GLfloat][16]
  ):<> void = let
  val ymax = near * tanf (fovy * float_of M_PI / 180.0f)
  val ymin = ~ymax
  val xmin = ymin * aspect
  val xmax = ymax * aspect
  val () = array_ptr_initialize_elt<GLfloat> (m, size1_of_int1 16, (GLfloat)0.0f)
in
  m.[0] := GLfloat (( 2.0f * near ) / ( xmax - xmin ));
  m.[4] := GLfloat 0.0f;
  m.[8] := GLfloat (( xmax + xmin ) / ( xmax - xmin ));
  m.[12] := GLfloat 0.0f;

  m.[1] := GLfloat 0.0f;
  m.[5] := GLfloat (( 2.0f * near ) / ( ymax - ymin ));
  m.[9] := GLfloat (( ymax + ymin ) / ( ymax - ymin ));
  m.[13] := GLfloat 0.0f;

  m.[2] := GLfloat 0.0f;
  m.[6] := GLfloat 0.0f;
  m.[10] := GLfloat (~( far + near ) / ( far - near ));
  m.[14] := GLfloat (~( 2.0f * far * near ) / ( far - near ));

  m.[3] := GLfloat 0.0f;
  m.[7] := GLfloat 0.0f;
  m.[11] := (GLfloat) ~1.0f;
  m.[15] :=  GLfloat 0.0f
end // end of [make_projection_matrix]

fn make_x_rot_matrix (angle: GLfloat, m: &(@[GLfloat?][16]) >> @[GLfloat][16]):<> void = let
  val c = (GLfloat) (cosf (float_of angle * 3.14f / 180.0f))
  val s = (GLfloat) (sinf (float_of angle * 3.14f / 180.0f))
  val () = array_ptr_initialize_elt<GLfloat> (m, size1_of_int1 16, (GLfloat)0.0f)  
in
  m.[0] := (GLfloat)1.0f;
  m.[5] := c;
  m.[6] := (GLfloat)(~float_of s);
  m.[9] := s;
  m.[10] := c;
  m.[15] := (GLfloat)1.0f
end // end of [make_x_rot_matrix]

fn make_y_rot_matrix (angle: GLfloat, m: &(@[GLfloat?][16]) >> @[GLfloat][16]):<> void = let
  val c = (GLfloat) (cosf (float_of angle * 3.14f / 180.0f))
  val s = (GLfloat) (sinf (float_of angle * 3.14f / 180.0f))
  val () = array_ptr_initialize_elt<GLfloat> (m, size1_of_int1 16, (GLfloat)0.0f)
in
  m.[0] := c;
  m.[2] := s;
  m.[5] := (GLfloat)1.0f;
  m.[8] := (GLfloat)(~float_of s);
  m.[10] := c;
  m.[15] := (GLfloat)1.0f
end // end of [make_y_rot_matrix]

fn make_trans_matrix (
  xt: GLfloat, yt: GLfloat, zt: GLfloat, m: &(@[GLfloat?][16]) >> @[GLfloat][16]
) :<> void = let
  val () = array_ptr_initialize_elt<GLfloat> (m, 16, (GLfloat)0.0f)
in
  m.[0] := (GLfloat)1.0f;
  m.[3] := xt;
  m.[5] := (GLfloat)1.0f;
  m.[7] := yt;
  m.[10] := (GLfloat)1.0f;
  m.[11] := zt;
  m.[15] := (GLfloat)1.0f
end // end of [make_trans_matrix]

fn make_scale_matrix (
  xs: GLfloat, ys: GLfloat, zs: GLfloat, m: &(@[GLfloat?][16]) >> @[GLfloat][16]
) :<> void = let
  val () = array_ptr_initialize_elt<GLfloat> (m, 16, (GLfloat)0.0f)
in
  m.[0] := xs;
  m.[5] := ys;
  m.[10] := zs;
  m.[15] := (GLfloat)1.0f
end // end of [make_scale_matrix]

fn mul_matrix (
  p: &(@[GLfloat?][16]) >> @[GLfloat][16]
, a: &(@[GLfloat][16])
, b: &(@[GLfloat][16])
) : void = let
  #define F float_of
  #define G GLfloat_of_float
  // this is only for the sake of making the code below typecheck!
  val () = array_ptr_initialize_elt<GLfloat> (p, 16, G 0.0f)

  val ai0 = F a.[0] and ai1 = F a.[1] and ai2 = F a.[2] and ai3 = F a.[3]
  val () = p.[0] := G (ai0 * F b.[0] + ai1 * F b.[4] + ai2 * F b.[8] + ai3 * F b.[12]);
  val () = p.[1] := G (ai0 * F b.[1] + ai1 * F b.[5] + ai2 * F b.[9] + ai3 * F b.[13]);
  val () = p.[2] := G (ai0 * F b.[2] + ai1 * F b.[6] + ai2 * F b.[10] + ai3 * F b.[14]);
  val () = p.[3] := G (ai0 * F b.[3] + ai1 * F b.[7] + ai2 * F b.[11] + ai3 * F b.[15])

  val ai0 = F a.[4] and ai1 = F a.[5] and ai2 = F a.[6] and ai3 = F a.[7]
  val () = p.[4] := G (ai0 * F b.[0] + ai1 * F b.[4] + ai2 * F b.[8] + ai3 * F b.[12]);
  val () = p.[5] := G (ai0 * F b.[1] + ai1 * F b.[5] + ai2 * F b.[9] + ai3 * F b.[13]);
  val () = p.[6] := G (ai0 * F b.[2] + ai1 * F b.[6] + ai2 * F b.[10] + ai3 * F b.[14]);
  val () = p.[7] := G (ai0 * F b.[3] + ai1 * F b.[7] + ai2 * F b.[11] + ai3 * F b.[15])  

  val ai0 = F a.[8] and ai1 = F a.[9] and ai2 = F a.[10] and ai3 = F a.[11]
  val () = p.[8] := G (ai0 * F b.[0] + ai1 * F b.[4] + ai2 * F b.[8] + ai3 * F b.[12]);
  val () = p.[9] := G (ai0 * F b.[1] + ai1 * F b.[5] + ai2 * F b.[9] + ai3 * F b.[13]);
  val () = p.[10] := G (ai0 * F b.[2] + ai1 * F b.[6] + ai2 * F b.[10] + ai3 * F b.[14]);
  val () = p.[11] := G (ai0 * F b.[3] + ai1 * F b.[7] + ai2 * F b.[11] + ai3 * F b.[15])

  val ai0 = F a.[12] and ai1 = F a.[13] and ai2 = F a.[14] and ai3 = F a.[15]
  val () = p.[12] := G (ai0 * F b.[0] + ai1 * F b.[4] + ai2 * F b.[8] + ai3 * F b.[12]);
  val () = p.[13] := G (ai0 * F b.[1] + ai1 * F b.[5] + ai2 * F b.[9] + ai3 * F b.[13]);
  val () = p.[14] := G (ai0 * F b.[2] + ai1 * F b.[6] + ai2 * F b.[10] + ai3 * F b.[14]);
  val () = p.[15] := G (ai0 * F b.[3] + ai1 * F b.[7] + ai2 * F b.[11] + ai3 * F b.[15])
in
  ()
end // end of [mul_matrix]

(* ****** ****** *)

extern
fun draw (): void = "draw"
implement draw () = let
  var !p_verts with pf_verts = @[GLfloat](
    (GLfloat)~1.0f, (GLfloat)~1.0f, (GLfloat)~1.0f
  , (GLfloat)1.0f, (GLfloat)~1.0f, (GLfloat)~1.0f
  , (GLfloat)0.0f, (GLfloat)1.0f, (GLfloat)~1.0f
  ) // end of [!p_verts]
  var !p_colors with pf_colors = @[GLfloat](
    (GLfloat)1.0f, (GLfloat)0.0f, (GLfloat)0.0f
  , (GLfloat)0.0f, (GLfloat)1.0f, (GLfloat)0.0f
  , (GLfloat)0.0f, (GLfloat)0.0f, (GLfloat)1.0f
  ) // end of [!p_colors]
  var !p_proj = @[GLfloat][16]() and !p_model = @[GLfloat][16]() and !p_view = @[GLfloat][16]()
  val () = () where {
    val w = winx where { prval vbox pf_at = pf_view_winx }
    val h = winy where { prval vbox pf_at = pf_view_winy }
    val () = glViewport (GLint_of_int1 0, GLint_of_int1 0,
                         GLsizei_of_int1 (int1_of_int w), GLsizei_of_int1 (int1_of_int h))
    val () = make_projection_matrix (
      30.0f
    , float_of w / float_of h
    , 1.0f, 300.0f, !p_proj
    ) // end of [val]
  } // end of [where]
  val () = () where {
    var !p_a = @[GLfloat][16]() and !p_b = @[GLfloat][16]()
    var !p_s = @[GLfloat][16]() and !p_t = @[GLfloat][16]()
    val () = make_x_rot_matrix ((GLfloat)(~float_of view_rotx), !p_a) where { prval vbox pf = pf_view_rotx }
    val () = make_y_rot_matrix ((GLfloat)(~float_of view_roty), !p_b) where { prval vbox pf = pf_view_roty }
    val () = make_scale_matrix (view_dist, view_dist, view_dist, !p_s) where {
      prval vbox pf = pf_view_dist
    } // end of [where]
    val () = mul_matrix (!p_t, !p_a, !p_b)
    val () = mul_matrix (!p_model, !p_s, !p_t)
  }
  val () = make_trans_matrix ((GLfloat)0.0f, (GLfloat)0.0f, (GLfloat)~10.0f, !p_view)
in
  // set model-view-projection
  () where {
    var !p_mat with pf_mat = @[GLfloat][16]()
    var !p_tmp = @[GLfloat][16]()
    // calculate...
    val () = mul_matrix (!p_tmp, !p_view, !p_model)
    val () = mul_matrix (!p_mat, !p_proj, !p_tmp)
    // ...and set
    prval pf_mat1 = array_v_sing (pf_mat)
    val uni = let prval vbox pf_um = pf_u_matrix in u_matrix end
    val () = glUniformMatrix4fv (uni, (GLsizei)1, GL_FALSE, !p_mat)
    prval () = pf_mat := array_v_unsing pf_mat1
  };
  glClear (GL_COLOR_BUFFER_BIT lor GL_DEPTH_BUFFER_BIT);
  () where {
    prval pfmul = mul_make {3,3} ()
    prval pfmat = matrix_v_of_array_v {GLfloat} (pfmul, pf_verts)
    val () = glVertexAttribPointer (pfmat | attr_pos, (GLint)3, GL_FLOAT, GL_FALSE, (GLsizei)0, p_verts)
    prval (pfmul', pfarr) = array_v_of_matrix_v {GLfloat} (pfmat)
    prval () = mul_isfun (pfmul, pfmul')
    prval () = pf_verts := pfarr
  }; // end of [where]
(*  () where {
    prval pfmul = mul_make {3,3} ()
    prval pfmat = matrix_v_of_array_v {GLfloat} (pfmul, pf_colors)
    val () = glVertexAttribPointer (pfmat | attr_color, (GLint)3, GL_FLOAT, GL_FALSE, (GLsizei)0, p_colors)
    prval (pfmul', pfarr) = array_v_of_matrix_v {GLfloat} (pfmat)
    prval () = mul_isfun (pfmul, pfmul')
    prval () = pf_colors := pfarr
  }; *) // end of [where]
  glEnableVertexAttribArray attr_pos;
//  glEnableVertexAttribArray attr_color;
  glDrawArrays (GL_TRIANGLES, (GLint)0, (GLsizei)3);
  glDisableVertexAttribArray attr_pos;
//  glDisableVertexAttribArray attr_color
end // end of [draw]

(* ****** ****** *)

// new window size or exposure
extern
fun reshape {w,h:pos} (width: int w, height: int h): void = "reshape"
implement reshape (w, h) = let
  val () = winx := w where { prval vbox pf_at = pf_view_winx }
  val () = winy := h where { prval vbox pf_at = pf_view_winy }
in
  // nothing
end

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
   attr.event_mask = StructureNotifyMask | ExposureMask | KeyPressMask | KeyReleaseMask;
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
	      keypress(1, 0);
	      break;
	    case XK_Right:
	      keypress(1, 1);
	      break;
	    case XK_Up:
	      keypress(1, 2);
	      break;
	    case XK_Down:
	      keypress(1, 3);
	      break;
	    default:
               r = XLookupString(&event.xkey, buffer, sizeof(buffer),
                                 NULL, NULL);
	       if (buffer[0] == 'a' || buffer[0] == 'A') {
		 keypress(1, 4);
	       }
	       else if (buffer[0] == 'z' || buffer[0] == 'Z') {
		 keypress(1, 5);
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
      case KeyRelease:
	if (!X11_KeyRepeat (dpy, &event)) {
	  char buffer[10];
	  int r, code;
	  code = XLookupKeysym(&event.xkey, 0);
	  switch (code) {
	  case XK_Left:
	    keypress(0, 0);
	    break;
	  case XK_Right:
	    keypress(0, 1);
	    break;
	  case XK_Up:
	    keypress(0, 2);
	    break;
	  case XK_Down:
	    keypress(0, 3);
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
   const char *meshname = "bunny.obj";

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
}
%}
