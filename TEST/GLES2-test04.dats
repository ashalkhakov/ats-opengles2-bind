(*
** This program shows the technique of "Level Set Contour Textures"
** in OpenGL ES 2.0. Adapted from the work of the same name by
** Stefan Gustavson (stefan DOT gustavson AT gmail DOT com).
**
** FIXME: at the moment, the GL ES2 implementation provided by Mesa 7.2
** (on my system) does not enable GL_OES_standard_derivatives extension
** in fragment shaders although it reports the extension as available;
** therefore, we pretend that we work with OpenGL 2.1 in the driver code.
**
** Controls:
** - use arrows to pan
** - use A/Z to zoom in/out
** - use X/C to rotate around Z-axis (i.e., to roll)
** - use F1-F4 to select shader
** - use 1-4 to select texture
*)
//
// Author: Artyom Shakhakov (artyom DOT shalkhakov AT gmail DOT com)
// Time: December, 2011
// License: LGPL3
//

(* ****** ****** *)

staload "libc/SATS/math.sats" // for [M_PI]
staload _ = "prelude/DATS/array.dats" // template function definitions
staload _(*anonymous*) = "prelude/DATS/reference.dats"

(* ****** ****** *)

staload "contrib/GLES2/SATS/gl2.sats"

(* ****** ****** *)

staload "SATS/util.sats"
staload "SATS/mat4.sats"

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

var view_zoom = 1.0f
val (pf_view_zoom | ()) =
  vbox_make_view_ptr {float} (view@ view_zoom | &view_zoom)
// end of [val]

var view_px = (GLfloat)0.0f
val (pf_view_px | ()) =
  vbox_make_view_ptr {GLfloat} (view@ view_px | &view_px)
// end of [val]

var view_py = (GLfloat)0.0f
val (pf_view_py | ()) =
  vbox_make_view_ptr {GLfloat} (view@ view_py | &view_py)
// end of [val]

var view_rotz = (GLfloat)0.0f
val (pf_view_rotz | ()) =
  vbox_make_view_ptr {GLfloat} (view@ view_rotz | &view_rotz)
// end of [val]

(* ******* ****** *)

var hndl_tex = size_of_int 0
val (pf_view_hndl_tex | ()) =
  vbox_make_view_ptr {size_t} (view@ hndl_tex | &hndl_tex)
// end of [prval]

var hndl_pro = size_of_int 0
val (pf_view_hndl_pro | ()) =
  vbox_make_view_ptr {size_t} (view@ hndl_pro | &hndl_pro)
// end of [prval]

(* ******* ****** *)

val attr_pos = (GLuint)(uint_of 0)
val attr_texc = (GLuint)(uint_of 1)

(* ******* ****** *)

extern
fun shader_change (code: size_t): void = "shader_change"
implement shader_change (code) = let
  prval vbox pf_at = pf_view_hndl_pro in
  hndl_pro := code
end // end of [shader_change]

extern
fun texture_change (code: size_t): void = "texture_change"
implement texture_change (code) = let
  prval vbox pf_at = pf_view_hndl_tex in
  hndl_tex := code
end // end of [texture_change]

extern
fun keypress (code: natLt 8): void = "keypress"
implement keypress (code) = let
  fn addf {l:addr} (
      pf: vbox (GLfloat @ l)
    | p: ptr l, d: float
    ) :<!ref> void = let
    prval vbox pf_at = pf
  in
    !p := GLfloat (float_of !p + d)
  end // end of [addf]
  fn addz {l:addr} (
    pf: vbox (float @ l)
  | p: ptr l, d: float
  ) :<!ref> void = let
    prval vbox pf_at = pf in
    // do not go beyond 180 degrees FOV
    !p := float_of (max (0.26, double_of (!p) * pow (1.1, double_of d)))
  end // end of [addz]
in
  case+ code of
  | 0 => addf (pf_view_px | &view_px, 0.1f)
  | 1 => addf (pf_view_px | &view_px, ~0.1f)
  | 2 => addf (pf_view_py | &view_py, ~0.1f)
  | 3 => addf (pf_view_py | &view_py, 0.1f)
  | 4 => addz (pf_view_zoom | &view_zoom, 0.1f)
  | 5 => addz (pf_view_zoom | &view_zoom, ~0.1f)
  | 6 => addf (pf_view_rotz | &view_rotz, 5.0f)
  | 7 => addf (pf_view_rotz | &view_rotz, ~5.0f)
end // end of [keypress]

(* ****** ****** *)

local

// program and uniform locations
// (each such uniform is a free variable)
viewtypedef pbind = @{
  p= GLprogram
  // NOTE: -1 means "does not exist in a program",
  // and passing it to the corresponding functions
  // will have no effect (incl. no errors raised by GL)
, l_mvp= GLint  // model-view-projection matrix
, l_texw= GLint // gradient texture width
, l_texh= GLint // gradient texture height
} // end of [pbind]

// textures
viewtypedef tex = @{
  gt= GLtexture1
, gt_w= size_t
, gt_h= size_t
, rt= GLtexture1
} // end of [tex]

viewtypedef array4 (a:viewt@ype) = [l:addr] (free_gc_v (a?, 4, l), array_v (a, 4, l) | ptr l)

viewtypedef gpustate = @{
  texs= array4 tex
, prog= array4 pbind
} // end of [gpustate]

// AS: in need for a better name
viewtypedef state = Option_vt gpustate

val the_state = ref<state> (None_vt ())

fun state_free (x: &gpustate >> gpustate?): void = begin
  array_ptr_free_fun<tex> (
    x.texs.0, x.texs.1 | x.texs.2, 4
  , lam (x) => $effmask_all (glDeleteTexture x.gt; glDeleteTexture x.rt)
  );
  array_ptr_free_fun<pbind> (
    x.prog.0, x.prog.1 | x.prog.2, 4
  , lam (x) => $effmask_all (glDeleteProgram x.p)
  )
end // end of [state_free]

in // of [local]

fun the_state_destroy (): void = let
  val (vbox pf | p) = ref_get_view_ptr {state} (the_state)
in
  case+ !p of
  | Some_vt (!p1) => begin
      $effmask_all (state_free (!p1));
      free@ {gpustate} (!p);
      !p := None_vt ()
    end // end of [begin]
  | None_vt () => fold@ !p
end // end of [the_state_destroy]

fun the_state_init (): void = let
  fn init (): gpustate = let
    fn char_of_digit (x: sizeLt 4):<> c1har = case+ int1_of_size1 x of
      // by far the easiest way to persuade typechecker
      // that result is a non-zero character
      | 0 => '1' | 1 => '2' | 2 => '3' | 3 => '4'
    // end of [char_of_digit]
    fn load_programs (x: &(@[pbind?][4]) >> @[pbind][4]): void = let
      fn upload_program (i: sizeLt 4, res: &pbind? >> pbind): void = let
        var stat: GLint
        val fragShader = glCreateShader GL_FRAGMENT_SHADER
        val () = () where {
          var !p_buf with pf_buf = @[char]('t', 'e', 's', 't', '0', '4', '-', 'X', '.', 'f', 'r', 'a', 'g', '\000')
          val () = !p_buf.[7] := char_of_digit i
          prval pf1_buf = bytes_v_of_chars_v pf_buf
          val () = bytes_strbuf_trans (pf1_buf | p_buf, 13)
          val (fpf | str) = strbuf_takeout_ptr (pf1_buf | p_buf)
          val () = shader_from_file (fragShader, str)
          prval () = fpf (str)
          prval () = pf_buf := chars_v_of_b0ytes_v (bytes_v_of_strbuf_v pf1_buf)
        } // end of [where]
        val vertShader = glCreateShader GL_VERTEX_SHADER
        val () = shader_from_file (vertShader, "test04.vert")
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
        glBindAttribLocation (program, attr_texc, "texcoord");
        glLinkProgram program; // needed to put attribs into effect
        // setup known uniforms
        glUniform1i (glGetUniformLocation (program, "gradtexture"), (GLint)0);
        glUniform1i (glGetUniformLocation (program, "reftexture"), (GLint)1);
        // get uniform locations
        res.l_mvp := glGetUniformLocation (program, "mvp");
        res.l_texw := glGetUniformLocation (program, "texw");
        res.l_texh := glGetUniformLocation (program, "texh");
        res.p := program;
        // shader objects are reference-counted
        glDeleteShader vertShader;
        glDeleteShader fragShader
      end // end of [upload_program]
    in
      array_ptr_initialize_fun<pbind> (x, 4, upload_program)
    end // end of [load_programs]

    fn load_textures (x: &(@[tex?][4]) >> @[tex][4]): void = let
      fn upload_tex (i: sizeLt 4, tex: &tex? >> tex): void = () where {
        var w: size_t and h: size_t // uninitialized and not used
        var !p_buf with pf_buf = @[char](
          'd', 'a', 't', 'a', '/', 'l', 's', 'c', 't', '/', 't', 'e', 'x', '1', '.', 't', 'g', 'a', '\000'
        ) // end of [var]
        val () = !p_buf.[13] := char_of_digit i
        // "tex<i>.tga"
        // The special shader used to render this texture performs its own minification
        // and magnification. Specify nearest neighbor sampling for speed.
        val () = glGenTexture (tex.gt)
        val () = () where {
          prval pf1_buf = bytes_v_of_chars_v pf_buf
          val () = bytes_strbuf_trans (pf1_buf | p_buf, 18)
          val (fpf | str) = strbuf_takeout_ptr (pf1_buf | p_buf)
          val () = texture_from_file_dim (tex.gt, str, tex.gt_w, tex.gt_h, GL_NEAREST, GL_CLAMP_TO_EDGE)
          prval () = fpf (str)
          prval () = pf_buf := chars_v_of_b0ytes_v (bytes_v_of_strbuf_v pf1_buf)
        } // end of [where]
        // "ref<i>.tga"
        val () = glGenTexture (tex.rt)
        val () = () where {
          val () = (!p_buf.[10] := 'r'; !p_buf.[12] := 'f')
          prval pf1_buf = bytes_v_of_chars_v pf_buf
          val () = bytes_strbuf_trans (pf1_buf | p_buf, 18)
          val (fpf | str) = strbuf_takeout_ptr (pf1_buf | p_buf)
          val () = texture_from_file_dim (tex.rt, str, w, h, GL_LINEAR, GL_CLAMP_TO_EDGE)
          prval () = fpf (str)
          prval () = pf_buf := chars_v_of_b0ytes_v (bytes_v_of_strbuf_v pf1_buf)
        } // end of [where]
      } // end of [where]
    in
      array_ptr_initialize_fun<tex> (x, 4, upload_tex)
    end // end of [load_textures]
    
    val (pf1_gc, pf1_arr | p1) = array_ptr_alloc<tex> 4
    val (pf2_gc, pf2_arr | p2) = array_ptr_alloc<pbind> 4
  in
    load_textures (!p1); load_programs (!p2);
   @{texs= @(pf1_gc, pf1_arr | p1), prog= @(pf2_gc, pf2_arr | p2)}
  end // end of [init]
  val (vbox pf | p) = ref_get_view_ptr {state} (the_state)
in
  case+ !p of
  | Some_vt (!p1) => begin
      $effmask_all (state_free (!p1); !p1 := init ());
      fold@ !p
    end // end of [begin]
  | ~None_vt () => let
      val () = !p := Some_vt {gpustate} (?)
      val+ Some_vt (!p1) = !p
    in
      !p1 := $effmask_all (init ());
      fold@ !p
    end // end of [let]
end // end of [the_state_init]

fn the_state_draw (mvp: &GLmat4, tid: sizeLt 4, pid: sizeLt 4): void = let
  fun program_apply (x: &pbind, mvp: &GLmat4, texw: GLfloat, texh: GLfloat): void = let
    fun umat4 (loc: GLint, m: &GLmat4): void = () where {
      prval pf_mat1 = array_v_sing (view@ m)
      val p = &m
      val () = glUniformMatrix4fv (loc, (GLsizei)1, GL_FALSE, !p)
      prval () = view@ m := array_v_unsing pf_mat1
    } // end of [umat4]
  in
    glUseProgram x.p;
    umat4 (x.l_mvp, mvp);
    glUniform1f (x.l_texw, texw);
    glUniform1f (x.l_texh, texh)
  end // end of [program_apply]
  fn bind (unit: GLenum, x: !GLtexture1): void = begin
    glActiveTexture unit;
    glEnable GL_TEXTURE_2D;
    glBindTexture (GL_TEXTURE_2D, x)
  end // end of [bind]
  fun draw_quad (): void = let
    #define G GLfloat_of_float
    var !p_texcoord with pf_texcoord = @[GLfloat](
      G 0.0f, G 0.0f, G 1.0f, G 0.0f, G 0.0f, G 1.0f, G 1.0f, G 1.0f
    ) // end of [var]
    var !p_verts with pf_verts = @[GLfloat](
      G ~5.0f, G ~5.0f
    , G 5.0f,  G ~5.0f
    , G ~5.0f, G 5.0f
    , G 5.0f,  G 5.0f
    ) // end of [p_verts]
  in
    () where {
      prval pfmul = mul_make {4,2} ()
      prval pfmat = matrix_v_of_array_v {GLfloat} (pfmul, pf_verts)
      val () = glVertexAttribPointer (pfmat | attr_pos, (GLint)2, GL_FLOAT, GL_FALSE, (GLsizei)0, p_verts)
      prval (pfmul', pfarr) = array_v_of_matrix_v {GLfloat} (pfmat)
      prval () = mul_isfun (pfmul, pfmul')
      prval () = pf_verts := pfarr
    }; // end of [where]
    () where {
      prval pfmul = mul_make {4,2} ()
      prval pfmat = matrix_v_of_array_v {GLfloat} (pfmul, pf_texcoord)
      val () = glVertexAttribPointer (pfmat | attr_texc, (GLint)2, GL_FLOAT, GL_FALSE, (GLsizei)0, p_texcoord)
      prval (pfmul', pfarr) = array_v_of_matrix_v {GLfloat} (pfmat)
      prval () = mul_isfun (pfmul, pfmul')
      prval () = pf_texcoord := pfarr
    }; // end of [where]
    glEnableVertexAttribArray attr_pos;
    glEnableVertexAttribArray attr_texc;
    glDrawArrays (GL_TRIANGLE_STRIP, (GLint)0, (GLsizei)4);
    glDisableVertexAttribArray attr_pos;
    glDisableVertexAttribArray attr_texc;
  end // end of [draw_quad]
  fn draw (st: &gpustate, mvp: &GLmat4, tid: sizeLt 4, pid: sizeLt 4): void = let
    val (pf1_at, fpf1 | pt) = array_ptr_takeout<tex> (st.texs.1 | st.texs.2, tid)
    val (pf2_at, fpf2 | pp) = array_ptr_takeout<pbind> (st.prog.1 | st.prog.2, pid)
  in
    bind (GL_TEXTURE0, pt->gt);
    bind (GL_TEXTURE1, pt->rt);
    program_apply (
      !pp, mvp
    , (GLfloat)(float_of (pt->gt_w))
    , (GLfloat)(float_of (pt->gt_h))
    );
    draw_quad ();
    () where {
      prval () = st.texs.1 := fpf1 (pf1_at)
      prval () = st.prog.1 := fpf2 (pf2_at)
    } // end of [where]
  end // end of [draw]
  val (vbox pf | p) = ref_get_view_ptr {state} (the_state)
in
  case+ !p of
  | Some_vt (!p1) => ($effmask_all (draw (!p1, mvp, tid, pid)); fold@ (!p))
  | None_vt () => fold@ (!p)
end // end of [the_state_draw]

end // end of [local]

(* ****** ****** *)

extern
fun init (): void = "init"
implement init () = begin
  the_state_init ();
  glClearColor (x, x, x, (GLclampf)0.0f) where {
    val x = (GLclampf)0.4f
  }; // end of [where]
  assert_errmsg (glGetError () = GL_NO_ERROR, "[init]: glGetError() <> GL_NO_ERROR")
end // end of [init]

(* ****** ****** *)

extern
fun draw (): void = "draw"
implement draw () = let
  #define F float_of
  #define G GLfloat_of_float

  // NOTE: [mvp] maps from object space to clip space
  var !p_mvp = @[GLfloat][16]()
  // AS: note the way transforms are concatenated, essentially,
  // the syntactically "last" operation is applied first
  // (that is, we first go from object space to world space,
  // then from world space to clip space -- opposite to the textual order)
  val () = () where {
    val w = winx where { prval vbox pf_at = pf_view_winx }
    val h = winy where { prval vbox pf_at = pf_view_winy }
    val () = glViewport (GLint_of_int1 0, GLint_of_int1 0,
                         GLsizei_of_int1 (int1_of_int w), GLsizei_of_int1 (int1_of_int h))
    // world -> clip
    // handle interactive zoom by changing FOV (scaling would cause clipping)
    val () = mat_persp (30.0f / zoom, float_of w / float_of h, 1.0f, 300.0f, !p_mvp) where {
      prval vbox pf_at = pf_view_zoom
      val zoom = view_zoom
    } // end of [where]
    val () = mat_trn (!p_mvp, px, py, G ~5.0f) where {
      val px = view_px where { prval vbox pf_at = pf_view_px }
      val py = view_py where { prval vbox pf_at = pf_view_py }
    } // end of [where]
    val () = mat_rot (!p_mvp, G (2.0f * F M_PI * F view_rotz / 360.0f), G 0.0f, G 0.0f, G 1.0f) where {
      prval vbox pf = pf_view_rotz
    } // end of [where]
    // object -> world
    val () = mat_trn (!p_mvp, G 0.0f, G 0.0f, G ~5.0f)
    val () = mat_rot (!p_mvp, G (F M_PI), G 1.0f, G 0.0f, G 0.0f)
  } // end of [where]
in
  glClear (GL_COLOR_BUFFER_BIT lor GL_DEPTH_BUFFER_BIT);
  the_state_draw (!p_mvp, tid, pid) where {
    val tid = size1_of_size hndl_tex where { prval vbox pf = pf_view_hndl_tex }
    val () = assert_errmsg (tid >= 0 && tid < 4, "INTERNAL ERROR: texture identifier is out of bounds")
    val pid = size1_of_size hndl_pro where { prval vbox pf = pf_view_hndl_pro }
    val () = assert_errmsg (pid >= 0 && pid < 4, "INTERNAL ERROR: program identifier is out of bounds")
  } // end of [where]
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
      // AS: GLES2 -> GL2.1
      EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT /* EGL_OPENGL_ES2_BIT */,
      EGL_NONE
   };
   static const EGLint ctx_attribs[] = {
     // AS: GLES2 -> GL2.1
     //      EGL_CONTEXT_CLIENT_VERSION, 2,
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

   // assert(config);
   // assert(num_configs > 0);

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
   attr.event_mask = StructureNotifyMask | ExposureMask | KeyPressMask
     | ButtonPressMask | ButtonReleaseMask;
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

   eglBindAPI(/* EGL_OPENGL_ES_API */ EGL_OPENGL_API); // AS: GLES2 -> GL2.1

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
	    case XK_F1: shader_change (0); break;
	    case XK_F2: shader_change (1); break;
	    case XK_F3: shader_change (2); break;
	    case XK_F4: shader_change (3); break;
	    case XK_1: texture_change (0); break;
	    case XK_2: texture_change (1); break;
	    case XK_3: texture_change (2); break;
	    case XK_4: texture_change (3); break;
	    case XK_Left: keypress(0); break; // pan
	    case XK_Right: keypress(1); break; // pan
	    case XK_Up: keypress(2); break; // pan
	    case XK_Down: keypress(3); break; // pan
	    default:
               r = XLookupString(&event.xkey, buffer, sizeof(buffer),
                                 NULL, NULL);
	       if (buffer[0] == 'a' || buffer[0] == 'A') {
		 keypress(4); // zoom in
	       }
	       else if (buffer[0] == 'z' || buffer[0] == 'Z') {
		 keypress(5); // zoom out
	       }
	       else if (buffer[0] == 'x' || buffer[0] == 'X') {
		 keypress(6); // rotate around Z
	       }
	       else if (buffer[0] == 'c' || buffer[0] == 'c') {
		 keypress(7); // rotate around Z
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

   for (i = 1; i < argc; i++) {
     if (strcmp(((char **)argv)[i], "-display") == 0) {
       dpyName = ((char **)argv)[i+1]; // FIXME: reading past end?
       i++;
     }
     else if (strcmp(((char **)argv)[i], "-info") == 0) {
       printInfo = GL_TRUE;
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

   init();

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

(* end of [GLES2-test04.dats] *)
