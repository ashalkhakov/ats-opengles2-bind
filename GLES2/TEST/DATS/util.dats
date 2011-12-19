//
// Author: Artyom Shakhakov (artyom DOT shalkhakov AT gmail DOT com)
// Time: May, 2011
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no need for staloading at run-time
#define ATS_DYNLOADFLAG 0 // no need for dynloading at run-time

(* ****** ****** *)

staload "libc/SATS/stdio.sats"

staload "libats/SATS/biarray.sats"
staload "libats/SATS/bimatrix.sats"

staload "contrib/GLES2/SATS/gl2.sats"

staload "util.sats"

staload _ = "prelude/DATS/array.dats"

(* ****** ****** *)
//
// primitive file reading functions
//
fn fr_byte (fl: &FILE r): uint = let
  val res = fgetc_err (file_mode_lte_r_r | fl)
in
  if res >= 0 then uint_of res
  else begin
    exit_errmsg {uint 0} (1, "ATS: [image_input]: unexpected end of file\n")
  end // end of [if]
end // end of [fr_byte]

fn fr_short (fl: &FILE r): uint = let
  val r1 = fr_byte fl
  val r2 = fr_byte fl
  val res = r1 lor (r2 << 8)
in
  res
end

fn fr_int (fl: &FILE r): uint = let
  val r1 = fr_short fl
  val r2 = fr_short fl
in
  // bytes are read in this order: X Y Z W
  // and the result is: X lor (Y << 8) lor (Z << 16) lor (W << 24)
  r1 lor (r2 << 16)
end // end of [fr_int]

(* ****** ****** *)

implement shader_from_source (x, src) = let
  val () = glShaderSource__string (x, src)
  val () = glCompileShader x
  var stat: GLint // uninitialized
  val () = glGetShaderiv (x, GL_COMPILE_STATUS, stat)
in
  if int_of_GLint stat = 0 then let
    #define BUFSZ 1024
    var !p_log with pf_log = @[byte][BUFSZ]()
    var len: GLsizei // uninitialized
    prval pf_len = Some_v (view@ (len))
    val () = glGetShaderInfoLog (pf_log, pf_len | x, (GLsizei)BUFSZ, &len, p_log)
    prval Some_v pf = pf_len; prval () = view@ (len) := pf
    val () = print ("Shader compilation error:\n")
    val () = fprint_strbuf (stdout_ref, !p_log) // FIXME: [print_strbuf] doesn't compile
    val () = print_newline ()
    prval () = pf_log := bytes_v_of_strbuf_v (pf_log)
  in
    exit {void} (1)
  end // end of [if]
end // end of [shader_from_source]

(* ****** ****** *)

implement shader_from_file (x, name) = let
  fun file_length {m:fm} (pf_mod: file_mode_lte (m, r) | f: &FILE m): lint = let
    val ofs = ftell_exn f
    val () = fseek_exn (f, lint_of_int 0, SEEK_END)
    val sz = ftell_exn f
    val () = fseek_exn (f, ofs, SEEK_SET)
  in
    sz
  end // end of [file_length]

  extern
  fun fread_b0yte_exn
    {n_buf:int}
    {n:nat | n <= n_buf}
    {m:fm} (
    pf_mod: file_mode_lte (m, r)
  | buf: &b0ytes (n_buf) >> bytes (n_buf), n: size_t n, f: &FILE m
  ) :<!exn> void = "atslib_fread_byte_exn"
  // end of [fread_b0yte_exn]

  val (pfopt | p_ifp) = fopen_err (name, file_mode_r)
in
  if p_ifp > null then let
    prval Some_v (pf) = pfopt
    val len = size1_of_size (size_of_lint (file_length (file_mode_lte_r_r | !p_ifp)))
    val (pfopt_mem | p_mem) = malloc_ngc (len + 1)
  in
    if p_mem > null then let
      prval malloc_v_succ (pf_ngc, pf_mem) = pfopt_mem
      val () = fread_b0yte_exn (file_mode_lte_r_r | !p_mem, len, !p_ifp)
      val () = bytes_strbuf_trans (pf_mem | p_mem, len)
      val (fpf | str) = strbuf_takeout_ptr (pf_mem | p_mem)
      val () = shader_from_source (x, str)
      prval () = fpf (str)
      prval () = pf_mem := bytes_v_of_strbuf_v (pf_mem)
    in
      free_ngc (pf_ngc, pf_mem | p_mem);
      fclose_exn (pf | p_ifp)
    end else let
      prval malloc_v_fail () = pfopt_mem
      val () = prerrf ("[shader_from_file]: failed to allocate %d bytes\n", @(int_of_size len))
    in
      fclose_exn (pf | p_ifp);
      exit {void} (1)
    end // end of [if]
  end else let
    prval None_v () = pfopt
  in
    prerrf ("[shader_from_file]: can't open [%s]\n", @("name"));
    exit {void} (1)
  end // end of [if]
end // end of [shader_from_file]

(* ****** ****** *)
// TGA image loading

local

fun row_read {n:nat} {l1,l2:addr} (
  pf_arr: !biarray_v (uint?, n, l1, l2) >> biarray_v (uint, n, l1, l2)
| base: ptr l1
, n: size_t n
, f: (&FILE r) -> uint
, env: &FILE r
) : ptr l2 = let
  fun loop {n,m:nat} {l1,l2,l3:addr} .<m>. (
      pf1_arr: biarray_v (uint, n, l1, l2)
    , pf2_arr: biarray_v (uint?, m, l2, l3)
    | p1: ptr l1, p2: ptr l2, m: size_t m, f: (&FILE r) -> uint, env: &FILE r
    ) : (biarray_v (uint, n+m, l1, l3) | ptr l3) =
    if m = 0 then let
      prval () = biarray_v_unnil {uint?} (pf2_arr) in
      (pf1_arr | p2)
    end else let
      prval (pf_at, pf21_arr) = biarray_v_uncons {uint?} (pf2_arr)
      val () = !p2 := f (env)
    in
      loop (biarray_v_snoc {uint} (pf1_arr, pf_at), pf21_arr | p1, p2+sizeof<uint>, m-1, f, env)
    end // end of [loop]
  val (pf1_arr | res) = loop (biarray_v_nil {uint} (), pf_arr | base, base, n, f, env)
in
  pf_arr := pf1_arr; res
end // end of [row_read]

fun row_read_rle {n:nat} {l1,l2:addr} (
  pf_arr: !biarray_v (uint?, n, l1, l2) >> biarray_v (uint, n, l1, l2)
| p: ptr l1
, n: size_t n
, f: (&FILE r) -> uint
, env: &FILE r
): ptr l2 = let
  #define i2s size_of_int
  fun row_read_cst {n:nat} {l1,l2:addr} (
    pf_arr: !biarray_v (uint?, n, l1, l2) >> biarray_v (uint, n, l1, l2)
  | base: ptr l1, n: size_t n, x: uint
  ) : ptr l2 = let
    prval pf1_mul = biarray_v_offset {uint?} (pf_arr)
    prval pf1_arr = array_v_of_biarray_v {uint?} (pf_arr)
    val () = array_ptr_initialize_elt<uint> (!base, n, x)
    prval () = pf_arr := biarray_v_of_array_v {uint} (pf1_mul, pf1_arr)
    val (pf2_mul | nsz) = n szmul2 sizeof<uint>
    prval () = mul_isfun (pf1_mul, pf2_mul)
  in
    base+nsz
  end // end of [row_read_cst]

  extern
  prfun biarray_v_split
    {a:viewt@ype}
    {n:int} {i:nat | i <= n}
    {l1,l2:addr} (
    pf: biarray_v (a, n, l1, l2)
  ) :<prf> [l:addr] (biarray_v (a, i, l1, l), biarray_v (a, n-i, l, l2))

  extern
  prfun biarray_v_unsplit
    {a:viewt@ype}
    {n1,n2:int}
    {l1,l2,l3:addr} (
    pf1: biarray_v (a, n1, l1, l2)
  , pf2: biarray_v (a, n2, l2, l3)
  ) :<prf> biarray_v (a, n1+n2, l1, l3)

  fun loop {n,m:nat} {l1,l2,l3:addr} .<m>. (
      pf1_arr: biarray_v (uint, n, l1, l2)
    , pf2_arr: biarray_v (uint?, m, l2, l3)
    | p1: ptr l1, p2: ptr l2, m: size_t m, f: (&FILE r) -> uint, env: &FILE r
    ) : (biarray_v (uint, n+m, l1, l3) | ptr l3) =
    if m = 0 then let
      prval () = biarray_v_unnil {uint?} (pf2_arr)
    in
      (pf1_arr | p2)
    end else let
      val hd = size_of_uint (fr_byte env)
      val sz = size1_of_size (hd land (i2s 0x7f)) + 1
      stavar sz:int
      val sz = sz: size_t sz
      val () = assert_errmsg (sz <= m, "[row_read_rle]: spans rows")
      prval (pf31_arr, pf32_arr) = biarray_v_split (pf2_arr) // : (biarray_v (a?, sz, l2, lx), biarray_v (a?, m-sz, lx, l3))
      stavar lx:addr
      prval pf31_arr = pf31_arr: biarray_v (uint?, sz, l2, lx)
      val p4 =
        if :(pf31_arr : biarray_v (uint, sz, l2, lx)) => land_size_size (hd, i2s 0x80) > i2s 0 then begin
          row_read_cst (pf31_arr | p2, sz, f env)
        end else row_read (pf31_arr | p2, sz, f, env)
      // end of [val]
      prval pf_arr = biarray_v_unsplit (pf1_arr, pf31_arr) // : biarray_v (a, n+sz, l1, lx)
    in
      loop (pf_arr, pf32_arr | p1, p4, m-sz, f, env)
    end // end of [loop]
  val (pf_biarr | res) = loop (
      biarray_v_nil {uint} (), pf_arr
    | p, p, n, f, env
    ) // end of [val]
  prval () = pf_arr := pf_biarr
in
  res
end // end of [row_read_rle]

fun image_initialize_topdown {m,n:nat} {l:addr} (
  pf_mat: !matrix_v (uint?, m, n, l) >> matrix_v (uint, m, n, l)
| base: ptr l
, row: size_t m, col: size_t n
, f: {l1,l2:addr} (
    !biarray_v (uint?, n, l1, l2) >> biarray_v (uint, n, l1, l2)
  | ptr l1, size_t n, (&FILE r) -> uint, &FILE r
  ) -> ptr l2
, g: (&FILE r) -> uint
, env: &FILE r
) : void = let
  fun loop {k:nat | k <= m} {l1,l2:addr} (
    pf_m: !bimatrix_v (uint?, m-k, n, l1, l2) >> bimatrix_v (uint, m-k, n, l1, l2)
  | r: size_t k, row: size_t m, col: size_t n
  , p: ptr l1
  , f: {l1,l2:addr} (
      !biarray_v (uint?, n, l1, l2) >> biarray_v (uint, n, l1, l2)
    | ptr l1, size_t n, (&FILE r) -> uint, &FILE r
    ) -> ptr l2
  , g: (&FILE r) -> uint
  , env: &FILE r
  ): void = if r < row then let
    prval (pf_at, pf1_m) = bimatrix_v_uncons {uint?} (pf_m)
    prval pf1_mul = biarray_v_offset {uint?} (pf_at)
    val p1 = f (pf_at | p, col, g, env)
  in
    loop (pf1_m | r+1, row, col, p1, f, g, env);
    pf_m := bimatrix_v_cons (pf_at, pf1_m)
  end else let
    prval () = bimatrix_v_unnil {uint?} (pf_m)
  in
    pf_m := bimatrix_v_nil {uint} ()
  end // end of [loop]
  prval pf1_mat = bimatrix_v_of_matrix_v (pf_mat)
in
  loop (pf1_mat | 0, row, col, base, f, g, env);
  pf_mat := matrix_v_of_bimatrix_v (pf1_mat)
end // end of [image_initialize_topdown]

fun image_initialize_bottomup {m,n:nat} {l:addr} (
  pf_mat: !matrix_v (uint?, m, n, l) >> matrix_v (uint, m, n, l)
| base: ptr l
, row: size_t m, col: size_t n
, f: {l1,l2:addr} (
    !biarray_v (uint?, n, l1, l2) >> biarray_v (uint, n, l1, l2)
  | ptr l1, size_t n, (&FILE r) -> uint, &FILE r
  ) -> ptr l2
, g: (&FILE r) -> uint
, env: &FILE r
) : void = let
  fun loop {k:nat | k <= m} {l1,l2:addr} (
    pf_m: !bimatrix_v (uint?, m-k, n, l1, l2) >> bimatrix_v (uint, m-k, n, l1, l2)
  | r: size_t k, row: size_t m, col: size_t n
  , p: ptr l2
  , f: {l1,l2:addr} (
      !biarray_v (uint?, n, l1, l2) >> biarray_v (uint, n, l1, l2)
    | ptr l1, size_t n, (&FILE r) -> uint, &FILE r
    ) -> ptr l2
  , g: (&FILE r) -> uint
  , env: &FILE r
  ) : void = if r < row then let
    prval [l:addr] (pf1_m, pf_at) = bimatrix_v_unsnoc {uint?} (pf_m)
    prval pf1_mul = biarray_v_offset {uint?} (pf_at)
    val [ofs:int] (pf2_mul | ofs) = col szmul2 sizeof<uint>
    prval () = mul_isfun (pf1_mul, pf2_mul)
    val p1 = (p-ofs) : ptr (l2-ofs)
    prval () = (): [l == l2-ofs] void
    val _ = f (pf_at | p1, col, g, env)
    val () = loop (pf1_m | r+1, row, col, p1, f, g, env);
    prval () = pf_m := bimatrix_v_snoc (pf1_m, pf_at)
  in
    // nothing
  end else let
    prval () = bimatrix_v_unnil {uint?} (pf_m)
    prval () = pf_m := bimatrix_v_nil {uint} ()
  in
    // nothing
  end // end of [loop]
  prval pf1_mat = bimatrix_v_of_matrix_v (pf_mat)
  prval (pf11_mul, pf12_mul) = bimatrix_v_offset (pf1_mat)
  val (pf21_mul | rc) = row szmul2 col
  prval () = mul_isfun (pf11_mul, pf21_mul)
  val (pf22_mul | ofs) = rc szmul2 sizeof<uint>
  prval () = mul_isfun (pf12_mul, pf22_mul)
  val () = loop (pf1_mat | 0, row, col, base+ofs, f, g, env);
  prval () = pf_mat := matrix_v_of_bimatrix_v (pf1_mat)
in
  // nothing
end // end of [image_initialize_bottomup]

in // of [local]

extern
fun image_input (
    fl: &FILE r
  , w: &size_t? >> size_t w
  , h: &size_t? >> size_t h
  , psz: &size_t? >> size_t
  , p: &ptr? >> ptr l
  ) : #[w,h:nat] #[l:addr] (
    matrix_v (uint, w, h, l)
  | (matrix_v (uint, w, h, l) | ptr l) -<lin> void
  )

implement image_input (fl, w, h, psz, p) = let
  val id_len = fr_byte fl
  val () = () where {
    val cmt = fr_byte fl // color map type
    val () = assert_errmsg (cmt = 0u, "[image_input]: color-mapped TGA images not supported\n")
  } // end of [where]
  val imt = fr_byte fl // image type (2 for uncompressed RGB, 3 for uncompressed grayscale)
  val () = assert_errmsg (
      imt = 2u || imt = 3u || imt = 10u || imt = 11u
    , "[image_input]: only type 2 (RGB), 3 (grayscale), 10 (RGB RLE), 11 (grayscale RLE) TGA images are supported\n"
    ) // end of [val]
  val _ = fr_short fl  // color map index
  val _ = fr_short fl  // color map length
  val _ = fr_byte fl   // color map size
  val ox = fr_short fl // x origin
  val oy = fr_short fl // y origin
  val [m:int] col = uint1_of_uint (fr_short fl) // width
  val col = size1_of_uint1 col
  val () = assert_errmsg (col > 0, "[image_input]: zero width!")
  val [n:int] row = uint1_of_uint (fr_short fl) // height
  val row = size1_of_uint1 row
  val () = assert_errmsg (row > 0, "[image_input]: zero height!")
  val ps = fr_byte fl  // pixel size
  val () = begin
    if ps = 32u || ps = 24u then assert_errmsg (imt = 2u || imt = 10u, "only 24- or 32-bit images supported for RGB")
    else if ps = 8u then assert_errmsg (imt = 3u || imt = 11u, "only 8-bit images supported for grayscale")
  end // end of [val]
  val () = psz := size_of_uint ps
  val ar = fr_byte fl  // attributes
  val () = fseek_exn (fl, lint_of (int_of_uint id_len), SEEK_CUR) // skip comment
  val (pf1_mul | sz1) = col szmul2 row
  val (pf2_mul | sz2) = sz1 szmul2 sizeof<uint>
  val [l:addr] (pf_optmem | p_mem) = malloc_ngc (sz2)
in
  if p_mem > null then let
    prval malloc_v_succ (pf_ngc, pf_mem) = pf_optmem
    prval pf_arr = array_v_of_bytes_v {uint} (pf2_mul, pf_mem)
    prval pf_mat = matrix_v_of_array_v (pf1_mul, pf_arr)

    val () = printf ("[image_input]: loading TGA, %d x %d pixels, pixel size %d bpp, image type %d\n"
                    , @(int1_of_size1 col, int1_of_size1 row, int_of_uint ps, int_of_uint imt))

    val appf = begin
      case+ 0 of
      | _ when ps = 8u => lam (f: &FILE r): uint => let
          val b = fr_byte f in ((255u << 24) lor (b << 16) lor (b << 8) lor b)
        end // end of [lam]
      | _ when ps = 24u => lam (f: &FILE r): uint => let
          val b = fr_byte f; val g = fr_byte f; val r = fr_byte f in
          ((255u << 24) lor (b << 16) lor (g << 8) lor r)
        end // end of [lam]
      | _ when ps = 32u => lam (f: &FILE r): uint => let
          val b = fr_byte f; val g = fr_byte f; val r = fr_byte f
          val a = fr_byte f in
          ((a << 24) lor (b << 16) lor (g << 8) lor r)
        end // end of [lam]
      | _ => exit_errmsg (1, "[image_input]: illegal pixel size\n")
    end // end of [begin]
    val readf = if imt = 10u || imt = 11u then row_read_rle {n} else row_read {n}
  in
    // small terminological confusion here:
    // - OpenGL assumes image rows go bottom-up (with increasing memory addresses)
    // - normally in ATS, I would assume image rows go top-down (with increasing memory addresses)
    // - in TGA, images usually go bottom-up (with increasing memory addresses)
    if :(pf_mat: matrix_v (uint, m, n, l)) =>  (ar land 0x20u) = 0u then begin
      // bottom-up in TGA, no need to invert
      image_initialize_topdown (pf_mat | p_mem, col, row, readf, appf, fl)
    end else begin
      // top-down in TGA, need to invert (go BOTTOM-UP)
      image_initialize_bottomup (pf_mat | p_mem, col, row, readf, appf, fl)
    end; // end of [if]
    w := col; h := row; p := p_mem;
    #[.. | (pf_mat | lam (pf_mat | p_mat) =<lin> let
      prval (pf_mul, pf_arr) = array_v_of_matrix_v (pf_mat)
      prval () = mul_isfun (pf_mul, pf1_mul)
      prval (pf_mul, pf_mem) = bytes_v_of_array_v {uint} (pf_arr)
      prval () = mul_isfun (pf_mul, pf2_mul)
    in
      free_ngc (pf_ngc, pf_mem | p_mat)
    end) ]
  end else let
    prval malloc_v_fail () = pf_optmem
    #define s2i int1_of_size1
  in
    prerrf ("[image_input]: failed to allocate %d bytes (for %dx%d image)\n", @(s2i sz2, s2i col, s2i row));
    // by insistence of typechecker ...
    w := size1_of_int1 0; h := size1_of_int1 0; p := null;
    exit (1)
  end // end of [if]
end // end of [image_input]

end // of [local]

implement texture_from_file (tex, filename): void = let
//
  val () = prerr "[texture_from_file]: trying "
  val () = prerr filename
  val () = prerr_newline ()
//
  val (pf_fl | p_fl) = fopen_exn (__cast filename, file_mode_r) where {
    // NB: [fopen_exn] does not hold onto the passed string
    extern castfn __cast (x: !strptr1):<> string
  } // end of [where]
  var imw: size_t and imh: size_t and psz: size_t and p_im: ptr // uninitialized
  val (pf_mat | free) = image_input (!p_fl, imw, imh, psz, p_im)
  val () = fclose_exn (pf_fl | p_fl)
  fn is_pow2 (x: size_t):<> bool = (x land (x-1)) = 0 && x > 0
in
  assert_errmsg (is_pow2 imw, "[texture_from_file]: width is not power of two!");
  assert_errmsg (is_pow2 imh, "[texture_from_file]: height is not power of two!");
  glBindTexture (GL_TEXTURE_2D, tex);
  TexImage2D (
      GL_TEXTURE_2D, (GLint)0, ifmt
    , (GLsizei)imw, (GLsizei)imh
    , 0, GL_RGBA, GL_UNSIGNED_BYTE, !p_im
    ) where {
    val ifmt = if psz = 24 then GL_RGB else GL_RGBA
    extern
    fun TexImage2D
      {a:t@ype} {w,h:int} (
      target: GLenum
    , level: GLint
    , internalFormat: GLenum
    , width: GLsizei w
    , height: GLsizei h
    , border: natLt(2)
    , format: GLenum
    , type: GLenum_type (a)
    , texels: &mtrxt (uint, w, h)
    ) : void
      = "mac#atsctrb_glTexImage2D"
    // end of [TexImage2D]
  }; // end of [where]
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  // NOTE: in some cases, we need CLAMP_TO_EDGE, in others, REPEAT
  // (REPEAT allows us to "tile" a texture)
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
(*
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
*)
  free (pf_mat | p_im)
end // end of [texture_from_file]
