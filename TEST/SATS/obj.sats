(*
** Wavefront OBJ file loader based on the GLM library by Nate Robbins.
** Written by Artyom Shalkhakov, June 2011.
*)

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no staloading at run-time

(* ****** ****** *)

typedef float2_t = @(float, float)
typedef float3_t = @(float, float, float)
typedef size3_t = @(size_t, size_t, size_t)
// zero-based!
typedef sizeOptLt (n:int) = [i:nat | n <= 0 || i < n] size_t (i) // n > 0 -> i < n
typedef faceidx (nv:int, nn:int, ntc:int) = @{vidx= sizeLt nv, nidx= sizeOptLt nn, tidx= sizeOptLt ntc}
typedef faceidx = [v,n,tc:nat] @{
  vidx= size_t v
, nidx= size_t n
, tidx= size_t tc
} // end of [faceidx]
// AS: note on order of winding:
// - OpenGL (ES) uses counter-clockwise as the default
// - OBJ: unsure (seems counter-clockwise)
typedef triangle = @(faceidx, faceidx, faceidx)
typedef triangle (nv:int, nn:int, ntc:int) = @(
  faceidx (nv, nn, ntc)
, faceidx (nv, nn, ntc)
, faceidx (nv, nn, ntc)
) // end of [triangle]

(* ****** ****** *)

viewtypedef arrsz_vt (a:viewt@ype, n:int, l:addr) =
  @(array_v (a, n, l), free_gc_v (a?, n, l) | size_t n, ptr l)
// end of [arrsz_vt]

viewtypedef arrsz_vt (a:viewt@ype, n:int) = [l:addr] arrsz_vt (a, n, l)
viewtypedef arrsz0_vt (a:viewt@ype) = arrsz_vt (a, 0, null)

(* ****** ****** *)

viewtypedef surf (nm:int, nv:int, nn:int, ntc:int, nf:int) = @{
  mtl= sizeLt nm
, faces= arrsz_vt (triangle (nv, nn, ntc), nf)
} // end of [surf]

viewtypedef surf (nm:int, nv:int, nn:int, ntc:int) = [nf:nat] surf (nm, nv, nn, ntc, nf)
viewtypedef surf = [nm,nv,nn,ntc:nat] surf (nm, nv, nn, ntc)

viewtypedef surf0 = @{
  mtl= size_t
, faces= arrsz0_vt triangle?
} // end of [surf0]

// NOTE: only diffuse mapping ATM
viewtypedef mtl = @{
  id= strptr1      // the same as in the source MTL file
, kd= float3_t     // diffuse color
, map_kd= strptr0  // empty or path relative to OBJ file
} // end of [mtl]

viewtypedef mesh (nm:int, nv:int, nn:int, ntc:int, ns:int) = @{
  mtllib= arrsz_vt (mtl, nm)
, verts= arrsz_vt (float3_t, nv)
, norms= arrsz_vt (float3_t, nn)
// AS: OBJ considers [(0,0)] to be the top left of a texture
// (will need to flip [y] coordinate in order to use with OpenGL)
, texcoords= arrsz_vt (float2_t, ntc)
, surfs= arrsz_vt (surf (nm, nv, nn, ntc), ns)
} // end of [mesh]

viewtypedef mesh (nm:int, ns:int) = [nv,nn,ntc:nat] mesh (nm, nv, nn, ntc, ns)
viewtypedef mesh = [nm,ns:nat] mesh (nm, ns)

viewtypedef mesh0 = @{
  mtllib= arrsz0_vt mtl
, verts= arrsz0_vt float3_t
, norms= arrsz0_vt float3_t
, texcoords= arrsz0_vt float2_t
, surfs= arrsz0_vt surf0
} // end of [mesh0]

(* ****** ****** *)

// may throw an exception
fun mesh_from_file (
 name: string, m: &mesh0? >> mesh
) : void

fun mesh_free (m: mesh): void

(* ****** ****** *)

(* end of [obj.sats] *)
