staload "GLES2/SATS/gl2.sats"

fun shader_from_source {l:agz} (x: !GLshader, src: !strptr l): void
fun shader_from_file (x: !GLshader, name: !READ(string)): void

fun texture_from_file (x: !GLtexture, filename: !READ(string)): void