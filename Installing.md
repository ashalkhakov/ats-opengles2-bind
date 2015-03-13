# Prerequisites #

It is assumed that you have set up a working installation of GLES2 (for now, we are developing against the Xorg/Mesa GLES2 implementation on Linux).

Moreover, the bindings will only work with ATS unstable for now, since they depend on some modules that are not included into ATS 0.2.6, namely:
  * `libats/SATS/bivector.sats` and `libats/SATS/bimatrix.sats`
  * re-entrant lexing mode of `atslex` tool

# The procedure #

To compile the bindings into an `.o`-file, please do the following:
  * navigate to your `$ATSHOME/contrib/` directory, and run [gles2](make.md) -- this will create a subdirectory `GLES2` in your "contrib" directory and place the code there
  * in the `$ATSHOME/contrib/GLES2` directory, run `make atsctrb_GLES2.o` to produce `atsctrb_GLES2.o`

To compile examples, please do the following (assuming that `atsctrb_GLES2.o` has been built successfully):
  * `cd` into the `TEST` subdirectory
  * issue `make test01` if you want to build the first test (and likewise for others, there are currently three tests to choose from)
  * if everything goes as expected, there should be a `test01` executable in the current directory

Bear in mind that some of the example programs rely upon external resources (images, meshes, shaders) being present -- you will have to get some of these yourself (or write a note to the author on where they could find some data that is free of licensing issues).

# Some background #

The tests have been successfully compiled on Ubuntu 10.10 x86 with ATS/Anairiats unstable and GCC 4.4.4.