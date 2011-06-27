#
#
# Some programs involving OpenGL ES 2.0
#
#

######

ATSUSRQ="$(ATSHOME)"
ATSLIBQ="$(ATSHOME)"
ifeq ("$(ATSHOME)","")
ATSUSRQ="/usr"
ATSLIBQ="/usr/lib/ats-anairiats-0.2.4"
endif

######

ATSCC=$(ATSUSRQ)/bin/atscc
ATSLEX=$(ATSUSRQ)/bin/atslex
HOME=/home/artyom/proj/ats-ogles2
ATSMISCCTRB=$(HOME)/GLES2/atsctrb_GLES2.o

######

.PHONY:: all
all:: checkall
all:: cleanall

######

checkall::
cleanall:: clean

######

checkall:: test01 test02 test03
cleanall:: ; $(RMF) test01 test02 test03

util.o: util_sats.o util_dats.o
	ld -r -o $@ util_sats.o util_dats.o

util_sats.o: SATS/util.sats
	$(ATSCC) -IATS $(HOME) -I $(HOME) -o $@ -c SATS/util.sats
util_dats.o: DATS/util.dats
	$(ATSCC) -IATS $(HOME) -I $(HOME) -o $@ -c DATS/util.dats

obj_sats.o: SATS/obj.sats
	$(ATSCC) -IATS $(HOME) -I $(HOME) \
	  -o $@ -c SATS/obj.sats
######

obj_lats.dats: DATS/obj.lats
	$(ATSLEX) < $< > $@
obj_lats_dats.o: obj_lats.dats obj_sats.o
	$(ATSCC) -IATS $(HOME) -I $(HOME) \
	  -o $@ -c obj_lats.dats obj_sats.o

######

mat4.o: mat4_sats.o mat4_dats.o
	ld -r -o $@ mat4_sats.o mat4_dats.o

mat4_sats.o: SATS/mat4.sats
	$(ATSCC) -IATS $(HOME) -I $(HOME) -o $@ -c SATS/mat4.sats -lm
mat4_dats.o: DATS/mat4.dats
	$(ATSCC) -IATS $(HOME) -I $(HOME) -o $@ -c DATS/mat4.dats -lm

######

test01: GLES2-test01.dats util.o
	$(ATSCC) -IATS $(HOME) -I $(HOME) \
	  -o test01 GLES2-test02.dats util.o \
	  $(ATSMISCCTRB) -lm -lX11 -lEGL -lGLESv2

######

test02: GLES2-test02.dats util.o
	$(ATSCC) -IATS $(HOME) -I $(HOME) \
	  -o test02 GLES2-test02.dats util.o \
	  $(ATSMISCCTRB) -lm -lX11 -lEGL -lGLESv2

######

test03: GLES2-test03.dats util.o obj_lats_dats.o mat4.o
	$(ATSCC) -IATS $(HOME) -I $(HOME) \
	  -o test03 GLES2-test03.dats \
	   prelude/DATS/list_vt.dats libats/DATS/rarray.dats libats/SATS/rarray.sats libc/SATS/stdlib.sats libc/SATS/fcntl.sats \
	   libc/sys/SATS/types.sats \
	   libats/lex/lexing.dats libats/lex/lexing.sats libats/lex/tables.dats \
	   libats/SATS/genarrays.sats libats/DATS/genarrays.dats libats/DATS/fmatrix.dats libats/SATS/fmatrix.sats \
	   prelude/DATS/reference.dats \
	  $(ATSMISCCTRB) util.o obj_lats_dats.o mat4.o \
	  -lX11 -lEGL -lGLESv2 -lats_lex

#######

RMF = rm -f

#######

clean:
	$(RMF) *~
	$(RMF) *_?ats.c *_?ats.o
	$(RMF) obj_lats.dats

###### end of [Makefile] #######
