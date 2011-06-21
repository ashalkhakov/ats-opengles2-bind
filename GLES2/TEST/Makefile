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
ATSMISCCTRB=$(MISCATSHOME)/GLES2/atsctrb_GLES2.o

######

.PHONY:: all
all:: checkall
all:: cleanall

######

checkall::
cleanall:: clean

#######

checkall:: test01 test02 test03
cleanall:: ; $(RMF) test01 test02 test03

util.o: util_sats.o util_dats.o
	ld -r -o $@ util_sats.o util_dats.o

util_sats.o: SATS/util.sats
	$(ATSCC) -IATS $(MISCATSHOME) -I $(MISCATSHOME) \
	  -o $@ -c SATS/util.sats
util_dats.o: DATS/util.dats
	$(ATSCC) -IATS $(MISCATSHOME) -I $(MISCATSHOME) \
	  -o $@ -c DATS/util.dats

obj_sats.o: SATS/obj.sats
	$(ATSCC) -IATS $(MISCATSHOME) -I $(MISCATSHOME) \
	  -o $@ -c SATS/obj.sats

obj_lats.dats: DATS/obj.lats
	$(ATSLEX) < $< > $@
obj_lats_dats.o: obj_lats.dats obj_sats.o
	$(ATSCC) -IATS $(MISCATSHOME) -I $(MISCATSHOME) \
	  -o $@ -c obj_lats.dats obj_sats.o

test01: GLES2-test01.dats util.o
	$(ATSCC) -IATS $(MISCATSHOME) -I $(MISCATSHOME) \
	  -o test01 GLES2-test01.dats $(ATSMISCCTRB) util.o -lm -lX11 -lEGL -lGLESv2

test02: GLES2-test02.dats util.o
	$(ATSCC) -IATS $(MISCATSHOME) -I $(MISCATSHOME) \
	  -o test02 GLES2-test02.dats $(ATSMISCCTRB) util.o -lm -lX11 -lEGL -lGLESv2

test03: GLES2-test03.dats util.o obj_lats_dats.o
	$(ATSCC) -IATS $(MISCATSHOME) -I $(MISCATSHOME) \
	  -o test03 GLES2-test03.dats \
	   prelude/DATS/list_vt.dats libats/DATS/rarray.dats libats/SATS/rarray.sats libc/SATS/stdlib.sats libc/SATS/fcntl.sats \
	   libc/sys/SATS/types.sats \
	   libats/lex/lexing.dats libats/lex/lexing.sats libats/lex/tables.dats \
	   libats/SATS/genarrays.sats \
	   prelude/DATS/reference.dats \
	  $(ATSMISCCTRB) util.o obj_lats_dats.o \
	  -lm -lX11 -lEGL -lGLESv2 -lats_lex

#######

RMF = rm -f

#######

clean:
	$(RMF) *~
	$(RMF) *_?ats.c *_?ats.o
	$(RMF) obj_lats.dats

###### end of [Makefile] #######
