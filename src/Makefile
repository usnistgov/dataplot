FC = gfortran
CC = gcc

# FEATURES to enable during compilation
# all: READLINE NCURSES GD TIFF VPX CAIRO X11 GKS OPENGL PLOT
FEATURES = GD CAIRO X11 READLINE NCURSES 

# PREFIX is the parent of where are installed
# for non-root install, consider PREFIX=$(HOME) or /var/tmp
# DESTDIR is used by packaging programs (e.g., rpmbuild).
PREFIX = $(DESTDIR)/usr/local
BINDIR = $(PREFIX)/bin

# where to find additional dataplot files (help, menus, etc).
DPLIBDIR=$(PREFIX)/lib/dataplot

FFLAGS += -O2 -DLINUX -fdefault-real-8 -fdefault-double-8
CFLAGS += -O2

# special flags for dp1
DP1FLAGS = -DDDOUBLE -DINTEGER32 -DHAVE_ISNAN -DHAVE_EXECUTE_COMMAND_LINE -DDPLIBDIR=\'$(DPLIBDIR)\'


# object files sorted by size for fastest parallel compilation; dp2.o first as it takes the longest
# ls -S *.F | egrep -v 'msfort_intel' | sed 's/\.F/.o/g' | xargs
OBJS = dp40.o dp36.o dp42.o dp19.o dp11.o dp38.o dp20.o dp21.o dp2.o dp3.o dp43.o dp18.o dp29.o dp4.o dp37.o dp16.o dp22.o dp9.o dp32.o dp8.o dp41.o dp17.o dp31.o dp15.o dp6.o dp45.o dp12.o dp5.o dp13.o dp30.o dp44.o dp10.o dp25.o starpac.o edsub.o dp27.o dp14.o dp33.o optimi.o dp28.o fit3b.o odrpck.o dp23.o dp26.o dp1.o dp35.o dp7.o dp34.o cluster.o dp24.o dp39.o main.o dpdds3.o edsear.o compgeom.o edmai2.o dpdds.o dpdds2.o edinit.o edwrst.o


ifneq (,$(findstring READLINE,$(FEATURES)))
OBJS += rldp.o
FFLAGS4 = -DHAVE_READLINE
LDFLAGS += -lreadline
endif

ifneq (,$(findstring NCURSES,$(FEATURES)))
LDFLAGS += -lncurses
endif

ifneq (,$(findstring TIFF,$(FEATURES)))
FEATURES += GD
CFLAGS += -DHAVE_GD_TIFF
LDFLAGS += -ltiff
endif

ifneq (,$(findstring VPX,$(FEATURES)))
FEATURES += GD
CFLAGS += -DHAVE_GD_VPX
LDFLAGS += -lvpx
endif

ifneq (,$(findstring GD,$(FEATURES)))
OBJS += gd.o
#LDFLAGS += -lgd -lpng -ljpeg -lz -lfontconfig -lfreetype -lpthread 
LDFLAGS += -lgd
FFLAGS4 += -DHAVE_GD
endif

ifneq (,$(findstring CAIRO,$(FEATURES)))
OBJS += cairo.o
FEATURES += X11
LDFLAGS += -lcairo
FFLAGS4 += -DHAVE_CAIRO
CFLAGS += -DHAVE_X11
#CFLAGS += -I/usr/include/cairo
endif

ifneq (,$(findstring PLOT,$(FEATURES)))
OBJS += libplot.o
FEATURES += X11
LDFLAGS += -lplot
#LDFLAGS += -lplot -lXaw -lXmu -lXt -lSM -lICE -lXext
FFLAGS4 += -DHAVE_LIBPLOT
endif

ifneq (,$(findstring X11,$(FEATURES)))
OBJS += x11.o
FFLAGS4 += -DHAVE_X11
LDFLAGS += -lX11
endif

ifneq (,$(findstring GKS,$(FEATURES)))
LDFLAGS += -lgks
FFLAGS4 += -DHAVE_OPEN_GKS
endif

ifneq (,$(findstring OPENGL,$(FEATURES)))
OBJS += gl.o
LDFLAGS += -lGLU -lGL
FFLAGS4 += -DHAVE_OPEN_GL
endif



all: xdataplot dataplot

dataplot: $(OBJS)
	$(FC) -o $@  $(OBJS)  $(LDFLAGS)

dp1.o: dp1.F
	$(FC) $(FFLAGS) $(DP1FLAGS) -c -o $@ $<

dp16.o: dp16.F
	$(FC) $(FFLAGS) $(FFLAGS4) -c -o $@ $<

dp24.o: dp24.F
	$(FC) $(FFLAGS) $(FFLAGS4) -c -o $@ $<

dp38.o: dp38.F
	$(FC) $(FFLAGS) $(FFLAGS4) -c -o $@ $<

gd.o: gd.c
	$(CC) $(CFLAGS) -c -o $@ $<

xdataplot:
	sed "s#\(^dplibdir=\).*#\1$(DPLIBDIR)#" < xdataplot.in > xdataplot
	sed "s#\(^library[[:space:]]\+unix[[:space:]]\+\).*#\1$(DPLIBDIR)#" < ../lib/frscript/xdpConfig.in > ../lib/frscript/xdpConfig


.PHONY: clean

clean:
	rm -f *.o dataplot xdataplot ../lib/frscript/xdpConfig

install: dataplot xdataplot
	@echo installing binary to $(BINDIR)
	-[ ! -d $(BINDIR) ] && mkdir -vp $(BINDIR) ||:
	install -pm 755 -t $(BINDIR) dataplot xdataplot 

	@echo installing libs to $(DPLIBDIR)
	-[ ! -d $(DPLIBDIR) ] && mkdir -vp $(DPLIBDIR) ||:
	cp -a ../lib/* $(DPLIBDIR)
