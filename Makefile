#
CC=gcc
CFLAGSLBFGS= $(CFLAGS) -fPIC -DBP_FREE -O3 -fomit-frame-pointer -Wall -Wstrict-prototypes -Wmissing-prototypes -g -O2  -I$(srcdir)/liblbfgs-1.10/include
LIBDIR=lib/$(SWIARCH)/
LDFLAGS= $(LDSOFLAGS) -rdynamic   -Wl,--enable-new-dtags 
LDFLAGSMAC= $(LDSOFLAGS) -rdynamic -lswipl
#
#
# You shouldn't need to change what follows.
#
srcdir=.
SO=so
#4.1VPATH=../../../packages/yap-lbfgs:../../../packages/yap-lbfgs/OPTYap
CWD=$(PWD)
#

OBJS=swi_lbfgs.o lbfgs.o
SOBJS=swi_lbfgs.$(SOEXT)

#in some systems we just create a single object, in others we need to
# create a libray

all: $(SOBJS) 

swi_lbfgs.o: $(srcdir)/swi_lbfgs.c
	$(CC) -c $(CFLAGSLBFGS) $(srcdir)/swi_lbfgs.c -o swi_lbfgs.o

lbfgs.o: $(srcdir)/liblbfgs-1.10/lib/lbfgs.c
	$(CC) -c $(CFLAGSLBFGS) -I $(srcdir)/liblbfgs-1.10/lib $(srcdir)/liblbfgs-1.10/lib/lbfgs.c -o lbfgs.o

swi_lbfgs.$(SOEXT): swi_lbfgs.o lbfgs.o
	if [[ $(SWIARCH) ==  *darwin* ]] ; then \
  $(CC) -export-dynamic $(LDFLAGSMAC) -o swi_lbfgs.$(SOEXT) swi_lbfgs.o lbfgs.o ; \
  else gcc -shared -export-dynamic $(LDFLAGS) -o swi_lbfgs.$(SOEXT) swi_lbfgs.o lbfgs.o ; \
  fi

install: all
	mkdir -p $(LIBDIR)
	/usr/bin/install -c $(SOBJS) lib/$(SWIARCH)

check:

distclean:
	rm -f *.o  $(OBJS) $(SOBJS) lib/$(SWIARCH)/$(SOBJS)

