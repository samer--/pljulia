JL_SHARE = $(shell julia -e 'print(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia"))')
CFLAGS   += $(shell $(JL_SHARE)/julia-config.jl --cflags)
LDFLAGS  += $(shell $(JL_SHARE)/julia-config.jl --ldflags)
LDLIBS   += $(shell $(JL_SHARE)/julia-config.jl --ldlibs)

TARGET=julia4pl

SOBJ=$(PACKSODIR)/$(TARGET).$(SOEXT)

all:	$(SOBJ)

$(SOBJ): c/$(TARGET).o
	mkdir -p $(PACKSODIR)
	$(LD) $(LDSOFLAGS) $(LDFLAGS) -o $@ $(SWISOLIB) $< $(LDLIBS)
	strip -x $@

check::
install::
clean:
	rm -f c/$(TARGET).o

distclean: clean
	rm -f $(SOBJ)

install-me:
	swipl -f none -g "pack_install('.',[upgrade(true), interactive(false), link(true)]), halt"

publish:
	swipl -f none -g "pack_property(pljulia,download(D)), pack_install(D,[upgrade(true),interactive(false),register(true)]), halt"
