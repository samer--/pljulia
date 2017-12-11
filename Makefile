JL_CONFIG = $(shell julia -e 'print(joinpath(JULIA_HOME, Base.DATAROOTDIR, "julia", "julia-config.jl"))')
CFLAGS    += $(shell julia $(JL_CONFIG) --cflags)
LDSOFLAGS += $(shell julia $(JL_CONFIG) --ldflags)
LDLIBS     = $(shell julia $(JL_CONFIG) --ldlibs)

TARGET=julia4pl

SOBJ=$(PACKSODIR)/$(TARGET).$(SOEXT)

all:	$(SOBJ)

$(SOBJ): c/$(TARGET).o
	mkdir -p $(PACKSODIR)
	$(LD) $(LDSOFLAGS) -o $@ $(SWISOLIB) $< $(LDLIBS)
	strip -x $@

check::
install::
clean:
	rm -f c/$(TARGET).o

distclean: clean
	rm -f $(SOBJ)

install-me:
	swipl -f none -g "pack_install('file:.',[upgrade(true)]), halt"

publish:
	swipl -f none -g "pack_property(pljulia,download(D)), pack_install(D,[upgrade(true),interactive(false)]), halt"
