include ../include.mk
include ../vsn.mk


ifeq ($(TYPE),debug)
DEBUG_FLAGS = -Ddebug +debug_info
else
DEBUG_FLAGS =
endif

include ../vsn.mk
VSN=$(ERLSOM_VSN)

MODULES=erlsom_compile   \
erlsom        erlsom_parseXsd  erlsom_write \
erlsom_lib      erlsom_pass2     erlsom_writeHrl \
erlsom_add  erlsom_parse    erlsom_sax erlsom_ucs \
erlsom_sax_latin1 erlsom_sax_utf8 erlsom_sax_utf16be \
erlsom_sax_utf16le erlsom_simple_form erlsom_sax_lib \
erlsom_sax_list
HRL_FILES= erlsom_sax.hrl erlsom_compile.hrl erlsom.hrl erlsom_parse.hrl

EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) ../ebin/erlsom.app
ERLC_FLAGS+=-W $(DEBUG_FLAGS) 

#
# Targets
#

all: $(EBIN_FILES) 

$(EBIN_FILES) : $(HRL_FILES)

debug:
	$(MAKE) TYPE=debug

clean:
	rm -f $(EBIN_FILES)

install: all
	$(INSTALL) -m 755 -d $(DESTDIR)/$(APPDIR)/include
	$(INSTALL) -m 644 ../include/* $(DESTDIR)/$(APPDIR)/include
	$(INSTALL) -d $(DESTDIR)/$(APPDIR)/ebin;
	cd ..; cp -r ebin $(DESTDIR)$(APPDIR)/


DIALYZER_FLAGS+=-Wno_return

dialyzer: .dialyzer.ok

.dialyzer.ok:	$(MODULES:%=../ebin/%.$(EMULATOR))
	dialyzer  $(DIALYZER_FLAGS) -c ../ebin
	touch .dialyzer.ok
