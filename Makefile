
SUBDIRS	=	src 
include ./include.mk
include vsn.mk


all debug clean:	
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then ( cd $$d && $(MAKE) $@ ) || exit 1 ; fi ; \
	  done

install:	all 
	set -e ; \
	for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then ( cd $$d && $(MAKE) $@ ) || exit 1 ; fi ; \
	  done; \
	echo "** beam files went into $(DESTDIR)/$(APPDIR)/ebin"



docs:
	( cd doc && $(MAKE) docs )

conf_clean:
	-rm include.mk config.cache config.status config.log 2> /dev/null

touch:
	find . -name '*' -print | xargs touch -m
	find . -name '*.erl' -print | xargs touch -m

