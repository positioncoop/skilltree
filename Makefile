EXECUTABLE=$(BINDIR)/skilltree
DEPS=
TESTMAIN=src/Test.hs
INSTALLFLAGS=-j -fdevelopment --reorder-goals
MOODEVEL=-c devel.cfg
MOOTEST=-c test.cfg

EXEC=cabal exec --
RUN=$(EXEC) runghc -isrc
BINDIR=.cabal-sandbox/bin
BUILDDIR=dist
SOURCES=$(shell find src -type f -iname '*.hs')
DEPDIR=deps
SHELL=/bin/bash

.PHONY: all install clean superclean test init deps sandbox tags confirm \
	dbsetup dbup dbtest dbnew dbrevert

all: init install test tags

install: $(EXECUTABLE)

$(EXECUTABLE): $(SOURCES)
	cabal install $(INSTALLFLAGS)

test:
	$(RUN) $(TESTMAIN)

run: $(EXECUTABLE)
	$(EXECUTABLE)

clean:
	rm -rf $(BUILDDIR) $(EXECUTABLE)

superclean: confirm clean
	rm -rf $(DEPDIR) .cabal-sandbox/ cabal.sandbox.config TAGS

confirm:
	@read -r -p "Are you sure? [y/N] " CONTINUE; \
	[[ ! $$CONTINUE =~ ^[Yy]$$ ]] && exit 1; echo "Continuing...";

init: sandbox deps


deps: $(patsubst %, $(DEPDIR)/%.d, $(DEPS)) $(DEPDIR)/digestive-functors

$(DEPDIR)/digestive-functors:
	git clone -b snap-upload-fix https://github.com/positioncoop/digestive-functors.git $@
	cabal sandbox add-source $(DEPDIR)/digestive-functors/digestive-functors-snap


$(DEPDIR)/%.d:
	git clone git@github.com:$*.git $@
	cabal sandbox add-source $@


sandbox: cabal.sandbox.config

cabal.sandbox.config:
	cabal sandbox init


tags: TAGS

TAGS: $(SOURCES)
	$(EXEC) haskdogs -e


dbsetup:
	psql template1 -Upostgres -hdb_1 -c "CREATE DATABASE skilltree_devel"
	psql template1 -Upostgres -hdb_1 -c "CREATE DATABASE skilltree_test"
	psql template1 -Upostgres -hdb_1 -c "CREATE USER skilltree_user WITH PASSWORD '111'"
	psql template1 -Upostgres -hdb_1 -c "GRANT ALL ON DATABASE skilltree_devel TO skilltree_user"
	psql template1 -Upostgres -hdb_1 -c "GRANT ALL ON DATABASE skilltree_test TO skilltree_user"

db:
	PGPASSWORD=111 psql skilltree_devel -Uskilltree_user -hdb_1

dbup:
	moo upgrade $(MOODEVEL)
	moo upgrade $(MOOTEST)

dbtest:
	moo test $(MOODEVEL) $(MIGRATION)
	moo test $(MOOTEST) $(MIGRATION)

dbnew:
	moo new $(MOODEVEL) $(MIGRATION)

dbrevert:
	moo revert $(MOODEVEL) $(MIGRATION)
	moo revert $(MOOTEST) $(MIGRATION)
