EXECUTABLE=$(BINDIR)/skilltree
DEPS=
TESTMAIN=test/Main.hs
INSTALLFLAGS=-j -fdevelopment --reorder-goals
MOODEVEL=-c devel.cfg
MOOTEST=-c test.cfg

EXEC=cabal exec --
RUN=$(EXEC) runghc -isrc
BINDIR=.cabal-sandbox/bin
BUILDDIR=dist
SOURCES=$(shell find src -type f -iname '*.hs') skilltree.cabal
DEPDIR=deps
SHELL=/bin/bash

VAGRANT=$(shell cat .vagrantmode 2> /dev/null)
VAGRANT_CMD=vagrant ssh -c "export PATH=$$PATH:/home/vagrant/.cabal/bin:/home/vagrant/ghc/bin:/vagrant/.cabal-sandbox/bin; export LANG=C.UTF-8; cd /vagrant; $(1)"

PRODUCTION_HOST=69.164.222.149


.PHONY: all install clean superclean test init deps sandbox tags confirm \
	dbup dbtest dbnew dbrevert production-init production-provision production-keter
	deploy repl

all: init install test tags

install: $(EXECUTABLE)

$(EXECUTABLE): $(SOURCES)
ifeq ($(VAGRANT),1)
	$(call VAGRANT_CMD, cabal install $(INSTALLFLAGS))
else
	cabal install $(INSTALLFLAGS)
endif

test: $(EXECUTABLE)
ifeq ($(VAGRANT),1)
	vagrant ssh -- -f -N -R 4444:localhost:4444
	java -jar $(DEPDIR)/webdriver-bin/selenium-server-standalone-2.42.2.jar &> log/selenium.log &
	$(call VAGRANT_CMD, $(EXECUTABLE) -p 8001 -e test &> log/test.log) &
	@sleep 1
	$(call VAGRANT_CMD, $(RUN) $(TESTMAIN))
	$(call VAGRANT_CMD, killall $(EXECUTABLE))
	killall java
# not sure how to kill the tunnel...
# kill `ps aux | grep "ssh vagrant" | grep -v grep | awk '{print $2}'`
else
	java -jar $(DEPDIR)/webdriver-bin/selenium-server-standalone-2.42.2.jar &> log/selenium.log &
	$(EXECUTABLE) -p 8001 -e test &> log/test.log &
	@sleep 1
	$(RUN) $(TESTMAIN)
	killall .cabal-sandbox/bin/skilltree
	killall java
endif

run: $(EXECUTABLE)
ifeq ($(VAGRANT),1)
	$(call VAGRANT_CMD, $(EXECUTABLE))
else
	$(EXECUTABLE)
endif

clean:
	rm -rf $(BUILDDIR) $(EXECUTABLE)

superclean: confirm clean
	rm -rf $(DEPDIR) .cabal-sandbox/ cabal.sandbox.config TAGS

confirm:
	@read -r -p "Are you sure? [y/N] " CONTINUE; \
	[[ ! $$CONTINUE =~ ^[Yy]$$ ]] && exit 1; echo "Continuing...";

init: sandbox deps


deps: $(patsubst %, $(DEPDIR)/%.d, $(DEPS)) $(DEPDIR)/digestive-functors $(DEPDIR)/webdriver-bin

$(DEPDIR)/digestive-functors:
ifeq ($(VAGRANT),1)
	$(call VAGRANT_CMD, git clone -b snap-upload-fix https://github.com/positioncoop/digestive-functors.git $@)
	$(call VAGRANT_CMD, cabal sandbox add-source $(DEPDIR)/digestive-functors/digestive-functors-snap)
else
	git clone -b snap-upload-fix git@github.com:positioncoop/digestive-functors.git $@
	cabal sandbox add-source $(DEPDIR)/digestive-functors/digestive-functors-snap
endif

$(DEPDIR)/webdriver-bin:
	git clone https://github.com/positioncoop/webdriver-bin $@
	ln -s `pwd`/$@/chromedriver $(HOME)/bin/chromedriver

$(DEPDIR)/%.d:
ifeq ($(VAGRANT),1)
	$(call VAGRANT_CMD, git clone https://github.com/$*.git $@)
	$(call VAGRANT_CMD, cabal sandbox add-source $@)
else
	git clone git@github.com:$*.git $@
	cabal sandbox add-source $@
endif

sandbox: cabal.sandbox.config

cabal.sandbox.config:
ifeq ($(VAGRANT),1)
	$(call VAGRANT_CMD, cabal sandbox init)
else
	cabal sandbox init
endif

tags: TAGS

TAGS: $(SOURCES)
	$(EXEC) haskdogs -e


repl:
ifeq ($(VAGRANT),1)
	$(call VAGRANT_CMD, cabal exec ghci -- -isrc src/Site.hs)
else
	cabal exec ghci -- -isrc src/Site.hs
endif

db:
ifeq ($(VAGRANT),1)
	$(call VAGRANT_CMD, PGPASSWORD=111 psql skilltree_devel -Uskilltree_user -hlocalhost)
else
	PGPASSWORD=111 psql skilltree_devel -Uskilltree_user -hlocalhost
endif

dbup:
ifeq ($(PRODUCTION),1)
	rsync --checksum -ave 'ssh '  migrations/* host@$(PRODUCTION_HOST):/srv/migrations
	ssh host@$(PRODUCTION_HOST) "moo upgrade -c moo.cfg"
else
ifeq ($(VAGRANT),1)
	$(call VAGRANT_CMD, moo upgrade $(MOODEVEL))
	$(call VAGRANT_CMD, moo upgrade $(MOOTEST))
else
	moo upgrade $(MOODEVEL)
	moo upgrade $(MOOTEST)
endif
endif

dbtest:
ifeq ($(VAGRANT),1)
	$(call VAGRANT_CMD, moo test $(MOODEVEL) $(MIGRATION))
	$(call VAGRANT_CMD, moo test $(MOOTEST) $(MIGRATION))
else
	moo test $(MOODEVEL) $(MIGRATION)
	moo test $(MOOTEST) $(MIGRATION)
endif

dbnew:
ifeq ($(VAGRANT),1)
	$(call VAGRANT_CMD, moo new $(MOODEVEL) $(MIGRATION))
else
	moo new $(MOODEVEL) $(MIGRATION)
endif

dbrevert:
ifeq ($(VAGRANT),1)
	$(call VAGRANT_CMD, moo revert $(MOODEVEL) $(MIGRATION))
	$(call VAGRANT_CMD, moo revert $(MOOTEST) $(MIGRATION))
else
	moo revert $(MOODEVEL) $(MIGRATION)
	moo revert $(MOOTEST) $(MIGRATION)
endif

keter-build:
	$(call VAGRANT_CMD, cabal install -j --reorder-goals)
	cp .cabal-sandbox/bin/skilltree skilltree

keter-tar:
	tar czfv skilltree.keter skilltree config prod.cfg static snaplets log/.gitkeep tmp/.gitkeep
	rm skilltree

keter-deploy: keter-tar
	scp skilltree.keter host@$(PRODUCTION_HOST):/opt/keter/incoming

deploy: keter-build keter-deploy

production-init: production-provision production-keter production-moo

production-provision:
	ansible-playbook -i provisioning/inventory --vault-password-file=provisioning/password.txt provisioning/web.yml --ask-pass

production-keter:
	vagrant ssh-config > .vagrant-ssh-config
	scp -F .vagrant-ssh-config skilltree:/home/vagrant/.cabal/bin/keter keter
	rm .vagrant-ssh-config
	scp keter root@$(PRODUCTION_HOST):/opt/keter/bin/
	rm keter
	ssh root@$(PRODUCTION_HOST) update-rc.d keter defaults
	ssh root@$(PRODUCTION_HOST) service keter start

production-moo:
	vagrant ssh-config > .vagrant-ssh-config
	scp -F .vagrant-ssh-config skilltree:/home/vagrant/.cabal/bin/moo moo
	rm .vagrant-ssh-config
	scp moo root@$(PRODUCTION_HOST):/usr/bin/
	rm moo

production-log:
	ssh host@$(PRODUCTION_HOST) tail -F /opt/keter/log/app-skilltree/current.log

production-keter-log:
	ssh host@$(PRODUCTION_HOST) tail -F /opt/keter/log/keter/current.log
