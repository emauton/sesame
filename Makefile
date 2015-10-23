# Most of this Makefile depends upon Basho's work. See
#   https://github.com/basho/riak &
#   https://github.com/basho/node_package
REBAR            = rebar

OVERLAY_VARS    ?=

.PHONY: deps docs

all: deps compile pfkeyport

capability: pfkeyport
	sudo setcap cap_net_admin=ep pfkeyport

pfkeyport: pfkeyport.c
	gcc -Wall -pedantic -o pfkeyport pfkeyport.c -lev -lcap

export EXOMETER_PACKAGES="(basic)"
compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean: docsclean
	$(REBAR) clean
	rm -f pfkeyport

distclean: clean relclean docsclean pkgclean
	$(REBAR) delete-deps

rel: all relclean
	$(REBAR) generate $(OVERLAY_VARS)

lint:
	elvis rock

relclean:
	rm -rf rel/sesame

xref:
	$(REBAR) skip_deps=true xref

DIALYZER_APPS = erts kernel stdlib sasl common_test eunit tools runtime_tools \
	syntax_tools compiler debugger inets os_mon

PLT = $(HOME)/dialyzer/sesame_plt

plt:
	@# Regarding the initial '-', this *always* warns & we're fine with that.
	@# Cf. https://www.gnu.org/software/make/manual/html_node/Errors.html
	@# We remove some deps for being unreliable in terms of specs.
	-mkdir $(HOME)/dialyzer
	-test ! -f "$(PLT)" && ( dialyzer --build_plt --output_plt $(PLT) \
	                                  --apps $(DIALYZER_APPS)         \
	                                  -r deps                         \
	                                  --statistics --verbose ;        \
	                         dialyzer --remove_from_plt --plt $(PLT)  \
	                                  --statistics --verbose          \
	                                  -r deps/exometer deps/exometer_core deps/recon  )

dialyze: plt
	@# Some of riak_core still doesn't define -callback specs, so we ignore
	@# that warning.
	dialyzer --plt $(PLT) -Wno_undefined_callbacks \
	         --statistics --verbose apps/*/ebin

# To test a specific app / module, use e.g.
#    rebar apps=$APP skip_deps=true eunit suites=$MODULE
eunit:
	$(REBAR) skip_deps=true eunit

tests: compile xref dialyze eunit

docs: docsclean
	$(REBAR) skip_deps=true doc
	cp apps/sesame/doc/*.png apps/sesame/edoc

docsclean:
	rm -rf apps/*/edoc

stage : rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/sesame/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/sesame/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/sesame/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/sesame/lib;)

## node_package packaging config, cf. https://github.com/basho/node_package
.PHONY: package
PKG_VERSION=0.${CIRCLE_BUILD_NUM}
PKG_ID=sesame-${PKG_VERSION}
export PKG_VERSION PKG_ID REBAR OVERLAY_VARS

package.src: deps
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ HEAD | (cd package && tar -xf -)
	${MAKE} -C package/$(PKG_ID) deps
	mkdir -p package/$(PKG_ID)/priv
	#git --git-dir=.git describe --tags >package/$(PKG_ID)/priv/vsn.git
	for dep in package/$(PKG_ID)/deps/*; do \
             echo "Processing dep: $${dep}"; \
             mkdir -p $${dep}/priv; \
             git --git-dir=$${dep}/.git describe --tags >$${dep}/priv/vsn.git; \
        done
	find package/$(PKG_ID) -depth -name ".git" -exec rm -rf {} \;
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

dist: package.src
	cp package/$(PKG_ID).tar.gz .

package: package.src
	${MAKE} -C package -f $(PKG_ID)/deps/node_package/Makefile

pkgclean:
	rm -rf package
