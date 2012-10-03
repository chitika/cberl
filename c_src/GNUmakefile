DEPS=../deps

all: $(DEPS)/libcouchbase/.libs/libcouchbase.la

clean:
	if test -e $(DEPS)/libcouchbase/Makefile; then \
		cd $(DEPS)/libcouchbase; make clean;\
		cd ../libvbucket; make clean;\
		cd ../libevent; make clean;\
		cd ..; rm -rf libevent libcouchbase libvbucket;\
	else \
		true;\
	fi

distclean:
	@rm -rf $(DEPS)

$(DEPS)/libevent:
	@mkdir -p $(DEPS)
	@git clone https://github.com/nmathewson/Libevent.git $(DEPS)/libevent
	@cd $(DEPS)/libevent && ./autogen.sh && ./configure CFLAGS='' LDFLAGS='' && make
	
$(DEPS)/libvbucket: $(DEPS)/libevent
	@git clone https://github.com/couchbase/libvbucket.git $(DEPS)/libvbucket
	@cd $(DEPS)/libvbucket && ./config/autorun.sh && ./configure  CFLAGS='' LDFLAGS='' && make

$(DEPS)/libcouchbase/.libs/libcouchbase.la: $(DEPS)/libvbucket
	@git clone https://github.com/couchbase/libcouchbase.git $(DEPS)/libcouchbase
	@cd	$(DEPS)/libcouchbase && ./config/autorun.sh  && ./configure --enable-static\
	   	CPPFLAGS='-I../libvbucket/include -I../libevent/include'\
	   	LDFLAGS='-L../libvbucket/.libs -L../libevent/.libs' --disable-couchbasemock\
		&& make
