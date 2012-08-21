DEPS=../deps

all: $(DEPS)/libcouchbase

clean:
	@rm -rf $(DEPS)

$(DEPS)/libevent:
	@mkdir -p $(DEPS)
	@git clone https://github.com/nmathewson/Libevent.git $(DEPS)/libevent
	@cd $(DEPS)/libevent && ./autogen.sh && ./configure && make
	
$(DEPS)/libvbucket: $(DEPS)/libevent
	@git clone https://github.com/couchbase/libvbucket.git $(DEPS)/libvbucket
	@cd $(DEPS)/libvbucket && ./config/autorun.sh && ./configure && make

$(DEPS)/libcouchbase: $(DEPS)/libvbucket
	@git clone https://github.com/couchbase/libcouchbase.git $(DEPS)/libcouchbase
	@cd	$(DEPS)/libcouchbase && ./config/autorun.sh  && ./configure\
	   	CPPFLAGS='-I../libvbucket/include -I../libevent/include'\
	   	LDFLAGS='-L../libvbucket/.libs -L../libevent/.libs'\
		&& make
