DEPS=../deps

all: deps

clean:
	@rm -rf $(DEPS)

deps:
	@if [ ! -d "$(DEPS)/libvbucket" ]; then \
		mkdir -p $(DEPS);\
		git clone https://github.com/couchbase/libvbucket.git $(DEPS)/libvbucket;\
		git clone https://github.com/couchbase/libcouchbase.git $(DEPS)/libcouchbase;\
		git clone https://github.com/nmathewson/Libevent.git $(DEPS)/libevent;\
		cd $(DEPS)/libevent && ./autogen.sh && ./configure && make && make install;\
		cd ../libvbucket && ./config/autorun.sh && ./configure && make && make install;\
		cd ../libcouchbase && ./config/autorun.sh  && ./configure --disable-couchbasemock && make && make install;\
	fi
