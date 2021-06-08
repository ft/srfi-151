TOPDIR = .

LOAD_PATH = $(TOPDIR)/scheme
TEST_PATH = $(TOPDIR)/test
GUILE_BINARY ?= guile
GUILE_CALL = $(GUILE_BINARY) -L $(LOAD_PATH) -C $(LOAD_PATH) --no-auto-compile
GUILD_BINARY ?= guild

CFLAGS = -O3 -Wunsupported-warning -Wunused-variable -Wunused-toplevel
CFLAGS += -Wunbound-variable -Warity-mismatch -Wduplicate-case-datum
CFLAGS += -Wbad-case-datum -Wformat -L$(LOAD_PATH)

COMPILE = $(GUILD_BINARY) compile $(CFLAGS)
MODULES = $(TOPDIR)/scheme/srfi/srfi-151.scm
OBJECTS = ${MODULES:.scm=.go}

TESTS = $(TOPDIR)/test/*.t
RUNTESTS = $(TOPDIR)/tools/run-single-test
PROVE = tap-harness -e '$(RUNTESTS)'
INSTALL = $(GUILE_CALL) $(TOPDIR)/tools/install

.SUFFIXES: .scm .go

all:
	@echo
	@echo " Available targets:"
	@echo
	@echo "           all: This help text"
	@echo "       install: Install scheme modules to system paths"
	@echo "       compile: Byte-compile the client library"
	@echo "         clean: Remove byte-compiled scheme code"
	@echo "          test: Run test suite"
	@echo "  test-verbose: Run test suite (with verbose test harness)"
	@echo "    test-debug: Run test suite (With all debugging enabled)"
	@echo

.scm.go:
	$(COMPILE) -o $@ $<

compile: $(OBJECTS)

clean:
	find . -name "*.go" -exec rm -f '{}' +
	find . -name "*~" -exec rm -f '{}' +

install:
	$(INSTALL)

test:
	$(PROVE) $(TESTS)

test-verbose:
	$(PROVE) --verbose $(TESTS)

.PHONY: all compile clean install test test-verbose
