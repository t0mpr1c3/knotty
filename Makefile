# Adapted from: http://www.greghendershott.com/2017/04/racket-makefiles.html
SHELL=/bin/sh

PACKAGE-NAME=knotty

DEPS-FLAGS=--check-pkg-deps --unused-pkg-deps

help:
	@echo "install - install package along with dependencies"
	@echo "remove - remove package"
	@echo "build - compile libraries"
	@echo "build-docs - build self-contained docs that could be hosted somewhere"
	@echo "build-all - compile libraries, build docs, and check dependencies"
	@echo "clean - remove all build artifacts"
	@echo "check-deps - check dependencies"
	@echo "test - run tests"
	@echo "test-with-errortrace - run tests with error tracing"
	@echo "errortrace - alias for test-with-errortrace"
	@echo "cover - run test coverage checker and view report"
	@echo "cover-coveralls - run test coverage and upload to Coveralls"
	@echo "coverage-check - run test coverage checker"
	@echo "coverage-report - view test coverage report"
	@echo "docs - view docs in a browser"

# Primarily for use by CI.
# Install dependencies as well as linking this as a package.
install:
	raco pkg install --deps search-auto --link $(PWD)/$(PACKAGE-NAME) $(PWD)/$(PACKAGE-NAME)-lib

remove:
	raco pkg remove $(PACKAGE-NAME) $(PACKAGE-NAME)-lib

# Primarily for day-to-day dev.
# Build libraries from source.
build:
	raco setup --no-docs --pkgs $(PACKAGE-NAME)-lib

# Primarily for day-to-day dev.
# Build libraries from source, build docs (if any), and check dependencies.
build-all:
	raco setup $(DEPS-FLAGS) --pkgs $(PACKAGE-NAME) $(PACKAGE-NAME)-lib

# Primarily for CI, for building backup docs that could be used in case
# the main docs at docs.racket-lang.org become unavailable.
build-docs:
	scribble +m --redirect-main http://pkg-build.racket-lang.org/doc/ --htmls --dest ./manual ./$(PACKAGE-NAME)/scribblings/$(PACKAGE-NAME).scrbl

# Note: Each collection's info.rkt can say what to clean, for example
# (define clean '("compiled" "doc" "doc/<collect>")) to clean
# generated docs, too.
clean:
	raco setup --fast-clean --pkgs $(PACKAGE-NAME)-lib $(PACKAGE-NAME)-test $(PACKAGE-NAME)-doc

# Primarily for use by CI, after make install -- since that already
# does the equivalent of make setup, this tries to do as little as
# possible except checking deps.
check-deps:
	raco setup --no-docs $(DEPS-FLAGS) $(PACKAGE-NAME)

# Suitable for both day-to-day dev and CI
test:
	raco test -exp $(PACKAGE-NAME) $(PACKAGE-NAME)-lib

test-with-errortrace:
	racket -l errortrace -l racket -e '(require (submod "$(PACKAGE-NAME)/test/$(PACKAGE-NAME).rkt" test))'

errortrace: test-with-errortrace

docs:
	raco docs $(PACKAGE-NAME)

coverage-check:
	raco cover -b -d ./coverage -p $(PACKAGE-NAME) $(PACKAGE-NAME)-lib

coverage-report:
	open coverage/index.html

cover: coverage-check coverage-report

cover-coveralls:
	raco cover -b -f coveralls -p $(PACKAGE-NAME) $(PACKAGE-NAME)-lib

.PHONY:	help install remove build build-docs build-all clean check-deps test test-with-errortrace errortrace docs cover coverage-check coverage-report cover-coveralls
