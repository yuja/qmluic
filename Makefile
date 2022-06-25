CARGO = cargo
CLANG_FORMAT = clang-format
CMAKE = cmake
DEBCHANGE = debchange
DEBUILD = debuild
GIT = git
NINJA = ninja
QMAKE = qmake

BUILD_DIR = target/build
BUILD_TYPE = Debug
PREFIX = /usr/local

DEB_VERSION = 0.1~$(shell date +"%Y%m%d").git$(shell git rev-parse --short HEAD)
QT_VERSION_MAJOR = $(word 1,$(subst ., ,$(shell $(QMAKE) -query QT_VERSION)))

ifneq ($(shell command -v $(NINJA) 2>/dev/null),)
CMAKE_FLAGS += -GNinja
endif

ifeq ($(QT_VERSION_MAJOR),6)
CMAKE_FLAGS += -DUSE_QT6=ON
endif

.PHONY: help
help:
	@echo 'Make targets:'
	@echo '  local      - build and install for in-place usage'
	@echo '  release    - build release binaries'
	@echo '  install    - install binaries'
	@echo '  deb        - build debian package'
	@echo '  clean      - remove build directory'
	@echo '  format     - run code formatter'
	@echo '  tests      - run linter and automated tests'
	@echo '  build-examples - generate .ui from example .qml files and build them'
	@echo
	@echo 'Make variables:'
	@echo '  CMAKE_FLAGS=$(CMAKE_FLAGS)'
	@echo '  QT_VERSION_MAJOR=$(QT_VERSION_MAJOR)'

.PHONY: local
local:
	$(MAKE) build install PREFIX=$(CURDIR)/target/local

.PHONY: release
release:
	$(MAKE) build BUILD_TYPE=Release

.PHONY: build
build:
	mkdir -p $(BUILD_DIR)
	cd $(BUILD_DIR) && $(CMAKE) -DCMAKE_BUILD_TYPE=$(BUILD_TYPE) \
		-DCMAKE_INSTALL_PREFIX=$(PREFIX) $(CMAKE_FLAGS) $(CURDIR)
	$(CMAKE) --build $(BUILD_DIR) --config $(BUILD_TYPE)

.PHONY: install
install:
	$(CMAKE) --install $(BUILD_DIR)

.PHONY: deb
deb:
	$(RM) -R debian
	cp -pR contrib/debian debian
	$(DEBCHANGE) --create --package qmluic -v "$(DEB_VERSION)" "New snapshot build."
	$(DEBUILD) -uc -us --build=binary

.PHONY: clean
clean:
	$(CARGO) clean
	$(RM) -R debian obj-*

.PHONY: format
format:
	$(CARGO) fmt --all
	find examples uiviewer -type f \( -name '*.cpp' -o -name '*.h' \) -print0 \
		| xargs -0 $(CLANG_FORMAT) -i

.PHONY: tests
tests:
	$(CARGO) clippy
	$(CARGO) test --workspace

.PHONY: build-examples
build-examples: BUILD_DIR = target/build-examples
build-examples: export PATH := $(CURDIR)/target/local/bin:$(PATH)
build-examples:
	mkdir -p $(BUILD_DIR)
	cd $(BUILD_DIR) && $(CMAKE) -DCMAKE_BUILD_TYPE=$(BUILD_TYPE) $(CMAKE_FLAGS) $(CURDIR)/examples
	$(CMAKE) --build $(BUILD_DIR) --config $(BUILD_TYPE)
