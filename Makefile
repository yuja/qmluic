CARGO = cargo
CLANG_FORMAT = clang-format
CMAKE = cmake
NINJA = ninja
QMAKE = qmake

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
	@echo '  debug      - build debug binaries'
	@echo '  release    - build release binaries'
	@echo '  format     - run code formatter'
	@echo '  tests      - run linter and automated tests'
	@echo '  generate-examples - generate .ui from example .qml files'
	@echo '  build-examples - generate .ui from example .qml files and build them'
	@echo
	@echo 'Make variables:'
	@echo '  CMAKE_FLAGS=$(CMAKE_FLAGS)'
	@echo '  QT_VERSION_MAJOR=$(QT_VERSION_MAJOR)'

.PHONY: debug
debug:
	$(CARGO) build --workspace
	$(MAKE) build-uiviewer BUILD_TYPE=Debug BUILD_DIR=target/$@-uiviewer
	ln -sf ../$@-uiviewer/qmluic-uiviewer target/$@

.PHONY: release
release:
	$(CARGO) build --workspace --release
	$(MAKE) build-uiviewer BUILD_TYPE=Release BUILD_DIR=target/$@-uiviewer
	ln -sf ../$@-uiviewer/qmluic-uiviewer target/$@

.PHONY: build-uiviewer
build-uiviewer:
	@[ -n "$(BUILD_DIR)" ] || { echo 'specify BUILD_DIR' >&2; exit 1; }
	@[ -n "$(BUILD_TYPE)" ] || { echo 'specify BUILD_TYPE' >&2; exit 1; }
	mkdir -p $(BUILD_DIR)
	cd $(BUILD_DIR) && $(CMAKE) -DCMAKE_BUILD_TYPE=$(BUILD_TYPE) $(CMAKE_FLAGS) $(CURDIR)/contrib/uiviewer
	$(CMAKE) --build $(BUILD_DIR) --config $(BUILD_TYPE)

.PHONY: format
format:
	$(CARGO) fmt --all
	find contrib/uiviewer examples -type f \( -name '*.cpp' -o -name '*.h' \) -print0 \
		| xargs -0 $(CLANG_FORMAT) -i

.PHONY: tests
tests:
	$(CARGO) clippy
	$(CARGO) test --workspace

.PHONY: generate-examples
generate-examples:
	find examples -type f -name '*.qml' -print0 \
		| xargs -0 $(CARGO) run -- generate-ui

.PHONY: build-examples
build-examples: BUILD_DIR = target/debug-examples
build-examples: BUILD_TYPE = Debug
build-examples: generate-examples
	mkdir -p $(BUILD_DIR)
	cd $(BUILD_DIR) && $(CMAKE) -DCMAKE_BUILD_TYPE=$(BUILD_TYPE) $(CMAKE_FLAGS) $(CURDIR)/examples
	$(CMAKE) --build $(BUILD_DIR) --config $(BUILD_TYPE)
