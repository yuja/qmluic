CARGO = cargo
CLANG_FORMAT = clang-format
CMAKE = cmake
NINJA = ninja

ifneq ($(shell command -v $(NINJA) 2>/dev/null),)
CMAKE_FLAGS += -GNinja
endif

.PHONY: help
help:
	@echo 'Make targets:'
	@echo '  debug      - build debug binaries'
	@echo '  release    - build release binaries'
	@echo '  format     - run code formatter'
	@echo '  tests      - run linter and automated tests'
	@echo '  generate-examples - generate .ui from example .qml files'

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
