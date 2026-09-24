# Vendor the report-safe surfviewjs browser runtime used by the R htmlwidget.
#
# The shipped bundle is pinned in $(VENDOR_MARKER): a surfviewjs commit plus
# the runtime patch in tools/. `make surfview` reproduces that exact bundle;
# `make surfview-repin` moves the pin to another commit (default: HEAD).

SURFVIEWJS_DIR ?= $(HOME)/code/jscode/surfviewjs
COMMIT ?= HEAD
REBUILD := python3 tools/rebuild-surfview-runtime.py
VENDOR_DIR := inst/htmlwidgets/lib/neurosurface
VENDOR_JS := $(VENDOR_DIR)/surfview.embed.iife.js
VENDOR_MARKER := $(VENDOR_DIR)/surfview.embed.commit
WIDGET_YAML := inst/htmlwidgets/surfwidget.yaml

.PHONY: all surfview surfview-repin check-surfviewjs verify-build show-version

all: surfview

surfview: check-surfviewjs
	$(REBUILD) "$(SURFVIEWJS_DIR)"
	@$(MAKE) --no-print-directory verify-build

surfview-repin: check-surfviewjs
	$(REBUILD) "$(SURFVIEWJS_DIR)" --repin "$(COMMIT)"
	@$(MAKE) --no-print-directory verify-build

check-surfviewjs:
	@git -C "$(SURFVIEWJS_DIR)" rev-parse --git-dir >/dev/null 2>&1 || { \
		echo "Missing surfviewjs git checkout: $(SURFVIEWJS_DIR)"; exit 1; \
	}

# Offline consistency check of the committed bundle, patch, and marker.
verify-build:
	@test $$(wc -c < "$(VENDOR_JS)") -gt 10000
	@grep -q '^commit=[0-9a-f]\{40\}$$' "$(VENDOR_MARKER)"
	@EXPECTED=$$(grep '^sha256=' "$(VENDOR_MARKER)" | cut -d= -f2); \
	ACTUAL=$$(shasum -a 256 "$(VENDOR_JS)" | cut -d ' ' -f1); \
	test "$$EXPECTED" = "$$ACTUAL" || { echo "Bundle SHA-256 does not match $(VENDOR_MARKER)"; exit 1; }
	@PATCH=$$(grep '^patch=' "$(VENDOR_MARKER)" | cut -d= -f2); \
	EXPECTED=$$(grep '^patch_sha256=' "$(VENDOR_MARKER)" | cut -d= -f2); \
	ACTUAL=$$(shasum -a 256 "$$PATCH" | cut -d ' ' -f1); \
	test "$$EXPECTED" = "$$ACTUAL" || { echo "Runtime patch SHA-256 does not match $(VENDOR_MARKER)"; exit 1; }
	@REV=$$(grep '^three_revision=' "$(VENDOR_MARKER)" | cut -d= -f2); \
	grep -q "\"$$REV\"" "$(VENDOR_JS)" || { echo "Three.js revision $$REV not found in bundle"; exit 1; }
	@grep -q 'name: surfview' "$(WIDGET_YAML)"
	@grep -q 'surfview.embed.iife.js' "$(WIDGET_YAML)"
	@echo "Verified vendored surfview embed and provenance marker."

show-version:
	@cat "$(VENDOR_MARKER)"
