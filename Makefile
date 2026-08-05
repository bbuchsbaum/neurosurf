# Vendor the report-safe surfviewjs browser runtime used by the R htmlwidget.

SURFVIEWJS_DIR := $(HOME)/code/jscode/surfviewjs
SURFVIEWJS_PACKAGE := $(SURFVIEWJS_DIR)/package.json
SURFVIEWJS_BUILD := $(SURFVIEWJS_DIR)/dist/surfview.embed.iife.js
SURFVIEWJS_SOURCES := $(shell find $(SURFVIEWJS_DIR)/src -type f \( -name '*.js' -o -name '*.ts' \) 2>/dev/null)
VENDOR_DIR := inst/htmlwidgets/lib/neurosurface
VENDOR_JS := $(VENDOR_DIR)/surfview.embed.iife.js
VENDOR_MARKER := $(VENDOR_DIR)/surfview.embed.commit
WIDGET_YAML := inst/htmlwidgets/surfwidget.yaml

.PHONY: all surfview check-surfviewjs sync-surfviewjs verify-build show-version clean-surfview

all: surfview

surfview: sync-surfviewjs verify-build

check-surfviewjs:
	@test -f "$(SURFVIEWJS_PACKAGE)" || { \
		echo "Missing controlled surfviewjs checkout: $(SURFVIEWJS_DIR)"; exit 1; \
	}

$(SURFVIEWJS_BUILD): $(SURFVIEWJS_SOURCES) $(SURFVIEWJS_PACKAGE) | check-surfviewjs
	cd "$(SURFVIEWJS_DIR)" && npm run build

$(VENDOR_DIR):
	mkdir -p "$@"

sync-surfviewjs: $(SURFVIEWJS_BUILD) | $(VENDOR_DIR)
	cp "$(SURFVIEWJS_BUILD)" "$(VENDOR_JS)"
	@SOURCE_COMMIT=$$(git -C "$(SURFVIEWJS_DIR)" rev-parse HEAD); \
	SOURCE_VERSION=$$(node -p "require('$(SURFVIEWJS_PACKAGE)').version"); \
	ARTIFACT_SHA=$$(shasum -a 256 "$(VENDOR_JS)" | cut -d ' ' -f 1); \
	{ \
		echo "source=bbuchsbaum/surfviewjs"; \
		echo "commit=$$SOURCE_COMMIT"; \
		echo "version=$$SOURCE_VERSION"; \
		echo "sha256=$$ARTIFACT_SHA"; \
		echo "three_revision=185"; \
	} > "$(VENDOR_MARKER)"

verify-build: $(VENDOR_JS) $(VENDOR_MARKER)
	@test $$(wc -c < "$(VENDOR_JS)") -gt 10000
	@grep -q '^commit=[0-9a-f]\{40\}$$' "$(VENDOR_MARKER)"
	@grep -q '^three_revision=185$$' "$(VENDOR_MARKER)"
	@grep -q 'const t="185"' "$(VENDOR_JS)"
	@EXPECTED=$$(grep '^sha256=' "$(VENDOR_MARKER)" | cut -d= -f2); \
	ACTUAL=$$(shasum -a 256 "$(VENDOR_JS)" | cut -d ' ' -f1); \
	test "$$EXPECTED" = "$$ACTUAL"
	@EXPECTED=$$(grep '^commit=' "$(VENDOR_MARKER)" | cut -d= -f2); \
	ACTUAL=$$(git -C "$(SURFVIEWJS_DIR)" rev-parse HEAD); \
	test "$$EXPECTED" = "$$ACTUAL"
	@grep -q 'name: surfview' "$(WIDGET_YAML)"
	@grep -q 'surfview.embed.iife.js' "$(WIDGET_YAML)"
	@echo "Verified vendored surfview embed and provenance marker."

show-version:
	@cat "$(VENDOR_MARKER)"

clean-surfview:
	rm -f "$(VENDOR_JS)" "$(VENDOR_MARKER)"
