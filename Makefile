# Makefile for neurosurf R package with surfviewjs integration
# Usage: make surfview

# ============================================================================
# Configuration
# ============================================================================

# Paths
# Source of truth for the widget library is the external surfviewjs checkout.
# The older inst/htmlwidgets/neurosurface subtree in this repo is not used by
# the vendor sync below.
SURFVIEWJS_DIR := $(HOME)/code/jscode/surfviewjs
HTMLWIDGETS_DIR := inst/htmlwidgets
NEUROSURFACE_LIB := $(HTMLWIDGETS_DIR)/lib/neurosurface
SURFWIDGET_YAML := $(HTMLWIDGETS_DIR)/surfwidget.yaml
PKGDOWN_NEUROSURFACE_DIRS := \
	docs/articles/interactive-surfaces_files/neurosurface-2.1.0 \
	docs/articles/displaying-surfaces_files/neurosurface-2.1.0 \
	docs/reference/libs/neurosurface-2.1.0

# Source files to watch for changes
SURFVIEWJS_SRC := $(shell find $(SURFVIEWJS_DIR)/src -type f -name '*.js' -o -name '*.ts' 2>/dev/null)
SURFVIEWJS_PKG := $(SURFVIEWJS_DIR)/package.json
SURFVIEWJS_BUILD := $(SURFVIEWJS_DIR)/dist/neurosurface.umd.js

# Build outputs
NEUROSURFACE_JS := $(NEUROSURFACE_LIB)/neurosurface.umd.js
NEUROSURFACE_MAP := $(NEUROSURFACE_LIB)/neurosurface.umd.js.map

# Colors for pretty output
COLOR_BLUE := \033[0;34m
COLOR_GREEN := \033[0;32m
COLOR_YELLOW := \033[0;33m
COLOR_RED := \033[0;31m
COLOR_RESET := \033[0m

# ============================================================================
# Main Targets
# ============================================================================

.PHONY: all
all: surfview

.PHONY: surfview
surfview: sync-surfviewjs update-yaml sync-pkgdown-surfview
	@echo "$(COLOR_GREEN)✓ surfview build complete$(COLOR_RESET)"
	@echo "$(COLOR_BLUE)JavaScript library: $(NEUROSURFACE_JS)$(COLOR_RESET)"
	@$(MAKE) --no-print-directory show-version

.PHONY: clean-surfview
clean-surfview:
	@echo "$(COLOR_YELLOW)Cleaning surfview build artifacts...$(COLOR_RESET)"
	rm -rf $(NEUROSURFACE_LIB)
	@echo "$(COLOR_GREEN)✓ Clean complete$(COLOR_RESET)"

.PHONY: clean-all
clean-all: clean-surfview
	@echo "$(COLOR_YELLOW)Cleaning surfviewjs build artifacts...$(COLOR_RESET)"
	cd $(SURFVIEWJS_DIR) && npm run clean 2>/dev/null || true
	@echo "$(COLOR_GREEN)✓ Full clean complete$(COLOR_RESET)"

# ============================================================================
# Build Dependencies
# ============================================================================

# Check if surfviewjs directory exists
.PHONY: check-surfviewjs
check-surfviewjs:
	@if [ ! -d "$(SURFVIEWJS_DIR)" ]; then \
		echo "$(COLOR_RED)Error: surfviewjs directory not found at $(SURFVIEWJS_DIR)$(COLOR_RESET)"; \
		echo "$(COLOR_YELLOW)Please check the SURFVIEWJS_DIR path in Makefile$(COLOR_RESET)"; \
		exit 1; \
	fi
	@if [ ! -f "$(SURFVIEWJS_PKG)" ]; then \
		echo "$(COLOR_RED)Error: package.json not found in $(SURFVIEWJS_DIR)$(COLOR_RESET)"; \
		exit 1; \
	fi

# Ensure npm dependencies are installed
$(SURFVIEWJS_DIR)/node_modules: $(SURFVIEWJS_PKG) | check-surfviewjs
	@echo "$(COLOR_BLUE)Installing surfviewjs npm dependencies...$(COLOR_RESET)"
	cd $(SURFVIEWJS_DIR) && npm install
	@touch $(SURFVIEWJS_DIR)/node_modules

# Build surfviewjs library
$(SURFVIEWJS_BUILD): $(SURFVIEWJS_SRC) $(SURFVIEWJS_DIR)/node_modules
	@echo "$(COLOR_BLUE)Building surfviewjs library...$(COLOR_RESET)"
	cd $(SURFVIEWJS_DIR) && npm run build || true
	@if [ ! -f "$(SURFVIEWJS_BUILD)" ]; then \
		echo "$(COLOR_RED)Error: Build failed - $(SURFVIEWJS_BUILD) not created$(COLOR_RESET)"; \
		exit 1; \
	fi
	@echo "$(COLOR_GREEN)✓ surfviewjs build complete$(COLOR_RESET)"
	@echo "$(COLOR_YELLOW)Note: TypeScript type generation errors can be ignored if UMD bundle exists$(COLOR_RESET)"

# Create target directory
$(NEUROSURFACE_LIB):
	@echo "$(COLOR_BLUE)Creating directory: $(NEUROSURFACE_LIB)$(COLOR_RESET)"
	mkdir -p $(NEUROSURFACE_LIB)

# Copy built files to neurosurf
.PHONY: sync-surfviewjs
sync-surfviewjs: $(SURFVIEWJS_BUILD) | $(NEUROSURFACE_LIB)
	@echo "$(COLOR_BLUE)Copying built files to neurosurf...$(COLOR_RESET)"
	cp $(SURFVIEWJS_DIR)/dist/neurosurface.umd.js $(NEUROSURFACE_JS)
	@if [ -f "$(SURFVIEWJS_DIR)/dist/neurosurface.umd.js.map" ]; then \
		cp $(SURFVIEWJS_DIR)/dist/neurosurface.umd.js.map $(NEUROSURFACE_MAP); \
		echo "$(COLOR_GREEN)✓ Copied source map$(COLOR_RESET)"; \
	fi
	@if [ -f "$(SURFVIEWJS_DIR)/dist/surfview.umd.js.map" ]; then \
		cp $(SURFVIEWJS_DIR)/dist/surfview.umd.js.map $(NEUROSURFACE_LIB)/surfview.umd.js.map; \
		echo "$(COLOR_GREEN)✓ Copied surfview source map$(COLOR_RESET)"; \
	elif [ -f "$(SURFVIEWJS_DIR)/dist/neurosurface.umd.js.map" ]; then \
		cp $(SURFVIEWJS_DIR)/dist/neurosurface.umd.js.map $(NEUROSURFACE_LIB)/surfview.umd.js.map; \
		echo "$(COLOR_GREEN)✓ Duplicated source map as surfview.umd.js.map$(COLOR_RESET)"; \
	fi
	@echo "$(COLOR_GREEN)✓ Files copied successfully$(COLOR_RESET)"

.PHONY: sync-pkgdown-surfview
sync-pkgdown-surfview: sync-surfviewjs
	@echo "$(COLOR_BLUE)Syncing pkgdown JavaScript assets...$(COLOR_RESET)"
	@for dir in $(PKGDOWN_NEUROSURFACE_DIRS); do \
		if [ -d "$$dir" ]; then \
			cp $(NEUROSURFACE_JS) "$$dir/neurosurface.umd.js"; \
			cp $(NEUROSURFACE_LIB)/surfview.umd.js.map "$$dir/surfview.umd.js.map"; \
			echo "$(COLOR_GREEN)✓ Synced $$dir$(COLOR_RESET)"; \
		else \
			echo "$(COLOR_YELLOW)Skipping missing directory: $$dir$(COLOR_RESET)"; \
		fi; \
	done

# ============================================================================
# Version Management
# ============================================================================

.PHONY: update-yaml
update-yaml: sync-surfviewjs
	@echo "$(COLOR_BLUE)Updating surfwidget.yaml with version from package.json...$(COLOR_RESET)"
	@VERSION=$$(node -p "require('$(SURFVIEWJS_PKG)').version" 2>/dev/null); \
	if [ -z "$$VERSION" ]; then \
		echo "$(COLOR_RED)Error: Could not read version from $(SURFVIEWJS_PKG)$(COLOR_RESET)"; \
		exit 1; \
	fi; \
	echo "$(COLOR_BLUE)Version detected: $$VERSION$(COLOR_RESET)"; \
	if [ -f "$(SURFWIDGET_YAML)" ]; then \
		sed -i.bak -E '/- name: neurosurface/,/src:/{s/(version:[[:space:]]*)[0-9]+\.[0-9]+\.[0-9]+/\1'"$$VERSION"'/;}' $(SURFWIDGET_YAML); \
		rm -f $(SURFWIDGET_YAML).bak; \
		echo "$(COLOR_GREEN)✓ Updated $(SURFWIDGET_YAML) to version $$VERSION$(COLOR_RESET)"; \
	else \
		echo "$(COLOR_YELLOW)Warning: $(SURFWIDGET_YAML) not found$(COLOR_RESET)"; \
	fi

.PHONY: show-version
show-version:
	@echo "$(COLOR_BLUE)Current versions:$(COLOR_RESET)"
	@SURFVIEW_VERSION=$$(node -p "require('$(SURFVIEWJS_PKG)').version" 2>/dev/null || echo "unknown"); \
	YAML_VERSION=$$(grep -A 2 "name: neurosurface" $(SURFWIDGET_YAML) | grep "version:" | sed -E 's/.*version:[[:space:]]*//' | head -1 2>/dev/null || echo "unknown"); \
	echo "  surfviewjs: $$SURFVIEW_VERSION"; \
	echo "  surfwidget.yaml: $$YAML_VERSION"; \
	if [ "$$SURFVIEW_VERSION" != "$$YAML_VERSION" ] && [ "$$SURFVIEW_VERSION" != "unknown" ] && [ "$$YAML_VERSION" != "unknown" ]; then \
		echo "$(COLOR_YELLOW)  ⚠ Version mismatch detected!$(COLOR_RESET)"; \
	else \
		echo "$(COLOR_GREEN)  ✓ Versions in sync$(COLOR_RESET)"; \
	fi

# ============================================================================
# Development Helpers
# ============================================================================

.PHONY: watch-surfview
watch-surfview:
	@echo "$(COLOR_BLUE)Watching for changes in surfviewjs...$(COLOR_RESET)"
	@echo "$(COLOR_YELLOW)Press Ctrl+C to stop$(COLOR_RESET)"
	@if ! command -v fswatch > /dev/null; then \
		echo "$(COLOR_RED)Error: fswatch not installed$(COLOR_RESET)"; \
		echo "$(COLOR_YELLOW)Install with: brew install fswatch$(COLOR_RESET)"; \
		exit 1; \
	fi
	@while true; do \
		$(MAKE) --no-print-directory surfview; \
		echo "$(COLOR_BLUE)Waiting for changes...$(COLOR_RESET)"; \
		fswatch -1 $(SURFVIEWJS_DIR)/src; \
	done

.PHONY: verify-build
verify-build: $(NEUROSURFACE_JS)
	@echo "$(COLOR_BLUE)Verifying build integrity...$(COLOR_RESET)"
	@if [ ! -f "$(NEUROSURFACE_JS)" ]; then \
		echo "$(COLOR_RED)✗ Build file missing: $(NEUROSURFACE_JS)$(COLOR_RESET)"; \
		exit 1; \
	fi
	@SIZE=$$(wc -c < "$(NEUROSURFACE_JS)" | tr -d ' '); \
	if [ $$SIZE -lt 10000 ]; then \
		echo "$(COLOR_RED)✗ Build file suspiciously small ($$SIZE bytes)$(COLOR_RESET)"; \
		exit 1; \
	fi
	@echo "$(COLOR_GREEN)✓ Build verification passed (Size: $$(numfmt --to=iec-i --suffix=B $$SIZE 2>/dev/null || echo $$SIZE bytes))$(COLOR_RESET)"
	@if grep -q "neurosurface" "$(NEUROSURFACE_JS)"; then \
		echo "$(COLOR_GREEN)✓ neurosurface namespace found$(COLOR_RESET)"; \
	else \
		echo "$(COLOR_YELLOW)⚠ neurosurface namespace not detected$(COLOR_RESET)"; \
	fi

.PHONY: status
status:
	@echo "$(COLOR_BLUE)Build Status$(COLOR_RESET)"
	@echo "════════════════════════════════════════"
	@$(MAKE) --no-print-directory show-version
	@echo ""
	@if [ -f "$(NEUROSURFACE_JS)" ]; then \
		echo "$(COLOR_GREEN)✓ Built files present$(COLOR_RESET)"; \
		echo "  Location: $(NEUROSURFACE_JS)"; \
		SIZE=$$(wc -c < "$(NEUROSURFACE_JS)" | tr -d ' '); \
		echo "  Size: $$(numfmt --to=iec-i --suffix=B $$SIZE 2>/dev/null || echo $$SIZE bytes)"; \
		echo "  Modified: $$(stat -f '%Sm' -t '%Y-%m-%d %H:%M:%S' $(NEUROSURFACE_JS) 2>/dev/null || stat -c '%y' $(NEUROSURFACE_JS) 2>/dev/null || echo 'unknown')"; \
	else \
		echo "$(COLOR_YELLOW)⚠ No built files found$(COLOR_RESET)"; \
		echo "  Run 'make surfview' to build"; \
	fi

# ============================================================================
# R Package Integration
# ============================================================================

.PHONY: r-check
r-check: surfview
	@echo "$(COLOR_BLUE)Running R CMD check...$(COLOR_RESET)"
	R -e "devtools::check()"

.PHONY: r-document
r-document:
	@echo "$(COLOR_BLUE)Updating R documentation...$(COLOR_RESET)"
	R -e "devtools::document()"

.PHONY: r-install
r-install: surfview r-document
	@echo "$(COLOR_BLUE)Installing neurosurf package...$(COLOR_RESET)"
	R -e "devtools::install()"

.PHONY: r-test
r-test: surfview
	@echo "$(COLOR_BLUE)Running R package tests...$(COLOR_RESET)"
	R -e "devtools::test()"

# ============================================================================
# Help
# ============================================================================

.PHONY: help
help:
	@echo "$(COLOR_BLUE)neurosurf Makefile - surfviewjs Integration$(COLOR_RESET)"
	@echo ""
	@echo "$(COLOR_GREEN)Main targets:$(COLOR_RESET)"
	@echo "  surfview          Build surfviewjs and copy to neurosurf (default)"
	@echo "  sync-pkgdown-surfview  Copy the synced bundle into checked-in pkgdown assets"
	@echo "  clean-surfview    Remove built files from neurosurf"
	@echo "  clean-all         Remove all build artifacts (including surfviewjs)"
	@echo ""
	@echo "$(COLOR_GREEN)Development:$(COLOR_RESET)"
	@echo "  status            Show current build status and versions"
	@echo "  verify-build      Verify build integrity"
	@echo "  watch-surfview    Auto-rebuild on source changes (requires fswatch)"
	@echo ""
	@echo "$(COLOR_GREEN)Version Management:$(COLOR_RESET)"
	@echo "  update-yaml       Update surfwidget.yaml with version from package.json"
	@echo "  show-version      Display current versions"
	@echo ""
	@echo "$(COLOR_GREEN)R Package:$(COLOR_RESET)"
	@echo "  r-check           Run R CMD check (builds surfview first)"
	@echo "  r-document        Update R documentation"
	@echo "  r-install         Install neurosurf package (builds surfview first)"
	@echo "  r-test            Run R package tests"
	@echo ""
	@echo "$(COLOR_GREEN)Configuration:$(COLOR_RESET)"
	@echo "  SURFVIEWJS_DIR = $(SURFVIEWJS_DIR)"
	@echo ""
