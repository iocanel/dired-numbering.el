##
# Eask generated template Makefile
#
# File located in https://github.com/emacs-eask/template-elisp/blob/master/Makefile
##

EMACS ?= emacs
EASK ?= eask

.PHONY: clean package install compile test checkdoc lint test-record

# CI entry point
#
# You can add or remove any commands here
#
# (Option 1): Basic for beginner, only tests for package's installation
ci: clean package install compile checkdoc lint test
# (Option 2): Advanced for a high-quality package
#ci: clean package install compile checkdoc lint test

# Build an package artefact, default to `dist` folder
#
# This is used to test if your package can be built correctly before the
# package installation.
package:
	@echo "Packaging..."
	$(EASK) package

# Install package
#
# If your package is a single file package, you generally wouldn't need to
install:
	@echo "Installing..."
	$(EASK) install

# Byte-compile package
#
# Compile all your package .el files to .elc
compile:
	@echo "Compiling..."
	$(EASK) compile

# Run regression tests
#
# The default test is `ert`; but Eask also support other regression test!
# See https://emacs-eask.github.io/Getting-Started/Commands-and-options/#-linter
test:
	@echo "Testing..."
	$(EASK) install-deps --dev || true
	$(EASK) test ert ./test/run-tests.el

# Run checkdoc
#
# See https://www.emacswiki.org/emacs/CheckDoc
checkdoc:
	@echo "Checking documentation..."
	$(EASK) lint checkdoc --strict

# Lint package metadata
#
# See https://github.com/purcell/package-lint
lint:
	@echo "Linting..."
	$(EASK) lint package

# Generate autoloads file
#
# NOTE: This is generally unnecessary
autoloads:
	@echo "Generating autoloads..."
	$(EASK) autoloads

# Generate -pkg file
#
# NOTE: This is generally unnecessary
pkg-file:
	@echo "Generating -pkg file..."
	$(EASK) pkg-file

# Clean up
#
# This will clean all the entire workspace including the following folders
# and files
#
#   - .eask folder (sandbox)
#   - all .elc files
clean:
	$(EASK) clean all

# Record a green run of the suite in the readme's Test status table
#
# Run it on a clean tree, right after committing, then commit the readme as a
# separate docs: commit.
test-record:
	python3 scripts/test-record.py
