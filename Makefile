ELISP_FILES := $(shell cask files)
ELC_FILES := $(patsubst %.el,%.elc,$(ELISP_FILES))

.PHONY: test compile clean
.INTERMEDIATE: .compile.intermediate

all: test

# We run clean-elc first because undercover.el doesn't support elc
# files. We run the tests first without loading flx-ido, and then with
# it. We only send the coverage report when running the full test
# suite.
test:
	cask clean-elc
	UNDERCOVER_CONFIG='((:send-report nil))' cask exec buttercup -L . tests
	cask exec buttercup -L . tests tests-with-flx-ido

compile: $(ELC_FILES)

$(ELC_FILES): .compile.intermediate
.compile.intermediate: $(ELISP_FILES)
	cask build

clean:
	cask clean-elc
