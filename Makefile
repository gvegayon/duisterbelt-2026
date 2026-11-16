# Build locally, commit docs/ (GitHub Pages serves it from main:/docs).

ERGM_JS_VERSION ?= 0.4.0
export ERGM_JS_VERSION

RESEARCH_REPO ?= ../../research/ergm-cond-consistency
REPORT_FIGS   := $(RESEARCH_REPO)/report/cond-failures_files/figure-commonmark

.PHONY: all preview publish clean update-ergm-js figs-from-repo

all:
	quarto render

preview:
	quarto preview index.qmd

update-ergm-js:
	sh scripts/update-ergm-js.sh

# Figures rendered by report/cond-failures.qmd in the research repository.
figs-from-repo:
	cp $(REPORT_FIGS)/fig-outcomes-1.png fig/cond-failures-outcomes.png
	cp $(REPORT_FIGS)/fig-error-1.png fig/cond-failures-error.png

publish: all
	git add docs
	git commit -m "Update slides" || echo "nothing to commit"
	git push

clean:
	rm -rf docs/index.html docs/index_files .quarto
