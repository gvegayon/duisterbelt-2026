# Build locally, commit docs/ (GitHub Pages serves it from main:/docs).

ERGM_JS_VERSION ?= 0.4.0
export ERGM_JS_VERSION

.PHONY: all preview publish clean update-ergm-js

all:
	quarto render

preview:
	quarto preview index.qmd

update-ergm-js:
	sh scripts/update-ergm-js.sh

publish: all
	git add docs
	git commit -m "Update slides" || echo "nothing to commit"
	git push

clean:
	rm -rf docs/index.html docs/index_files .quarto
