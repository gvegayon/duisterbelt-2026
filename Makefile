# Build locally, commit docs/ (GitHub Pages serves it from main:/docs).

.PHONY: all preview publish clean

all:
	quarto render

preview:
	quarto preview index.qmd

publish: all
	git add docs
	git commit -m "Update slides" || echo "nothing to commit"
	git push

clean:
	rm -rf docs/index.html docs/index_files .quarto
