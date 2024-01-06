public_dir = $(abspath ./public)

.PHONY: all clean

all: $(public_dir)/index.html $(public_dir)/app.js $(public_dir)/app.css

clean:
	rm -rf $(public_dir)
	mkdir -p $(public_dir)

$(public_dir)/index.html: src/index.html
	minify $< > $@

$(public_dir)/app.js: src/Umbra.elm
	elm make $< --output /tmp/app.js
	minify /tmp/app.js > $@

$(public_dir)/app.css: src/umbra.css
	minify $< > $@
