#!/usr/bin/env bash

# Compiling with ghcjs
stack build --stack-yaml=stack-ghcjs.yaml

# Moving the generated files to the js folder
mkdir -p js
cp -r $(stack path --local-install-root --stack-yaml=stack-ghcjs.yaml)/bin/mockClient.jsexe/{all.js,all.js.externs} js/

# Swapping the default html with the one serving a minified version
cp assets/html/index.html js/index.html

# Minifying all.js file using the closure compiler, and removing unnecessary files
cd js
ccjs all.js --externs=all.js.externs --compilation_level=ADVANCED_OPTIMIZATIONS > all.min.js
# ccjs all.js > all.min.js

# OPTIONAL: zipping, to see the actual transferred size of the app:
zopfli all.min.js
