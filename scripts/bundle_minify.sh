#!/bin/bash

set -e

# Ensure .esbuild directory exists
mkdir -p .esbuild

# Download esbuild if not exists
if [ ! -f ".esbuild/esbuild" ]; then
    echo "Downloading esbuild..."
    wget -qO- https://registry.npmjs.org/@esbuild/linux-x64/-/linux-x64-0.20.1.tgz | tar -xz -C /tmp
    mv /tmp/package/bin/esbuild .esbuild/esbuild
    chmod +x .esbuild/esbuild
fi

# Bundle and minify JS
echo "Bundling and minifying JS..."
.esbuild/esbuild src/static/main.js --bundle --minify --sourcemap --outfile=src/static/main.bundle.js

# Minify CSS
echo "Minifying CSS..."
.esbuild/esbuild src/static/style.css --minify --sourcemap --outfile=src/static/style.bundle.css

# Compute hashes
echo "Computing hashes and updating index.html..."
csshash=$(sha1sum src/static/style.bundle.css | awk '{print substr($0,1,8)}')
jshash=$(sha1sum src/static/main.bundle.js | awk '{print substr($0,1,8)}')

# Update index.html
sed -i "s/style\.css?hash=csshash/style.bundle.css?hash=$csshash/g" src/static/index.html
sed -i "s/main\.js?hash=jshash/main.bundle.js?hash=$jshash/g" src/static/index.html

echo "Done!"
