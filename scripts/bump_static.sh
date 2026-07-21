#!/bin/bash

# Bump static asset cache-busting hashes in index.html.
# Idempotent: replaces any existing hash value, not just the literal "csshash".

csshash=`sha1sum src/static/style.css | awk '{print substr($0,1,8)}'`
jshash=`cat src/static/main.js src/static/modules/*.js | sha1sum | awk '{print substr($0,1,8)}'`

sed -i -E "s/style\\.css\\?hash=[a-z0-9]+/style.css?hash=$csshash/g" src/static/index.html
sed -i -E "s/main\\.js\\?hash=[a-z0-9]+/main.js?hash=$jshash/g" src/static/index.html