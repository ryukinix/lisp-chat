#!/bin/bash

csshash=`sha1sum src/static/style.css | awk '{print substr($0,1,8)}'`
jshash=`cat src/static/main.js src/static/modules/*.js | sha1sum | awk '{print substr($0,1,8)}'`

sed -i "s/csshash/$csshash/g" src/static/index.html
sed -i "s/jshash/$jshash/g" src/static/index.html
