#!/bin/bash

csshash=`sha1sum src/static/style.css | awk '{print substr($0,1,8)}'`
jshash=`sha1sum src/static/chat.js | awk '{print substr($0,1,8)}'`

sed -i "s/csshash/$csshash/g" src/static/index.html
sed -i "s/jshash/$jshash/g" src/static/index.html
