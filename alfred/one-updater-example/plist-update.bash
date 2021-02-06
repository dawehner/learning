#!/usr/bin/env bash

search=".alfredworkflow"
replace=".plist"

for f in *.alfredworkflow; do
  unzip $f -d temp
  mv temp/info.plist ${f/$search/$replace}

  rm -rf temp
done
