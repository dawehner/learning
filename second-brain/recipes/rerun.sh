#!/usr/bin/env bash

set -e

SHOULD_RESET=0
SHOULD_UPLOAD=0

for arg in "$@"
do
    case $arg in
        -r|--reset)
        SHOULD_RESET=1
        shift # Remove --initialize from processing
        ;;
    esac
    case $arg in
        -u|--upload)
        SHOULD_UPLOAD=1
        shift # Remove --initialize from processing
        ;;
    esac
done

if [[ $SHOULD_RESET == 1 ]]; then
  rm -Rf download
  rm -Rf markdown
fi

'./get-convert-paprika.py'

cd foam-mkdocs-template
nix-shell ./env --command 'mkdocs build'
cd ..

if [[ $SHOULD_UPLOAD == 1 ]]; then
  scp -rp foam-mkdocs-template/site/* team@192.168.1.131:/volume1/web/recipes/
fi
