#!/usr/bin/env bash

set -e

rm -Rf download

nix-shell -p python38Packages.pyyaml python38Packages.jinja2 -c './get-convert-paprika.py'
