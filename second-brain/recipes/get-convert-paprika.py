#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python38 python38Packages.jinja2 python38Packages.pyyaml python38Packages.tqdm pandoc
# -*- coding: utf-8 -*-

import os
import yaml
import pprint
import jinja2
import re
from tqdm import tqdm
import shlex, subprocess
import glob
import shutil

if not os.path.exists('download'):
  os.system("paprika-recipes store-password")
  os.system("paprika-recipes download-recipes download")

if not os.path.exists('markdown'):
  os.mkdir('markdown')

if not os.path.exists('html'):
  os.mkdir('html')

if not os.path.exists('foam-mkdocs-template/docs'):
  os.mkdir('foam-mkdocs-template/docs')

files = glob.glob('foam-mkdocs-template/docs/**/*')
for f in files:
    if 'index.md' in f:
        continue
    os.remove(f)

templateLoader = jinja2.FileSystemLoader(searchpath="./")
templateEnv = jinja2.Environment(loader=templateLoader)
TEMPLATE_FILE = "recipe-template.md"
template = templateEnv.get_template(TEMPLATE_FILE)

download_path = "./download/"

files = os.listdir("./download")

for i in tqdm(range(len(files))):
  file = files[i]
  new_filename = file.replace(".yaml", ".md").replace('.paprikarecipe', '')
  paprika = yaml.safe_load(open(download_path + file, 'r'))
  # pprint.pp(paprika)

  if isinstance(paprika['directions'], str):
    paprika['directions'] = paprika['directions'].split("\n")

  # remove 1., 2. etc. from directions
  paprika['directions'] = [re.sub(r'^(\d+\.)(.+)', r'\2', st) for st in paprika['directions']]
 
  # Trim lines
  paprika['directions'] = [s.strip() for s in paprika['directions']]

  # Remove empty lines
  paprika['directions'] = list(filter(None, paprika['directions']))

  if isinstance(paprika['ingredients'], str):
    paprika['ingredients'] = paprika['ingredients'].split("\n")

  data = paprika
  outputText = template.render(data)
  fh = open('./markdown/' + new_filename, "w+")
  fh.write(outputText)
  fh.close()

  # html_name = new_filename.replace(".md", ".html")
  # command = 'pandoc --standalone -f markdown -t html5 -o "html/{}" "markdown/{}" --css pandoc.css'.format(html_name, new_filename)
  # subprocess.call(shlex.split(command))

# Copy images and markdown files to foam-mkdocs-template
os.system('cp -r markdown/* foam-mkdocs-template/docs/')
os.system('cp -r images foam-mkdocs-template/docs/')
