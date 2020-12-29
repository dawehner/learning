#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import yaml
import pprint
import jinja2
import re

if not os.path.exists('download'):
  os.system("paprika-recipes store-password")
  os.system("paprika-recipes download-recipes download")

if not os.path.exists('markdown'):
  os.mkdir('markdown')

templateLoader = jinja2.FileSystemLoader(searchpath="./")
templateEnv = jinja2.Environment(loader=templateLoader)
TEMPLATE_FILE = "recipe-template.md"
template = templateEnv.get_template(TEMPLATE_FILE)

download_path = "./download/"

files = os.listdir("./download")

for file in files:
  new_filename = file.replace(".yaml", ".md").replace('.paprikarecipe', '')
  paprika = yaml.load(open(download_path + file, 'r'))
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
  print(file)
