from jinja2 import Environment, FileSystemLoader
from os import makedirs, path
from datetime import datetime
import json
import ruamel
import ruamel.yaml

yaml = ruamel.yaml.YAML()

with open('./templates/model.json') as f:
  config_data = json.load(f)

env = Environment(
  loader=FileSystemLoader('./templates'),
  trim_blocks=True,
  lstrip_blocks=True,
)

model = {}
for d in config_data.get('configs', []):
  # Stan model
  template = env.get_template('model.jinja')

#  note = 'NOTE: THIS STAN CODE IS GENERATED VIA "update.py"'
  template.stream({
#    'note': note,
    **d
  }).dump('stan/{}.stan'.format(d.get('model')))

with open('./templates/model_LMA.json') as f:
  config_data = json.load(f)

# LMA only model
model = {}
for d in config_data.get('configs', []):
  # Stan model
  template = env.get_template('model_LMA.jinja')

  template.stream({
    **d
  }).dump('stan/{}.stan'.format(d.get('model')))

