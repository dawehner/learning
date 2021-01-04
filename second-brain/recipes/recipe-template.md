{# Jinja2 template for converting a pyprika Recipe object to Markdown #}
# {{ name }}

[[ recipes ]]

{% if categories -%}
## Categories
{% for category in categories -%}
[[recipes/{{ category.name -}}]]
{% endfor -%}
{% endif -%}

## Info

{% if on_favorites %}
⭐️
{% endif %}

{% if image_url %}
![{{-name -}}]({{-image_url-}})
{% endif %}

{% if prep_time %}
**Prep time:** {{ prep_time -}}
{% endif %}
{% if cook_time %}
**Cook time:** {{ cook_time -}}
{% endif %}
{% if servings %}
{% if servings | length == 2 %}
**Servings:** {{ servings.0 }}-{{ servings.1 -}}
{% else %}
**Servings:** {{ servings }}
{% endif %}
{% endif %}

{% if notes %}
{{ notes }}
{% endif %}

## Ingredients
{% for incredient in ingredients %}
{%- if incredient.amount -%}
- *{{ incredient.amount }}* {{ incredient.unit }} {{ incredient.label -}}
{%- else -%}
- {{ incredient -}}
{% endif %}
{% endfor %}

## Directions

{% for direction in directions %}
{{ loop.index }}. {{ direction -}}
{%endfor %}

{# If source is provided with a URL, make it a link. #}
{% if source and source_url %}
## Source: [{{ source }}]({{ source_url }})
{% elif source %}
## Source: {{ source }}
{% endif %}
