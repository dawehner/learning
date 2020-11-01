<?php

namespace vektah\parser_combinator\language\proto;

class Field
{
    public $label;
    public $type;
    public $identifier;
    public $index;
    public $options;
    public $default;

    public function __construct($label, $type, $identifier, $index, $options = null)
    {
        $this->identifier = $identifier;
        $this->index = $index;
        $this->label = $label;
        $this->type = $type;
        $this->options = $options;

        if (is_array($options)) {
            foreach ($options as $option) {
                if ($option->identifier == 'default') {
                    $this->default = $option->value;
                }
            }
        }
    }
}
