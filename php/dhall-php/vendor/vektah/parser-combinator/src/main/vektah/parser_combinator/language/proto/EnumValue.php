<?php

namespace vektah\parser_combinator\language\proto;

class EnumValue
{
    public $name;
    public $id;
    public $options;

    public function __construct($name, $id, array $options = [])
    {
        $this->name = $name;
        $this->id = $id;
        $this->options = $options;
    }
}
