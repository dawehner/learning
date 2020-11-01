<?php

namespace vektah\parser_combinator\language\proto;

class Option
{
    public $identifier;
    public $value;

    public function __construct($identifier, $value)
    {
        $this->identifier = $identifier;
        $this->value = $value;
    }
}
