<?php

namespace vektah\parser_combinator\language\proto;

class Identifier
{
    public $name;

    public function __construct($name)
    {
        $this->name = $name;
    }
}
