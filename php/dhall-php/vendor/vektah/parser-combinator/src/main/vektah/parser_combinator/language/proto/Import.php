<?php

namespace vektah\parser_combinator\language\proto;

class Import
{
    public $name;

    public function __construct($name)
    {
        $this->name = $name;
    }
}
