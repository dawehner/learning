<?php

namespace vektah\parser_combinator\language\proto;

class Extend
{
    public $name;
    public $members;

    public function __construct($name, $members)
    {
        $this->name = $name;
        $this->members = $members;
    }
}
