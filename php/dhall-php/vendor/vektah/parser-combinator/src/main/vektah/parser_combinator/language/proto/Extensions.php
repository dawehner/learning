<?php

namespace vektah\parser_combinator\language\proto;

class Extensions
{
    public $min;
    public $max;

    public function __construct($min, $max)
    {
        $this->max = $max;
        $this->min = $min;
    }
}
