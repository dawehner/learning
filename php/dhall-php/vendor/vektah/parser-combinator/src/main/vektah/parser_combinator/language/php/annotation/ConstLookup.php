<?php


namespace vektah\parser_combinator\language\php\annotation;

class ConstLookup
{
    public $class;
    public $static;
    public $line;

    public function __construct($static, $class = null, $line = 1)
    {
        $this->class = $class;
        $this->static = $static;
        $this->line = $line;
    }
}
