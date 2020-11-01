<?php


namespace vektah\parser_combinator\language\php\annotation;

class NonDoctrineAnnotation
{
    public $name;
    public $value;
    public $line;

    public function __construct($name, $value = '', $line = 1)
    {
        $this->name = $name;
        $this->value = $value;
        $this->line = $line;
    }
}
