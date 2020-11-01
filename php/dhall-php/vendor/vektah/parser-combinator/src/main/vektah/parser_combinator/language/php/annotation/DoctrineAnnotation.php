<?php


namespace vektah\parser_combinator\language\php\annotation;

class DoctrineAnnotation
{
    public $name;
    public $arguments = [];
    public $line;

    public function __construct($name, array $arguments = [], $line = 1)
    {
        $this->name = $name;
        $this->arguments = $arguments;
        $this->line = $line;
    }
}
