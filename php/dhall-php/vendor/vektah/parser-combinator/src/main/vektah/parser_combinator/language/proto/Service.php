<?php

namespace vektah\parser_combinator\language\proto;

class Service
{
    public $name;
    public $endpoints;

    public function __construct($name, array $endpoints)
    {
        $this->name = $name;
        $this->endpoints = $endpoints;
    }
}
