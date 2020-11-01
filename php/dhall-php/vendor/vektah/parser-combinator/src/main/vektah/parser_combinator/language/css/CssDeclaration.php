<?php

namespace vektah\parser_combinator\language\css;

class CssDeclaration
{
    /** @var  string */
    private $name;

    /** @var string */
    private $value;

    /** @var bool */
    private $important = false;

    /**
     * @param string $name
     * @param string $value
     * @param bool $important
     */
    public function __construct($name, $value, $important = false)
    {
        $this->name = $name;
        $this->value = $value;
        $this->important = $important;
    }

    /**
     * @return string
     */
    public function getName()
    {
        return $this->name;
    }

    /**
     * @return string
     */
    public function getValue()
    {
        return $this->value;
    }

    /**
     * @return boolean
     */
    public function getImportant()
    {
        return $this->important;
    }

    public function toCss()
    {
        $important = $this->important ? "!important" : '';
        return "$this->name:$this->value$important;";
    }
}
