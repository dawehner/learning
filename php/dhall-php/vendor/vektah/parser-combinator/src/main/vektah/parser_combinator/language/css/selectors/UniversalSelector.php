<?php


namespace vektah\parser_combinator\language\css\selectors;

use vektah\parser_combinator\language\css\CssObject;

class UniversalSelector extends Selector
{
    /** @var string */
    private $namespace;

    public function __construct($namespace = null)
    {
        $this->namespace = $namespace;
    }

    /**
     * @return string
     */
    public function getNamespace()
    {
        return $this->namespace;
    }

    public function __toString() {
        $namespace = $this->namespace ? ', ' . $this->namespace : '';
        return "Universal({$namespace})";
    }

    public function toCss() {
        $namespace = $this->namespace ? $this->namespace . '|' : '';
        return "{$namespace}*";
    }

    /**
     * @return CssObject
     */
    public function define()
    {
        return new CssObject();
    }

    public function matchesObject(CssObject $object)
    {
        return true;
    }
}
