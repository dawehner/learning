<?php


namespace vektah\parser_combinator\language\css\selectors;

use vektah\parser_combinator\language\css\CssObject;

class ClassSelector extends Selector
{
    /** @var string */
    private $class;

    public function __construct($class)
    {
        $this->class = $class;
    }

    /**
     * @return string
     */
    public function getClass()
    {
        return $this->class;
    }

    public function toCss() {
        return ".{$this->class}";
    }

    public function __toString() {
        return "Class($this->class)";
    }

    /**
     * @return CssObject
     */
    public function define()
    {
        $object = new CssObject();

        $object->classes[] = $this->class;

        return $object;
    }

    public function matchesObject(CssObject $object)
    {
        return in_array($this->class, $object->classes);
    }
}
