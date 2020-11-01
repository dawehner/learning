<?php


namespace vektah\parser_combinator\language\css\selectors;

use vektah\parser_combinator\language\css\CssObject;

class ElementSelector extends Selector
{
    /** @var string */
    private $namespace;

    /** @var string */
    private $element;

    public function __construct($element, $namespace = null)
    {
        $this->namespace = $namespace;
        $this->element = $element;
    }

    /**
     * @return string
     */
    public function getElement()
    {
        return $this->element;
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
        return "Element({$this->element}{$namespace})";
    }

    public function toCss() {
        $namespace = $this->namespace ? $this->namespace . '|' : '';
        return "{$namespace}{$this->element}";
    }

    public function matchesSelector(Selector $selector) {
        return $selector instanceof ElementSelector && $selector->element === $this->element;
    }

    /**
     * @return CssObject
     */
    public function define()
    {
        $object = new CssObject();

        $object->element = $this->element;

        return $object;
    }

    public function matchesObject(CssObject $object)
    {
        return strtolower($object->element) === strtolower($this->element);
    }
}
