<?php


namespace vektah\parser_combinator\language\css\selectors;

use vektah\parser_combinator\language\css\CssObject;

class HashSelector extends Selector
{
    /** @var string */
    private $hash;

    public function __construct($element)
    {
        $this->hash = $element;
    }

    /**
     * @return string
     */
    public function getElement()
    {
        return $this->hash;
    }

    public function toCss() {
        return "#{$this->hash}";
    }

    public function __toString() {
        return "Hash($this->hash)";
    }

    protected function matchesSelector(Selector $selector)
    {
        return $selector instanceof HashSelector && $selector->hash === $this->hash;
    }

    /**
     * @return CssObject
     */
    public function define()
    {
        $object = new CssObject();
        $object->id = $this->hash;

        return $object;
    }

    public function matchesObject(CssObject $object)
    {
        return strtolower($object->id) === strtolower($this->hash);
    }
}
