<?php


namespace vektah\parser_combinator\language\css;

class CssObject
{
    /** @var string[] */
    public $classes = [];

    /** @var string */
    public $id;

    /** @var string */
    public $element;

    /** @var CssObject[] */
    public $children = [];

    /** @var CssObject */
    public $parent;

    /** @var string[] */
    public $attributes = [];

    public $isRoot = false;

    public function __construct(array $initial = []) {
        foreach ($initial as $key => $value) {
            if (!property_exists($this, $key)) {
                throw new \UnexpectedValueException("$key does not exist");
            }
            $this->$key = $value;
        }
    }
}
