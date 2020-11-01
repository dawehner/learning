<?php


namespace vektah\parser_combinator\language\css\selectors;

use vektah\parser_combinator\language\css\CssObject;

class ChildSelector extends Selector
{
    private $parent;
    private $child;

    public function __construct(Selector $parent, Selector $child)
    {
        $this->child = $child;
        $this->parent = $parent;
    }

    public function toCss()
    {
        return "{$this->parent->toCss()} > {$this->child->toCss()}";
    }

    public function __toString() {
        return "Child($this->parent, $this->child)";
    }

    /**
     * @return CssObject
     */
    public function define()
    {
        $parent = $this->parent->define();
        $parent->children[] = $this->child->define();

        return $parent;
    }

    public function matchesObject(CssObject $object)
    {
        if (!$this->parent->matches($object)) {
            return false;
        }

        foreach ($object->children as $child) {
            if ($this->child->matches($child)) {
                return true;
            }
        }

        return false;
    }
}
