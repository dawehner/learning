<?php


namespace vektah\parser_combinator\language\css\selectors;

use vektah\parser_combinator\language\css\CssObject;

class AdjacentSelector extends Selector
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
        return "{$this->parent->toCss()} + {$this->child->toCss()}";
    }

    public function __toString() {
        return "Adjacent($this->parent, $this->child)";
    }

    public function define()
    {
        $object = new CssObject();
        $object->children = [
            $this->parent->define(),
            $this->child->define(),
        ];
    }

    public function matchesObject(CssObject $object)
    {
        $parent_found = false;
        $child_found = false;

        foreach ($object->children as $child) {
            if ($this->parent->matchesObject($child)) {
                $parent_found = true;
            }
            if ($this->child->matchesObject($child)) {
                $child_found = true;
            }
        }

        return $parent_found && $child_found;
    }
}
