<?php


namespace vektah\parser_combinator\language\css\selectors;

use vektah\parser_combinator\exception\GrammarException;
use vektah\parser_combinator\language\css\CssObject;

class AllSelector extends Selector
{
    /** @var Selector[] */
    private $children;

    public function __construct($children) {
        if (!is_array($children)) {
            $children = func_get_args();
        }

        foreach ($children as $child) {
            if (!($child instanceof Selector)) {
                throw new GrammarException('Children of All must be selectors. Got: ' . print_r($child, true));
            }
        }

        $this->children = $children;
    }

    /**
     * @return array
     */
    public function getChildren()
    {
        return $this->children;
    }

    public function toCss()
    {
        $child_css = array_map(function($child) {
            return $child->toCss();
        }, $this->children);

        return implode('', $child_css);
    }

    public function __toString()
    {
        return 'All(' .  implode(', ', $this->children) . ')';
    }

    public function define() {
        $object = new CssObject();

        foreach ($this->children as $child) {
            $child_object = $child->define();
            foreach ($child_object as $key => $value) {
                if ($value) {
                    if (is_array($object->$key)) {
                        $object->$key = array_merge($object->$key, $child_object->$key);
                    } else {
                        $object->$key = $child_object->$key;
                    }
                }
            }
        }

        return $object;
    }

    public function matchesObject(CssObject $object)
    {
        foreach ($this->children as $child) {
            if (!$child->matchesObject($object)) {
                return false;
            }
        }

        return true;
    }
}
