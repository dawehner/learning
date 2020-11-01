<?php


namespace vektah\parser_combinator\language\css\selectors;

use vektah\parser_combinator\exception\GrammarException;
use vektah\parser_combinator\language\css\CssObject;

class NotSelector extends Selector
{
    /** @var Selector[] */
    private $children;

    public function __construct($children) {
        if (!is_array($children)) {
            $children = func_get_args();
        }

        foreach ($children as $child) {
            if (!($child instanceof Selector)) {
                throw new GrammarException('Children must be selectors');
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
        $child_css = array_map(function(Selector $child) {
            return $child->toCss();
        }, $this->children);

        return ':not(' . implode('', $child_css) . ')';
    }

    public function __toString()
    {
        return 'Not(' .  implode(', ', $this->children) . ')';
    }

    public function define() {
        // Cant really define an object like this...
        return new CssObject();
    }

    public function matchesObject(CssObject $object)
    {
        foreach ($this->children as $child) {
            if ($child->matchesObject($object)) {
                return false;
            }
        }

        return true;
    }
}
