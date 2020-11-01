<?php


namespace vektah\parser_combinator\language\css\selectors;

use vektah\parser_combinator\language\css\CssObject;
use vektah\parser_combinator\language\css\CssSelectorParser;

abstract class Selector
{
    public function matches($selector) {
        if (!is_string($selector)) {
            throw new \InvalidArgumentException("$selector must be a string");
        }

        // When dealing with selectors describing objects commas should denote a match of any of these things.
        foreach (explode(',', $selector) as $part) {
            $part = trim($part);
            $selector = CssSelectorParser::instance()->parseString($part);
            $object = $selector->define();
            $object->isRoot = true;
            if ($this->matchesObject($object)) {
                return true;
            }
        }

        return false;
    }

    abstract public function matchesObject(CssObject $object);


    /**
     * @return CssObject
     */
    abstract public function define();
    abstract public function __toString();
    abstract public function toCss();
}
