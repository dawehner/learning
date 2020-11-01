<?php


namespace vektah\parser_combinator\language\css;

use vektah\parser_combinator\exception\ParseException;
use vektah\parser_combinator\language\css\selectors\AllSelector;
use vektah\parser_combinator\language\css\selectors\Selector;

class Css
{
    /** @var CssRuleSet[] */
    public $rulesets = [];

    /** @var CssObject[] */
    private static $objectCache;

    /**
     * @param CssRuleSet[] $rulesets
     *
     * @throws ParseException
     */
    public function __construct(array $rulesets) {
        foreach ($rulesets as $ruleset) {
            if (!($ruleset instanceof CssRuleSet)) {
                throw new ParseException('Non CssRuleSet in a Css file');
            }
        }
        $this->rulesets = $rulesets;
    }

    /**
     * @return CssRuleSet[]
     */
    public function getRulesets()
    {
        return $this->rulesets;
    }

    /**
     * @param string|CssObject|Selector $selector
     * @return CssObject
     *
     * @throws \LogicException
     */
    public static function buildObject($selector) {
        if ($selector instanceof CssObject) {
            return $selector;
        }

        if (is_string($selector)) {
            if (!isset(self::$objectCache[$selector])) {
                // When dealing with selectors describing objects commas should denote a match of any of these things.
                // An object that must match all of its children should do this.
                $choices = [];
                foreach (explode(',', $selector) as $part) {
                    $part = trim($part);
                    $choices[] = CssSelectorParser::instance()->parseString($part);
                }

                self::$objectCache[$selector] = new AllSelector($choices);
            }
            $selector = self::$objectCache[$selector];
        }

        if ($selector instanceof Selector) {
            return $selector->define();
        }

        throw new \LogicException('I dont know how to build an object from ' . get_class($selector));
    }

    /**
     * @param string $selector
     * @return CssDeclaration[]
     */
    public function getDeclarations($selector) {
        $object = self::buildObject($selector);

        $matches = [];
        foreach ($this->rulesets as $ruleset) {
            if ($ruleset->getSelector()->matchesObject($object)) {
                foreach ($ruleset->getDeclarations() as $declaration) {
                    $matches[$declaration->getName()] = $declaration;
                }
            }
        }

        return $matches;
    }

    /**
     * From a given selector work out what css applies
     *
     * @param string $selector
     * @return string
     */
    public function getMatchingCss($selector) {
        $declarations = $this->getDeclarations($selector);

        $css = '';

        foreach ($declarations as $declaration) {
            $css .= $declaration->toCss();
        }

        return $css;
    }

    public function toCss()
    {
        $css = '';
        foreach ($this->rulesets as $ruleset) {
            $css .= $ruleset->toCss();
        }

        return $css;
    }
}
