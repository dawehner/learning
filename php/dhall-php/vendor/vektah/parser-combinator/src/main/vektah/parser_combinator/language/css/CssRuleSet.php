<?php

namespace vektah\parser_combinator\language\css;

use vektah\parser_combinator\language\css\selectors\Selector;

class CssRuleSet
{
    /** @var Selector */
    private $selector;

    /** @var CssDeclaration[] */
    private $declarations;

    public function __construct($selector, $declarations = [])
    {
        $this->selector = $selector;
        $this->declarations = $declarations;
    }

    /**
     * @return CssDeclaration[]
     */
    public function getDeclarations()
    {
        return $this->declarations;
    }

    /**
     * @return Selector
     */
    public function getSelector()
    {
        return $this->selector;
    }

    public function toCss()
    {
        $css = $this->selector->toCss() . '{';

        foreach ($this->declarations as $declaration) {
            $css .= $declaration->toCss();
        }

        return $css . '}';
    }
}
