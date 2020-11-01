<?php

namespace vektah\parser_combinator\combinator;

use vektah\parser_combinator\exception\GrammarException;
use vektah\parser_combinator\parser\Parser;

abstract class Combinator extends Parser
{
    /** @var callable */
    public $formatCallback;

    /**
     * @var Parser[]
     */
    private $parsers;

    public function __construct($parsers = null)
    {
        if (!is_array($parsers)) {
            $parsers = func_get_args();
        }

        foreach ($parsers as $id => $parser) {
            $parser = Parser::sanitize($parser);
            $parsers[$id] = $parser;
            if (!$parser instanceof Parser) {
                throw new GrammarException('There is an object that is not a parser in this combinator.');
            }
        }
        $this->parsers = $parsers;
    }

    public function append(Parser $parser)
    {
        $this->parsers[] = $parser;
    }

    public function prepend(Parser $parser)
    {
        array_unshift($this->parsers, $parser);
    }

    public function getParsers()
    {
        return $this->parsers;
    }
}
