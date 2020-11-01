<?php

namespace vektah\parser_combinator\language;

use vektah\parser_combinator\Input;
use vektah\parser_combinator\Result;
use vektah\parser_combinator\exception\GrammarException;
use vektah\parser_combinator\parser\Parser;

class Grammar extends Parser
{
    /** @var Parser[] */
    private $parsers;

    public function __set($name, $parser) {
        $parser = Parser::sanitize($parser);
        $this->parsers[$name] = $parser;
        if (!$parser->hasName()) {
            $parser->setName($name);
        }

        return $parser;
    }

    public function __get($name) {
        if (!isset($this->parsers[$name])) {
            throw new GrammarException("Node $name is undefined");
        }

        return $this->parsers[$name];
    }

    /**
     * @param Input $input
     * @return Result
     */
    public function parse(Input $input)
    {
        return $this->root->parse($input);
    }
}
