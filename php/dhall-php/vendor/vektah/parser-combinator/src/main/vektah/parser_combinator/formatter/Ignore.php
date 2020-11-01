<?php

namespace vektah\parser_combinator\formatter;

use vektah\parser_combinator\Input;
use vektah\parser_combinator\Result;
use vektah\parser_combinator\parser\Parser;

class Ignore extends Parser
{
    private $parser;

    public function __construct($parser)
    {
        $this->parser = Parser::sanitize($parser);
    }
    /**
     * Parse the given input
     *
     * @param Input $input
     *
     * @return Result result
     */
    public function parse(Input $input)
    {
        $result = $this->parser->parse($input)->addParser($this);

        $result->hasData = false;
        $result->data = null;

        return $result;
    }
}
