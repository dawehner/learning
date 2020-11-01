<?php

namespace vektah\parser_combinator\formatter;

use vektah\parser_combinator\Input;
use vektah\parser_combinator\Result;
use vektah\parser_combinator\parser\Parser;

class ClosureWithResult extends Parser
{
    /** @var Parser */
    private $parser;

    /** @var  callable */
    private $function;

    public function __construct($parser, callable $function)
    {
        $this->function = $function;
        $this->parser = Parser::sanitize($parser);
    }

    /**
     * @param Input $input
     * @return Result
     */
    public function parse(Input $input)
    {
        $result = $this->parser->parse($input)->addParser($this);
        // Don't call the callback if there was an error.
        if ($result->errorMessage) {
            return $result;
        }

        $result->data = call_user_func($this->function, $result, $input);

        return $result;
    }
}
