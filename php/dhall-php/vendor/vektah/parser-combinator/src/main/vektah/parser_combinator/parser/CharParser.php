<?php

namespace vektah\parser_combinator\parser;

use vektah\parser_combinator\Input;
use vektah\parser_combinator\exception\GrammarException;

/**
 * Matches a single char
 */
class CharParser extends Parser
{
    private $char;

    /**
     * @param string $char
     * @throws GrammarException
     */
    public function __construct($char)
    {
        if (strlen($char) > 1) {
            throw new GrammarException('CharParsers only support a single char');
        }

        $this->char = $char;
    }

    /**
     * Parse the given input
     *
     * @param Input $input
     *
     * @return mixed result
     */
    public function parse(Input $input)
    {
        if ($input->peek(0) === $this->char) {
            $input->consume(1);
            return $input->matchHere($this->char);
        }

        return $input->errorHere("$this->char was not found.")->addParser($this);
    }
}
