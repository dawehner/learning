<?php

namespace vektah\parser_combinator\parser\literal;

use vektah\parser_combinator\Input;
use vektah\parser_combinator\Result;
use vektah\parser_combinator\combinator\Choice;
use vektah\parser_combinator\combinator\Sequence;
use vektah\parser_combinator\formatter\Closure;
use vektah\parser_combinator\formatter\Concatenate;
use vektah\parser_combinator\parser\Parser;

/**
 * Matches:
 *  - 0.1234
 *  - 1.2e123
 *  OPTIONALLY
 *  - 0.1234f
 */
class FloatLiteral extends Parser
{
    private $root;

    public function __construct($allowSuffix = true)
    {
        if ($allowSuffix) {
            $suffix = new Choice(['', 'f']);
        } else {
            $suffix = '';
        }

        $standardNotation = new Sequence('[0-9]+\.[0-9]*', $suffix);
        $sciNotation = new Sequence('[0-9]+\.?[0-9]*[eE]{1}[+-]?[0-9]*', $suffix);

        $this->root = new Closure(new Concatenate(new Choice([$sciNotation, $standardNotation])), function ($data) {
            return (float)$data;
        });
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
        return $this->root->parse($input);
    }
}
