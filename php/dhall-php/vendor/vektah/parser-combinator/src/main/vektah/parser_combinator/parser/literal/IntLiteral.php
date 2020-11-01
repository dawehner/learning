<?php

namespace vektah\parser_combinator\parser\literal;

use vektah\parser_combinator\Input;
use vektah\parser_combinator\Result;
use vektah\parser_combinator\combinator\Choice;
use vektah\parser_combinator\combinator\Sequence;
use vektah\parser_combinator\formatter\Closure;
use vektah\parser_combinator\formatter\Concatenate;
use vektah\parser_combinator\formatter\Ignore;
use vektah\parser_combinator\parser\Parser;
use vektah\parser_combinator\parser\PositiveMatch;

/**
 * Generic integer literal parser.
 *
 * Will capture:
 *  - 1234 as decimal int
 *  - -1234 as negative decimal int
 *  - 033 as octal
 *  - 090 as decimal
 *  - 0xFF as hex
 */
class IntLiteral extends Parser
{
    private $root;

    public function __construct($octal = true, $hex = true)
    {
        // Order here is important, if hex fails to match try oct, otherwise its an int.
        $this->root = new Choice();

        if ($hex) {
            $octInt = new Closure(new Concatenate(new Sequence([new Ignore('0'), '[0-7]', PositiveMatch::instance(), '[0-7]*'])), function($data) {
                return octdec($data);
            });

            $this->root->append($octInt);
        }

        if ($hex) {
            $hexInt = new Closure(new Concatenate(new Sequence(new Ignore('0'), new Ignore('[xX]'), PositiveMatch::instance(), '[A-Fa-f0-9]+')), function($data) {
                return hexdec($data);
            });

            $this->root->append($hexInt);
        }

        $decInt =  new Closure('-?[0-9]+', function ($data) {
            return (int)$data;
        });

        $this->root->append($decInt);

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
