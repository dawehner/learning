<?php

namespace vektah\parser_combinator\parser;

use vektah\parser_combinator\Input;
use vektah\parser_combinator\Result;
use vektah\parser_combinator\combinator\Many;
use vektah\parser_combinator\combinator\OptionalChoice;
use vektah\parser_combinator\combinator\Sequence;
use vektah\parser_combinator\formatter\Closure;

class RepSep extends Parser
{
    private $root;

    public function __construct($parser, $separator = ',', $allow_trailing = true) {
        $parser = Parser::sanitize($parser);
        $separator = Parser::sanitize($separator);

        $sequence = new Sequence(
            $parser,
            new Many(new Sequence($separator, $parser))
        );

        if ($allow_trailing) {
            $sequence->append(new OptionalChoice($separator));
        }

        $this->root = new Closure(new OptionalChoice($sequence), function($data) {
            $result = [];

            if (is_array($data)) {
                $result[] = $data[0];

                foreach ($data[1] as $datum) {
                    $result[] = $datum[1];
                }
            }

            return $result;
        });

        $this->root->setName('repsep');
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
