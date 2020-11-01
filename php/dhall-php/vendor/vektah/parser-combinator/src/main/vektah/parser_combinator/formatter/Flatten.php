<?php

namespace vektah\parser_combinator\formatter;

use vektah\parser_combinator\Input;
use vektah\parser_combinator\parser\Parser;

class Flatten extends Parser
{
    private $parser;

    public function __construct(Parser $parser)
    {
        $this->parser = $parser;
    }

    public function parse(Input $input)
    {
        $original_result = $this->parser->parse($input)->addParser($this);

        if (is_array($original_result->data)) {
            $result = array();
            array_walk_recursive(
                $original_result->data,
                function ($v, $k) use (&$result) {
                    $result[] = $v;
                }
            );

            $original_result->data = $result;

            return $original_result;
        } else {
            return $original_result;
        }
    }
}
