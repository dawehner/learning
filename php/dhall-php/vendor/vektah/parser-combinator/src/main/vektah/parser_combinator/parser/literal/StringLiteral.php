<?php

namespace vektah\parser_combinator\parser\literal;

use vektah\parser_combinator\Input;
use vektah\parser_combinator\Result;
use vektah\parser_combinator\combinator\Many;
use vektah\parser_combinator\combinator\Sequence;
use vektah\parser_combinator\formatter\Closure;
use vektah\parser_combinator\formatter\Concatenate;
use vektah\parser_combinator\parser\Parser;
use vektah\parser_combinator\parser\PositiveMatch;

class StringLiteral extends Parser
{

    private $root;

    public function __construct()
    {
        $this->root = new Closure(
            new Sequence('"', PositiveMatch::instance(), new Concatenate(new Many('[^\n\r\f"\\\\]+', '\\\\[bnrf"\\\\]')), '"'),
            function($data) {
                $str = str_replace('\"', '"', $data[1]);
                return preg_replace_callback('/\\\\([\\\\a-zA-Z0-9])/', function($matches) {
                    // TODO: There must be a better way to do this...
                    return eval("return \"\\$matches[1]\";");
                }, $str);
            }
        );
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
