<?php

namespace vektah\parser_combinator\parser;

use vektah\parser_combinator\Input;

class EofParser extends Parser
{
    use SingletonTrait;

    public function parse(Input $input)
    {
        if (!$input->complete()) {
            return $input->errorHere("Unable to process {$input->get()}")->addParser($this);
        }

        return $input->nonCapturingMatchHere()->addParser($this);
    }
}
