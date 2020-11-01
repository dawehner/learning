<?php

namespace vektah\parser_combinator\combinator;


use vektah\parser_combinator\Input;
use vektah\parser_combinator\exception\GrammarException;

class Not extends Combinator
{
    public function parse(Input $input)
    {
        if ($input->complete()) {
            return $input->errorHere('Not cannot be matched at end of stream.')->addParser($this);
        }

        // Initially no parser should match, this would be a zero width result
        if ($this->matches($input)) {
            return $input->errorHere("Found a match at the start of the source {$input->get()}")->addParser($this);
        }

        $consumed = '';
        while (!$this->matches($input) && !$input->complete()) {
            $consumed .= $input->peek(0);
            $input->consume(1);
        }

        return $input->matchHere([$consumed])->addParser($this);
    }

    private function matches(Input $input) {
        $initialOffset = $input->getOffset();

        foreach ($this->getParsers() as $parser) {
            // Keep trying each of the parsers until one matches.

            $result = $parser->parse($input);

            // Errors and positive results will stop us from searching.
            if ($result->match) {
                if ($initialOffset === $input->getOffset()) {
                    throw new GrammarException('There was a zero width match inside a Not parser.');
                }

                $input->setOffset($initialOffset);
                return true;
            }

            // To be safe we rewind the input after each attempt.
            $input->setOffset($initialOffset);
        }

        return false;
    }
}
