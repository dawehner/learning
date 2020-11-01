<?php

namespace vektah\parser_combinator\combinator;

use vektah\parser_combinator\Input;

/**
 * Matches any one of the parsers
 */
class Choice extends Combinator
{
    public function parse(Input $input)
    {
        $initialOffset = $input->getOffset();

        foreach ($this->getParsers() as $parser) {
            // Keep trying each of the parsers until one matches.

            $result = $parser->parse($input);

            // Errors and positive results will stop us from searching.
            if (!$result->errorMessage || $result->positiveMatch) {
                return $result->addParser($this);
            }

            // To be safe we rewind the input after each attempt.
            $input->setOffset($initialOffset);
        }

        return $input->errorHere("Could not find any options that match {$input->get()}")->addParser($this);
    }
}
