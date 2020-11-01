<?php

namespace vektah\parser_combinator\combinator;

use vektah\parser_combinator\Input;

/**
 * Matches any one of the parsers
 */
class OptionalChoice extends Combinator
{
    public function parse(Input $input)
    {
        $initialOffset = $input->getOffset();

        foreach ($this->getParsers() as $parser) {
            // Keep trying each of the parsers until one matches.

            $result = $parser->parse($input);

            // Stop on the first match
            if (!$result->errorMessage || $result->hasData) {
                return $result->addParser($this);
            }

            // To be safe we rewind the input after each attempt.
            $input->setOffset($initialOffset);
        }

        return $input->matchHere(null)->addParser($this);
    }
}
