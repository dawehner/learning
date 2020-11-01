<?php

namespace vektah\parser_combinator\combinator;

use vektah\parser_combinator\Input;

class Sequence extends Combinator
{
    public function parse(Input $input)
    {
        $aggregatedData = [];
        $isPositive = false;
        $initialOffset = $input->getOffset();

        foreach ($this->getParsers() as $parser) {
            $result = $parser->parse($input);

            if ($result->hasData) {
                $aggregatedData[] = $result->data;
            }

            // Any single positive match in the sequence makes the whole sequence positive.
            if ($result->positiveMatch) {
                $isPositive = true;
            }

            // Any single error causes the whole sequence to error
            if ($result->errorMessage) {
                $result->positiveMatch = $isPositive;

                if ($aggregatedData) {
                    $result->errorMessage .= "\nPrevious tokens:\n";

                    foreach ($aggregatedData as $token) {
                        if (is_callable([$token, '__toString'])) {
                            $result->errorMessage .= ' - ' . $token->__toString() . "\n";
                        } else {
                            $result->errorMessage .= ' - ' . print_r($token, true) . "\n";
                        }
                    }
                }
                return $result->addParser($this);
            }
        }

        if ($aggregatedData === []) {
            return $input->nonCapturingMatchHere($isPositive, $initialOffset)->addParser($this);
        } else {
            return $input->matchHere($aggregatedData, $isPositive, $initialOffset)->addParser($this);
        }
    }
}
