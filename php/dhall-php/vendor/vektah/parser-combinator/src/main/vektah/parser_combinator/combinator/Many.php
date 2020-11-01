<?php

namespace vektah\parser_combinator\combinator;


use vektah\parser_combinator\Input;
use vektah\parser_combinator\exception\GrammarException;

class Many extends Choice
{
    private $min;
    private $max;

    public function __construct($parsers = null, $min = 0, $max = null)
    {
        if (!is_array($parsers)) {
            $parsers = func_get_args();
        } else {
            if (!is_numeric($min) && !is_null($min)) {
                throw new GrammarException('Min must be numeric');
            }

            if (!is_numeric($max) && !is_null($max)) {
                throw new GrammarException('Max must be numeric');
            }

            $this->min = $min;
            $this->max = $max;
        }

        parent::__construct($parsers);
    }

    public function parse(Input $input)
    {
        $aggregatedResult = [];
        $isPositive = false;
        $count = 0;

        $lastOffset = $input->getOffset();
        while ($this->max === null || $count < $this->max) {
            $result = parent::parse($input)->addParser($this);

            if ($result->positiveMatch) {
                $isPositive = true;
            }

            if ($result->errorMessage) {
                if ($count < $this->min) {
                    return $input->errorHere("Expected {$this->min} elements, but found $count");
                } elseif ($result->positiveMatch) {
                    // If a branch got far enough to assert itself but still returned an error then we need to propagate
                    return $result;
                }

                // Most errors are normal, they just signal the end of the repetition.
                break;
            }

            if ($input->getOffset() == $lastOffset) {
                throw new GrammarException('At ' . $input->getPositionDescription() . ": parser {$result->getParserStack()} did not consume any input, this is a bug with the grammar. Make sure there are no zero width matches in a Many. The result in  was {$result}");
            }

            if ($result->hasData) {
                $aggregatedResult[] = $result->data;
            }

            $lastOffset++;
            $count++;
        }

        if ($count < $this->min) {
            return $input->errorHere("Expected {$this->min} elements, but found $count", $isPositive)->addParser($this);
        }

        return $input->matchHere($aggregatedResult, $isPositive);
    }
}
