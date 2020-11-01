<?php

namespace vektah\parser_combinator\parser;

use vektah\parser_combinator\exception\GrammarException;

class CharRangeParser extends RegexParser
{
    public function __construct(array $ranges, $min = null, $max = null, $capture = true)
    {
        if (!is_numeric($min) && !is_null($min)) {
            throw new GrammarException('Min must be numeric');
        }

        if (!is_numeric($max) && !is_null($max)) {
            throw new GrammarException('Max must be numeric');
        }

        if (!is_bool($capture)) {
            throw new GrammarException('Capture must be a boolean');
        }

        $regex = '[';

        foreach ($ranges as $first => $last) {
            // If the value is an array with one value then treat it as a sequence of chars instead of a range.
            if (is_array($last)) {
                if (!is_string($last[0])) {
                    throw new GrammarException('CharRangeParsers may contain array values containing only strings!');
                }
                $regex .= $last[0];
            } else {
                $first = dechex(ord($first));
                $last = dechex(ord($last));
                $regex .= "\\x$first-\\x$last";
            }
        }

        if (!$min) {
            $min = 0;
        }

        $regex .= ']{' . $min . ',' . $max . '}';

        parent::__construct($regex);
    }
}
