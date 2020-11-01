<?php

namespace vektah\parser_combinator\exception;


use Exception;
use vektah\parser_combinator\parser\Parser;

class GrammarException extends Exception
{
    /**
     * Workaround until php 5.5's class operator is implemented.
     *
     * @see http://www.php.net/manual/en/language.oop5.basic.php#language.oop5.basic.class.class
     */
    const _CLASS = __CLASS__;

    public function __construct($message) {
        parent::__construct($message . "\nParser Stack:\n" . Parser::getParserStack());
    }
}
