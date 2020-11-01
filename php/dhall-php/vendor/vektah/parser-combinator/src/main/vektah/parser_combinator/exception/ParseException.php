<?php
namespace vektah\parser_combinator\exception;

use Exception;

class ParseException extends Exception
{
    /**
     * Workaround until php 5.5's class operator is implemented.
     *
     * @see http://www.php.net/manual/en/language.oop5.basic.php#language.oop5.basic.class.class
     */
    const _CLASS = __CLASS__;
}
