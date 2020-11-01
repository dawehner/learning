<?php

namespace vektah\parser_combinator\test\parser;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\Input;
use vektah\parser_combinator\parser\CharParser;

class CharParserTest extends TestCase
{
    public function testMatch()
    {
        $matcher = new CharParser(" ");

        $input = new Input('    asdf');
        $this->assertEquals(' ', $matcher->parse($input)->data);

        $this->assertEquals(1, $input->getOffset());
    }

    public function testNotMatch()
    {
        $matcher = new CharParser("x");

        $input = new Input('    asdf');
        $this->assertEquals('', $matcher->parse($input)->data);

        $this->assertEquals(0, $input->getOffset());
    }
}
