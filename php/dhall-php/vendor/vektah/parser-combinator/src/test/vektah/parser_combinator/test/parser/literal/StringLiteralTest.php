<?php

namespace vektah\parser_combinator\test\parser\literal;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\Input;
use vektah\parser_combinator\parser\literal\StringLiteral;

class StringLiteralTest extends TestCase
{
    public function testBasic()
    {
        $parser = new StringLiteral();

        $this->assertEquals('asdf', $parser->parse(new Input('"asdf"'))->data);
    }

    public function testEscaping()
    {
        $parser = new StringLiteral();

        $this->assertEquals("\b", $parser->parse(new Input('"\b"'))->data);
        $this->assertEquals('"', $parser->parse(new Input('"\""'))->data);
        $this->assertEquals('\\', $parser->parse(new Input('"\\\\"'))->data);
    }
}
