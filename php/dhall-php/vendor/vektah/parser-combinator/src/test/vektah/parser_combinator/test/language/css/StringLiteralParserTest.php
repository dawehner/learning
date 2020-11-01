<?php


namespace vektah\parser_combinator\test\language\css;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\Input;
use vektah\parser_combinator\language\css\StringLiteralParser;

class StringLiteralParserTest extends TestCase
{
    /** @var StringLiteralParser */
    private $parser;

    public function setUp() {
        $this->parser = new StringLiteralParser();
    }

    public function testDoubleQuotedString() {
        $token = $this->parser->parse(new Input('"f\'o\'o"'));

        $this->assertEquals("f'o'o", $token->data);
    }

    public function testSingleQuotedString() {
        $token = $this->parser->parse(new Input("'f\"o\"o'"));

        $this->assertEquals('f"o"o', $token->data);
    }
}
