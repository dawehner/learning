<?php

namespace vektah\parser_combinator\test\parser;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\Input;
use vektah\parser_combinator\parser\RegexParser;

class RegexParserTest extends TestCase
{
    public function testMatch()
    {
        $parser = new RegexParser('asdf', 'i');
        $this->assertEquals('asdf', $parser->parse(new Input('asdf'))->data);
        $this->assertEquals('Asdf', $parser->parse(new Input('Asdf'))->data);
    }

    public function testNonMatching()
    {
        $parser = new RegexParser('asdf', 'i');

        $result = $parser->parse(new Input('fff'));
        $this->assertEquals("Expected regex '~\\Gasdf~i' to match 'fff', it does not.", $result->errorMessage);
        $this->assertEquals(0, $result->offset);
    }
}
