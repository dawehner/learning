<?php

namespace vektah\parser_combinator\test\parser;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\Input;
use vektah\parser_combinator\parser\CharRangeParser;

class CharRangeParserTest extends TestCase
{
    public function testMatchAlphaNum()
    {
        $parser = new CharRangeParser(['a' => 'z', 'A' => 'Z', '0' => '9']);

        $this->assertEquals('asdZ0239', $parser->parse(new Input('asdZ0239-'))->data);
        $this->assertEquals('asdf', $parser->parse(new Input('asdf-1234-'))->data);
    }

    public function testBoundaries()
    {
        $parser = new CharRangeParser(['a' => 'f', 'A' => 'F', '0' => '9']);

        $this->assertEquals('FFFF00', $parser->parse(new Input('FFFF00'))->data);
        $this->assertEquals('FF', $parser->parse(new Input('FFGGG00'))->data);
    }

    public function testMatchChars()
    {
        $parser = new CharRangeParser(['0' => '9', ['asdf']]);

        $this->assertEquals('asdf0123', $parser->parse(new Input('asdf0123ee'))->data);
        $this->assertEquals('asdf', $parser->parse(new Input('asdf-1234-'))->data);
    }

    public function testConsumed()
    {
        $input = new Input('asdf00');
        $parser = new CharRangeParser(['0' => '9', ['asdf']]);

        $this->assertEquals('asdf00', $parser->parse($input)->data);

        $this->assertTrue($input->complete());
    }

    public function testAsciiRange()
    {
        $parser = new CharRangeParser(["\0" => "\177"]);

        $this->assertEquals('asdf0123ee', $parser->parse(new Input('asdf0123ee'))->data);
        $this->assertEquals('asdf-1234-', $parser->parse(new Input('asdf-1234-'))->data);
    }

    public function testMinError()
    {
        $parser = new CharRangeParser(['a' => 'z', 'A' => 'Z', '0' => '9'], 1);
        $result = $parser->parse(new Input('-----'));

        $this->assertNull($result->data);
        $this->assertNotNull($result->errorMessage);
    }
}
