<?php

namespace vektah\parser_combinator\test\parser\literal;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\Input;
use vektah\parser_combinator\parser\literal\IntLiteral;

class IntLiteralTest extends TestCase
{
    public function testDecimal()
    {
        $parser = new IntLiteral();

        $this->assertEquals(-12, $parser->parse(new Input('-12'))->data);
        $this->assertEquals(12, $parser->parse(new Input('12'))->data);
        $this->assertEquals(0, $parser->parse(new Input('0'))->data);
        $this->assertEquals(1234567, $parser->parse(new Input('1234567'))->data);
    }

    public function testHex()
    {
        $parser = new IntLiteral();

        $this->assertEquals(0x10, $parser->parse(new Input('0x10'))->data);
        $this->assertEquals(0xFF, $parser->parse(new Input('0xFF'))->data);
        $this->assertEquals(0xFFFF, $parser->parse(new Input('0xFFFF'))->data);
    }

    public function testOctal()
    {
        $parser = new IntLiteral();

        $this->assertEquals(000, $parser->parse(new Input('000'))->data);
        $this->assertEquals(010, $parser->parse(new Input('010'))->data);
        $this->assertEquals(077, $parser->parse(new Input('077'))->data);
    }
}
