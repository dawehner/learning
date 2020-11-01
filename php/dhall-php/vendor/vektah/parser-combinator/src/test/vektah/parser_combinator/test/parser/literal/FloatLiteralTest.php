<?php

namespace vektah\parser_combinator\test\parser\literal;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\Input;
use vektah\parser_combinator\parser\literal\FloatLiteral;

class FloatLiteralTest extends TestCase
{
    public function testNormalNotationNoSuffix()
    {
        $parser = new FloatLiteral(false, false);

        $this->assertEquals(1.23, $parser->parse(new Input('1.23'))->data);
        $this->assertEquals(1.0, $parser->parse(new Input('1.0'))->data);

        $input = new Input('1.2f');
        $this->assertEquals(1.2, $parser->parse($input)->data);

        $this->assertEquals(3, $input->getOffset());
        $this->assertEquals('f', $input->get());
    }

    public function testSciNotation()
    {
        $parser = new FloatLiteral();

        $this->assertEquals(1200, $parser->parse(new Input('1.2e3'))->data);
        $this->assertEquals(1200, $parser->parse(new Input('1.2e+3'))->data);
        $this->assertEquals(0.0012, $parser->parse(new Input('1.2e-3'))->data);
        $this->assertEquals(0.001, $parser->parse(new Input('1e-3'))->data);
    }

    public function testWithOptionalSuffix()
    {
        $parser = new FloatLiteral(true, false);

        $this->assertEquals(1.23, $parser->parse(new Input('1.23'))->data);
        $this->assertEquals(1.0, $parser->parse(new Input('1.0'))->data);
        $this->assertEquals(1.23, $parser->parse(new Input('1.23f'))->data);
        $this->assertEquals(1.0, $parser->parse(new Input('1.0f'))->data);
    }
}
