<?php

namespace vektah\parser_combinator\test\parser;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\Input;
use vektah\parser_combinator\parser\RepSep;

class RepSepTest extends TestCase
{
    public function testNone()
    {
        $many = new RepSep('asdf');

        $this->assertEquals([], $many->parse(new Input('qqqq'))->data);
    }

    public function testOne()
    {
        $many = new RepSep('asdf');

        $this->assertEquals(['asdf'], $many->parse(new Input('asdf'))->data);
    }

    public function testMany()
    {
        $many = new RepSep('asdf');

        $this->assertEquals(['asdf', 'asdf'], $many->parse(new Input('asdf,asdf'))->data);
    }

    public function testTrailingAlllowed()
    {
        $many = new RepSep('asdf');
        $input = new Input('asdf,asdf,');

        $this->assertEquals(['asdf', 'asdf'], $many->parse($input)->data);

        $this->assertTrue($input->complete());
    }

    public function testTrailingNotAlllowed()
    {
        $many = new RepSep('asdf', ',', false);
        $input = new Input('asdf,asdf,');

        $this->assertEquals(['asdf', 'asdf'], $many->parse($input)->data);

        $this->assertFalse($input->complete());
    }
}
