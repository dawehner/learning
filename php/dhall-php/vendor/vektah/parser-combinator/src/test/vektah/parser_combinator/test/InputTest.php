<?php

namespace vektah\parser_combinator\test;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\Input;

class InputTest extends TestCase
{
    public function testStartsWithAtStart()
    {
        $input = new Input('asdf');

        $this->assertTrue($input->startsWith('asdf'));
        $this->assertTrue($input->startsWith('asd'));
        $this->assertTrue($input->startsWith('as'));
        $this->assertTrue($input->startsWith('a'));
    }

    public function testStartsWithAtOffset()
    {
        $input = new Input('asdf', 1);

        $this->assertTrue($input->startsWith('sdf'));
    }

    public function testDoesNotStartWith()
    {
        $input = new Input('asdf');

        $this->assertFalse($input->startsWith('_asdf'));
        $this->assertFalse($input->startsWith('asdf_'));
    }

    public function testDoesNotStartWithAtOffset()
    {
        $input = new Input('asdf', 1);

        $this->assertFalse($input->startsWith('_sdf'));
        $this->assertFalse($input->startsWith('sdf_'));
        $this->assertFalse($input->startsWith('df_'));
    }

    public function testMatchesAtStart()
    {
        $input = new Input('asdf____');

        $this->assertTrue($input->match('/^asdf/', $matches));
        $this->assertEquals(['asdf'], $matches);
    }

    public function testMatchesAtOffset()
    {
        $input = new Input('____asdf____', 4);

        $this->assertTrue($input->match('/\Gasdf/', $matches));
        $this->assertEquals(['asdf'], $matches);
    }

    public function testBoundedMatchesAtOffset()
    {
        $input = new Input('____asdf____', 4);

        $this->assertTrue($input->match('/\Gasdf/', $matches, 4));
        $this->assertEquals(['asdf'], $matches);
    }

    public function testBadMatchAtOffset()
    {
        $input = new Input('____asdf____', 4);

        $this->assertFalse($input->match('/\G____/', $matches));
    }

    public function testGet()
    {
        $input = new Input('asdf');

        $this->assertEquals('asdf', $input->get());
    }

    public function getAtOffset()
    {
        $input = new Input('asdf', 1);

        $this->assertEquals('sdf', $input->get());
    }

    public function testGetWithLimit()
    {
        $input = new Input('asdf', 1);

        $this->assertEquals('sd', $input->get(2));
    }

    public function testSetOffset()
    {
        $input = new Input('asdf');
        $input->setOffset(2);
        $this->assertEquals(2, $input->getOffset());
        $this->assertEquals('df', $input->get());
    }

    public function testLines()
    {
        $input = new Input("____\n____\n____\n____");

        $this->assertEquals(1, $input->getLine(0));
        $this->assertEquals(1, $input->getLine(1));
        $this->assertEquals(1, $input->getLine(2));
        $this->assertEquals(1, $input->getLine(3));
        $this->assertEquals(1, $input->getLine(4));     // 4th character is the newline itself

        $this->assertEquals(2, $input->getLine(5));
        $this->assertEquals(2, $input->getLine(6));
        $this->assertEquals(2, $input->getLine(7));
        $this->assertEquals(2, $input->getLine(8));
        $this->assertEquals(2, $input->getLine(9));     // The second newline

        $this->assertEquals(3, $input->getLine(10));
        $this->assertEquals(3, $input->getLine(11));
        $this->assertEquals(3, $input->getLine(12));
        $this->assertEquals(3, $input->getLine(13));
        $this->assertEquals(3, $input->getLine(14));

        $this->assertEquals(4, $input->getLine(15));
        $this->assertEquals(4, $input->getLine(16));
        $this->assertEquals(4, $input->getLine(17));
        $this->assertEquals(4, $input->getLine(18));
        $this->assertEquals(4, $input->getLine(19));
    }

    public function testPeek()
    {
        $input = new Input('1234');

        $this->assertEquals('1', $input->peek(0));
        $this->assertEquals('2', $input->peek(1));
        $this->assertEquals('3', $input->peek(2));
        $this->assertEquals('4', $input->peek(3));

        $this->assertEquals(0, $input->getOffset());
    }
}
