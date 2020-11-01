<?php

namespace vektah\parser_combinator\test\combinator;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\Input;
use vektah\parser_combinator\combinator\Not;
use vektah\parser_combinator\exception\GrammarException;

class NotTest extends TestCase
{
    public function testSingle()
    {
        $not = new Not(['asdf']);

        $this->assertEquals(['hjkl'], $not->parse(new Input('hjklasdf'))->data);
    }

    public function testMultiple() {
        $not = new Not(['a', 'b']);

        $this->assertEquals(['qqqq'], $not->parse(new Input('qqqqa'))->data);
        $this->assertEquals(['qqqq'], $not->parse(new Input('qqqqb'))->data);
    }

    public function testInputReset() {
        $not = new Not(['a']);
        $input = new Input('zzzabbb');

        $this->assertEquals(['zzz'], $not->parse($input)->data);
        $this->assertEquals('abbb', $input->get());
    }

    public function testNotChar() {
        $not = new Not('[abcde]+');
        $input = new Input('zzzabbb');

        $this->assertEquals(['zzz'], $not->parse($input)->data);
        $this->assertEquals('abbb', $input->get());
    }

    public function testNotCharZeroWidth() {
        $not = new Not('');
        $input = new Input('zzzabbb');

        $this->setExpectedException(GrammarException::_CLASS);
        $not->parse($input)->data;
    }

    public function testEmpty() {
        $not = new Not('a');
        $input = new Input('a');
        $result = $not->parse($input);

        $this->assertNull($result->data);
        $this->assertNotNull($result->errorMessage);
    }
}
