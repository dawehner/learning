<?php

namespace vektah\parser_combinator\test\combinator;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\Input;
use vektah\parser_combinator\combinator\Choice;

class ChoiceTest extends TestCase
{
    public function testSingleOneOf()
    {
        $combinator = new Choice(['asdf']);

        $this->assertEquals('asdf', $combinator->parse(new Input("asdf"))->data);
    }

    public function testMultiple()
    {
        $combinator = new Choice(['asdf', 'hjkl']);

        $this->assertEquals('hjkl', $combinator->parse(new Input("hjkl"))->data);
    }
}
