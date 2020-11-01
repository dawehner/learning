<?php

namespace vektah\parser_combinator\test\combinator;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\Input;
use vektah\parser_combinator\combinator\Sequence;
use vektah\parser_combinator\formatter\Ignore;

class SequenceTest extends TestCase
{
    public function testSequenceOfOne()
    {
        $parser = new Sequence(['asdf']);

        $this->assertEquals(['asdf'], $parser->parse(new Input('asdf'))->data);
    }

    public function testSequenceOfTwo()
    {
        $parser = new Sequence(['asdf', 'hjkl']);

        $this->assertEquals(['asdf', 'hjkl'], $parser->parse(new Input('asdfhjkl'))->data);
    }

    public function testMismatch()
    {
        $parser = new Sequence(['asdf', 'hjkl']);
        $result = $parser->parse(new Input('asdfasdf'));
        $this->assertEquals("Expected regex '~\\Ghjkl~' to match 'asdf', it does not.\nPrevious tokens:\n - asdf\n", $result->errorMessage);
        $this->assertEquals(4, $result->offset);
    }

    public function testNonCapturingChild()
    {
        $parser = new Sequence('asdf', new Ignore('hjkl'));

        $this->assertEquals(['asdf'], $parser->parse(new Input('asdfhjkl'))->data);
    }

    public function testAllNonCapturingChildren()
    {
        $parser = new Sequence(new Ignore('asdf'), new Ignore('hjkl'));

        $this->assertFalse($parser->parse(new Input('asdfhjkl'))->hasData);
    }
}
