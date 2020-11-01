<?php

namespace vektah\parser_combinator\test\language;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\Input;
use vektah\parser_combinator\language\JsonParser;

class JsonParserTest extends TestCase
{
    public function testTypes()
    {
        $parser = new JsonParser();
        $this->assertSame('asdf', $parser->parse(new Input('"asdf"')));
        $this->assertSame(11, $parser->parse(new Input('11')));
        $this->assertSame(11.11, $parser->parse(new Input('11.11')));
        $this->assertSame(true, $parser->parse(new Input('true')));
        $this->assertSame(false, $parser->parse(new Input('false')));
        $this->assertSame(null, $parser->parse(new Input('null')));
        $this->assertSame(['asdf', 'hjkl'], $parser->parse(new Input('["asdf", "hjkl"]')));
        $this->assertSame(['asdf' => 'hjkl'], $parser->parse(new Input('{"asdf": "hjkl"}')));
        $this->assertSame(['asdf' => 'hjkl', 'a' => 'b'], $parser->parse(new Input('{"asdf": "hjkl", "a": "b"}')));
        $this->assertSame([['asdf' => 'asdf', 'qwer' => 'qwer']], $parser->parse(new Input('[{"asdf": "asdf", "qwer": "qwer"}]')));
    }

    public function testComposerJson()
    {
        $parser = new JsonParser();
        $file = file_get_contents(__DIR__ . '/../../../../../../composer.json');

        $expected = json_decode($file, JSON_OBJECT_AS_ARRAY);

        $this->assertEquals($expected, $parser->parse(new Input($file)));
    }
}
