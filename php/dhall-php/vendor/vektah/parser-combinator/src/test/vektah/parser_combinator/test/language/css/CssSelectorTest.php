<?php


namespace vektah\parser_combinator\test\language\css;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\Input;
use vektah\parser_combinator\language\css\CssObject;
use vektah\parser_combinator\language\css\CssSelectorParser;

/**
 * These tests are taken from http://www.w3.org/Style/CSS/Test/CSS3/Selectors/current/html/index.html
 */
class CssSelectorTest extends TestCase
{
    /** @var CssSelectorParser */
    private $parser;

    public function setUp() {
        $this->parser = new CssSelectorParser();
    }

    private function assertMatch($selector, $object_definition) {
        $ast = $this->parser->parseString($selector);

        $this->assertEquals($selector, $ast->toCss());

        $this->assertTrue($ast->matches($object_definition), "$ast does not match $object_definition");
    }

    private function assertNotMatch($selector, $object_definition) {
        $ast = $this->parser->parseString($selector);

        $this->assertEquals($selector, $ast->toCss());

        $this->assertFalse($ast->matches($object_definition), "$ast matches $object_definition");
    }

    public function testElement()
    {
        $input = 'p';

        $ast = $this->parser->parseString($input);

        $this->assertEquals($input, $ast->toCss());
        $this->assertEquals('Element(p)', $ast->__toString());
        $this->assertEquals(new CssObject(['element' => 'p']), $ast->define());


        $this->assertTrue($ast->matches('p'));
        $this->assertFalse($ast->matches('li'));
    }

    public function testGroups()
    {
        $input = 'li, p';

        $ast = $this->parser->parseString($input);

        $this->assertEquals($input, $ast->toCss());
        $this->assertEquals('Any(Element(li), Element(p))', $ast->__toString());
        $this->assertEquals(new CssObject(['children' => [new CssObject(['element' => 'li']), new CssObject(['element' => 'p'])]]), $ast->define());

        $this->assertTrue($ast->matches('p'));
        $this->assertTrue($ast->matches('li'));
        $this->assertFalse($ast->matches('div'));
    }

    public function testUniversalNoNamespace()
    {
        $input = '*';

        $ast = $this->parser->parseString($input);

        $this->assertEquals($input, $ast->toCss());
        $this->assertEquals('Universal()', $ast->__toString());

        $this->assertEquals(new CssObject(), $ast->define());

        $this->assertTrue($ast->matches('p'));
        $this->assertTrue($ast->matches('li'));
    }

    public function testUniversalOmitted()
    {
        $input = '#foo';

        $ast = $this->parser->parseString($input);
        $this->assertEquals(new CssObject(['id' => 'foo']), $ast->define());

        $this->assertEquals($input, $ast->toCss());
        $this->assertEquals('Hash(foo)', $ast->__toString());

        $this->assertTrue($ast->matches('p#foo'));
        $this->assertTrue($ast->matches('li#foo'));
    }

    public function testAttribute()
    {
        $input = 'p[title]';


        $ast = $this->parser->parseString($input);

        $this->assertEquals(new CssObject(['element' => 'p', 'attributes' => ['title' => true]]), $ast->define());

        $this->assertEquals($input, $ast->toCss());
        $this->assertEquals('All(Element(p), Attribute(title))', $ast->__toString());

        $this->assertTrue($ast->matches('p[title]'));
        $this->assertTrue($ast->matches("p[title='foo']"));
    }

    public function testAttributeEquals()
    {
        $input = "address[title='foo']";


        $ast = $this->parser->parseString($input);

        $this->assertEquals(new CssObject(['element' => 'address', 'attributes' => ['title' => 'foo']]), $ast->define());

        $this->assertEquals($input, $ast->toCss());
        $this->assertEquals("All(Element(address), Attribute(title='foo'))", $ast->__toString());

        $this->assertTrue($ast->matches("address[title='foo']"));
    }

    public function testMultipleAttributeEquals()
    {
        $input = "p[class~='foo']";
        $ast = $this->parser->parseString($input);

        $this->assertEquals(new CssObject(['element' => 'p', 'attributes' => ['class' => 'foo']]), $ast->define());

        $this->assertEquals($input, $ast->toCss());
        $this->assertEquals("All(Element(p), Attribute(class~='foo'))", $ast->__toString());

        $this->assertTrue($ast->matches("p[class='bar foo baz']"));
        $this->assertFalse($ast->matches("p[class='bar foobies baz']"));
    }

    /**
     * Section 6.3.1: Represents the att attribute whose value is a
     * space-separated list of words, one of which is exactly "val". If this
     * selector is used, the words in the value must not contain spaces
     * (since they are separated by spaces).
     */
    public function testMultipleAttributeEquals2()
    {

        $input = "p[title~='hello world']";
        $ast = $this->parser->parseString($input);

        $this->assertEquals(new CssObject(['element' => 'p', 'attributes' => ['title' => 'hello world']]), $ast->define());

        $this->assertEquals($input, $ast->toCss());
        $this->assertEquals("All(Element(p), Attribute(title~='hello world'))", $ast->__toString());

        $this->assertFalse($ast->matches("p[class='hello world']"));
    }

    public function testAttributeValueSelectorsWithHyphens()
    {
        $input = "p[lang|='en']";
        $ast = $this->parser->parseString($input);

        $this->assertEquals(new CssObject(['element' => 'p', 'attributes' => ['lang' => 'en']]), $ast->define());

        $this->assertEquals($input, $ast->toCss());
        $this->assertEquals("All(Element(p), Attribute(lang|='en'))", $ast->__toString());

        $this->assertTrue($ast->matches("p[lang='en-gb']"));
        $this->assertFalse($ast->matches("p[lang='fr-en']"));
    }

    public function testSubstringMatchingAtBeginning()
    {
        $input = "p[title^='foo']";
        $ast = $this->parser->parseString($input);

        $this->assertEquals(new CssObject(['element' => 'p', 'attributes' => ['title' => 'foo']]), $ast->define());

        $this->assertEquals($input, $ast->toCss());
        $this->assertEquals("All(Element(p), Attribute(title^='foo'))", $ast->__toString());

        $this->assertTrue($ast->matches("p[title='foobar']"));
        $this->assertFalse($ast->matches("p[title='barfoo']"));
    }

    public function testSubstringMatchingAtEnd()
    {
        $input = "p[title$='bar']";
        $ast = $this->parser->parseString($input);

        $this->assertEquals(new CssObject(['element' => 'p', 'attributes' => ['title' => 'bar']]), $ast->define());

        $this->assertEquals($input, $ast->toCss());
        $this->assertEquals("All(Element(p), Attribute(title$='bar'))", $ast->__toString());

        $this->assertTrue($ast->matches("p[title='foobar']"));
        $this->assertFalse($ast->matches("p[title='barfoo']"));
    }

    public function testSubstringContains()
    {
        $input = "p[title*='bar']";
        $ast = $this->parser->parseString($input);

        $this->assertEquals(new CssObject(['element' => 'p', 'attributes' => ['title' => 'bar']]), $ast->define());

        $this->assertEquals($input, $ast->toCss());
        $this->assertEquals("All(Element(p), Attribute(title*='bar'))", $ast->__toString());

        $this->assertTrue($ast->matches("p[title='foobarbaz']"));
        $this->assertFalse($ast->matches("p[title='foo']"));
    }

    public function testClassSelectors()
    {
        $this->assertMatch('.t1', '.t1');
        $this->assertMatch('li.t1', 'li.t1');
        $this->assertNotMatch('p.te', 'p.test');
    }

    public function testMultipleClassSelectors()
    {
        $this->assertMatch('p.t1', 'p.t1.t2');
        $this->assertMatch('p.t2', 'p.t1.t2');
        $this->assertMatch('p.te.st', 'p.te.st');
        $this->assertMatch('p.t5.t5', 'p.t5');

        $this->assertNotMatch('p.te.st', 'p.test');
        $this->assertNotMatch('p.t1.fail', 'p.t1');
        $this->assertNotMatch('p.fail.t1', 'p.t1');
    }

    public function testNegated()
    {
        $this->assertMatch('div:not(.t1)', 'div.t2');

        $this->assertNotMatch('.t1:not(.t2)', 'p.t1.t2');
        $this->assertNotMatch('.t2:not(.t1)', 'p.t1.t2');
        $this->assertNotMatch(':not(.t1).t2', 'p.t1.t2');
        $this->assertNotMatch(':not(.t2):not(.t2)', 'p.t1.t2');
    }

    public function testID()
    {
        $input = "#t1";
        $ast = $this->parser->parseString($input);

        $this->assertEquals(new CssObject(['id' => 't1']), $ast->define());

        $this->assertEquals($input, $ast->toCss());
        $this->assertEquals("Hash(t1)", $ast->__toString());

        $this->assertTrue($ast->matches("p#t1"));
        $this->assertFalse($ast->matches("p#t2"));
    }

    public function testMultipleIDSelectors()
    {
        $this->assertMatch('p', 'p#test');
        $this->assertMatch('#pass#pass', '#pass');

        $this->assertNotMatch('#test#fail', '#test');
        $this->assertNotMatch('#test#fail', '#fail');
    }

    public function testPseudoClass()
    {
        $this->assertMatch('*:link', 'a');

        $this->assertNotMatch('*:hover', 'a');
        $this->assertNotMatch('*:visited', 'a');
        $this->assertNotMatch('*:active', 'a');
    }

    public function testLangPseudoClass()
    {
        $this->assertMatch('li:lang(en-GB)', 'li[lang="en-GB"]');
        $this->assertMatch('li:lang(en-GB)', 'li[lang="en-GB-wa"]');

        $this->assertNotMatch('li:lang(en-GB)', 'li[lang="en-US"]');
        $this->assertNotMatch('li:lang(en-GB)', 'li[lang="fr"]');
    }

    public function testMultipleMatches()
    {
        $this->assertMatch('table.foo', 'table.foo, table.bar');
        $this->assertMatch('table.bar', 'table.foo, table.bar');
    }

    public function testEnabledPseudoClass()
    {
        $this->assertMatch('li:enabled', 'li');
        $this->assertNotMatch('li:enabled', 'li[disabled]');
        $this->assertNotMatch('li:enabled', 'li:disabled');
    }

    public function testDisabledPseudoClass()
    {
        $this->assertMatch('li:disabled', 'li[disabled]');
        $this->assertMatch('li:disabled', 'li:disabled');
        $this->assertNotMatch('li:disabled', 'li');
    }

    public function testCheckedPseudoClass()
    {
        $this->assertMatch('li:checked', 'li[checked]');
        $this->assertMatch('li:checked', 'li:checked');
        $this->assertNotMatch('li:checked', 'li');
    }

    public function testPseudoWithChildren() {
        $this->assertMatch('a.button:link td', 'a.button td');
    }

    public function testRoot()
    {
        $this->assertMatch('*:root', 'li');
    }

    public function testDescendantCombinator()
    {
        $input = "div.t1 p";
        $ast = $this->parser->parseString($input);

        $this->assertEquals(new CssObject(['element' => 'div', 'classes' => ['t1'], 'children' => [new CssObject(['element' => 'p'])]]), $ast->define());

        $this->assertEquals($input, $ast->toCss());
        $this->assertEquals("Descendant(All(Element(div), Class(t1)), Element(p))", $ast->__toString());

        $this->assertTrue($ast->matches("div.t1 p"));
        $this->assertTrue($ast->matches("div.t1 a p"));
    }

    public function testDoubleNot()
    {
        $this->assertNotMatch('p:not(:not(p))', 'p');
    }

    public function testDoesNotConsumeBlocks()
    {
        $input = new Input('h1 {}');
        $result = $this->parser->parse($input)->data;

        $this->assertEquals('Element(h1)', $result->__toString());
        $this->assertEquals(' {}', $input->get());
    }
}
