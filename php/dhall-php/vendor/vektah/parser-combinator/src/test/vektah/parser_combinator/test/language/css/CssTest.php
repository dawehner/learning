<?php


namespace vektah\parser_combinator\test\language\css;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\language\css\CssDeclaration;
use vektah\parser_combinator\language\css\CssParser;

class CssTest extends TestCase
{
    /** @var CssParser */
    private $parser;

    public function setUp() {
        $this->parser = new CssParser();
    }

    public function testEmptyFile()
    {
        $css = $this->parser->parseString('');

        $this->assertEquals([], $css->getDeclarations('h1'));
    }

    public function testEmptyRule()
    {
        $css = $this->parser->parseString('h1{}');

        $this->assertEquals([], $css->getDeclarations('h1'));
        $this->assertEquals('h1{}', $css->toCss());
    }

    public function testSimpleRule()
    {
        $css = $this->parser->parseString('h1 { color: brown }');
        $this->assertEquals(['color' => new CssDeclaration('color', 'brown')], $css->getDeclarations('h1'));
        $this->assertEquals('h1{color:brown;}', $css->toCss());
    }

    public function testSimpleRuleWithTrailingSemicolon()
    {
        $css = $this->parser->parseString('h1 { color: brown; }');
        $this->assertEquals(['color' => new CssDeclaration('color', 'brown')], $css->getDeclarations('h1'));
        $this->assertEquals('h1{color:brown;}', $css->toCss());
    }

    public function testMultipleRules()
    {
        $css = $this->parser->parseString('h1 { color: brown; background-color: #00f; }');
        $this->assertEquals(['color' => new CssDeclaration('color', 'brown'), 'background-color' => new CssDeclaration('background-color', '#00f')], $css->getDeclarations('h1'));
        $this->assertEquals('h1{color:brown;background-color:#00f;}', $css->toCss());
    }

    public function testComments()
    {
        $css = $this->parser->parseString('/*h1 { color: brown; background-color: #00f; } */');
        $this->assertEquals([], $css->getDeclarations('h1'));
    }

    public function testPercentage()
    {
        $css = $this->parser->parseString('h1 { -ms-text-size-adjust: 32.14157654%; }');
        $this->assertEquals(['-ms-text-size-adjust' => new CssDeclaration('-ms-text-size-adjust', '32.14157654%')], $css->getDeclarations('h1'));
        $this->assertEquals('h1{-ms-text-size-adjust:32.14157654%;}', $css->toCss());
    }

    public function testInteger()
    {
        $css = $this->parser->parseString('h1 { width: 10; }');
        $this->assertEquals(['width' => new CssDeclaration('width', '10')], $css->getDeclarations('h1'));
        $this->assertEquals('h1{width:10;}', $css->toCss());
    }

    public function testFloat()
    {
        $css = $this->parser->parseString('h1 {line-height: 1.3;}');
        $this->assertEquals(['line-height' => new CssDeclaration('line-height', '1.3')], $css->getDeclarations('h1'));
        $this->assertEquals('h1{line-height:1.3;}', $css->toCss());
    }

    public function testDimension()
    {
        $css = $this->parser->parseString('h1 {width: 10px;}');
        $this->assertEquals(['width' => new CssDeclaration('width', '10px')], $css->getDeclarations('h1'));
        $this->assertEquals('h1{width:10px;}', $css->toCss());
    }

    public function testFunction()
    {
        $css = $this->parser->parseString('h1 {box-shadow: 0 4px 3px -1px rgba(0, 0, 0, 0.3);}');

        $this->assertEquals(['box-shadow' => new CssDeclaration('box-shadow', '0 4px 3px -1px rgba(0,0,0,0.3)')], $css->getDeclarations('h1'));
        $this->assertEquals('h1{box-shadow:0 4px 3px -1px rgba(0,0,0,0.3);}', $css->toCss());
    }

    public function testNestedFunctions()
    {
        $this->parser->parseString('h1 {background-image: -webkit-gradient(linear, 50% 0%, 50% 100%, color-stop(0%, #ffe400), color-stop(100%, #ffc000));}');
    }

    public function testImportant()
    {
        $css = $this->parser->parseString('h1 { -ms-text-size-adjust: 100% !important}');
        $this->assertEquals(['-ms-text-size-adjust' => new CssDeclaration('-ms-text-size-adjust', '100%', true)], $css->getDeclarations('h1'));
        $this->assertEquals('h1{-ms-text-size-adjust:100%!important;}', $css->toCss());
    }

    public function testMultipleMatches()
    {
        $css = $this->parser->parseString('table.header {padding: 10px 0 5px;} table.columns{ margin: 0 auto; }');
        $this->assertEquals('padding:10px 0 5px;margin:0 auto;', $css->getMatchingCss('table.header, table.columns'));
    }
}
