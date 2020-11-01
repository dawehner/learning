<?php


namespace vektah\parser_combinator\test\language\php\annotation;

use PHPUnit_Framework_TestCase as TestCase;
use vektah\parser_combinator\language\php\annotation\ConstLookup;
use vektah\parser_combinator\language\php\annotation\DoctrineAnnotation;
use vektah\parser_combinator\language\php\annotation\NonDoctrineAnnotation;
use vektah\parser_combinator\language\php\annotation\PhpAnnotationParser;

class PhpAnnotationParserTest extends TestCase
{
    /** @var PhpAnnotationParser */
    private $parser;

    public function setUp()
    {
        $this->parser = new PhpAnnotationParser();
    }

    public function testAnnotationWithoutBrackets()
    {
        $this->assertEquals([new DoctrineAnnotation('InheritanceType')], $this->parser->parseString('@InheritanceType'));
    }

    public function testAnnotationWithBrackets()
    {
        $this->assertEquals([new DoctrineAnnotation('InheritanceType')], $this->parser->parseString('@InheritanceType()'));
    }

    public function testAnnotationWithString()
    {
        $this->assertEquals([new DoctrineAnnotation('InheritanceType', ['value' => 'JOINED'])], $this->parser->parseString('@InheritanceType("JOINED")'));
    }

    public function testMixedArguments()
    {
        $this->assertEquals(
            [new DoctrineAnnotation('RequestMapping', ['value' => '/deposit/bank', 'method' => new ConstLookup('POST', 'HttpMethod')])],
            $this->parser->parseString('@RequestMapping("/deposit/bank", method = HttpMethod::POST)')
        );

    }

    public function testAnnotationWithEmptyArray()
    {
        $this->assertEquals(
            [new DoctrineAnnotation('InheritanceType', ['value' => []])],
            $this->parser->parseString('@InheritanceType({})')
        );
    }

    public function testAnnotationWithConstant()
    {
        $this->assertEquals(
            [new DoctrineAnnotation('InheritanceType', ['value' => new ConstLookup('BAR')])],
            $this->parser->parseString('@InheritanceType(BAR)')
        );
    }

    public function testAnnotationWithClassConstant()
    {
        $this->assertEquals(
            [new DoctrineAnnotation('InheritanceType', ['value' => new ConstLookup('BAR', 'Foo')])],
            $this->parser->parseString('@InheritanceType(Foo::BAR)')
        );
    }

    public function testAnnotationWithClassConstantInNamedParam()
    {
        $this->assertEquals(
            [new DoctrineAnnotation('InheritanceType', ['foo' => new ConstLookup('BAR', 'Foo')])],
            $this->parser->parseString('@InheritanceType(foo = Foo::BAR)')
        );
    }

    public function testAnnotationWithClassConstantsInArray()
    {
        $this->assertEquals(
            [new DoctrineAnnotation('InheritanceType', ['value' => [new ConstLookup('BAR', 'Foo'), new ConstLookup('BAZ', 'Foo')]])],
            $this->parser->parseString('@InheritanceType({Foo::BAR, Foo::BAZ})')
        );
    }

    public function testAnnotationWithArray()
    {
        $this->assertEquals(
            [new DoctrineAnnotation('InheritanceType', ['value' => ['email']])],
            $this->parser->parseString('@InheritanceType({"email"})')
        );
    }

    public function testAnnotationWithHash()
    {
        $this->assertEquals(
            [new DoctrineAnnotation('InheritanceType', ['value' => ['name' => 'email']])],
            $this->parser->parseString('@InheritanceType({"name" = "email"})')
        );
    }

    public function testNonDoctrineAnnotation()
    {
        $this->assertEquals(
            [new NonDoctrineAnnotation('param', 'foo $bar A FooBar')],
            $this->parser->parseString('@param foo $bar A FooBar')
        );
    }

    public function testDoctrineAnnotationWithDocCommentTags()
    {
        $this->assertEquals(
            [new DoctrineAnnotation('InheritanceType')],
            $this->parser->parseString('/** @InheritanceType */')
        );
    }

    public function testNonDoctrineAnnotationWithDocCommentTags()
    {
        $this->assertEquals(
            [new NonDoctrineAnnotation('param', 'foo $bar A FooBar')],
            $this->parser->parseString('/** @param foo $bar A FooBar */')
        );
    }

    public function testDerpstyleParams()
    {
        $this->assertEquals(
            [new NonDoctrineAnnotation('param[in]', 'foo $bar A FooBar')],
            $this->parser->parseString('/** @param[in] foo $bar A FooBar */')
        );
    }

    public function testMixedDocBlock()
    {
        $doc = '/**
             * Displays and processes the add form for a new Customer.
             *
             * @RequestMapping("/add")
             * @Permissions("customer.add")
             * A second comment
             * @param foo $bar
             *              Some more comments
             */';

        $this->assertEquals(
            [
                'Displays and processes the add form for a new Customer.',
                new DoctrineAnnotation('RequestMapping', ['value' => '/add'], 4),
                new DoctrineAnnotation('Permissions', ['value' => 'customer.add'], 5),
                'A second comment',
                new NonDoctrineAnnotation('param', "foo \$bar\n                           Some more comments", 7),
            ],
            $this->parser->parseString($doc)
        );
    }

    public function testEmptyDocBlock()
    {
        $doc = '/**
             *
             */';

        $this->assertEquals([], $this->parser->parseString($doc));
    }
}
