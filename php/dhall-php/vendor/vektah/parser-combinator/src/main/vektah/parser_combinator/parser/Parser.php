<?php

namespace vektah\parser_combinator\parser;


use vektah\parser_combinator\Input;
use vektah\parser_combinator\Result;
use vektah\parser_combinator\combinator\Sequence;
use vektah\parser_combinator\exception\ParseException;
use vektah\parser_combinator\formatter\Closure;

abstract class Parser
{
    private $name;

    /**
     * Parse the given input
     *
     * @param Input $input
     *
     * @return Result result
     */
    abstract public function parse(Input $input);

    public static function sanitize($parser) {
        if (is_string($parser)) {
            if (strlen($parser) === 1) {
                $parser = new CharParser($parser);
            } else {
                $parser = new RegexParser($parser);
            }
        }

        return $parser;
    }

    /**
     * @param string $input
     *
     * @throws ParseException
     * @return mixed
     */
    public function parseString($input) {
        $input = new Input($input);
        $parser = new Closure(new Sequence($this, new EofParser()), function($data) {
            return $data[0];
        });

        $result = $parser->parse($input);

        if ($result->errorMessage) {
            $location = '';

            if ($result->offset) {
                $location = 'At ' . $input->getPositionDescription($result->offset) . ': ';
            }
            throw new ParseException($location . $result->errorMessage . "\nParser Stack:\n - " . $result->getParserStack() . "\n");
        }
        return $result->data;
    }

    public function getName() {
        if (!$this->hasName()) {
            return join('', array_slice(explode('\\', get_class($this)), -1));
        }

        return $this->name;
    }

    public function hasName() {
        return $this->name;
    }

    public function setName($name) {
        $this->name = $name;
    }

    /**
     * @return Parser[]
     */
    public static function getActiveParsers() {
        $parsers = [];
        $lastParser = null;
        $trace = debug_backtrace();

        foreach ($trace as $frame) {
            if (isset($frame['object']) && $frame['object'] instanceof Parser && $frame['object'] !== $lastParser) {
                $lastParser = $parsers[] = $frame['object'];
            }
        }

        return $parsers;
    }

    public static function getParserStack()
    {
        $message = '';
        foreach (self::getActiveParsers() as $parser) {
            if ($parser->hasName()) {
                $message .= ' - #' . $parser->getName() . "\n";
            } else {
                $message .= ' - ' . $parser->getName() . "\n";
            }

        }

        return substr($message, 0, strlen($message) - 1);
    }

    public static function getInlineParserStack()
    {
        $message = '';
        foreach (array_reverse(self::getActiveParsers()) as $parser) {
            $message .= $parser->getName(). '.';
        }

        return substr($message, 0, strlen($message) - 1);
    }
}
