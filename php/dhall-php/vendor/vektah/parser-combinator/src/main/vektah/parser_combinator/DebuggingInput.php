<?php

namespace vektah\parser_combinator;

use vektah\parser_combinator\parser\Parser;

/**
 * Subclasses Input to give detailed debugging information about which parsers are consuming input.
 */
class DebuggingInput extends Input
{
    private $log = '';

    private $parsers = [];

    private function log($message) {
        file_put_contents('/tmp/log', $message . "\n", FILE_APPEND);
    }

    private function &getParserData() {
        $parser = Parser::getInlineParserStack();

        if (!isset($this->parsers[$parser])) {
            $this->parsers[$parser] = [
                'name' => $parser,
                'consumed' => 0,
                'rewound' => 0,
            ];
        }

        return $this->parsers[$parser];
    }

    public function getAndConsume($bytes) {
        $this->log(Parser::getInlineParserStack() . " - consume($bytes, '{$this->get($bytes)}')");
        $this->getParserData()['consumed'] += $bytes;
        return parent::getAndConsume($bytes);
    }

    public function consume($bytes)
    {
        $this->log(Parser::getInlineParserStack() . " - consume($bytes, '{$this->get($bytes)}')");
        $this->getParserData()['consumed'] += $bytes;
        parent::consume($bytes);
    }

    public function setOffset($offset)
    {
        $diff = abs($this->getOffset() - $offset);
        if ($offset < $this->getOffset()) {
            $this->getParserData()['rewound'] += $diff;
            $this->log(Parser::getInlineParserStack() . " - rewind($diff)");
        } elseif ($offset > $this->getOffset()) {
            $this->getParserData()['consumed'] += $diff;
            $this->log(Parser::getInlineParserStack() . " - consume($diff, '{$this->get($diff)}')");
        }

        parent::setOffset($offset);
    }

    public function topConsumers()
    {
        usort($this->parsers, function($a, $b) {
            return $a['consumed'] < $b['consumed'];
        });

        return $this->parsers;
    }

    public function topBackTrackers()
    {
        usort($this->parsers, function($a, $b) {
            return $a['rewound'] < $b['rewound'];
        });

        return $this->parsers;
    }

    /**
     * @return string
     */
    public function getLog()
    {
        return $this->log;
    }
}
