<?php

namespace vektah\parser_combinator;

use vektah\parser_combinator\exception\ParseException;

/**
 * Input to a parser. As this gets handed off to sub parsers the offset and limit will be modified to avoid
 * copying the string (which could be very large)
 */
class Input
{
    private $string;
    private $offset;
    private $strlen;

    /** @var array A list of all of the line endings in the string where the key is the line number and the value is the
     *             position within the string that line starts. */
    private $lineEndings = [];

    public function __construct($string, $offset = 0)
    {
        $this->offset = $offset;
        $this->string = $string;
        $this->strlen = strlen($string);

        // Magical first line (There is always one line in a file.
        $this->lineEndings[] = -1;
        $last = chr(0);
        $lines = 1;

        for ($i = 0; $i < $this->strlen; $i++) {
            $ch = $string[$i];

            if ($ch == "\n" || ($ch == "\r" && $last != "\n")) {
                $this->lineEndings[] = $i;
                $lines++;
            }

            $last = $ch;
        }

        // If there is no final newline add one.
        if ($this->lineEndings[count($this->lineEndings) - 1] != $this->strlen - 1) {
            $this->lineEndings[] = $this->strlen - 1;
        }
    }

    /**
     * Uses a prebuilt line ending array to do fast offset -> line lookups
     *
     * @param int $offset
     * @return int
     */
    public function getLine($offset)
    {
        $num_lines = count($this->lineEndings);

        // If there is only one line then we are on it.
        if ($num_lines == 0 || $num_lines == 1) {
            return 1;
        }

        if ($offset > $this->strlen - 1) {
            $offset = $this->strlen - 1;
        } elseif ($offset < 0) {
            $offset = 0;
        }

        $index_min = 0;
        $index_max = $num_lines - 1;

        while (true) {
            $index_mid = (int)floor(($index_min + $index_max) / 2);
            $value = $this->lineEndings[$index_mid];

            if ($value < $offset) {
                if ($offset < $this->lineEndings[$index_mid + 1]) {
                    return $index_mid + 1;
                }

                $index_min = $index_mid + 1;
            } elseif ($offset < $value) {
                if ($this->lineEndings[$index_mid] < $offset) {
                    return $index_mid - 1;
                }

                $index_max = $index_mid - 1;
            } else {
                return $index_mid;
            }
        }
    }

    public function getLineOffset($line)
    {
        return $this->lineEndings[$line - 1];
    }

    /**
     * @param string $string            A string to check against the current input
     * @param bool $case_sensitive    If true compare case sensitively
     * @return bool true if the input starts with the given string
     */
    public function startsWith($string, $case_sensitive = true)
    {
        $len = strlen($string);
        if ($len > $this->strlen - $this->offset) {
            return false;
        }

        return substr_compare($this->string, $string, $this->offset, $len, !$case_sensitive) === 0;
    }

    /**
     * @return string the character at offset $i from the current position
     */
    public function peek($i = 0)
    {
        if ($this->offset + $i >= $this->strlen) {
            return null;
        }
        return $this->string[$this->offset + $i];
    }

    public function match($pattern, &$matches)
    {
        try {
            return preg_match($pattern, $this->string, $matches, 0, $this->offset) === 1;
        } catch (\Exception $e) {
            throw new ParseException("Regex failed to compile '$pattern': {$e->getMessage()}");
        }
    }

    /**
     * @param int $limit
     * `
     * @return string the current input.
     */
    public function get($limit = null)
    {
        if ($limit === null) {
            $limit = $this->strlen;
        }
        return substr($this->string, $this->offset, $limit);
    }

    /**
     * @param int $bytes the number of bytes to consume
     */
    public function consume($bytes)
    {
        $this->offset += $bytes;
    }

    public function getAndConsume($bytes) {
        $result = substr($this->string, $this->offset, $bytes);
        $this->offset += $bytes;

        return $result;
    }

    public function getOffset()
    {
        return $this->offset;
    }

    public function setOffset($offset)
    {
        $this->offset = $offset;
    }

    public function getPositionDescription($offset = null)
    {
        if ($offset === null) {
            $offset = $this->offset;
        }

        $line = $this->getLine($offset);
        $offset = $this->offset - $this->getLineOffset($line);
        return 'line ' . $line . ' offset ' . $offset;
    }

    public function errorHere($message, $positive = false, $offset = null) {
        $result = new Result();
        $result->errorMessage = $message;
        $result->hasData = false;
        $result->match = false;
        $result->positiveMatch = $positive;
        $result->offset = $offset === null ? $this->offset : $offset;

        return $result;
    }

    public function matchHere($data, $positive = false, $offset = null)
    {
        $result = new Result();
        $result->hasData = true;
        $result->data = $data;
        $result->match = true;
        $result->positiveMatch = $positive;
        $result->offset = $offset === null ? $this->offset : $offset;

        return $result;
    }

    public function nonCapturingMatchHere($positive = false, $offset = null)
    {
        $result = new Result();
        $result->match = true;
        $result->hasData = false;
        $result->positiveMatch = $positive;
        $result->offset = $offset === null ? $this->offset : $offset;

        return $result;
    }

    public function complete()
    {
        return $this->offset >= $this->strlen;
    }

    public function strlen()
    {
        return $this->strlen;
    }
}
