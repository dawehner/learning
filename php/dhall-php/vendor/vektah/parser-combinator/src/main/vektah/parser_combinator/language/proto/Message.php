<?php

namespace vektah\parser_combinator\language\proto;

class Message
{
    /** @var string */
    public $name;

    /** @var array */
    public $members;

    /** @var Field[] */
    public $fields = [];

    /** @var Option[] */
    public $options = [];

    public function __construct($name, $members)
    {
        $this->name = $name;
        $this->members = $members;

        foreach ($members as $member) {
            if ($member instanceof Field) {
                $this->fields[] = $member;
            }

            if ($member instanceof Option) {
                $this->options[] = $member;
            }
        }
    }
}
